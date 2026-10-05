/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use std::collections::{HashMap, HashSet};
use std::path::Path;

use thrustc_compile_time::BuiltinValue;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

pub(crate) fn append_imported_top_level_declarations(
    ctx: &thrustc_c_import_synthesis::context::CImportContext,
    span: Span,
    out: &mut String,
    warnings: &mut Vec<CompilationIssue>,
) {
    {
        let mut structs: Vec<&thrustc_c_import_synthesis::model::CImportedStruct> =
            ctx.structs().iter().collect();

        structs.sort_by(|a, b| a.name().cmp(b.name()));

        for s in structs {
            let structure_modificator: &thrustc_typesystem::type_modificators::StructureTypeModificator =
                s.metadata().get_struct_type_modificator();

            out.push_str("struct ");
            out.push_str(s.name());
            out.push_str(" @public");

            if structure_modificator.llvm().is_packed() {
                out.push_str(" @packed");
            }

            if let Some(align) = structure_modificator.llvm().align() {
                out.push_str(" @align(");
                out.push_str(&align.to_string());
                out.push(')');
            }

            out.push_str(" {\n");

            for (idx, (field_name, field_type)) in s.fields().iter().enumerate() {
                out.push_str("    ");
                out.push_str(field_name);
                out.push_str(": ");
                out.push_str(&crate::format_type_thrust(field_type));

                if idx + 1 != s.fields().len() {
                    out.push(',');
                }

                out.push('\n');
            }

            out.push_str("}\n\n");
        }
    }

    {
        let mut enums: Vec<&thrustc_c_import_synthesis::model::CImportedEnum> =
            ctx.enums().iter().collect();

        enums.sort_by(|a, b| a.name().cmp(b.name()));

        for e in enums {
            out.push_str("enum ");
            out.push_str(e.name());
            out.push_str(" @public {\n");

            for (name, value) in e.fields() {
                out.push_str("    ");
                out.push_str(name);
                out.push_str(": ");
                out.push_str(&crate::format_type_thrust(e.underlying_type()));
                out.push_str(" = ");
                out.push_str(&value.to_string());
                out.push_str(";\n");
            }

            out.push_str("}\n\n");
        }
    }

    {
        let mut typedefs: Vec<&thrustc_c_import_synthesis::model::CImportedTypedef> =
            ctx.typedefs().iter().collect();

        typedefs.sort_by(|a, b| a.name().cmp(b.name()));

        for t in typedefs.iter() {
            out.push_str("type ");
            out.push_str(t.name());
            out.push_str(" @public = ");
            out.push_str(&crate::format_type_thrust(t.ty()));
            out.push_str(";\n");
        }

        if !typedefs.is_empty() {
            out.push('\n');
        }
    }

    {
        let mut consts: Vec<&thrustc_c_import_synthesis::model::CImportedConstant> =
            ctx.constants().iter().collect();

        consts.sort_by(|a, b| a.name().cmp(b.name()));

        for c in consts.iter() {
            out.push_str("const ");
            out.push_str(c.name());
            out.push_str(": ");
            out.push_str(&crate::format_type_thrust(c.kind()));
            out.push_str(" @public = ");

            let expr: String = crate::format_builtin_value_thrust(c.value(), c.kind(), warnings);

            out.push_str(&expr);
            out.push_str(";\n");
        }

        if !consts.is_empty() {
            out.push('\n');
        }
    }

    {
        let mut funcs: Vec<&thrustc_c_import_synthesis::model::CImportedFunction> =
            ctx.functions().iter().collect();

        funcs.sort_by(|a, b| a.name().cmp(b.name()));

        for f in funcs {
            let parameter_names: &[String] = f.parameter_names();
            let parameter_types: &[Type] = f.parameter_types();

            out.push_str("fn ");
            out.push_str(f.name());
            out.push('(');

            for idx in 0..parameter_names.len() {
                if idx != 0 {
                    out.push_str(", ");
                }

                let name: &str = parameter_names
                    .get(idx)
                    .map(String::as_str)
                    .unwrap_or("arg");
                let fallback_ty: Type = Type::Void { span };
                let ty: &Type = parameter_types.get(idx).unwrap_or(&fallback_ty);

                out.push_str(name);
                out.push_str(": ");
                out.push_str(&crate::format_type_thrust(ty));
            }

            out.push_str(") ");
            out.push_str(&crate::format_type_thrust(f.return_type()));
            out.push_str(" @public");
            out.push_str(" @extern(\"");
            out.push_str(f.external_name());
            out.push_str("\")");
            out.push_str(" @convention(\"");
            out.push_str(f.convention());
            out.push_str("\")");

            if f.variadic() {
                out.push_str(" @arbitraryArgs @noArgCount");
            }

            out.push_str(";\n");
        }
    }
}

pub(crate) fn append_translated_top_level_declarations(
    root: &clang::Entity<'_>,
    canonical_input: &Path,
    out: &mut String,
    warnings: &mut Vec<CompilationIssue>,
    span: Span,
) -> Result<(), CompilationIssue> {
    let entities: Vec<clang::Entity<'_>> = root.get_children();

    let mut macro_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut typedef_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut global_var_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut record_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut union_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut enum_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut function_definition_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut function_prototype_decls: Vec<clang::Entity<'_>> = Vec::new();

    let mut global_decl_indexes: HashMap<clang::Entity<'_>, usize> = HashMap::new();

    for e in entities.into_iter() {
        if !crate::entity_originates_in_main_file(&e, canonical_input) {
            continue;
        }

        match e.get_kind() {
            clang::EntityKind::StructDecl if e.is_definition() => record_decls.push(e),
            clang::EntityKind::UnionDecl if e.is_definition() => union_decls.push(e),
            clang::EntityKind::EnumDecl if e.is_definition() => enum_decls.push(e),
            clang::EntityKind::TypedefDecl => {
                typedef_decls.push(e);

                if let Some(underlying) = e.get_typedef_underlying_type() {
                    let canonical: clang::Type<'_> = underlying.get_canonical_type();

                    if let Some(decl) = canonical.get_declaration() {
                        match (canonical.get_kind(), decl.get_kind()) {
                            (clang::TypeKind::Record, clang::EntityKind::StructDecl)
                                if decl.is_definition() =>
                            {
                                record_decls.push(decl)
                            }
                            (clang::TypeKind::Record, clang::EntityKind::UnionDecl)
                                if decl.is_definition() =>
                            {
                                union_decls.push(decl)
                            }
                            (clang::TypeKind::Enum, _) if decl.is_definition() => {
                                enum_decls.push(decl)
                            }
                            _ => {}
                        }
                    }
                }
            }
            clang::EntityKind::FunctionDecl if e.is_definition() => {
                function_definition_decls.push(e)
            }
            clang::EntityKind::FunctionDecl => function_prototype_decls.push(e),
            clang::EntityKind::MacroDefinition => macro_decls.push(e),
            clang::EntityKind::VarDecl => {
                let canonical: clang::Entity<'_> = e.get_canonical_entity();
                let initializer_present: bool = self::find_var_initializer(&e).is_some();
                let storage_class: Option<clang::StorageClass> = e.get_storage_class();

                let candidate_score: u8 = if initializer_present { 4 } else { 0 }
                    + if !matches!(storage_class, Some(clang::StorageClass::Extern)) {
                        2
                    } else {
                        0
                    }
                    + if e.is_definition() { 1 } else { 0 };

                if let Some(index) = global_decl_indexes.get(&canonical).copied() {
                    let current_initializer_present: bool =
                        self::find_var_initializer(&global_var_decls[index]).is_some();
                    let current_storage_class: Option<clang::StorageClass> =
                        global_var_decls[index].get_storage_class();

                    let current_score: u8 = if current_initializer_present { 4 } else { 0 }
                        + if !matches!(current_storage_class, Some(clang::StorageClass::Extern)) {
                            2
                        } else {
                            0
                        }
                        + if global_var_decls[index].is_definition() {
                            1
                        } else {
                            0
                        };

                    if candidate_score > current_score {
                        global_var_decls[index] = e;
                    }
                } else {
                    global_decl_indexes.insert(canonical, global_var_decls.len());
                    global_var_decls.push(e);
                }
            }

            _ => {}
        }
    }

    record_decls.sort_by_key(|a| a.get_name());
    record_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    union_decls.sort_by_key(|a| a.get_name());
    union_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    enum_decls.sort_by_key(|a| a.get_name());
    enum_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    function_definition_decls.sort_by_key(|a| a.get_name());
    function_definition_decls
        .dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    function_prototype_decls.sort_by_key(|a| a.get_name());
    function_prototype_decls
        .dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    macro_decls.sort_by_key(|a| a.get_name());
    macro_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    global_var_decls.sort_by_key(|a| a.get_name());

    typedef_decls.sort_by_key(|a| a.get_name());
    typedef_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    let mut definition_entities: HashSet<clang::Entity<'_>> = HashSet::new();
    let mut enum_name_overrides: HashMap<clang::Entity<'_>, String> = HashMap::new();

    let mut emitted_typedefs: usize = 0;
    let mut emitted_constants: usize = 0;
    let mut emitted_prototypes: usize = 0;

    for definition in function_definition_decls.iter() {
        definition_entities.insert(definition.get_canonical_entity());
    }

    for typedef_decl in typedef_decls.iter() {
        if let Some(name) = typedef_decl.get_name()
            && let Some(underlying) = typedef_decl.get_typedef_underlying_type()
        {
            let canonical: clang::Type<'_> = underlying.get_canonical_type();

            if canonical.get_kind() == clang::TypeKind::Enum
                && let Some(decl) = canonical.get_declaration()
            {
                enum_name_overrides
                    .entry(decl.get_canonical_entity())
                    .or_insert(name);
            }
        }
    }

    for record in record_decls.iter() {
        let src: String = self::translate_record_decl(record, "struct", span)?;

        out.push_str(&src);
        out.push('\n');
    }

    for union_decl in union_decls.iter() {
        let src: String = self::translate_record_decl(union_decl, "union", span)?;

        out.push_str(&src);
        out.push('\n');
    }

    for enum_decl in enum_decls.iter() {
        let fallback_name: Option<&str> = enum_name_overrides
            .get(&enum_decl.get_canonical_entity())
            .map(String::as_str);
        let src: String = self::translate_enum_decl(enum_decl, fallback_name, span)?;

        out.push_str(&src);
        out.push('\n');
    }

    for typedef_decl in typedef_decls.iter() {
        let Some(name) = typedef_decl.get_name() else {
            continue;
        };

        let Some(underlying) = typedef_decl.get_typedef_underlying_type() else {
            continue;
        };

        let alias_name: String = crate::sanitize_identifier_for_thrust(&name);
        let canonical: clang::Type<'_> = underlying.get_canonical_type();

        let alias_target: Option<String> = if canonical.get_kind() == clang::TypeKind::Record {
            if let Some(decl) = canonical.get_declaration() {
                if let Some(record_name) = decl.get_name() {
                    let record_name: String = crate::sanitize_identifier_for_thrust(&record_name);

                    if record_name == alias_name {
                        None
                    } else {
                        Some(record_name)
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else if canonical.get_kind() == clang::TypeKind::Enum {
            if let Some(decl) = canonical.get_declaration() {
                let enum_name: Option<String> = decl.get_name().or_else(|| {
                    enum_name_overrides
                        .get(&decl.get_canonical_entity())
                        .map(String::to_string)
                });

                if let Some(enum_name) = enum_name {
                    let enum_name: String = crate::sanitize_identifier_for_thrust(&enum_name);

                    if enum_name == alias_name {
                        None
                    } else {
                        Some(enum_name)
                    }
                } else {
                    Some("s32".into())
                }
            } else {
                Some("s32".into())
            }
        } else {
            Some(crate::format_clang_type_thrust(&underlying).map_err(|msg| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported typedef '{alias_name}': {msg}"),
                    None,
                    span,
                )
            })?)
        };

        if let Some(alias_target) = alias_target {
            out.push_str("type ");
            out.push_str(&alias_name);
            out.push_str(" = ");
            out.push_str(&alias_target);
            out.push_str(";\n");

            emitted_typedefs = emitted_typedefs.saturating_add(1);
        }
    }

    if emitted_typedefs > 0 {
        out.push('\n');
    }

    {
        for macro_decl in macro_decls.iter() {
            let Some(raw_name) = macro_decl.get_name() else {
                continue;
            };

            let name: String = crate::sanitize_identifier_for_thrust(&raw_name);

            if unsafe { macro_decl.is_function_like_macro_unchecked() } {
                continue;
            }

            if let Some(result) = macro_decl.evaluate() {
                let translated: Option<(Type, BuiltinValue)> = match result {
                    clang::EvaluationResult::SignedInteger(value) => {
                        Some((Type::S64 { span }, BuiltinValue::Integer(value as u64)))
                    }
                    clang::EvaluationResult::UnsignedInteger(value) => {
                        Some((Type::U64 { span }, BuiltinValue::Integer(value)))
                    }
                    clang::EvaluationResult::Float(value) => {
                        Some((Type::F64 { span }, BuiltinValue::Float(value)))
                    }
                    clang::EvaluationResult::String(value)
                    | clang::EvaluationResult::ObjCString(value) => Some((
                        Type::Array {
                            base_type: Box::new(Type::Char { span }),
                            infered_type: None,
                            metadata: thrustc_typesystem::type_metadata::ArrayTypeMetadata::new(
                                None,
                                None,
                            ),
                            span,
                        },
                        BuiltinValue::CString(value.to_bytes().to_vec()),
                    )),
                    _ => None,
                };

                if let Some((kind, builtin_value)) = translated {
                    let kind_text: String = crate::format_type_thrust(&kind);
                    let value_text: String =
                        crate::format_builtin_value_thrust(&builtin_value, &kind, warnings);

                    out.push_str("const ");
                    out.push_str(&name);
                    out.push_str(": ");
                    out.push_str(&kind_text);
                    out.push_str(" = ");
                    out.push_str(&value_text);
                    out.push_str(";\n");

                    emitted_constants = emitted_constants.saturating_add(1);
                }
            }
        }

        if emitted_constants > 0 {
            out.push('\n');
        }
    }

    for global_var_decl in global_var_decls.iter() {
        let src: String = self::translate_global_var_decl(global_var_decl, span)?;

        out.push_str(&src);
        out.push('\n');
    }

    if !global_var_decls.is_empty() {
        out.push('\n');
    }

    for function_prototype in function_prototype_decls.iter() {
        if definition_entities.contains(&function_prototype.get_canonical_entity()) {
            continue;
        }

        let src: String = self::translate_function_prototype(function_prototype, span)?;

        out.push_str(&src);
        out.push('\n');

        emitted_prototypes = emitted_prototypes.saturating_add(1);
    }

    if emitted_prototypes > 0 {
        out.push('\n');
    }

    for f in function_definition_decls.iter() {
        let src: String = self::translate_function(f, span)?;

        out.push_str(&src);
        out.push('\n');
    }

    Ok(())
}

fn translate_function(entity: &clang::Entity<'_>, span: Span) -> Result<String, CompilationIssue> {
    let Some(name) = entity.get_name() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Encountered a function without a name.".into(),
            None,
            span,
        ));
    };

    let name: String = crate::sanitize_identifier_for_thrust(&name);

    let ret_ty: String = entity
        .get_result_type()
        .and_then(|ty| crate::format_clang_type_thrust(&ty).ok())
        .unwrap_or_else(|| "void".into());

    let children: Vec<clang::Entity<'_>> = entity.get_children();
    let body: Option<clang::Entity<'_>> = children
        .iter()
        .find(|child| child.get_kind() == clang::EntityKind::CompoundStmt)
        .copied();

    let mut parameter_names: HashSet<String> = HashSet::new();
    let mut mutated_parameters: HashSet<String> = HashSet::new();

    if let Some(args) = entity.get_arguments() {
        for argument in args.iter() {
            if let Some(argument_name) = argument.get_name() {
                parameter_names.insert(argument_name);
            }
        }
    }

    if let Some(body_entity) = body.as_ref() {
        self::collect_mutated_parameter_names(
            body_entity,
            &parameter_names,
            &mut mutated_parameters,
        );
    }

    let mut params: Vec<String> = Vec::new();
    let mut parameter_locals: Vec<(String, String, String)> = Vec::new();

    if let Some(args) = entity.get_arguments() {
        for (idx, arg) in args.iter().enumerate() {
            let original_name: String = arg.get_name().unwrap_or_else(|| format!("arg{idx}"));
            let param_name: String = crate::sanitize_identifier_for_thrust(&original_name);

            let Some(param_ty) = arg.get_type() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Missing type for parameter '{param_name}' in function '{name}'."),
                    None,
                    span,
                ));
            };

            let ty_text: String =
                crate::format_parameter_type_thrust(&param_ty).map_err(|msg| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Unsupported parameter type for '{param_name}' in '{name}': {msg}"),
                        None,
                        span,
                    )
                })?;

            let needs_local_copy: bool = mutated_parameters.contains(&original_name);
            let needs_local_copy: bool = needs_local_copy
                || param_ty.get_canonical_type().get_kind() == clang::TypeKind::Record;

            if needs_local_copy {
                let signature_name: String = format!("{param_name}_param");
                params.push(format!("{signature_name}: {ty_text}"));
                parameter_locals.push((param_name, signature_name, ty_text));
            } else {
                params.push(format!("{param_name}: {ty_text}"));
            }
        }
    }

    let mut out: String = String::new();

    out.push_str("fn ");
    out.push_str(&name);
    out.push('(');
    out.push_str(&params.join(", "));
    out.push_str(") ");
    out.push_str(&ret_ty);

    if name == "main" {
        out.push_str(" @public");
    }

    out.push_str(" {\n");

    let Some(body) = body else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Missing function body for '{name}'."),
            None,
            span,
        ));
    };

    for (local_name, signature_name, ty_text) in parameter_locals.iter() {
        out.push_str("    var ");
        out.push_str(local_name);
        out.push_str(": ");
        out.push_str(ty_text);
        out.push_str(" = ");
        out.push_str(signature_name);
        out.push_str(";\n");
    }

    if !parameter_locals.is_empty() {
        out.push('\n');
    }

    let mut lines: Vec<String> = Vec::new();

    for child in body.get_children() {
        let translated: Vec<String> =
            crate::stmt::translate_stmt(&child, 1, span, Some(&ret_ty), None)?;

        lines.extend(translated);
    }

    for line in lines.into_iter() {
        out.push_str(&line);
        out.push('\n');
    }

    out.push('}');

    Ok(out)
}

fn translate_function_prototype(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let Some(raw_name) = entity.get_name() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Encountered a function prototype without a name.".into(),
            None,
            span,
        ));
    };

    if matches!(
        entity.get_storage_class(),
        Some(clang::StorageClass::Static | clang::StorageClass::PrivateExtern)
    ) || matches!(
        entity.get_linkage(),
        Some(clang::Linkage::Internal | clang::Linkage::UniqueExternal)
    ) {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!(
                "Static/internal function prototype '{raw_name}' without a body is not supported."
            ),
            None,
            span,
        ));
    }

    let name: String = crate::sanitize_identifier_for_thrust(&raw_name);
    let return_type: String = entity
        .get_result_type()
        .and_then(|ty| crate::format_clang_type_thrust(&ty).ok())
        .unwrap_or_else(|| "void".into());

    let mut parameter_texts: Vec<String> = Vec::new();

    if let Some(arguments) = entity.get_arguments() {
        for (index, argument) in arguments.iter().enumerate() {
            let parameter_name: String = argument
                .get_name()
                .map(|name| crate::sanitize_identifier_for_thrust(&name))
                .unwrap_or_else(|| format!("arg{index}"));

            let Some(parameter_type) = argument.get_type() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Missing type for parameter '{parameter_name}' in '{name}'."),
                    None,
                    span,
                ));
            };

            let parameter_type_text: String =
                crate::format_parameter_type_thrust(&parameter_type).map_err(|msg| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!(
                            "Unsupported parameter type for '{parameter_name}' in '{name}': {msg}"
                        ),
                        None,
                        span,
                    )
                })?;

            parameter_texts.push(format!("{parameter_name}: {parameter_type_text}"));
        }
    }

    let convention: &str = crate::format_clang_calling_convention_thrust(
        entity.get_type().and_then(|ty| ty.get_calling_convention()),
    )
    .map_err(|message| {
        CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Unsupported calling convention for prototype '{name}': {message}"),
            None,
            span,
        )
    })?;

    let variadic: bool = entity.get_type().is_some_and(|ty| ty.is_variadic());
    let mut out: String = String::new();

    out.push_str("fn ");
    out.push_str(&name);
    out.push('(');
    out.push_str(&parameter_texts.join(", "));
    out.push_str(") ");
    out.push_str(&return_type);
    out.push_str(" @public @extern(\"");
    out.push_str(&raw_name);
    out.push_str("\") @convention(\"");
    out.push_str(convention);
    out.push_str("\")");

    if variadic {
        out.push_str(" @arbitraryArgs @noArgCount");
    }

    out.push(';');

    Ok(out)
}

fn translate_global_var_decl(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let Some(raw_name) = entity.get_name() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Encountered a global variable without a name.".into(),
            None,
            span,
        ));
    };

    let name: String = crate::sanitize_identifier_for_thrust(&raw_name);
    let Some(var_type) = entity.get_type() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Missing type for global '{name}'."),
            None,
            span,
        ));
    };

    let canonical_type: clang::Type<'_> = var_type.get_canonical_type();
    let initializer: Option<clang::Entity<'_>> = self::find_var_initializer(entity);
    let initializer_text: Option<String> = if let Some(initializer) = initializer {
        let mut initializer_text: String =
            self::translate_global_initializer(&initializer, &canonical_type, span)?;

        let initializer_kind: clang::EntityKind = initializer.get_kind();
        let scalar_global: bool = matches!(
            canonical_type.get_kind(),
            clang::TypeKind::Bool
                | clang::TypeKind::CharS
                | clang::TypeKind::CharU
                | clang::TypeKind::SChar
                | clang::TypeKind::UChar
                | clang::TypeKind::Short
                | clang::TypeKind::UShort
                | clang::TypeKind::Int
                | clang::TypeKind::UInt
                | clang::TypeKind::Long
                | clang::TypeKind::ULong
                | clang::TypeKind::LongLong
                | clang::TypeKind::ULongLong
                | clang::TypeKind::UInt128
                | clang::TypeKind::Float
                | clang::TypeKind::Double
                | clang::TypeKind::Enum
        );

        if scalar_global
            && matches!(
                initializer_kind,
                clang::EntityKind::BinaryOperator
                    | clang::EntityKind::CompoundAssignOperator
                    | clang::EntityKind::ParenExpr
                    | clang::EntityKind::UnexposedExpr
            )
        {
            let maybe_bitwise_expr: bool = initializer
                .get_range()
                .map(|range| {
                    let tokens: Vec<String> = range
                        .tokenize()
                        .into_iter()
                        .map(|token| token.get_spelling())
                        .collect();

                    tokens.iter().any(|token| {
                        matches!(token.as_str(), "|" | "&" | "^" | "<<" | ">>")
                    })
                })
                .unwrap_or(false);

            if maybe_bitwise_expr {
                let expected_type_text: String =
                    crate::format_clang_type_thrust(&canonical_type).unwrap_or_default();

                if !expected_type_text.is_empty() {
                    initializer_text = format!("({initializer_text}) as {expected_type_text}");
                }
            }
        }

        Some(initializer_text)
    } else if matches!(entity.get_storage_class(), Some(clang::StorageClass::Extern)) {
        None
    } else {
        Some(self::translate_zero_initializer(&canonical_type, span)?)
    };

    let type_text: String = if canonical_type.get_kind() == clang::TypeKind::IncompleteArray {
        if let Some(initializer) = initializer {
            if initializer.get_kind() == clang::EntityKind::InitListExpr {
                let element_type: clang::Type<'_> = canonical_type
                    .get_element_type()
                    .ok_or_else(|| {
                        CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            format!("Incomplete array global '{name}' is missing an element type."),
                            None,
                            span,
                        )
                    })?;
                let element_type_text: String = crate::format_clang_type_thrust(&element_type)
                    .map_err(|msg| {
                        CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            format!("Unsupported element type for global '{name}': {msg}"),
                            None,
                            span,
                        )
                    })?;
                let element_count: usize = initializer.get_children().len();

                format!("array[{element_type_text}; {element_count}]")
            } else {
                crate::format_clang_type_thrust(&var_type).map_err(|msg| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Unsupported type for global '{name}': {msg}"),
                        None,
                        span,
                    )
                })?
            }
        } else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Incomplete array global '{name}' requires an initializer."),
                None,
                span,
            ));
        }
    } else {
        crate::format_clang_type_thrust(&var_type).map_err(|msg| {
            CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Unsupported type for global '{name}': {msg}"),
                None,
                span,
            )
        })?
    };

    let is_mutable: bool = !var_type.is_const_qualified();
    let is_external: bool = !matches!(
        entity.get_storage_class(),
        Some(clang::StorageClass::Static | clang::StorageClass::PrivateExtern)
    ) && !matches!(
        entity.get_linkage(),
        Some(clang::Linkage::Internal | clang::Linkage::UniqueExternal)
    );

    let mut out: String = String::new();

    out.push_str("static ");

    if is_mutable {
        out.push_str("mut ");
    }

    out.push_str(&name);
    out.push_str(": ");
    out.push_str(&type_text);

    if is_external {
        out.push_str(" @public @extern(\"");
        out.push_str(&raw_name);
        out.push_str("\")");
    }

    if let Some(initializer_text) = initializer_text {
        out.push_str(" = ");
        out.push_str(&initializer_text);
    }

    out.push(';');

    Ok(out)
}

pub(crate) fn translate_global_initializer(
    entity: &clang::Entity<'_>,
    expected_type: &clang::Type<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    if entity.get_kind() == clang::EntityKind::CompoundLiteralExpr {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if let Some(value) = children.last() {
            return self::translate_global_initializer(value, expected_type, span);
        }
    }

    if matches!(
        entity.get_kind(),
        clang::EntityKind::GNUNullExpr | clang::EntityKind::NullPtrLiteralExpr
    ) {
        return Ok("nullptr".into());
    }

    if entity.get_kind() == clang::EntityKind::InitListExpr {
        let canonical_type: clang::Type<'_> = expected_type.get_canonical_type();
        let initializer_children: Vec<clang::Entity<'_>> = entity.get_children();

        if initializer_children.len() == 1
            && canonical_type.get_kind() != clang::TypeKind::Record
            && canonical_type.get_kind() != clang::TypeKind::ConstantArray
            && canonical_type.get_kind() != clang::TypeKind::IncompleteArray
        {
            return self::translate_global_initializer(&initializer_children[0], expected_type, span);
        }

        if matches!(
            canonical_type.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        ) {
            let element_type: clang::Type<'_> = canonical_type
                .get_element_type()
                .ok_or_else(|| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Array initializer is missing an element type.".into(),
                        None,
                        span,
                    )
                })?;

            let mut values: Vec<String> = Vec::new();

            for child in initializer_children.iter() {
                values.push(self::translate_global_initializer(child, &element_type, span)?);
            }

            if canonical_type.get_kind() == clang::TypeKind::ConstantArray {
                let size: usize = canonical_type.get_size().ok_or_else(|| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Constant array initializer has unknown size.".into(),
                        None,
                        span,
                    )
                })?;

                if values.len() > size {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Array initializer has more elements than the destination array.".into(),
                        None,
                        span,
                    ));
                }

                while values.len() < size {
                    values.push(self::translate_zero_initializer(&element_type, span)?);
                }
            }

            return Ok(format!("fixed[{}]", values.join(", ")));
        }

        if canonical_type.get_kind() == clang::TypeKind::Record {
            let Some(record_decl) = canonical_type.get_declaration() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Record initializer is missing a declaration.".into(),
                    None,
                    span,
                ));
            };

            if record_decl.get_kind() == clang::EntityKind::UnionDecl {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Union initializers are not supported yet.".into(),
                    None,
                    span,
                ));
            }

            let record_name: String = crate::format_clang_type_thrust(&canonical_type).map_err(|msg| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported record initializer type: {msg}"),
                    None,
                    span,
                )
            })?;
            let record_definition: clang::Entity<'_> = record_decl.get_definition().unwrap_or(record_decl);
            let fields: Vec<clang::Entity<'_>> = record_definition
                .get_children()
                .into_iter()
                .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
                .collect();
            if initializer_children.len() > fields.len() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Record initializer has more values than fields.".into(),
                    None,
                    span,
                ));
            }

            let mut field_initializers: Vec<String> = Vec::new();

            for (field_index, field) in fields.iter().enumerate() {
                let Some(field_name) = field.get_name() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unnamed fields are not supported in record initializers.".into(),
                        None,
                        span,
                    ));
                };

                let Some(field_type) = field.get_type() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Missing type for field '{field_name}' in record initializer."),
                        None,
                        span,
                    ));
                };

                let translated_value: String = if let Some(initializer_child) = initializer_children.get(field_index) {
                    self::translate_global_initializer(initializer_child, &field_type, span)?
                } else {
                    self::translate_zero_initializer(&field_type, span)?
                };

                field_initializers.push(format!(
                    "{}: {}",
                    crate::sanitize_identifier_for_thrust(&field_name),
                    translated_value
                ));
            }

            return Ok(format!("new {record_name} {{ {} }}", field_initializers.join(", ")));
        }

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Unsupported aggregate initializer.".into(),
            None,
            span,
        ));
    }

    let translated: String = crate::expr::translate_expr(entity, span)?;
    let expected_type_text: String = crate::format_clang_type_thrust(expected_type).unwrap_or_default();

    if expected_type_text.contains("ptr[char]")
        && (translated.starts_with('"') || translated.starts_with("n#\""))
    {
        return Ok(format!("({translated}) as {expected_type_text}"));
    }

    Ok(translated)
}

pub(crate) fn translate_zero_initializer(
    ty: &clang::Type<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    match canonical.get_kind() {
        clang::TypeKind::Void => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Cannot zero-initialize a void object.".into(),
            None,
            span,
        )),

        clang::TypeKind::Bool => Ok("false".into()),

        clang::TypeKind::CharS
        | clang::TypeKind::CharU
        | clang::TypeKind::SChar
        | clang::TypeKind::UChar
        | clang::TypeKind::Short
        | clang::TypeKind::UShort
        | clang::TypeKind::Int
        | clang::TypeKind::UInt
        | clang::TypeKind::Long
        | clang::TypeKind::ULong
        | clang::TypeKind::LongLong
        | clang::TypeKind::ULongLong
        | clang::TypeKind::UInt128 => Ok("0".into()),

        clang::TypeKind::Float | clang::TypeKind::Double => Ok("0.0".into()),

        clang::TypeKind::Enum => {
            let type_text: String = crate::format_clang_type_thrust(&canonical).map_err(|msg| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported enum zero initializer: {msg}"),
                    None,
                    span,
                )
            })?;

            Ok(format!("0 as {type_text}"))
        }

        clang::TypeKind::Pointer => Ok("nullptr".into()),

        clang::TypeKind::ConstantArray => {
            let element_type: clang::Type<'_> = canonical.get_element_type().ok_or_else(|| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Constant array zero initializer is missing an element type.".into(),
                    None,
                    span,
                )
            })?;
            let size: usize = canonical.get_size().ok_or_else(|| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Constant array zero initializer has unknown size.".into(),
                    None,
                    span,
                )
            })?;

            let mut values: Vec<String> = Vec::with_capacity(size);

            for _ in 0..size {
                values.push(self::translate_zero_initializer(&element_type, span)?);
            }

            Ok(format!("fixed[{}]", values.join(", ")))
        }

        clang::TypeKind::Record => {
            let Some(record_decl) = canonical.get_declaration() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Record zero initializer is missing a declaration.".into(),
                    None,
                    span,
                ));
            };

            if record_decl.get_kind() == clang::EntityKind::UnionDecl {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Union zero initializers are not supported yet.".into(),
                    None,
                    span,
                ));
            }

            let record_name: String = crate::format_clang_type_thrust(&canonical).map_err(|msg| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported record zero initializer: {msg}"),
                    None,
                    span,
                )
            })?;
            let record_definition: clang::Entity<'_> = record_decl.get_definition().unwrap_or(record_decl);
            let fields: Vec<clang::Entity<'_>> = record_definition
                .get_children()
                .into_iter()
                .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
                .collect();

            let mut field_initializers: Vec<String> = Vec::with_capacity(fields.len());

            for field in fields.iter() {
                let Some(field_name) = field.get_name() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unnamed fields are not supported in record zero initializers.".into(),
                        None,
                        span,
                    ));
                };

                let Some(field_type) = field.get_type() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Missing field type for '{field_name}' in zero initializer."),
                        None,
                        span,
                    ));
                };

                field_initializers.push(format!(
                    "{}: {}",
                    crate::sanitize_identifier_for_thrust(&field_name),
                    self::translate_zero_initializer(&field_type, span)?
                ));
            }

            Ok(format!("new {record_name} {{ {} }}", field_initializers.join(", ")))
        }

        clang::TypeKind::IncompleteArray => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Incomplete arrays require an initializer in source translation.".into(),
            None,
            span,
        )),

        clang::TypeKind::VariableArray => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "VLA zero initializers are not supported.".into(),
            None,
            span,
        )),

        clang::TypeKind::DependentSizedArray => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Dependent-sized array zero initializers are not supported.".into(),
            None,
            span,
        )),

        other => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Unsupported zero initializer type: {other:?}"),
            None,
            span,
        )),
    }
}

pub(crate) fn find_var_initializer<'top_level>(
    entity: &'top_level clang::Entity<'top_level>,
) -> Option<clang::Entity<'top_level>> {
    let has_initializer: bool = entity.get_range().is_some_and(|range| {
        range.tokenize().iter().any(|token| token.get_spelling() == "=")
    });

    if !has_initializer {
        return None;
    }

    let children: Vec<clang::Entity<'_>> = entity.get_children();

    for child in children.iter().rev() {
        if matches!(
            child.get_kind(),
            clang::EntityKind::InitListExpr
                | clang::EntityKind::CompoundLiteralExpr
                | clang::EntityKind::GNUNullExpr
                | clang::EntityKind::NullPtrLiteralExpr
        ) {
            return Some(*child);
        }
    }

    for child in children.iter() {
        if matches!(
            child.get_kind(),
            clang::EntityKind::UnaryExpr
                | clang::EntityKind::InitListExpr
                | clang::EntityKind::CompoundLiteralExpr
                | clang::EntityKind::GNUNullExpr
                | clang::EntityKind::NullPtrLiteralExpr
        ) {
            return Some(*child);
        }

        if crate::is_supported_expr_kind(child.get_kind()) {
            return Some(*child);
        }
    }

    None
}

fn collect_mutated_parameter_names(
    entity: &clang::Entity<'_>,
    parameter_names: &HashSet<String>,
    mutated_parameters: &mut HashSet<String>,
) {
    match entity.get_kind() {
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                for child in entity.get_children() {
                    self::collect_mutated_parameter_names(
                        &child,
                        parameter_names,
                        mutated_parameters,
                    );
                }

                return;
            }

            let is_assignment: bool =
                crate::extract_binary_operator(entity, &children[0], &children[1])
                    .map(|op| {
                        matches!(
                            op,
                            "="
                                | "+="
                                | "-="
                                | "*="
                                | "/="
                                | "%="
                                | "&="
                                | "|="
                                | "^="
                                | "<<="
                                | ">>="
                        )
                    })
                    .unwrap_or(false);

            if is_assignment && let Some(left) = children.first() {
                let candidate_name: Option<String> = match left.get_kind() {
                    clang::EntityKind::DeclRefExpr => left.get_name(),
                    clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr => left
                        .get_children()
                        .into_iter()
                        .find_map(|child| match child.get_kind() {
                            clang::EntityKind::DeclRefExpr => child.get_name(),
                            _ => None,
                        }),
                    _ => None,
                };

                if let Some(name) = candidate_name && parameter_names.contains(&name) {
                    mutated_parameters.insert(name);
                }
            }
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                return;
            };

            let tokens: Vec<String> = range
                .tokenize()
                .into_iter()
                .map(|token| token.get_spelling())
                .collect();

            if tokens.iter().any(|token| token == "++" || token == "--") {
                let children: Vec<clang::Entity<'_>> = entity.get_children();

                if let Some(operand) = children.first() {
                    let candidate_name: Option<String> = match operand.get_kind() {
                        clang::EntityKind::DeclRefExpr => operand.get_name(),
                        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr => operand
                            .get_children()
                            .into_iter()
                            .find_map(|child| match child.get_kind() {
                                clang::EntityKind::DeclRefExpr => child.get_name(),
                                _ => None,
                            }),
                        _ => None,
                    };

                    if let Some(name) = candidate_name && parameter_names.contains(&name) {
                        mutated_parameters.insert(name);
                    }
                }
            }
        }

        _ => {}
    }

    for child in entity.get_children() {
        self::collect_mutated_parameter_names(&child, parameter_names, mutated_parameters);
    }
}

fn translate_record_decl(
    entity: &clang::Entity<'_>,
    keyword: &str,
    span: Span,
) -> Result<String, CompilationIssue> {
    let children: Vec<clang::Entity<'_>> = entity.get_children();

    let name: Option<String> = entity.get_name().or_else(|| {
        children
            .iter()
            .copied()
            .into_iter()
            .find(|child| child.get_kind() == clang::EntityKind::TypedefDecl)
            .and_then(|child| child.get_name())
    });

    let Some(name) = name else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Anonymous structs are not supported in C translation output.".into(),
            None,
            span,
        ));
    };

    let name: String = crate::sanitize_identifier_for_thrust(&name);

    let mut out: String = String::new();
    let is_packed: bool = children
        .iter()
        .any(|child| child.get_kind() == clang::EntityKind::PackedAttr);
    let align: Option<u64> = if children
        .iter()
        .any(|child| child.get_kind() == clang::EntityKind::AlignedAttr)
    {
        entity
            .get_type()
            .and_then(|record_type| record_type.get_alignof().ok())
            .and_then(|align| align.try_into().ok())
            .filter(|align| *align > 1)
    } else {
        None
    };

    let fields: Vec<clang::Entity<'_>> = children
        .iter()
        .copied()
        .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
        .collect();

    out.push_str(keyword);
    out.push(' ');
    out.push_str(&name);

    if is_packed {
        out.push_str(" @packed");
    }

    if let Some(align) = align {
        out.push_str(" @align(");
        out.push_str(&align.to_string());
        out.push(')');
    }

    out.push_str(" {\n");

    for (field_index, field) in fields.iter().enumerate() {
        if field.get_bit_field_width().is_some() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Bitfields are not supported in {keyword} '{name}'."),
                None,
                span,
            ));
        }

        let Some(field_name) = field.get_name() else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Unnamed fields are not supported in {keyword} '{name}'."),
                None,
                span,
            ));
        };

        let field_name: String = crate::sanitize_identifier_for_thrust(&field_name);

        let Some(field_ty) = field.get_type() else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Missing type for field '{field_name}' in struct '{name}'."),
                None,
                span,
            ));
        };

        let canonical_field_ty: clang::Type<'_> = field_ty.get_canonical_type();
        let canonical_field_kind: clang::TypeKind = canonical_field_ty.get_kind();

        if canonical_field_kind == clang::TypeKind::Record {
            let field_decl: Option<clang::Entity<'_>> = canonical_field_ty.get_declaration();

            if field_decl.as_ref().and_then(clang::Entity::get_name).is_none() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!(
                        "Anonymous inline record members are not supported in {keyword} '{name}'."
                    ),
                    None,
                    span,
                ));
            }
        }

        if canonical_field_kind == clang::TypeKind::VariableArray {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("VLA fields are not supported in {keyword} '{name}'."),
                None,
                span,
            ));
        }

        if canonical_field_kind == clang::TypeKind::DependentSizedArray {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!(
                    "Dependent-sized array fields are not supported in {keyword} '{name}'."
                ),
                None,
                span,
            ));
        }

        if canonical_field_kind == clang::TypeKind::IncompleteArray {
            let message: String = if field_index + 1 == fields.len() {
                format!("Flexible array members are not supported in {keyword} '{name}'.")
            } else {
                format!("Incomplete array fields are not supported in {keyword} '{name}'.")
            };

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                message,
                None,
                span,
            ));
        }

        let ty_text: String = crate::format_clang_type_thrust(&field_ty).map_err(|msg| {
            CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                format!("Unsupported type for field '{field_name}' in struct '{name}': {msg}"),
                None,
                span,
            )
        })?;

        out.push_str("    ");
        out.push_str(&field_name);
        out.push_str(": ");
        out.push_str(&ty_text);
        out.push_str(",\n");
    }

    out.push_str("}\n");

    Ok(out)
}

fn translate_enum_decl(
    entity: &clang::Entity<'_>,
    fallback_name: Option<&str>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let Some(name) = entity.get_name().or_else(|| fallback_name.map(str::to_string)) else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Anonymous enums are not supported in C translation output.".into(),
            None,
            span,
        ));
    };

    let name: String = crate::sanitize_identifier_for_thrust(&name);

    let mut out: String = String::new();

    out.push_str("enum ");
    out.push_str(&name);
    out.push_str(" {\n");

    for constant in entity.get_children() {
        if constant.get_kind() != clang::EntityKind::EnumConstantDecl {
            continue;
        }

        let Some(constant_name) = constant.get_name() else {
            continue;
        };

        let constant_name: String = crate::sanitize_identifier_for_thrust(&constant_name);

        out.push_str("    ");
        out.push_str(&constant_name);

        let has_explicit_value: bool = constant.get_range().is_some_and(|range| {
            range
                .tokenize()
                .into_iter()
                .any(|token| token.get_spelling() == "=")
        });

        if has_explicit_value && let Some((signed, unsigned)) = constant.get_enum_constant_value() {
            let value: String = if signed < 0 {
                signed.to_string()
            } else {
                unsigned.to_string()
            };

            out.push_str(" = ");
            out.push_str(&value);
        }

        out.push_str(",\n");
    }

    out.push_str("}\n");

    Ok(out)
}
