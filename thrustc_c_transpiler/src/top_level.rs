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

use std::collections::HashSet;
use std::path::Path;

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
            out.push_str("struct ");
            out.push_str(s.name());
            out.push_str(" @public {\n");

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
            out.push_str(" @convention(\"C\")");

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
    span: Span,
) -> Result<(), CompilationIssue> {
    let entities: Vec<clang::Entity<'_>> = root.get_children();

    let mut record_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut union_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut enum_decls: Vec<clang::Entity<'_>> = Vec::new();
    let mut function_decls: Vec<clang::Entity<'_>> = Vec::new();

    for e in entities.into_iter() {
        if !crate::entity_originates_in_main_file(&e, canonical_input) {
            continue;
        }

        match e.get_kind() {
            clang::EntityKind::StructDecl if e.is_definition() => record_decls.push(e),
            clang::EntityKind::UnionDecl if e.is_definition() => union_decls.push(e),
            clang::EntityKind::EnumDecl if e.is_definition() => enum_decls.push(e),
            clang::EntityKind::TypedefDecl => {
                self::collect_typedef_record_or_enum(
                    &e,
                    &mut record_decls,
                    &mut union_decls,
                    &mut enum_decls,
                );
            }
            clang::EntityKind::FunctionDecl if e.is_definition() => function_decls.push(e),

            _ => {}
        }
    }

    record_decls.sort_by_key(|a| a.get_name());
    record_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    union_decls.sort_by_key(|a| a.get_name());
    union_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    enum_decls.sort_by_key(|a| a.get_name());
    enum_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    function_decls.sort_by_key(|a| a.get_name());

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
        let src: String = self::translate_enum_decl(enum_decl, span)?;

        out.push_str(&src);
        out.push('\n');
    }

    for f in function_decls.iter() {
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
    let mut body: Option<clang::Entity<'_>> = None;

    for child in children.iter() {
        if child.get_kind() == clang::EntityKind::CompoundStmt {
            body = Some(*child);
            break;
        }
    }

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

    let lines: Vec<String> = crate::stmt::translate_compound_stmt(&body, 1, span, Some(&ret_ty))?;

    for line in lines.into_iter() {
        out.push_str(&line);
        out.push('\n');
    }

    out.push('}');

    Ok(out)
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
                    .map(|op| matches!(op, "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>="))
                    .unwrap_or(false);

            if is_assignment
                && let Some(left) = children.first()
                && let Some(name) = self::extract_decl_ref_name(left)
                && parameter_names.contains(&name)
            {
                mutated_parameters.insert(name);
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

                if let Some(operand) = children.first()
                    && let Some(name) = self::extract_decl_ref_name(operand)
                    && parameter_names.contains(&name)
                {
                    mutated_parameters.insert(name);
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
    let name: Option<String> = entity.get_name().or_else(|| {
        entity
            .get_children()
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

    out.push_str(keyword);
    out.push(' ');
    out.push_str(&name);
    out.push_str(" {\n");

    for field in entity.get_children() {
        if field.get_kind() != clang::EntityKind::FieldDecl {
            continue;
        }

        let Some(field_name) = field.get_name() else {
            continue;
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

fn translate_enum_decl(entity: &clang::Entity<'_>, span: Span) -> Result<String, CompilationIssue> {
    let Some(name) = entity.get_name() else {
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

fn collect_typedef_record_or_enum<'clang>(
    entity: &clang::Entity<'clang>,
    record_decls: &mut Vec<clang::Entity<'clang>>,
    union_decls: &mut Vec<clang::Entity<'clang>>,
    enum_decls: &mut Vec<clang::Entity<'clang>>,
) {
    let Some(underlying) = entity.get_typedef_underlying_type() else {
        return;
    };

    let canonical: clang::Type<'_> = underlying.get_canonical_type();

    let Some(decl) = canonical.get_declaration() else {
        return;
    };

    match (canonical.get_kind(), decl.get_kind()) {
        (clang::TypeKind::Record, clang::EntityKind::StructDecl) if decl.is_definition() => {
            record_decls.push(decl)
        }
        (clang::TypeKind::Record, clang::EntityKind::UnionDecl) if decl.is_definition() => {
            union_decls.push(decl)
        }
        (clang::TypeKind::Enum, _) if decl.is_definition() => enum_decls.push(decl),

        _ => {}
    }
}

fn extract_decl_ref_name(entity: &clang::Entity<'_>) -> Option<String> {
    match entity.get_kind() {
        clang::EntityKind::DeclRefExpr => entity.get_name(),

        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr => entity
            .get_children()
            .into_iter()
            .find_map(|child| self::extract_decl_ref_name(&child)),

        _ => None,
    }
}
