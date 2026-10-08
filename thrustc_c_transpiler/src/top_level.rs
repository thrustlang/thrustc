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

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

use crate::location::Location;

pub fn append_translated_top_level_declarations(
    root: &clang::Entity<'_>,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
    out: &mut String,
    span: Span,
) {
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

    let mut emitted_functions: usize = 0;

    macro_ctx.get_mut_macro_table().scan_macro_expansions(root);

    let mut system_classified = Vec::new();

    for system_entity in entities.iter() {
        if system_entity.get_kind() != clang::EntityKind::MacroDefinition {
            continue;
        }

        if !system_entity.is_in_system_header() {
            continue;
        }

        if unsafe { !system_entity.is_function_like_macro_unchecked() } {
            continue;
        }

        if let Some((system_name, system_kind)) =
            crate::macros::classify_macro_definition(system_entity)
        {
            system_classified.push((*system_entity, system_name, system_kind));
        }
    }

    crate::macros::reclassify_macros_calling_statements(&mut system_classified);

    macro_ctx
        .get_mut_macro_table()
        .register_function_like_macros(&system_classified);

    macro_ctx
        .get_mut_macro_table()
        .register_statement_macros(&system_classified);

    let main_entities = entities
        .into_iter()
        .filter(|e| macro_ctx.get_macro_table().entity_originates_in_input(e));

    self::classify_top_level_declarations(
        main_entities,
        &mut record_decls,
        &mut union_decls,
        &mut enum_decls,
        &mut typedef_decls,
        &mut function_definition_decls,
        &mut function_prototype_decls,
        &mut macro_decls,
        &mut global_var_decls,
        &mut global_decl_indexes,
    );

    record_decls.sort_by_key(|a| a.get_name());
    record_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    union_decls.sort_by_key(|a| a.get_name());
    union_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    enum_decls.sort_by_key(|a| a.get_name());
    enum_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    function_definition_decls.sort_by_key(|a| a.get_name());
    function_definition_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

    function_prototype_decls.sort_by_key(|a| a.get_name());
    function_prototype_decls.dedup_by(|a, b| a.get_canonical_entity() == b.get_canonical_entity());

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
        let src: String = self::translate_record_decl(record, "struct", span, macro_ctx);

        out.push_str(&src);
        out.push('\n');
    }

    for union_decl in union_decls.iter() {
        let src: String = self::translate_record_decl(union_decl, "struct", span, macro_ctx);

        out.push_str(&src);
        out.push('\n');
    }

    for enum_decl in enum_decls.iter() {
        let fallback_name: Option<&str> = enum_name_overrides
            .get(&enum_decl.get_canonical_entity())
            .map(String::as_str);

        let src: String = self::translate_enum_decl(enum_decl, fallback_name, span, macro_ctx);

        out.push_str(&src);
        out.push('\n');
    }

    let valid_typedefs = typedef_decls.iter().filter_map(|typedef_decl| {
        let name: String = typedef_decl.get_name()?;

        let underlying: clang::Type<'_> = typedef_decl.get_typedef_underlying_type()?;

        Some((typedef_decl, name, underlying))
    });

    for (typedef_decl, name, underlying) in valid_typedefs {
        let alias_name: String = { crate::util::sanitize_thrust_identifier(&name) };

        let canonical: clang::Type<'_> = underlying.get_canonical_type();

        let alias_target: Option<String> = match canonical.get_kind() {
            clang::TypeKind::Record => canonical
                .get_declaration()
                .and_then(|decl| decl.get_name())
                .and_then(|record_name| {
                    let record_name: String =
                        { crate::util::sanitize_thrust_identifier(&record_name) };

                    (record_name != alias_name).then_some(record_name)
                }),

            clang::TypeKind::Enum => {
                canonical
                    .get_declaration()
                    .map_or(Some("s32".into()), |decl| {
                        let enum_name: Option<String> = decl.get_name().or_else(|| {
                            enum_name_overrides
                                .get(&decl.get_canonical_entity())
                                .map(String::to_string)
                        });

                        enum_name.map_or(Some("s32".into()), |enum_name| {
                            let enum_name: String =
                                { crate::util::sanitize_thrust_identifier(&enum_name) };

                            (enum_name != alias_name).then_some(enum_name)
                        })
                    })
            }

            _ => {
                let prefix: String = crate::macros::expansion_prefix(typedef_decl);

                Some(crate::type_format::format_clang_type_thrust(
                    &underlying,
                    macro_ctx,
                    &prefix,
                    span,
                ))
            }
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

    for global_var_decl in global_var_decls.iter() {
        let src: String = self::translate_global_var_decl(global_var_decl, span, macro_ctx);

        out.push_str(&src);
        out.push('\n');
    }

    if !global_var_decls.is_empty() {
        out.push('\n');
    }

    let mut macro_fns: String = String::new();

    crate::macros::append_translated_macro_consts(
        &macro_decls,
        out,
        &mut macro_fns,
        macro_ctx,
        span,
        &mut emitted_constants,
        &mut emitted_functions,
    );

    let needed_prototypes = function_prototype_decls
        .iter()
        .filter(|function_prototype| {
            !definition_entities.contains(&function_prototype.get_canonical_entity())
        })
        .filter(|function_prototype| {
            function_prototype
                .get_name()
                .map(|name| {
                    crate::builtins::CanonicalBuiltin::from_called_function_name(&name).is_none()
                        && crate::builtins::HeapOperation::from_called_function_name(&name)
                            .is_none()
                })
                .unwrap_or(true)
        });

    for function_prototype in needed_prototypes {
        let src: String = self::translate_function_prototype(function_prototype, span, macro_ctx);

        out.push_str(&src);
        out.push('\n');

        emitted_prototypes = emitted_prototypes.saturating_add(1);
    }

    if emitted_prototypes > 0 {
        out.push('\n');
    }

    out.push_str(&macro_fns);

    for f in function_definition_decls.iter() {
        let src: String = self::translate_function(f, span, macro_ctx);

        out.push_str(&src);
        out.push('\n');
    }

    for statement_macro_function_text in macro_ctx
        .get_mut_macro_table()
        .take_pending_statement_macro_functions()
        .iter()
    {
        out.push_str(statement_macro_function_text);
        out.push('\n');
    }
}

pub fn translate_global_initializer(
    entity: &clang::Entity<'_>,
    expected_type: &clang::Type<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);

    if entity.get_kind() == clang::EntityKind::CompoundLiteralExpr {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if let Some(value) = children.last() {
            return self::translate_global_initializer(value, expected_type, span, macro_ctx);
        }
    }

    if matches!(
        entity.get_kind(),
        clang::EntityKind::GNUNullExpr | clang::EntityKind::NullPtrLiteralExpr
    ) {
        return "nullptr".into();
    }

    if entity.get_kind() == clang::EntityKind::InitListExpr {
        let canonical_type: clang::Type<'_> = expected_type.get_canonical_type();
        let initializer_children: Vec<clang::Entity<'_>> = entity.get_children();

        if initializer_children.len() == 1
            && canonical_type.get_kind() != clang::TypeKind::Record
            && canonical_type.get_kind() != clang::TypeKind::ConstantArray
            && canonical_type.get_kind() != clang::TypeKind::IncompleteArray
        {
            return self::translate_global_initializer(
                &initializer_children[0],
                expected_type,
                span,
                macro_ctx,
            );
        }

        if matches!(
            canonical_type.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        ) {
            let Some(element_type) = canonical_type.get_element_type() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Array initializer is missing an element type."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ),
                );
                return Default::default();
            };

            let mut values: Vec<String> = Vec::new();

            for child in initializer_children.iter() {
                values.push(self::translate_global_initializer(
                    child,
                    &element_type,
                    span,
                    macro_ctx,
                ));
            }

            if canonical_type.get_kind() == clang::TypeKind::ConstantArray {
                let Some(size) = canonical_type.get_size() else {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(
                        CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!("C translation failed:\n{prefix}Constant array initializer has unknown size."),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ),
                    );
                    return Default::default();
                };

                if values.len() > size {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Array initializer has more elements than the destination array."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                }

                while values.len() < size {
                    values.push(self::translate_zero_initializer(
                        &element_type,
                        span,
                        Some(entity),
                        macro_ctx,
                    ));
                }
            }

            return format!("fixed[{}]", values.join(", "));
        }

        if canonical_type.get_kind() == clang::TypeKind::Record {
            let Some(record_decl) = canonical_type.get_declaration() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Record initializer is missing a declaration."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let record_name: String = crate::type_format::format_clang_type_thrust(
                &canonical_type,
                macro_ctx,
                &prefix,
                span,
            );

            let record_definition: clang::Entity<'_> =
                record_decl.get_definition().unwrap_or(record_decl);

            let fields: Vec<clang::Entity<'_>> = record_definition
                .get_children()
                .into_iter()
                .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
                .collect();
            if initializer_children.len() > fields.len() {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Record initializer has more values than fields."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            }

            let mut field_initializers: Vec<String> = Vec::new();

            for (field_index, field) in fields.iter().enumerate() {
                let Some(field_name) = field.get_name() else {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Unnamed fields are not supported in record initializers."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                };

                let Some(field_type) = field.get_type() else {
                    macro_ctx
                        .get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            {
                                let detail: String = format!(
                                    "Missing type for field '{field_name}' in record initializer."
                                );

                                format!("C translation failed:\n{prefix}{detail}")
                            },
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                };

                let translated_value: String =
                    if let Some(initializer_child) = initializer_children.get(field_index) {
                        self::translate_global_initializer(
                            initializer_child,
                            &field_type,
                            span,
                            macro_ctx,
                        )
                    } else {
                        self::translate_zero_initializer(&field_type, span, Some(entity), macro_ctx)
                    };

                field_initializers.push(format!(
                    "{}: {}",
                    { crate::util::sanitize_thrust_identifier(&field_name) },
                    translated_value
                ));
            }

            return format!("new {record_name} {{ {} }}", field_initializers.join(", "));
        }

        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!("C translation failed:\n{prefix}Unsupported aggregate initializer."),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    }

    let translated: String = if let Some((heap_operation, heap_call)) =
        crate::builtins::HeapOperation::resolve_heap_call(entity)
    {
        let pointee: Option<clang::Type<'_>> =
            expected_type.get_canonical_type().get_pointee_type();

        if let Some(halloc_text) = heap_operation.try_lower_heap_call(
            &heap_call,
            pointee.as_ref(),
            macro_ctx,
            &prefix,
            span,
        ) {
            halloc_text
        } else {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}{}",
                        heap_operation.rejection_reason()
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }
    } else {
        crate::expr::translate_expr(entity, span, Location::RValue, macro_ctx)
    };

    crate::type_format::cast_expression_to_type(entity, translated, expected_type, span, macro_ctx)
}

fn translate_record_decl(
    entity: &clang::Entity<'_>,
    keyword: &str,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
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
        macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            format!("C translation failed:\n{prefix}Anonymous structs are not supported in C translation output."),
            "Rewrite the C input to avoid the unsupported construct.".into(),
            None,
            span,
        ));
        return Default::default();
    };

    let name: String = { crate::util::sanitize_thrust_identifier(&name) };

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
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("Bitfields are not supported in {keyword} '{name}'.");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }

        let Some(field_name) = field.get_name() else {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("Unnamed fields are not supported in {keyword} '{name}'.");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        };

        let field_name: String = { crate::util::sanitize_thrust_identifier(&field_name) };

        let Some(field_ty) = field.get_type() else {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("Missing type for field '{field_name}' in struct '{name}'.");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        };

        let canonical_field_ty: clang::Type<'_> = field_ty.get_canonical_type();
        let canonical_field_kind: clang::TypeKind = canonical_field_ty.get_kind();

        if canonical_field_kind == clang::TypeKind::Record {
            let field_decl: Option<clang::Entity<'_>> = canonical_field_ty.get_declaration();

            if field_decl
                .as_ref()
                .and_then(clang::Entity::get_name)
                .is_none()
            {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {

                    let detail: String =

                    format!(
                        "Anonymous inline record members are not supported in {keyword} '{name}'."
                    );

                    format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            }
        }

        if canonical_field_kind == clang::TypeKind::VariableArray {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("VLA fields are not supported in {keyword} '{name}'.");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }

        if canonical_field_kind == clang::TypeKind::DependentSizedArray {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String = format!(
                            "Dependent-sized array fields are not supported in {keyword} '{name}'."
                        );

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }

        if canonical_field_kind == clang::TypeKind::IncompleteArray {
            let message: String = if field_index + 1 == fields.len() {
                format!("Flexible array members are not supported in {keyword} '{name}'.")
            } else {
                format!("Incomplete array fields are not supported in {keyword} '{name}'.")
            };

            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}{message}"),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }

        let ty_text: String =
            crate::type_format::format_clang_type_thrust(&field_ty, macro_ctx, &prefix, span);

        out.push_str("    ");
        out.push_str(&field_name);
        out.push_str(": ");
        out.push_str(&ty_text);
        out.push_str(",\n");
    }

    out.push_str("}\n");

    out
}

fn translate_global_var_decl(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(raw_name) = entity.get_name() else {
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!(
                    "C translation failed:\n{prefix}Encountered a global variable without a name."
                ),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    };

    let name: String = { crate::util::sanitize_thrust_identifier(&raw_name) };

    let Some(var_type) = entity.get_type() else {
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                {
                    let detail: String = format!("Missing type for global '{name}'.");

                    format!("C translation failed:\n{prefix}{detail}")
                },
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    };

    let canonical_type: clang::Type<'_> = var_type.get_canonical_type();
    let base_pending: usize = macro_ctx.pending_statements_len();

    let initializer: Option<clang::Entity<'_>> = self::find_var_initializer(entity);
    let initializer_text: Option<String> = match (initializer, entity.get_storage_class()) {
        (Some(initializer), _) => {
            let mut initializer_text: String =
                self::translate_global_initializer(&initializer, &canonical_type, span, macro_ctx);

            if macro_ctx.pending_statements_len() > base_pending {
                let _ = macro_ctx.take_pending_statements_since(base_pending);

                macro_ctx.get_mut_transpiler_context().add_error_fail(
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Ternary in global initializer is not supported."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ),
                );
                return Default::default();
            }

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
                let maybe_bitwise_expr: bool = crate::macro_lex::entity_spellings(&initializer)
                    .iter()
                    .any(|token| matches!(token.as_str(), "|" | "&" | "^" | "<<" | ">>"));

                if maybe_bitwise_expr {
                    let expected_type_text: String = crate::type_format::format_clang_type_thrust(
                        &canonical_type,
                        macro_ctx,
                        &prefix,
                        span,
                    );

                    if !expected_type_text.is_empty() {
                        initializer_text = format!("({initializer_text}) as {expected_type_text}");
                    }
                }
            }

            Some(initializer_text)
        }

        (None, Some(clang::StorageClass::Extern)) => None,

        (None, _) => {
            let pending_base: usize = macro_ctx.pending_statements_len();

            let zero: String =
                self::translate_zero_initializer(&canonical_type, span, Some(entity), macro_ctx);

            if macro_ctx.pending_statements_len() > pending_base {
                let _ = macro_ctx.take_pending_statements_since(pending_base);
            }

            Some(zero)
        }
    };

    let type_text: String = if canonical_type.get_kind() == clang::TypeKind::IncompleteArray {
        if let Some(initializer) = initializer {
            if initializer.get_kind() == clang::EntityKind::InitListExpr {
                let Some(element_type) = canonical_type.get_element_type() else {
                    let detail: String =
                        format!("Incomplete array global '{name}' is missing an element type.");

                    macro_ctx
                        .get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!("C translation failed:\n{prefix}{detail}"),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                };

                let element_type_text: String = crate::type_format::format_clang_type_thrust(
                    &element_type,
                    macro_ctx,
                    &prefix,
                    span,
                );

                let element_count: usize = initializer.get_children().len();

                format!("array[{element_type_text}; {element_count}]")
            } else {
                crate::type_format::format_clang_type_thrust(&var_type, macro_ctx, &prefix, span)
            }
        } else {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("Incomplete array global '{name}' requires an initializer.");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }
    } else {
        crate::type_format::format_clang_type_thrust(&var_type, macro_ctx, &prefix, span)
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

    out
}

pub fn translate_zero_initializer(
    ty: &clang::Type<'_>,
    span: Span,
    origin: Option<&clang::Entity<'_>>,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = origin
        .map(crate::macros::expansion_prefix)
        .unwrap_or_default();

    let canonical: clang::Type<'_> = ty.get_canonical_type();

    match canonical.get_kind() {
        clang::TypeKind::Void => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Cannot zero-initialize a void object."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }

        clang::TypeKind::Bool => "false".into(),

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
        | clang::TypeKind::UInt128 => "0".into(),

        clang::TypeKind::Float | clang::TypeKind::Double => "0.0".into(),

        clang::TypeKind::Enum => {
            let type_text: String =
                crate::type_format::format_clang_type_thrust(&canonical, macro_ctx, &prefix, span);

            format!("0 as {type_text}")
        }

        clang::TypeKind::Pointer => "nullptr".into(),

        clang::TypeKind::ConstantArray => {
            let Some(element_type) = canonical.get_element_type() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Constant array zero initializer is missing an element type."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ),
                );
                return Default::default();
            };

            let Some(size) = canonical.get_size() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Constant array zero initializer has unknown size."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ),
                );
                return Default::default();
            };

            let mut values: Vec<String> = Vec::with_capacity(size);

            for _ in 0..size {
                values.push(self::translate_zero_initializer(
                    &element_type,
                    span,
                    origin,
                    macro_ctx,
                ));
            }

            format!("fixed[{}]", values.join(", "))
        }

        clang::TypeKind::Record => {
            let Some(record_decl) = canonical.get_declaration() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Record zero initializer is missing a declaration."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let record_name: String =
                crate::type_format::format_clang_type_thrust(&canonical, macro_ctx, &prefix, span);

            let record_definition: clang::Entity<'_> =
                record_decl.get_definition().unwrap_or(record_decl);

            let fields: Vec<clang::Entity<'_>> = record_definition
                .get_children()
                .into_iter()
                .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
                .collect();

            let mut field_initializers: Vec<String> = Vec::with_capacity(fields.len());

            for field in fields.iter() {
                let Some(field_name) = field.get_name() else {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Unnamed fields are not supported in record zero initializers."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                };

                let Some(field_type) = field.get_type() else {
                    macro_ctx
                        .get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            {
                                let detail: String = format!(
                                    "Missing field type for '{field_name}' in zero initializer."
                                );

                                format!("C translation failed:\n{prefix}{detail}")
                            },
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                };

                field_initializers.push(format!(
                    "{}: {}",
                    { crate::util::sanitize_thrust_identifier(&field_name) },
                    self::translate_zero_initializer(&field_type, span, origin, macro_ctx)
                ));
            }

            format!("new {record_name} {{ {} }}", field_initializers.join(", "))
        }

        clang::TypeKind::IncompleteArray => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Incomplete arrays require an initializer in source translation."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }

        clang::TypeKind::VariableArray => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}VLA zero initializers are not supported."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }

        clang::TypeKind::DependentSizedArray => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Dependent-sized array zero initializers are not supported."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }

        other => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String =
                            format!("Unsupported zero initializer type: {other:?}");

                        format!("C translation failed:\n{prefix}{detail}")
                    },
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }
    }
}

pub fn append_imported_top_level_declarations(
    ctx: &thrustc_c_import_synthesis::context::CImportContext,
    span: Span,
    out: &mut String,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
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
                out.push_str(&crate::type_format::format_type_thrust(field_type));

                if idx + 1 != s.fields().len() {
                    out.push(',');
                }

                out.push('\n');
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
            out.push_str(&crate::type_format::format_type_thrust(t.ty()));
            out.push_str(";\n");
        }

        if !typedefs.is_empty() {
            out.push('\n');
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
                out.push_str(&crate::type_format::format_type_thrust(e.underlying_type()));
                out.push_str(" = ");
                out.push_str(&value.to_string());
                out.push_str(";\n");
            }

            out.push_str("}\n\n");
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
            out.push_str(&crate::type_format::format_type_thrust(c.kind()));
            out.push_str(" @public = ");

            let expr: String =
                crate::type_format::format_builtin_value_thrust(c.value(), c.kind(), macro_ctx);

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
                out.push_str(&crate::type_format::format_type_thrust(ty));
            }

            out.push_str(") ");
            out.push_str(&crate::type_format::format_type_thrust(f.return_type()));
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

fn translate_function(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(name) = entity.get_name() else {
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!("C translation failed:\n{prefix}Encountered a function without a name."),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    };

    let name: String = { crate::util::sanitize_thrust_identifier(&name) };

    let ret_ty: String = entity
        .get_result_type()
        .map(|ty| crate::type_format::format_clang_type_thrust(&ty, macro_ctx, &prefix, span))
        .filter(|text| !text.is_empty())
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
            let param_name: String = { crate::util::sanitize_thrust_identifier(&original_name) };

            let Some(param_ty) = arg.get_type() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        {
                            let detail: String = format!(
                                "Missing type for parameter '{param_name}' in function '{name}'."
                            );

                            format!("C translation failed:\n{prefix}{detail}")
                        },
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            let ty_text: String = crate::type_format::format_parameter_type_thrust(
                &param_ty, macro_ctx, &prefix, span,
            );

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
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                {
                    let detail: String = format!("Missing function body for '{name}'.");

                    format!("C translation failed:\n{prefix}{detail}")
                },
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
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
            crate::stmt::translate_stmt(&child, 1, span, Some(&ret_ty), None, macro_ctx);

        lines.extend(translated);
    }

    for line in lines.into_iter() {
        out.push_str(&line);
        out.push('\n');
    }

    out.push('}');

    out
}

fn translate_function_prototype(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(raw_name) = entity.get_name() else {
        macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            format!("C translation failed:\n{prefix}Encountered a function prototype without a name."),
            "Rewrite the C input to avoid the unsupported construct.".into(),
            None,
            span,
        ));
        return Default::default();
    };

    if matches!(
        entity.get_storage_class(),
        Some(clang::StorageClass::Static | clang::StorageClass::PrivateExtern)
    ) || matches!(
        entity.get_linkage(),
        Some(clang::Linkage::Internal | clang::Linkage::UniqueExternal)
    ) {
        macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            {

            let detail: String =

            format!(
                "Static/internal function prototype '{raw_name}' without a body is not supported."
            );

            format!("C translation failed:\n{prefix}{detail}")
            },
            "Rewrite the C input to avoid the unsupported construct.".into(),
            None,
            span,
        ));
        return Default::default();
    }

    let name: String = { crate::util::sanitize_thrust_identifier(&raw_name) };

    let return_type: String = entity
        .get_result_type()
        .map(|ty| crate::type_format::format_clang_type_thrust(&ty, macro_ctx, &prefix, span))
        .filter(|text| !text.is_empty())
        .unwrap_or_else(|| "void".into());

    let mut parameter_texts: Vec<String> = Vec::new();

    if let Some(arguments) = entity.get_arguments() {
        for (index, argument) in arguments.iter().enumerate() {
            let parameter_name: String = argument
                .get_name()
                .map(|name| crate::util::sanitize_thrust_identifier(&name))
                .unwrap_or_else(|| format!("arg{index}"));

            let Some(parameter_type) = argument.get_type() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        {
                            let detail: String = format!(
                                "Missing type for parameter '{parameter_name}' in '{name}'."
                            );

                            format!("C translation failed:\n{prefix}{detail}")
                        },
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            let parameter_type_text: String = crate::type_format::format_parameter_type_thrust(
                &parameter_type,
                macro_ctx,
                &prefix,
                span,
            );

            parameter_texts.push(format!("{parameter_name}: {parameter_type_text}"));
        }
    }

    let convention: &str = match crate::type_format::format_clang_calling_convention_thrust(
        entity.get_type().and_then(|ty| ty.get_calling_convention()),
    ) {
        Ok(target) => target,
        Err(message) => {
            let detail: String =
                format!("Unsupported calling convention for prototype '{name}': {message}");

            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}{detail}"),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }
    };

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

    out
}

#[allow(clippy::too_many_arguments)]
fn classify_top_level_declarations<'clang>(
    main_entities: impl Iterator<Item = clang::Entity<'clang>>,
    record_decls: &mut Vec<clang::Entity<'clang>>,
    union_decls: &mut Vec<clang::Entity<'clang>>,
    enum_decls: &mut Vec<clang::Entity<'clang>>,
    typedef_decls: &mut Vec<clang::Entity<'clang>>,
    function_definition_decls: &mut Vec<clang::Entity<'clang>>,
    function_prototype_decls: &mut Vec<clang::Entity<'clang>>,
    macro_decls: &mut Vec<clang::Entity<'clang>>,
    global_var_decls: &mut Vec<clang::Entity<'clang>>,
    global_decl_indexes: &mut HashMap<clang::Entity<'clang>, usize>,
) {
    for e in main_entities {
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

                let score_var = |has_initializer: bool,

                                 storage: Option<clang::StorageClass>,
                                 is_definition: bool|
                 -> u8 {
                    u8::from(has_initializer)
                        .saturating_mul(4)
                        .saturating_add(
                            u8::from(!matches!(storage, Some(clang::StorageClass::Extern)))
                                .saturating_mul(2),
                        )
                        .saturating_add(u8::from(is_definition))
                };

                let candidate_score: u8 =
                    score_var(initializer_present, storage_class, e.is_definition());

                match global_decl_indexes.get(&canonical).copied() {
                    Some(index) => {
                        let current_initializer_present: bool =
                            self::find_var_initializer(&global_var_decls[index]).is_some();

                        let current_storage_class: Option<clang::StorageClass> =
                            global_var_decls[index].get_storage_class();

                        let current_score: u8 = score_var(
                            current_initializer_present,
                            current_storage_class,
                            global_var_decls[index].is_definition(),
                        );

                        if candidate_score > current_score {
                            global_var_decls[index] = e;
                        }
                    }
                    None => {
                        global_decl_indexes.insert(canonical, global_var_decls.len());
                        global_var_decls.push(e);
                    }
                }
            }

            _ => {}
        }
    }
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
                crate::macro_lex::extract_binary_operator(entity, &children[0], &children[1])
                    .map(|op| {
                        matches!(
                            op.as_str(),
                            "=" | "+="
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

                if let Some(name) = candidate_name
                    && parameter_names.contains(&name)
                {
                    mutated_parameters.insert(name);
                }
            }
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                return;
            };

            let tokens: Vec<String> = crate::macro_lex::range_spellings(&range, entity);

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

                    if let Some(name) = candidate_name
                        && parameter_names.contains(&name)
                    {
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

fn translate_enum_decl(
    entity: &clang::Entity<'_>,
    fallback_name: Option<&str>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(name) = entity
        .get_name()
        .or_else(|| fallback_name.map(str::to_string))
    else {
        macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            format!("C translation failed:\n{prefix}Anonymous enums are not supported in C translation output."),
            "Rewrite the C input to avoid the unsupported construct.".into(),
            None,
            span,
        ));
        return Default::default();
    };

    let name: String = { crate::util::sanitize_thrust_identifier(&name) };

    let mut out: String = String::new();

    out.push_str("enum ");
    out.push_str(&name);
    out.push_str(" {\n");

    let enum_constants = entity
        .get_children()
        .into_iter()
        .filter(|constant| constant.get_kind() == clang::EntityKind::EnumConstantDecl)
        .filter_map(|constant| {
            let constant_name: String = constant.get_name()?;

            Some((constant, constant_name))
        });

    for (constant, constant_name) in enum_constants {
        let constant_name: String = { crate::util::sanitize_thrust_identifier(&constant_name) };

        out.push_str("    ");
        out.push_str(&constant_name);

        let has_explicit_value: bool = crate::macro_lex::entity_spellings(&constant)
            .iter()
            .any(|token| token == "=");

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

    out
}

pub fn find_var_initializer<'clang>(
    entity: &'clang clang::Entity<'clang>,
) -> Option<clang::Entity<'clang>> {
    let has_initializer: bool = crate::macro_lex::entity_spellings(entity)
        .iter()
        .any(|token| token == "=");

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

        if crate::clang_util::is_supported_expr_kind(child.get_kind()) {
            return Some(*child);
        }
    }

    None
}
