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

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use crate::location::Location;

pub fn translate_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    ctx: Location,
    macro_ctx: &mut crate::macros::MacroContext,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);

    if let Some(expression_text) =
        crate::macros::try_extract_expression_macro_call(macro_ctx, entity, span)
    {
        return expression_text;
    }

    match entity.get_kind() {
        clang::EntityKind::IntegerLiteral
        | clang::EntityKind::FloatingLiteral
        | clang::EntityKind::StringLiteral
        | clang::EntityKind::CharacterLiteral => {
            let Some(range) = entity.get_range() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        {
                            let detail: String = format!(
                                "Missing source range for expression kind {:?}.",
                                entity.get_kind()
                            );

                            format!("C translation failed:\n{prefix}{detail}")
                        },
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

            if let Some(first) = tokens.first() {
                let spelling: String = first.get_spelling();

                if entity.get_kind() == clang::EntityKind::IntegerLiteral {
                    return spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string();
                }

                if entity.get_kind() == clang::EntityKind::FloatingLiteral {
                    return spelling.trim_end_matches(['f', 'F', 'l', 'L']).to_string();
                }

                return spelling;
            }

            String::new()
        }

        clang::EntityKind::DeclRefExpr => self::translate_decl_ref_expr(entity, span, macro_ctx),

        clang::EntityKind::GNUNullExpr | clang::EntityKind::NullPtrLiteralExpr => "nullptr".into(),

        clang::EntityKind::MemberRefExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let Some(base_node) = children.first() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Malformed member reference expression."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            let Some(field_name) = entity.get_name() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Member reference expression is missing its field name."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let base: String = self::translate_expr(base_node, span, Location::RValue, macro_ctx);

            // Cadena de decisión LValue/RValue (Location) × clase de la base.
            // `ref` (AddressOf) anula cualquier RValue: el operando debe
            // conservarse como place sin loads intermedios, por eso `.`
            // (igual que el backend: GetLocation fuerza LValue + ptr).
            // En el resto de contextos solo `->` es direccionable y
            // cargable a la vez (struct_property_expr.rs:58-106):
            // `.` solo lee valores alocados y envuelve el tipo del campo
            // en Ptr (property.rs:187-196); por eso no se emite nunca
            // fuera de AddressOf.

            let operator: &str = if ctx.is_address_of() {
                "."
            } else {
                debug_assert!(ctx.is_direct() || ctx.is_load());

                "->"
            };

            let field_name: String = { crate::util::sanitize_thrust_identifier(&field_name) };

            format!("{base}{operator}{field_name}")
        }

        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|c| crate::clang_util::is_supported_expr_kind(c.get_kind()))
            {
                let inner: String = self::translate_expr(child, span, ctx, macro_ctx);

                if matches!(
                    child.get_kind(),
                    clang::EntityKind::IntegerLiteral
                        | clang::EntityKind::FloatingLiteral
                        | clang::EntityKind::StringLiteral
                        | clang::EntityKind::CharacterLiteral
                        | clang::EntityKind::DeclRefExpr
                        | clang::EntityKind::MemberRefExpr
                ) {
                    return inner;
                }

                if !entity.is_in_main_file() {
                    return inner;
                }

                return format!("({inner})");
            }

            let Some(range) = entity.get_range() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Missing source range for parenthesized expression."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            crate::macro_lex::tokens_to_thrust_source(&range.tokenize())
        }

        clang::EntityKind::UnexposedExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let supported_children: Vec<clang::Entity<'_>> = children
                .iter()
                .copied()
                .filter(|child| crate::clang_util::is_supported_expr_kind(child.get_kind()))
                .collect();

            if supported_children.len() == 1 {
                return self::translate_expr(&supported_children[0], span, ctx, macro_ctx);
            }

            let Some(range) = entity.get_range() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Missing source range for unexposed expression."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            crate::macro_lex::tokens_to_thrust_source(&range.tokenize())
        }

        clang::EntityKind::CallExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.is_empty() {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed call expression."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let callee: String =
                self::translate_expr(&children[0], span, Location::RValue, macro_ctx);

            let mut expected_parameter_types: Vec<clang::Type<'_>> = if let Some(reference) =
                entity.get_reference()
                && let Some(arguments) = reference.get_arguments()
            {
                arguments
                    .into_iter()
                    .filter_map(|argument| argument.get_type())
                    .collect()
            } else if matches!(
                children[0].get_kind(),
                clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
            ) {
                let nested_children: Vec<clang::Entity<'_>> = children[0].get_children();

                if nested_children.len() == 1
                    && let Some(reference) = nested_children[0].get_reference()
                    && let Some(arguments) = reference.get_arguments()
                {
                    arguments
                        .into_iter()
                        .filter_map(|argument| argument.get_type())
                        .collect()
                } else {
                    Vec::new()
                }
            } else if let Some(reference) = children[0].get_reference()
                && let Some(arguments) = reference.get_arguments()
            {
                arguments
                    .into_iter()
                    .filter_map(|argument| argument.get_type())
                    .collect()
            } else {
                Vec::new()
            };

            if expected_parameter_types.is_empty()
                && let Some(callee_type) = children[0].get_type()
                && let Some(argument_types) = callee_type
                    .get_argument_types()
                    .or_else(|| callee_type.get_pointee_type()?.get_argument_types())
            {
                expected_parameter_types = argument_types;
            }

            let mut args: Vec<String> = Vec::new();

            let call_arg_iter = children.iter().skip(1).enumerate();

            for (index, arg) in call_arg_iter {
                let arg_ctx: Location = match expected_parameter_types.get(index) {
                    Some(expected)
                        if matches!(
                            expected.get_canonical_type().get_kind(),
                            clang::TypeKind::Pointer
                        ) =>
                    {
                        Location::CallArg
                    }
                    _ => Location::RValue,
                };

                let translated: String = self::translate_expr(arg, span, arg_ctx, macro_ctx);

                let mut argument_type: Option<clang::Type<'_>> = arg.get_type();
                let mut probe: clang::Entity<'_> = *arg;

                while matches!(
                    probe.get_kind(),
                    clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
                ) {
                    let nested_children: Vec<clang::Entity<'_>> = probe.get_children();

                    if nested_children.len() != 1 {
                        break;
                    }

                    probe = nested_children[0];

                    if let Some(probe_type) = probe.get_type() {
                        let probe_canonical: clang::Type<'_> = probe_type.get_canonical_type();

                        if matches!(
                            probe_canonical.get_kind(),
                            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
                        ) {
                            argument_type = Some(probe_type);
                            break;
                        }

                        if argument_type.is_none() {
                            argument_type = Some(probe_type);
                        }
                    }
                }

                if let Some(argument_type) = argument_type {
                    let argument_canonical: clang::Type<'_> = argument_type.get_canonical_type();

                    if matches!(
                        argument_canonical.get_kind(),
                        clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
                    ) {
                        let mut stack: Vec<clang::Entity<'_>> = vec![*arg];
                        let mut is_string_literal_like: bool = false;

                        while let Some(candidate) = stack.pop() {
                            match candidate.get_kind() {
                                clang::EntityKind::StringLiteral => {
                                    is_string_literal_like = true;
                                    break;
                                }
                                clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr => {
                                    stack.extend(candidate.get_children());
                                }
                                _ => {}
                            }
                        }

                        if !is_string_literal_like
                            && let Some(element_type) = argument_canonical.get_element_type()
                        {
                            if let Some((false, extents)) =
                                crate::expr_analysis::analyze_nested_scalar_array_type(
                                    &argument_type,
                                )
                                && extents.len() > 1
                            {
                                let zero_count: usize = extents.len();

                                let zero_iter = (0..zero_count).map(|_| "[0]");

                                let zero_path: String = zero_iter.collect();

                                args.push(format!("ref {translated}{zero_path}"));
                                continue;
                            }

                            let canonical_element_type: clang::Type<'_> =
                                element_type.get_canonical_type();

                            if canonical_element_type.get_kind() == clang::TypeKind::Record {
                                args.push(format!("ref {translated}[0]"));
                                continue;
                            }

                            let element_text: String = crate::type_format::format_clang_type_thrust(
                                &element_type,
                                macro_ctx,
                                &prefix,
                                span,
                            );

                            args.push(format!("{translated} as ptr[{element_text}]"));
                            continue;
                        }
                    }
                }

                if let Some(expected_type) = expected_parameter_types.get(index) {
                    args.push(crate::type_format::cast_expression_to_type(
                        arg,
                        translated,
                        expected_type,
                        span,
                        macro_ctx,
                    ));
                } else {
                    args.push(translated);
                }
            }

            if let Some(canonical) =
                crate::builtins::CanonicalBuiltin::from_called_function_name(&callee)
                && let Some(canonical_text) = canonical.rewrite_builtin_call(&children[1..], &args)
            {
                return canonical_text;
            }

            if let Some(heap_operation) =
                crate::builtins::HeapOperation::from_called_function_name(&callee)
            {
                if let Some(halloc_text) =
                    heap_operation.try_lower_heap_call(entity, None, macro_ctx, &prefix, span)
                {
                    return halloc_text;
                }

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

            format!("{callee}({})", args.join(", "))
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Missing source range for unary operator."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let spellings: Vec<String> = crate::macro_lex::range_spellings(&range, entity);

            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let Some(operand) = children.first() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed unary operator."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            let is_increment: bool = spellings.contains(&"++".to_string());
            let is_decrement: bool = spellings.contains(&"--".to_string());

            if is_increment || is_decrement {
                let operator_text: &str = if is_increment { "++" } else { "--" };

                let is_prefix: bool = spellings
                    .first()
                    .is_some_and(|spelling| spelling == "++" || spelling == "--");

                let is_suffix: bool = spellings
                    .last()
                    .is_some_and(|spelling| spelling == "++" || spelling == "--");

                if !is_prefix && !is_suffix {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Malformed increment/decrement operator."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                }

                let cleaned: clang::Entity<'_> =
                    crate::expr_analysis::clean_expression_wrappers(operand);

                let target_kind: clang::EntityKind = cleaned.get_kind();

                let target_type_kind: Option<clang::TypeKind> = cleaned
                    .get_type()
                    .map(|target_type| target_type.get_canonical_type().get_kind());

                let is_arithmetic_target: bool = target_type_kind.is_some_and(|kind| {
                    matches!(
                        kind,
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
                            | clang::TypeKind::UInt128
                            | clang::TypeKind::Float
                            | clang::TypeKind::Double
                    )
                });

                if !is_arithmetic_target {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Increment/decrement requires an integer or floating-point operand."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                }

                if target_kind == clang::EntityKind::DeclRefExpr {
                    let name: String =
                        self::translate_expr(operand, span, Location::RValue, macro_ctx);

                    if name.contains("::") {
                        macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Increment/decrement of qualified symbols is not supported."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    }

                    if is_prefix {
                        return format!("{operator_text}{name}");
                    }

                    return format!("{name}{operator_text}");
                }

                let cleaned_spellings: Vec<String> = crate::macro_lex::entity_spellings(&cleaned);

                let is_place_target: bool = matches!(
                    target_kind,
                    clang::EntityKind::MemberRefExpr | clang::EntityKind::ArraySubscriptExpr
                ) || (target_kind == clang::EntityKind::UnaryOperator
                    && cleaned_spellings.contains(&"*".to_string())
                    && !cleaned_spellings.contains(&"++".to_string())
                    && !cleaned_spellings.contains(&"--".to_string()));

                if !is_place_target {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Unsupported increment/decrement operand."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                }

                let place_text: String =
                    self::translate_expr(&cleaned, span, Location::RValue, macro_ctx);

                if is_prefix {
                    return format!("{operator_text}({place_text})");
                }

                return format!("({place_text}){operator_text}");
            }

            let op: Option<&str> = spellings.iter().find_map(|s| match s.as_str() {
                "&" | "*" | "!" | "+" | "-" | "~" => Some(s.as_str()),
                _ => None,
            });

            let Some(op) = op else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Unsupported unary operator{}.",
                            crate::macros::origin_note(entity)
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            let operand_ctx: Location = if op == "&" {
                Location::AddressOf
            } else {
                Location::RValue
            };

            let operand_text: String = self::translate_expr(operand, span, operand_ctx, macro_ctx);

            match op {
                "&" => format!("ref {operand_text}"),
                "*" => {
                    if let Some(pointer_target) =
                        self::translate_pointer_target_expr(operand, span, macro_ctx)
                    {
                        return pointer_target;
                    }

                    let needs_parens: bool = matches!(
                        operand.get_kind(),
                        clang::EntityKind::BinaryOperator
                            | clang::EntityKind::CompoundAssignOperator
                    );

                    if needs_parens {
                        format!("(deref ({operand_text}))")
                    } else {
                        format!("(deref {operand_text})")
                    }
                }
                "!" => format!("!{operand_text}"),
                "+" => operand_text,
                "-" => format!("-{operand_text}"),
                "~" => {
                    let cleaned_operand: clang::Entity<'_> =
                        crate::expr_analysis::clean_expression_wrappers(operand);

                    let operand_type_kind: Option<clang::TypeKind> = cleaned_operand
                        .get_type()
                        .map(|operand_type| operand_type.get_canonical_type().get_kind());

                    let needs_widening: bool = operand_type_kind.is_some_and(|kind| {
                        matches!(
                            kind,
                            clang::TypeKind::Bool
                                | clang::TypeKind::CharS
                                | clang::TypeKind::CharU
                                | clang::TypeKind::SChar
                                | clang::TypeKind::UChar
                                | clang::TypeKind::Short
                                | clang::TypeKind::UShort
                                | clang::TypeKind::Enum
                        )
                    });

                    if needs_widening {
                        return format!("~({operand_text} as s32)");
                    }

                    let is_integer_operand: bool = operand_type_kind.is_some_and(|kind| {
                        matches!(
                            kind,
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
                                | clang::TypeKind::UInt128
                        )
                    });

                    if is_integer_operand {
                        return format!("~{operand_text}");
                    }

                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Bitwise-not requires an integer operand."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                    Default::default()
                }
                _ => {
                    macro_ctx
                        .get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unsupported unary operator{}.",
                                crate::macros::origin_note(entity)
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));

                    Default::default()
                }
            }
        }

        clang::EntityKind::UnaryExpr => {
            let Some(range) = entity.get_range() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Missing source range for unary expression."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let tokens: Vec<String> = crate::macro_lex::range_spellings(&range, entity);

            if tokens.first().is_some_and(|token| token == "sizeof") {
                let children: Vec<clang::Entity<'_>> = entity.get_children();
                let operand_type: Option<clang::Type<'_>> = children
                    .iter()
                    .find_map(|child| child.get_type())
                    .or_else(|| children.last().and_then(|child| child.get_type()));

                let result_type: Option<clang::Type<'_>> = entity.get_type();

                let unsupported_operand_kind: bool = operand_type.as_ref().is_some_and(|operand| {
                    matches!(
                        operand.get_canonical_type().get_kind(),
                        clang::TypeKind::LongDouble | clang::TypeKind::Complex
                    )
                });

                let anonymous_record_operand: bool = operand_type.as_ref().is_some_and(|operand| {
                    let canonical = operand.get_canonical_type();

                    canonical.get_kind() == clang::TypeKind::Record
                        && canonical
                            .get_declaration()
                            .map(|declaration| {
                                declaration
                                    .get_name()
                                    .map(|name| name.contains("(unnamed"))
                                    .unwrap_or(true)
                            })
                            .unwrap_or(true)
                });

                let literal_size: Option<u64> = if !unsupported_operand_kind
                    && (operand_type.is_none() || anonymous_record_operand)
                {
                    if let Some(evaluated) = entity.evaluate() {
                        match evaluated {
                            clang::EvaluationResult::UnsignedInteger(size) => Some(size),

                            clang::EvaluationResult::SignedInteger(size) => {
                                u64::try_from(size).ok()
                            }

                            _ => None,
                        }
                    } else {
                        operand_type
                            .as_ref()
                            .and_then(|operand| operand.get_sizeof().ok().map(|size| size as u64))
                    }
                } else {
                    None
                };

                if let Some(literal_size) = literal_size {
                    return format!("{literal_size}");
                }

                let Some(operand_type) = operand_type else {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Unable to determine operand type for sizeof expression."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                };

                let operand_type_text: String = crate::type_format::format_clang_type_thrust(
                    &operand_type,
                    macro_ctx,
                    &prefix,
                    span,
                );

                if operand_type_text.is_empty() {
                    match operand_type.get_sizeof() {
                        Ok(evaluated_size)
                            if !matches!(
                                operand_type.get_canonical_type().get_kind(),
                                clang::TypeKind::LongDouble | clang::TypeKind::Complex
                            ) =>
                        {
                            return format!("{evaluated_size}");
                        }

                        _ => {}
                    }
                }

                let mut sizeof_text: String = format!("abiSizeOf({operand_type_text})");

                if let Some(result_type) = result_type {
                    let result_type_text: String = crate::type_format::format_clang_type_thrust(
                        &result_type,
                        macro_ctx,
                        &prefix,
                        span,
                    );

                    sizeof_text = format!("({sizeof_text}) as {result_type_text}");
                }

                return sizeof_text;
            }

            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Unsupported unary expression."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));

            Default::default()
        }

        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed binary operator."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let lhs_ctx: Location = if crate::macro_lex::is_assignment_expression(entity) {
                Location::LValue
            } else {
                Location::RValue
            };

            let left: String = self::translate_expr(&children[0], span, lhs_ctx, macro_ctx);

            let right: String =
                self::translate_expr(&children[1], span, Location::RValue, macro_ctx);

            let Some(op) =
                crate::macro_lex::extract_binary_operator(entity, &children[0], &children[1])
                    .or_else(|| {
                        entity.get_range().and_then(|range| {
                            crate::macro_lex::extract_binary_operator_from_tokens(
                                &crate::macro_lex::range_spellings(&range, entity),
                            )
                            .map(|operator| operator.to_string())
                        })
                    })
            else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Unsupported binary operator{}.",
                            crate::macros::origin_note(entity)
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            };

            if op == "," {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}The comma operator is not supported in C translation output."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            }

            let is_assignment: bool = matches!(
                op.as_str(),
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
            );

            let mut right: String = right;

            if is_assignment {
                if let Some(lhs_type) = children[0].get_type() {
                    let is_pointer_assign: bool = matches!(
                        op.as_str(),
                        "+=" | "-=" | "<<=" | ">>=" | "&=" | "|=" | "^="
                    ) && lhs_type.get_canonical_type().get_kind()
                        == clang::TypeKind::Pointer;

                    if !is_pointer_assign {
                        right = crate::type_format::cast_expression_to_type(
                            &children[1],
                            right,
                            &lhs_type,
                            span,
                            macro_ctx,
                        );
                    }
                }
            }

            format!("{left} {op} {right}")
        }

        clang::EntityKind::CStyleCastExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let Some(value_node) = children
                .iter()
                .rev()
                .find(|c| crate::clang_util::is_supported_expr_kind(c.get_kind()))
            else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Unsupported cast expression."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            let Some(to_ty) = entity.get_type() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Missing cast type."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            if let Some((heap_operation, heap_call)) =
                crate::builtins::HeapOperation::resolve_heap_call(value_node)
            {
                let pointee: Option<clang::Type<'_>> =
                    to_ty.get_canonical_type().get_pointee_type();

                if let Some(halloc_text) = heap_operation.try_lower_heap_call(
                    &heap_call,
                    pointee.as_ref(),
                    macro_ctx,
                    &prefix,
                    span,
                ) {
                    return halloc_text;
                }

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

            let value: String = self::translate_expr(value_node, span, Location::RValue, macro_ctx);

            if to_ty.get_canonical_type().get_kind() == clang::TypeKind::Void {
                return value;
            }

            let to_ty_text: String =
                crate::type_format::format_clang_type_thrust(&to_ty, macro_ctx, &prefix, span);

            format!("{value} as {to_ty_text}")
        }

        clang::EntityKind::ConditionalOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 3 {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed conditional operator."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let cond: String =
                self::translate_condition_expr(&children[0], span, Location::RValue, macro_ctx);
            let then_expr: String =
                self::translate_expr(&children[1], span, Location::RValue, macro_ctx);
            let else_expr: String =
                self::translate_expr(&children[2], span, Location::RValue, macro_ctx);

            let result_type: Option<clang::Type<'_>> = entity.get_type();

            let type_text: Option<String> = result_type.as_ref().map(|ty| {
                crate::type_format::format_clang_type_thrust(ty, macro_ctx, &prefix, span)
            });

            let type_text: String = match type_text {
                Some(text) if !text.is_empty() => text,
                _ => "s32".into(),
            };

            let then_expr: String = match result_type.as_ref() {
                Some(expected) => crate::type_format::cast_expression_to_type(
                    &children[1],
                    then_expr,
                    expected,
                    span,
                    macro_ctx,
                ),
                None => then_expr,
            };

            let else_expr: String = match result_type.as_ref() {
                Some(expected) => crate::type_format::cast_expression_to_type(
                    &children[2],
                    else_expr,
                    expected,
                    span,
                    macro_ctx,
                ),
                None => else_expr,
            };

            let temporary: String = macro_ctx.next_temporary_name();

            macro_ctx.push_pending_statement(format!("var {temporary}: {type_text};"));
            macro_ctx.push_pending_statement(format!("if {cond} {{"));
            macro_ctx.push_pending_statement(format!("    {temporary} = {then_expr};"));
            macro_ctx.push_pending_statement("} else {".into());
            macro_ctx.push_pending_statement(format!("    {temporary} = {else_expr};"));
            macro_ctx.push_pending_statement("}".into());

            temporary
        }

        clang::EntityKind::CompoundLiteralExpr => {
            let Some(compound_type) = entity.get_type() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Compound literal is missing its type."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            crate::top_level::translate_global_initializer(entity, &compound_type, span, macro_ctx)
        }

        clang::EntityKind::InitListExpr => {
            let Some(init_type) = entity.get_type() else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Initializer list is missing its type."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            crate::top_level::translate_global_initializer(entity, &init_type, span, macro_ctx)
        }

        clang::EntityKind::ArraySubscriptExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Malformed array subscript expression."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            }

            let base_node: &clang::Entity<'_> = &children[0];
            let index_node: &clang::Entity<'_> = &children[1];

            // Bajo `ref` (AddressOf) el subíndice debe conservarse como
            // place `[i]` sin loads: `ref` anula cualquier RValue y
            // `ref x->[i]` es E0008 (VALUE WITHOUT ADDRESS). La base
            // propaga el contexto (cadena `&s.arr[i]`); el índice se
            // lee como valor.
            if ctx.is_address_of() {
                let base: String = self::translate_expr(base_node, span, ctx, macro_ctx);

                let index: String =
                    self::translate_expr(index_node, span, Location::RValue, macro_ctx);

                return format!("{base}[{index}]");
            }

            if let Some(linearized) =
                self::try_translate_linearized_array_subscript(entity, span, macro_ctx)
            {
                return linearized;
            }

            // La base de un subíndice se indexa sobre su dirección (GEP
            // del backend): una base MemberRef (limpiando wrappers de decay)
            // se traduce en AddressOf (`.` sin loads intermedios,
            // `buffer.data->[0]`), el resto en RValue (`pp->[0]->[0]`,
            // `records[i]`, `xs->[2]`).

            let base_cleaned: clang::Entity<'_> =
                crate::expr_analysis::clean_expression_wrappers(base_node);

            let base_ctx: Location =
                if matches!(base_cleaned.get_kind(), clang::EntityKind::MemberRefExpr) {
                    Location::AddressOf
                } else {
                    Location::RValue
                };

            let base: String = self::translate_expr(base_node, span, base_ctx, macro_ctx);
            let base_type: Option<clang::Type<'_>> =
                crate::expr_analysis::resolve_expression_type(base_node);

            let index: String = self::translate_expr(index_node, span, Location::RValue, macro_ctx);

            let uses_place_index: bool = base_type.is_some_and(|base_type| {
                let canonical_type: clang::Type<'_> = base_type.get_canonical_type();

                match canonical_type.get_kind() {
                    clang::TypeKind::Pointer => {
                        canonical_type
                            .get_pointee_type()
                            .is_some_and(|pointee_type| {
                                let pointee_type: clang::Type<'_> =
                                    pointee_type.get_canonical_type();

                                matches!(
                                    pointee_type.get_kind(),
                                    clang::TypeKind::Record
                                        | clang::TypeKind::ConstantArray
                                        | clang::TypeKind::IncompleteArray
                                )
                            })
                    }
                    clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray => {
                        canonical_type
                            .get_element_type()
                            .is_some_and(|element_type| {
                                let element_type: clang::Type<'_> =
                                    element_type.get_canonical_type();

                                matches!(
                                    element_type.get_kind(),
                                    clang::TypeKind::Record
                                        | clang::TypeKind::ConstantArray
                                        | clang::TypeKind::IncompleteArray
                                )
                            })
                    }
                    _ => false,
                }
            });

            if uses_place_index {
                format!("{base}[{index}]")
            } else {
                format!("{base}->[{index}]")
            }
        }

        other => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    {
                        let detail: String = format!("Unsupported expression kind: {other:?}");

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

#[allow(clippy::only_used_in_recursion)]
pub fn translate_condition_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    ctx: Location,
    macro_ctx: &mut crate::macros::MacroContext,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);

    match entity.get_kind() {
        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|candidate| crate::clang_util::is_supported_expr_kind(candidate.get_kind()))
            {
                let inner: String = self::translate_condition_expr(child, span, ctx, macro_ctx);

                return format!("({inner})");
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

            format!("({value}) != 0")
        }

        clang::EntityKind::UnexposedExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() == 1
                && crate::clang_util::is_supported_expr_kind(children[0].get_kind())
            {
                return self::translate_condition_expr(&children[0], span, ctx, macro_ctx);
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

            format!("({value}) != 0")
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!(
                        "C translation failed:\n{prefix}Missing source range for unary operator."
                    ),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
                return Default::default();
            };

            let tokens: Vec<String> = crate::macro_lex::range_spellings(&range, entity);

            if tokens.contains(&"!".to_string()) {
                let children: Vec<clang::Entity<'_>> = entity.get_children();
                let Some(operand) = children.first() else {
                    macro_ctx
                        .get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!("C translation failed:\n{prefix}Malformed unary operator."),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                };

                let operand_text: String =
                    self::translate_condition_expr(operand, span, ctx, macro_ctx);

                return format!("!({operand_text})");
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

            format!("({value}) != 0")
        }

        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed binary operator."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let Some(op) =
                crate::macro_lex::extract_binary_operator(entity, &children[0], &children[1])
                    .or_else(|| {
                        entity.get_range().and_then(|range| {
                            crate::macro_lex::extract_binary_operator_from_tokens(
                                &crate::macro_lex::range_spellings(&range, entity),
                            )
                            .map(|operator| operator.to_string())
                        })
                    })
            else {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Unsupported binary operator{}.",
                            crate::macros::origin_note(entity)
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Default::default();
            };

            if matches!(op.as_str(), "&&" | "||") {
                let left: String =
                    self::translate_condition_expr(&children[0], span, ctx, macro_ctx);
                let right: String =
                    self::translate_condition_expr(&children[1], span, ctx, macro_ctx);

                return format!("({left}) {op} ({right})");
            }

            if matches!(op.as_str(), "==" | "!=" | "<" | "<=" | ">" | ">=") {
                let left_type: Option<clang::Type<'_>> = children[0].get_type();
                let right_type: Option<clang::Type<'_>> = children[1].get_type();
                let left_char: bool = left_type.as_ref().is_some_and(|ty| {
                    matches!(
                        ty.get_canonical_type().get_kind(),
                        clang::TypeKind::CharS | clang::TypeKind::CharU
                    )
                });

                let right_char: bool = right_type.as_ref().is_some_and(|ty| {
                    matches!(
                        ty.get_canonical_type().get_kind(),
                        clang::TypeKind::CharS | clang::TypeKind::CharU
                    )
                });

                let left_value: String =
                    self::translate_expr(&children[0], span, Location::RValue, macro_ctx);
                let right_value: String =
                    self::translate_expr(&children[1], span, Location::RValue, macro_ctx);

                let left: String = if left_char {
                    format!("({left_value}) as s32")
                } else {
                    left_value
                };

                let right: String = if right_char {
                    format!("({right_value}) as s32")
                } else {
                    right_value
                };

                return format!("{left} {op} {right}");
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

            format!("({value}) != 0")
        }

        _ => {
            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

            if entity
                .get_type()
                .is_some_and(|ty| ty.get_canonical_type().get_kind() == clang::TypeKind::Bool)
            {
                value
            } else {
                format!("({value}) != 0")
            }
        }
    }
}

fn try_translate_linearized_array_subscript(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext,
) -> Option<String> {
    let mut indices_rev: Vec<clang::Entity<'_>> = Vec::new();
    let mut probe: clang::Entity<'_> = crate::expr_analysis::clean_expression_wrappers(entity);

    while probe.get_kind() == clang::EntityKind::ArraySubscriptExpr {
        let children: Vec<clang::Entity<'_>> = probe.get_children();

        if children.len() < 2 {
            return None;
        }

        indices_rev.push(children[1]);
        probe = crate::expr_analysis::clean_expression_wrappers(&children[0]);
    }

    if indices_rev.len() < 2 {
        return None;
    }

    indices_rev.reverse();

    let source_type: clang::Type<'_> = crate::expr_analysis::resolve_expression_type(&probe)?;

    let (pointer_root, extents) =
        crate::expr_analysis::analyze_nested_scalar_array_type(&source_type)?;

    let source_canonical: clang::Type<'_> = source_type.get_canonical_type();
    let parameter_array_root: bool = !pointer_root
        && matches!(
            source_canonical.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        )
        && probe
            .get_reference()
            .is_some_and(|reference| reference.get_kind() == clang::EntityKind::ParmDecl);

    let extents_len: usize = extents.len();

    let indices_len: usize = indices_rev.len();

    let expected_len: usize = if pointer_root {
        extents_len.saturating_add(1)
    } else {
        extents_len
    };

    let lengths_match: bool = expected_len == indices_len;

    if !lengths_match {
        return None;
    }

    let mut root_text: String = self::translate_expr(&probe, span, Location::RValue, macro_ctx);

    if !pointer_root && !parameter_array_root {
        root_text = format!("ref {root_text}");

        for _ in 0..extents.len() {
            root_text.push_str("[0]");
        }
    }

    let mut terms: Vec<String> = Vec::with_capacity(indices_rev.len());

    for (index_position, index_node) in indices_rev.iter().enumerate() {
        let index_text: String =
            self::translate_expr(index_node, span, Location::RValue, macro_ctx);

        let start_extent: usize = if pointer_root {
            index_position
        } else {
            index_position.saturating_add(1)
        };

        let mut stride: usize = 1;

        for extent in extents.iter().skip(start_extent) {
            stride = stride.saturating_mul(*extent);
        }

        if stride == 1 {
            terms.push(format!("({index_text})"));
        } else {
            terms.push(format!("(({}) * {})", index_text, stride));
        }
    }

    if parameter_array_root || pointer_root {
        return Some(format!("{root_text}->[{}]", terms.join(" + ")));
    }

    Some(format!("({root_text})->[{}]", terms.join(" + ")))
}

fn translate_decl_ref_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext,
) -> String {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(range) = entity.get_range() else {
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                {
                    let detail: String = format!(
                        "Missing source range for expression kind {:?}.",
                        entity.get_kind()
                    );

                    format!("C translation failed:\n{prefix}{detail}")
                },
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));

        return Default::default();
    };

    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
    let fallback: String = crate::macro_lex::tokens_to_thrust_source(&tokens);

    let Some(reference) = entity.get_reference() else {
        return fallback;
    };

    let Some(reference_location) = reference.get_location() else {
        return fallback;
    };

    let Some(reference_file) = reference_location.get_file_location().file else {
        return fallback;
    };

    let Some(usage_location) = entity.get_location() else {
        return fallback;
    };

    let Some(usage_file) = usage_location.get_expansion_location().file else {
        return fallback;
    };

    let reference_path: std::path::PathBuf = reference_file.get_path();
    let usage_path: std::path::PathBuf = usage_file.get_path();

    let reference_path: std::path::PathBuf = reference_path
        .canonicalize()
        .unwrap_or(reference_path.clone());

    let usage_path: std::path::PathBuf = usage_path.canonicalize().unwrap_or(usage_path.clone());

    if reference_path == usage_path {
        return fallback;
    }

    let Some(module_stem) = reference_path.file_stem() else {
        return fallback;
    };

    let Some(reference_name) = reference.get_name().or_else(|| entity.get_name()) else {
        return fallback;
    };

    let module_name: String =
        { crate::util::sanitize_thrust_identifier(module_stem.to_string_lossy()) };

    let symbol_name: String = { crate::util::sanitize_thrust_identifier(&reference_name) };

    format!("{module_name}::{symbol_name}")
}

fn translate_pointer_target_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    macro_ctx: &mut crate::macros::MacroContext,
) -> Option<String> {
    if matches!(
        entity.get_kind(),
        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
    ) {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.len() == 1 && crate::clang_util::is_supported_expr_kind(children[0].get_kind())
        {
            return self::translate_pointer_target_expr(&children[0], span, macro_ctx);
        }
    }

    if entity.get_kind() == clang::EntityKind::BinaryOperator {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.len() >= 2
            && let Some(op) =
                crate::macro_lex::extract_binary_operator(entity, &children[0], &children[1])
            && op == "+"
        {
            let base: String =
                self::translate_expr(&children[0], span, Location::RValue, macro_ctx);
            let index: String =
                self::translate_expr(&children[1], span, Location::RValue, macro_ctx);

            return Some(format!("{base}->[{index}]"));
        }
    }

    let base: String = self::translate_expr(entity, span, Location::RValue, macro_ctx);

    Some(format!("{base}->[0]"))
}
