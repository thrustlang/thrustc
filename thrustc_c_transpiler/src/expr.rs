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

use crate::error::{TranspilerError, TranspilerResult};
use crate::location::Location;

pub fn translate_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    ctx: Location,
    macro_ctx: &mut crate::macros::MacroContext,
) -> TranspilerResult<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);

    if let Some(expression_text) =
        crate::macros::try_extract_expression_macro_call(macro_ctx, entity, span)
    {
        return Ok(expression_text);
    }

    match entity.get_kind() {
        clang::EntityKind::IntegerLiteral
        | clang::EntityKind::FloatingLiteral
        | clang::EntityKind::StringLiteral
        | clang::EntityKind::CharacterLiteral => {
            let Some(range) = entity.get_range() else {
                let issue: String = {
                    let detail: String = format!(
                        "Missing source range for expression kind {:?}.",
                        entity.get_kind()
                    );

                    format!("C translation failed:\n{prefix}{detail}")
                };

                macro_ctx
                    .get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        issue,
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                return Err(TranspilerError::Abort);
            };

            let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

            if let Some(first) = tokens.first() {
                if entity.get_kind() == clang::EntityKind::StringLiteral {
                    let mut concatenated: String = String::new();

                    for token in tokens.iter() {
                        let token_spelling: String = token.get_spelling();

                        let start: usize = token_spelling.find('"').map_or(0, |idx| idx + 1);
                        let end: usize = token_spelling.rfind('"').unwrap_or(token_spelling.len());

                        if start <= end {
                            concatenated.push_str(&token_spelling[start..end]);
                        }
                    }

                    return Ok(format!("\"{concatenated}\""));
                }

                let spelling: String = first.get_spelling();

                if entity.get_kind() == clang::EntityKind::CharacterLiteral {
                    let byte: Option<u8> = match entity.evaluate() {
                        Some(clang::EvaluationResult::SignedInteger(value)) => Some(value as u8),
                        Some(clang::EvaluationResult::UnsignedInteger(value)) => Some(value as u8),
                        _ => None,
                    };

                    // C character literals may use escapes Thrust does not parse identically
                    // (hex '\x41', octal '\101', etc.), so we materialize the byte through
                    // clang's evaluation and re-emit it as a canonical Thrust char literal,
                    // falling back to a numeric `(N as char)` for non-printable bytes.
                    if let Some(byte) = byte {
                        return Ok(match byte {
                            b'\n' => "'\\n'".to_string(),
                            b'\t' => "'\\t'".to_string(),
                            b'\r' => "'\\r'".to_string(),
                            0 => "'\\0'".to_string(),
                            b'\\' => "'\\\\'".to_string(),
                            b'\'' => "'\\''".to_string(),
                            b'"' => "'\\\"'".to_string(),
                            0x20..=0x7E => format!("'{}'", byte as char),
                            other => format!("({other} as char)"),
                        });
                    }
                }

                if entity.get_kind() == clang::EntityKind::IntegerLiteral {
                    let trimmed: &str = spelling.trim_end_matches(['u', 'U', 'l', 'L']);

                    let has_suffix: bool = trimmed.len() != spelling.len();

                    let is_octal: bool = trimmed.len() > 1
                        && trimmed.starts_with('0')
                        && !trimmed.starts_with("0x")
                        && !trimmed.starts_with("0X")
                        && trimmed.chars().all(|ch| ch.is_ascii_digit());

                    let normalized: String = if is_octal {
                        u64::from_str_radix(&trimmed[1..], 8)
                            .map(|value| value.to_string())
                            .unwrap_or_else(|_| trimmed.to_string())
                    } else {
                        trimmed.to_string()
                    };

                    let Some(ty) = entity.get_type() else {
                        macro_ctx.get_mut_transpiler_context().add_error_fail(
                            CompilationIssue::Error(
                                CompilationIssueCode::E0110,
                                format!(
                                    "C translation failed:\n{prefix}Missing type for integer literal."
                                ),
                                "Rewrite the C input to avoid the unsupported construct.".into(),
                                None,
                                span,
                            ),
                        );

                        return Err(TranspilerError::Abort);
                    };

                    let type_text: String = crate::type_format::format_clang_type_thrust(
                        &ty, macro_ctx, &prefix, span,
                    )?;

                    if has_suffix && !type_text.is_empty() && type_text != "s32" {
                        return Ok(format!("({normalized}) as {type_text}"));
                    }

                    return Ok(normalized);
                }

                if entity.get_kind() == clang::EntityKind::FloatingLiteral {
                    let normalized: &str = spelling.trim_end_matches(['f', 'F', 'l', 'L']);

                    let needs_normalization: bool = normalized.starts_with('.')
                        || normalized.ends_with('.')
                        || normalized.contains('e')
                        || normalized.contains('E');

                    if needs_normalization
                        && let Some(clang::EvaluationResult::Float(value)) = entity.evaluate()
                    {
                        let text: String = format!("{value}");

                        return Ok(if text.contains('.') {
                            text
                        } else {
                            format!("{text}.0")
                        });
                    }

                    return Ok(normalized.to_string());
                }

                return Ok(spelling);
            }

            Ok(String::new())
        }

        clang::EntityKind::DeclRefExpr => {
            Ok(self::translate_decl_ref_expr(entity, span, macro_ctx)?)
        }
        clang::EntityKind::GNUNullExpr | clang::EntityKind::NullPtrLiteralExpr => {
            Ok("nullptr".into())
        }
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

                return Err(TranspilerError::Abort);
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

                return Err(TranspilerError::Abort);
            };

            let base: String = self::translate_expr(base_node, span, Location::RValue, macro_ctx)?;

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

            let field_name: String = { crate::util::normalize_to_thrust_identifier(&field_name) };

            Ok(format!("{base}{operator}{field_name}"))
        }

        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|c| crate::clang_util::is_supported_expr_kind(c.get_kind()))
            {
                let inner: String = self::translate_expr(child, span, ctx, macro_ctx)?;

                if matches!(
                    child.get_kind(),
                    clang::EntityKind::IntegerLiteral
                        | clang::EntityKind::FloatingLiteral
                        | clang::EntityKind::StringLiteral
                        | clang::EntityKind::CharacterLiteral
                        | clang::EntityKind::DeclRefExpr
                        | clang::EntityKind::MemberRefExpr
                ) {
                    return Ok(inner);
                }

                if !entity.is_in_main_file() {
                    return Ok(inner);
                }

                return Ok(format!("({inner})"));
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

                return Err(TranspilerError::Abort);
            };

            Ok(crate::macro_lex::tokens_to_thrust_source(&range.tokenize()))
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

                return Err(TranspilerError::Abort);
            };

            Ok(crate::macro_lex::tokens_to_thrust_source(&range.tokenize()))
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

                return Err(TranspilerError::Abort);
            }

            let callee: String =
                self::translate_expr(&children[0], span, Location::RValue, macro_ctx)?;

            // Resolve the callee's parameter types for implicit argument casts. Prefer the
            // call's own reference; otherwise unwrap a single parenthesized/exposed callee
            // and fall back to the callee expression's reference. The first reference that
            // actually exposes arguments wins.
            let callee_reference: Option<clang::Entity<'_>> = if matches!(
                children[0].get_kind(),
                clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
            ) {
                let nested_children: Vec<clang::Entity<'_>> = children[0].get_children();

                if nested_children.len() == 1 {
                    nested_children[0].get_reference()
                } else {
                    None
                }
            } else {
                children[0].get_reference()
            };

            let mut expected_parameter_types: Vec<clang::Type<'_>> =
                [entity.get_reference(), callee_reference]
                    .into_iter()
                    .flatten()
                    .find_map(|reference| reference.get_arguments())
                    .map(|arguments| {
                        arguments
                            .into_iter()
                            .filter_map(|argument| argument.get_type())
                            .collect()
                    })
                    .unwrap_or_default();

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

                let translated: String = self::translate_expr(arg, span, arg_ctx, macro_ctx)?;

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

                                args.push(format!("{translated}{zero_path}"));
                                continue;
                            }

                            let canonical_element_type: clang::Type<'_> =
                                element_type.get_canonical_type();

                            if canonical_element_type.get_kind() == clang::TypeKind::Record {
                                args.push(format!("{translated}[0]"));
                                continue;
                            }

                            let element_text: String =
                                crate::type_format::format_clang_type_thrust(
                                    &element_type,
                                    macro_ctx,
                                    &prefix,
                                    span,
                                )?;

                            args.push(format!("{translated} as ptr[{element_text}]"));
                            continue;
                        }
                    }
                }

                let expected_is_record: bool =
                    expected_parameter_types.get(index).is_some_and(|expected| {
                        expected.get_canonical_type().get_kind() == clang::TypeKind::Record
                    });

                let arg_is_place: bool = matches!(
                    crate::expr_analysis::clean_expression_wrappers(arg).get_kind(),
                    clang::EntityKind::ArraySubscriptExpr
                );

                if expected_is_record && arg_is_place {
                    args.push(format!("deref {translated}"));
                } else if let Some(expected_type) = expected_parameter_types.get(index) {
                    args.push(crate::type_format::cast_expression_to_type(
                        arg,
                        translated,
                        expected_type,
                        span,
                        macro_ctx,
                    )?);
                } else {
                    args.push(translated);
                }
            }

            if let Some(canonical) =
                crate::builtins::CanonicalBuiltin::from_called_function_name(&callee)
                && let Some(canonical_text) = canonical.rewrite_builtin_call(&children[1..], &args)
            {
                return Ok(canonical_text);
            }

            if let Some(heap_operation) =
                crate::builtins::HeapOperation::from_called_function_name(&callee)
            {
                if let Some(halloc_text) =
                    heap_operation.try_lower_heap_call(entity, None, macro_ctx, &prefix, span)
                {
                    return Ok(halloc_text);
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

                return Err(TranspilerError::Abort);
            }

            Ok(format!("{callee}({})", args.join(", ")))
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
                return Err(TranspilerError::Abort);
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
                return Err(TranspilerError::Abort);
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
                    return Err(TranspilerError::Abort);
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

                let is_pointer_target: bool = target_type_kind == Some(clang::TypeKind::Pointer);

                if !is_arithmetic_target && !is_pointer_target {
                    macro_ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Increment/decrement requires an integer or floating-point operand."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                    return Err(TranspilerError::Abort);
                }

                if target_kind == clang::EntityKind::DeclRefExpr {
                    let name: String =
                        self::translate_expr(operand, span, Location::RValue, macro_ctx)?;

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
                        return Err(TranspilerError::Abort);
                    }

                    if is_pointer_target {
                        let pointee_is_pointer: bool = cleaned
                            .get_type()
                            .map(|ty| ty.get_canonical_type())
                            .and_then(|canonical| canonical.get_pointee_type())
                            .is_some_and(|inner| {
                                inner.get_canonical_type().get_kind() == clang::TypeKind::Pointer
                            });

                        return Ok(crate::pointer::Pointer::lower_advance(
                            &name,
                            "1",
                            is_increment,
                            pointee_is_pointer,
                        ));
                    }

                    if is_prefix {
                        return Ok(format!("{operator_text}{name}"));
                    }

                    return Ok(format!("{name}{operator_text}"));
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
                    return Err(TranspilerError::Abort);
                }

                let place_text: String =
                    self::translate_expr(&cleaned, span, Location::RValue, macro_ctx)?;

                if is_prefix {
                    return Ok(format!("{operator_text}({place_text})"));
                }

                return Ok(format!("({place_text}){operator_text}"));
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
                return Err(TranspilerError::Abort);
            };

            let operand_ctx: Location = if op == "&" {
                Location::AddressOf
            } else {
                Location::RValue
            };

            let operand_text: String = self::translate_expr(operand, span, operand_ctx, macro_ctx)?;

            match op {
                "&" => {
                    let is_place_operand: bool = matches!(
                        crate::expr_analysis::clean_expression_wrappers(operand).get_kind(),
                        clang::EntityKind::ArraySubscriptExpr
                            | clang::EntityKind::MemberRef
                            | clang::EntityKind::MemberRefExpr
                    );

                    if is_place_operand {
                        Ok(operand_text)
                    } else {
                        Ok(format!("ref ({operand_text})"))
                    }
                }
                "*" => {
                    if let Some(pointer_target) =
                        self::translate_transform_pointer_arithmetic(operand, span, macro_ctx)
                    {
                        return Ok(pointer_target);
                    }

                    let needs_parens: bool = matches!(
                        operand.get_kind(),
                        clang::EntityKind::BinaryOperator
                            | clang::EntityKind::CompoundAssignOperator
                    );

                    if needs_parens {
                        Ok(format!("(deref ({operand_text}))"))
                    } else {
                        Ok(format!("(deref {operand_text})"))
                    }
                }
                "!" => Ok(format!("!{operand_text}")),
                "+" => Ok(operand_text),
                "-" => {
                    let result_type: String = match entity.get_type() {
                        Some(ty) => crate::type_format::format_clang_type_thrust(
                            &ty, macro_ctx, &prefix, span,
                        )?,
                        None => "s32".to_string(),
                    };

                    Ok(format!("(-{operand_text}) as {result_type}"))
                }
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
                        return Ok(format!("~({operand_text} as s32)"));
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
                        if operand_text.contains(" as ") {
                            return Ok(format!("~({operand_text})"));
                        }

                        let operand_type_text: String = match cleaned_operand.get_type() {
                            Some(ty) => crate::type_format::format_clang_type_thrust(
                                &ty, macro_ctx, &prefix, span,
                            )?,
                            None => "s32".to_string(),
                        };

                        return Ok(format!("~({operand_text} as {operand_type_text})"));
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

                    Err(TranspilerError::Abort)
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

                    Err(TranspilerError::Abort)
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
                return Err(TranspilerError::Abort);
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
                    let canonical: clang::Type<'_> = operand.get_canonical_type();

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
                    return Ok(format!("{literal_size}"));
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

                    return Err(TranspilerError::Abort);
                };

                let operand_type_text: String = crate::type_format::format_clang_type_thrust(
                    &operand_type,
                    macro_ctx,
                    &prefix,
                    span,
                )?;

                if operand_type_text.is_empty() {
                    match operand_type.get_sizeof() {
                        Ok(evaluated_size)
                            if !matches!(
                                operand_type.get_canonical_type().get_kind(),
                                clang::TypeKind::LongDouble | clang::TypeKind::Complex
                            ) =>
                        {
                            return Ok(format!("{evaluated_size}"));
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
                    )?;

                    sizeof_text = format!("({sizeof_text}) as {result_type_text}");
                }

                return Ok(sizeof_text);
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

            Err(TranspilerError::Abort)
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

                return Err(TranspilerError::Abort);
            }

            let lhs_ctx: Location = if crate::macro_lex::is_assignment_expression(entity) {
                Location::LValue
            } else {
                Location::RValue
            };

            let left: String = self::translate_expr(&children[0], span, lhs_ctx, macro_ctx)?;

            let right: String =
                self::translate_expr(&children[1], span, Location::RValue, macro_ctx)?;

            let Some(op) =
                crate::macro_lex::lex_binary_operator(entity, &children[0], &children[1]).or_else(
                    || {
                        entity.get_range().and_then(|range| {
                            crate::macro_lex::extract_binary_operator_from_tokens(
                                &crate::macro_lex::range_spellings(&range, entity),
                            )
                            .map(|operator| operator.to_string())
                        })
                    },
                )
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

                return Err(TranspilerError::Abort);
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

                return Err(TranspilerError::Abort);
            }

            if matches!(op.as_str(), "&&" | "||") {
                let left: String = self::translate_condition_expr(
                    &children[0],
                    span,
                    Location::RValue,
                    macro_ctx,
                )?;
                let right: String = self::translate_condition_expr(
                    &children[1],
                    span,
                    Location::RValue,
                    macro_ctx,
                )?;

                return Ok(format!("({left}) {op} ({right})"));
            }

            let folds_as_constant: bool = !matches!(
                op.as_str(),
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
            ) && matches!(
                op.as_str(),
                "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^"
            );

            if folds_as_constant && let Some(result_type) = entity.get_type() {
                let evaluated: Option<String> = match entity.evaluate() {
                    Some(clang::EvaluationResult::SignedInteger(value)) => Some(value.to_string()),
                    Some(clang::EvaluationResult::UnsignedInteger(value)) => {
                        Some(value.to_string())
                    }
                    _ => None,
                };

                if let Some(value) = evaluated {
                    let result_type_text: String = crate::type_format::format_clang_type_thrust(
                        &result_type,
                        macro_ctx,
                        &prefix,
                        span,
                    )?;

                    if !result_type_text.is_empty() {
                        return Ok(format!("({value}) as {result_type_text}"));
                    }
                }
            }

            let is_assignment: bool = matches!(
                op.as_str(),
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
            );

            let left_type_kind: Option<clang::TypeKind> = children[0]
                .get_type()
                .map(|ty| ty.get_canonical_type().get_kind());

            let right_type_kind: Option<clang::TypeKind> = children[1]
                .get_type()
                .map(|ty| ty.get_canonical_type().get_kind());

            let left_is_pointer: bool = left_type_kind.is_some_and(|kind| {
                matches!(
                    kind,
                    clang::TypeKind::Pointer
                        | clang::TypeKind::ConstantArray
                        | clang::TypeKind::IncompleteArray
                )
            });

            let right_is_pointer: bool = right_type_kind.is_some_and(|kind| {
                matches!(
                    kind,
                    clang::TypeKind::Pointer
                        | clang::TypeKind::ConstantArray
                        | clang::TypeKind::IncompleteArray
                )
            });

            let pointee_is_pointer: bool = if left_is_pointer {
                children[0]
            } else {
                children[1]
            }
            .get_type()
            .map(|ty| ty.get_canonical_type())
            .and_then(|canonical| {
                canonical
                    .get_pointee_type()
                    .or_else(|| canonical.get_element_type())
            })
            .is_some_and(|inner| inner.get_canonical_type().get_kind() == clang::TypeKind::Pointer);

            if is_assignment && left_is_pointer && (op == "+=" || op == "-=") {
                return Ok(crate::pointer::Pointer::lower_advance(
                    &left,
                    &right,
                    op == "+=",
                    pointee_is_pointer,
                ));
            }

            if !is_assignment
                && let Some(transformed) = crate::pointer::Pointer::lower_binary(
                    left_is_pointer,
                    right_is_pointer,
                    pointee_is_pointer,
                    &op,
                    &left,
                    &right,
                )
            {
                return Ok(transformed);
            }

            let is_arithmetic_operation: bool = matches!(
                op.as_str(),
                "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^"
            );

            if !is_assignment
                && is_arithmetic_operation
                && !left_is_pointer
                && !right_is_pointer
                && let Some(result_type) = entity.get_type()
            {
                let left_entity: clang::Entity<'_> =
                    crate::expr_analysis::clean_expression_wrappers(&children[0]);
                let right_entity: clang::Entity<'_> =
                    crate::expr_analysis::clean_expression_wrappers(&children[1]);

                let left_char: bool = left_entity.get_kind() == clang::EntityKind::CharacterLiteral
                    || left_entity.get_type().is_some_and(|ty| {
                        matches!(
                            ty.get_canonical_type().get_kind(),
                            clang::TypeKind::CharS | clang::TypeKind::CharU
                        )
                    });

                let right_char: bool = right_entity.get_kind()
                    == clang::EntityKind::CharacterLiteral
                    || right_entity.get_type().is_some_and(|ty| {
                        matches!(
                            ty.get_canonical_type().get_kind(),
                            clang::TypeKind::CharS | clang::TypeKind::CharU
                        )
                    });

                let left_casted: String = if left_char {
                    format!("({left}) as s32")
                } else {
                    crate::type_format::cast_expression_to_type(
                        &children[0],
                        left.clone(),
                        &result_type,
                        span,
                        macro_ctx,
                    )?
                };

                let right_casted: String = if right_char {
                    format!("({right}) as s32")
                } else {
                    crate::type_format::cast_expression_to_type(
                        &children[1],
                        right.clone(),
                        &result_type,
                        span,
                        macro_ctx,
                    )?
                };

                return Ok(format!("{left_casted} {op} {right_casted}"));
            }

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
                        )?;
                    }
                }
            }

            Ok(format!("{left} {op} {right}"))
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

                return Err(TranspilerError::Abort);
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

                return Err(TranspilerError::Abort);
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
                    return Ok(halloc_text);
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

                return Err(TranspilerError::Abort);
            }

            let value: String =
                self::translate_expr(value_node, span, Location::RValue, macro_ctx)?;

            if to_ty.get_canonical_type().get_kind() == clang::TypeKind::Void {
                return Ok(value);
            }

            let to_ty_text: String =
                crate::type_format::format_clang_type_thrust(&to_ty, macro_ctx, &prefix, span)?;

            Ok(format!("({value}) as {to_ty_text}"))
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
                return Err(TranspilerError::Abort);
            }

            let cond: String =
                self::translate_condition_expr(&children[0], span, Location::RValue, macro_ctx)?;
            let then_expr: String =
                self::translate_expr(&children[1], span, Location::RValue, macro_ctx)?;
            let else_expr: String =
                self::translate_expr(&children[2], span, Location::RValue, macro_ctx)?;

            let result_type: Option<clang::Type<'_>> = entity.get_type();

            let type_text: Option<String> = match result_type.as_ref() {
                Some(ty) => Some(crate::type_format::format_clang_type_thrust(
                    ty, macro_ctx, &prefix, span,
                )?),
                None => None,
            };

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
                )?,
                None => then_expr,
            };

            let else_expr: String = match result_type.as_ref() {
                Some(expected) => crate::type_format::cast_expression_to_type(
                    &children[2],
                    else_expr,
                    expected,
                    span,
                    macro_ctx,
                )?,
                None => else_expr,
            };

            let temporary: String = macro_ctx.next_temporary_name();

            macro_ctx.push_pending_statement(format!("var {temporary}: {type_text};"));
            macro_ctx.push_pending_statement(format!("if {cond} {{"));
            macro_ctx.push_pending_statement(format!("    {temporary} = {then_expr};"));
            macro_ctx.push_pending_statement("} else {".into());
            macro_ctx.push_pending_statement(format!("    {temporary} = {else_expr};"));
            macro_ctx.push_pending_statement("}".into());

            Ok(temporary)
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

                return Err(TranspilerError::Abort);
            };

            Ok(crate::top_level::translate_global_initializer(
                entity,
                &compound_type,
                span,
                macro_ctx,
            )?)
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

                return Err(TranspilerError::Abort);
            };

            Ok(crate::top_level::translate_global_initializer(
                entity, &init_type, span, macro_ctx,
            )?)
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

                return Err(TranspilerError::Abort);
            }

            let base_node: &clang::Entity<'_> = &children[0];
            let index_node: &clang::Entity<'_> = &children[1];

            // Bajo `ref` (AddressOf) el subíndice debe conservarse como
            // place `[i]` sin loads: `ref` anula cualquier RValue y
            // `ref x->[i]` es E0008 (VALUE WITHOUT ADDRESS). La base
            // propaga el contexto (cadena `&s.arr[i]`); el índice se
            // lee como valor.
            if ctx.is_address_of() {
                let base: String = self::translate_expr(base_node, span, ctx, macro_ctx)?;

                let index: String =
                    self::translate_expr(index_node, span, Location::RValue, macro_ctx)?;

                return Ok(format!("{base}[{index}]"));
            }

            if let Some(linearized) = self::try_translate_linearized_array(entity, span, macro_ctx)
            {
                return Ok(linearized);
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

            let base: String = self::translate_expr(base_node, span, base_ctx, macro_ctx)?;
            let base_type: Option<clang::Type<'_>> =
                crate::expr_analysis::resolve_expression_type(base_node);

            let index: String =
                self::translate_expr(index_node, span, Location::RValue, macro_ctx)?;

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
                Ok(format!("{base}[{index}]"))
            } else {
                Ok(format!("{base}->[{index}]"))
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

            Err(TranspilerError::Abort)
        }
    }
}

#[allow(clippy::only_used_in_recursion)]
pub fn translate_condition_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    ctx: Location,
    macro_ctx: &mut crate::macros::MacroContext,
) -> TranspilerResult<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);

    match entity.get_kind() {
        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|candidate| crate::clang_util::is_supported_expr_kind(candidate.get_kind()))
            {
                let inner: String = self::translate_condition_expr(child, span, ctx, macro_ctx)?;

                return Ok(format!("({inner})"));
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx)?;

            Ok(format!("({value}) != 0"))
        }

        clang::EntityKind::UnexposedExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() == 1
                && crate::clang_util::is_supported_expr_kind(children[0].get_kind())
            {
                return self::translate_condition_expr(&children[0], span, ctx, macro_ctx);
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx)?;

            Ok(format!("({value}) != 0"))
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
                return Err(TranspilerError::Abort);
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
                    return Err(TranspilerError::Abort);
                };

                let operand_text: String =
                    self::translate_condition_expr(operand, span, ctx, macro_ctx)?;

                return Ok(format!("!({operand_text})"));
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx)?;

            Ok(format!("({value}) != 0"))
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
                return Err(TranspilerError::Abort);
            }

            let Some(op) =
                crate::macro_lex::lex_binary_operator(entity, &children[0], &children[1]).or_else(
                    || {
                        entity.get_range().and_then(|range| {
                            crate::macro_lex::extract_binary_operator_from_tokens(
                                &crate::macro_lex::range_spellings(&range, entity),
                            )
                            .map(|operator| operator.to_string())
                        })
                    },
                )
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

                return Err(TranspilerError::Abort);
            };

            if matches!(op.as_str(), "&&" | "||") {
                let left: String =
                    self::translate_condition_expr(&children[0], span, ctx, macro_ctx)?;
                let right: String =
                    self::translate_condition_expr(&children[1], span, ctx, macro_ctx)?;

                return Ok(format!("({left}) {op} ({right})"));
            }

            if matches!(op.as_str(), "==" | "!=" | "<" | "<=" | ">" | ">=") {
                let left_entity: clang::Entity<'_> =
                    crate::expr_analysis::clean_expression_wrappers(&children[0]);
                let right_entity: clang::Entity<'_> =
                    crate::expr_analysis::clean_expression_wrappers(&children[1]);

                let left_type: Option<clang::Type<'_>> = left_entity.get_type();
                let right_type: Option<clang::Type<'_>> = right_entity.get_type();

                let left_char: bool = left_type.as_ref().is_some_and(|ty| {
                    matches!(
                        ty.get_canonical_type().get_kind(),
                        clang::TypeKind::CharS | clang::TypeKind::CharU
                    )
                }) || left_entity.get_kind()
                    == clang::EntityKind::CharacterLiteral;

                let right_char: bool = right_type.as_ref().is_some_and(|ty| {
                    matches!(
                        ty.get_canonical_type().get_kind(),
                        clang::TypeKind::CharS | clang::TypeKind::CharU
                    )
                }) || right_entity.get_kind()
                    == clang::EntityKind::CharacterLiteral;

                let left_value: String =
                    self::translate_expr(&children[0], span, Location::RValue, macro_ctx)?;
                let right_value: String =
                    self::translate_expr(&children[1], span, Location::RValue, macro_ctx)?;

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

                return Ok(format!("{left} {op} {right}"));
            }

            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx)?;

            Ok(format!("({value}) != 0"))
        }

        _ => {
            let value: String = self::translate_expr(entity, span, Location::RValue, macro_ctx)?;

            if entity
                .get_type()
                .is_some_and(|ty| ty.get_canonical_type().get_kind() == clang::TypeKind::Bool)
            {
                Ok(value)
            } else {
                Ok(format!("({value}) != 0"))
            }
        }
    }
}

fn try_translate_linearized_array(
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

    let mut root_text: String =
        self::translate_expr(&probe, span, Location::RValue, macro_ctx).ok()?;

    if !pointer_root && !parameter_array_root {
        for _ in 0..extents.len() {
            root_text.push_str("[0]");
        }
    }

    let mut terms: Vec<String> = Vec::with_capacity(indices_rev.len());

    for (index_position, index_node) in indices_rev.iter().enumerate() {
        let index_text: String =
            self::translate_expr(index_node, span, Location::RValue, macro_ctx).ok()?;

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
) -> TranspilerResult<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let Some(range) = entity.get_range() else {
        let issue: String = {
            let detail: String = format!(
                "Missing source range for expression kind {:?}.",
                entity.get_kind()
            );

            format!("C translation failed:\n{prefix}{detail}")
        };

        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                issue,
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));

        return Err(TranspilerError::Abort);
    };

    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
    let fallback: String = crate::macro_lex::tokens_to_thrust_source(&tokens);

    let Some(reference) = entity.get_reference() else {
        return Ok(fallback);
    };

    if reference.get_kind() == clang::EntityKind::EnumConstantDecl {
        let variant_name: String = {
            let name: String = reference.get_name().unwrap_or_else(|| fallback.clone());

            crate::util::normalize_to_thrust_identifier(&name)
        };

        let enum_name: Option<String> = reference
            .get_semantic_parent()
            .and_then(|parent| parent.get_name())
            .map(|name| crate::util::normalize_to_thrust_identifier(&name));

        return match enum_name {
            Some(enum_name) => Ok(format!("{enum_name}=>{variant_name}")),
            None => Ok(variant_name),
        };
    }

    let Some(reference_location) = reference.get_location() else {
        return Ok(fallback);
    };

    let Some(reference_file) = reference_location.get_file_location().file else {
        return Ok(fallback);
    };

    let Some(usage_location) = entity.get_location() else {
        return Ok(fallback);
    };

    let Some(usage_file) = usage_location.get_expansion_location().file else {
        return Ok(fallback);
    };

    let reference_path: std::path::PathBuf = reference_file.get_path();
    let usage_path: std::path::PathBuf = usage_file.get_path();

    let reference_path: std::path::PathBuf = reference_path
        .canonicalize()
        .unwrap_or(reference_path.clone());

    let usage_path: std::path::PathBuf = usage_path.canonicalize().unwrap_or(usage_path.clone());

    if reference_path == usage_path {
        return Ok(fallback);
    }

    let Some(module_stem) = reference_path.file_stem() else {
        return Ok(fallback);
    };

    let Some(reference_name) = reference.get_name().or_else(|| entity.get_name()) else {
        return Ok(fallback);
    };

    let module_name: String =
        crate::util::normalize_to_thrust_identifier(module_stem.to_string_lossy());

    let symbol_name: String = crate::util::normalize_to_thrust_identifier(&reference_name);

    Ok(format!("{module_name}::{symbol_name}"))
}

fn translate_transform_pointer_arithmetic(
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
            return self::translate_transform_pointer_arithmetic(&children[0], span, macro_ctx);
        }
    }

    let pointee_is_pointer: bool = entity
        .get_type()
        .map(|ty| ty.get_canonical_type())
        .and_then(|canonical| canonical.get_pointee_type())
        .is_some_and(|inner| inner.get_canonical_type().get_kind() == clang::TypeKind::Pointer);

    if entity.get_kind() == clang::EntityKind::BinaryOperator {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.len() >= 2
            && let Some(op) =
                crate::macro_lex::lex_binary_operator(entity, &children[0], &children[1])
            && (op == "+" || op == "-")
        {
            let left_is_pointer: bool = children[0]
                .get_type()
                .map(|ty| ty.get_canonical_type().get_kind())
                .is_some_and(|kind| {
                    matches!(
                        kind,
                        clang::TypeKind::Pointer
                            | clang::TypeKind::ConstantArray
                            | clang::TypeKind::IncompleteArray
                    )
                });

            let (base, index): (String, String) = if left_is_pointer {
                let base: String =
                    self::translate_expr(&children[0], span, Location::RValue, macro_ctx).ok()?;
                let right_text: String =
                    self::translate_expr(&children[1], span, Location::RValue, macro_ctx).ok()?;

                if op == "-" {
                    (base, format!("0 - {right_text}"))
                } else {
                    (base, right_text)
                }
            } else {
                let base: String =
                    self::translate_expr(&children[1], span, Location::RValue, macro_ctx).ok()?;
                let index: String =
                    self::translate_expr(&children[0], span, Location::RValue, macro_ctx).ok()?;

                (base, index)
            };

            return Some(crate::pointer::Pointer::lower_deref(
                &base,
                &index,
                pointee_is_pointer,
            ));
        }
    }

    let base: String = self::translate_expr(entity, span, Location::RValue, macro_ctx).ok()?;

    Some(crate::pointer::Pointer::lower_deref(
        &base,
        "0",
        pointee_is_pointer,
    ))
}
