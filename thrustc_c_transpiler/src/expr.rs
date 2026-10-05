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

pub(crate) fn translate_expr(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    match entity.get_kind() {
        clang::EntityKind::IntegerLiteral
        | clang::EntityKind::FloatingLiteral
        | clang::EntityKind::StringLiteral
        | clang::EntityKind::CharacterLiteral => {
            let Some(range) = entity.get_range() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!(
                        "Missing source range for expression kind {:?}.",
                        entity.get_kind()
                    ),
                    None,
                    span,
                ));
            };

            let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

            if let Some(first) = tokens.first() {
                let spelling: String = first.get_spelling();

                if entity.get_kind() == clang::EntityKind::IntegerLiteral {
                    return Ok(spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string());
                }

                if entity.get_kind() == clang::EntityKind::FloatingLiteral {
                    return Ok(spelling.trim_end_matches(['f', 'F', 'l', 'L']).to_string());
                }

                return Ok(spelling);
            }

            Ok(String::new())
        }

        clang::EntityKind::DeclRefExpr => self::translate_decl_ref_expr(entity, span),

        clang::EntityKind::GNUNullExpr | clang::EntityKind::NullPtrLiteralExpr => {
            Ok("nullptr".into())
        }

        clang::EntityKind::MemberRefExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let Some(base_node) = children.first() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed member reference expression.".into(),
                    None,
                    span,
                ));
            };

            let Some(field_name) = entity.get_name() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Member reference expression is missing its field name.".into(),
                    None,
                    span,
                ));
            };

            let base: String = self::translate_expr(base_node, span)?;
            let base_probe: clang::Entity<'_> = self::peel_expression_wrappers(base_node);

            let uses_arrow: bool = if let Some(range) = entity.get_range() {
                let tokens: Vec<String> = range
                    .tokenize()
                    .into_iter()
                    .map(|token| token.get_spelling())
                    .collect();

                if tokens.iter().any(|token| token == "->") {
                    true
                } else {
                    matches!(base_probe.get_kind(), clang::EntityKind::ArraySubscriptExpr)
                }
            } else {
                matches!(base_probe.get_kind(), clang::EntityKind::ArraySubscriptExpr)
            };

            let operator: &str = if uses_arrow { "->" } else { "." };
            let field_name: String = crate::sanitize_identifier_for_thrust(&field_name);

            Ok(format!("{base}{operator}{field_name}"))
        }

        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|c| crate::is_supported_expr_kind(c.get_kind()))
            {
                let inner: String = self::translate_expr(child, span)?;

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
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing source range for parenthesized expression.".into(),
                    None,
                    span,
                ));
            };

            Ok(crate::tokens_to_thrust_source(&range.tokenize()))
        }

        clang::EntityKind::UnexposedExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let supported_children: Vec<clang::Entity<'_>> = children
                .iter()
                .copied()
                .filter(|child| crate::is_supported_expr_kind(child.get_kind()))
                .collect();

            if supported_children.len() == 1 {
                return self::translate_expr(&supported_children[0], span);
            }

            let Some(range) = entity.get_range() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing source range for unexposed expression.".into(),
                    None,
                    span,
                ));
            };

            Ok(crate::tokens_to_thrust_source(&range.tokenize()))
        }

        clang::EntityKind::CallExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.is_empty() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed call expression.".into(),
                    None,
                    span,
                ));
            }

            let callee: String = self::translate_expr(&children[0], span)?;

            let mut expected_parameter_types: Vec<clang::Type<'_>> = if let Some(reference) = entity.get_reference() {
                if let Some(arguments) = reference.get_arguments() {
                    arguments
                        .into_iter()
                        .filter_map(|argument| argument.get_type())
                        .collect()
                } else {
                    Vec::new()
                }
            } else if matches!(
                children[0].get_kind(),
                clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
            ) {
                let nested_children: Vec<clang::Entity<'_>> = children[0].get_children();

                if nested_children.len() == 1 {
                    if let Some(reference) = nested_children[0].get_reference() {
                        if let Some(arguments) = reference.get_arguments() {
                            arguments
                                .into_iter()
                                .filter_map(|argument| argument.get_type())
                                .collect()
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            } else if let Some(reference) = children[0].get_reference() {
                if let Some(arguments) = reference.get_arguments() {
                    arguments
                        .into_iter()
                        .filter_map(|argument| argument.get_type())
                        .collect()
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            if expected_parameter_types.is_empty() {
                if let Some(callee_type) = children[0].get_type() {
                    if let Some(argument_types) = callee_type.get_argument_types() {
                        expected_parameter_types = argument_types;
                    } else if let Some(pointee_type) = callee_type.get_pointee_type()
                        && let Some(argument_types) = pointee_type.get_argument_types()
                    {
                        expected_parameter_types = argument_types;
                    }
                }
            }

            let mut args: Vec<String> = Vec::new();

            for (index, arg) in children.iter().skip(1).enumerate() {
                let translated: String = self::translate_expr(arg, span)?;

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
                            if let Some((false, extents)) = self::analyze_nested_scalar_array_type(&argument_type)
                                && extents.len() > 1
                            {
                                let mut zero_path: String = String::new();

                                for _ in 0..extents.len() {
                                    zero_path.push_str("[0]");
                                }

                                args.push(format!("ref {translated}{zero_path}"));
                                continue;
                            }

                            let canonical_element_type: clang::Type<'_> = element_type.get_canonical_type();

                            if canonical_element_type.get_kind() == clang::TypeKind::Record {
                                args.push(format!("ref {translated}[0]"));
                                continue;
                            }

                            let element_text: String = crate::format_clang_type_thrust(&element_type)
                                .map_err(|msg| {
                                    CompilationIssue::Error(
                                        CompilationIssueCode::E0110,
                                        "C translation failed.".into(),
                                        format!("Unsupported array call argument type: {msg}"),
                                        None,
                                        span,
                                    )
                                })?;

                            args.push(format!("{translated} as ptr[{element_text}]"));
                            continue;
                        }
                    }
                }

                if let Some(expected_type) = expected_parameter_types.get(index) {
                    args.push(self::coerce_call_argument(
                        arg,
                        translated,
                        expected_type,
                        span,
                    )?);
                } else {
                    args.push(translated);
                }
            }

            Ok(format!("{callee}({})", args.join(", ")))
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing source range for unary operator.".into(),
                    None,
                    span,
                ));
            };

            let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
            let spellings: Vec<String> = tokens.into_iter().map(|t| t.get_spelling()).collect();

            if spellings.contains(&"++".to_string()) || spellings.contains(&"--".to_string()) {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Increment/decrement is only supported as a statement or for-loop increment."
                        .into(),
                    None,
                    span,
                ));
            }

            let children: Vec<clang::Entity<'_>> = entity.get_children();
            let Some(operand) = children.first() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed unary operator.".into(),
                    None,
                    span,
                ));
            };

            let operand_text: String = self::translate_expr(operand, span)?;

            let op: Option<&str> = spellings.iter().find_map(|s| match s.as_str() {
                "&" | "*" | "!" | "+" | "-" => Some(s.as_str()),
                _ => None,
            });

            let Some(op) = op else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported unary operator.".into(),
                    None,
                    span,
                ));
            };

            match op {
                "&" => {
                    Ok(format!("ref {operand_text}"))
                }
                "*" => {
                    if let Some(pointer_target) =
                        self::translate_pointer_target_expr(operand, span)?
                    {
                        return Ok(pointer_target);
                    }

                    let needs_parens: bool = matches!(
                        operand.get_kind(),
                        clang::EntityKind::BinaryOperator
                            | clang::EntityKind::CompoundAssignOperator
                            | clang::EntityKind::ConditionalOperator
                    );

                    if needs_parens {
                        Ok(format!("(deref ({operand_text}))"))
                    } else {
                        Ok(format!("(deref {operand_text})"))
                    }
                }
                "!" => Ok(format!("!{operand_text}")),
                "+" => Ok(operand_text),
                "-" => Ok(format!("-{operand_text}")),
                _ => Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported unary operator.".into(),
                    None,
                    span,
                )),
            }
        }

        clang::EntityKind::UnaryExpr => {
            let Some(range) = entity.get_range() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing source range for unary expression.".into(),
                    None,
                    span,
                ));
            };

            let tokens: Vec<String> = range
                .tokenize()
                .into_iter()
                .map(|token| token.get_spelling())
                .collect();

            if tokens.first().is_some_and(|token| token == "sizeof") {
                let children: Vec<clang::Entity<'_>> = entity.get_children();
                let operand_type: Option<clang::Type<'_>> = children
                    .iter()
                    .find_map(|child| child.get_type())
                    .or_else(|| children.last().and_then(|child| child.get_type()));
                let result_type: Option<clang::Type<'_>> = entity.get_type();

                let Some(operand_type) = operand_type else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unable to determine operand type for sizeof expression.".into(),
                        None,
                        span,
                    ));
                };

                let operand_type_text: String = crate::format_clang_type_thrust(&operand_type)
                    .map_err(|msg| {
                        CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            format!("Unsupported sizeof operand type: {msg}"),
                            None,
                            span,
                        )
                    })?;
                let mut sizeof_text: String = format!("abiSizeOf({operand_type_text})");

                if let Some(result_type) = result_type {
                    let result_type_text: String =
                        crate::format_clang_type_thrust(&result_type).map_err(|msg| {
                            CompilationIssue::Error(
                                CompilationIssueCode::E0110,
                                "C translation failed.".into(),
                                format!("Unsupported sizeof result type: {msg}"),
                                None,
                                span,
                            )
                        })?;

                    sizeof_text = format!("({sizeof_text}) as {result_type_text}");
                }

                return Ok(sizeof_text);
            }

            Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                "Unsupported unary expression.".into(),
                None,
                span,
            ))
        }

        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed binary operator.".into(),
                    None,
                    span,
                ));
            }

            let left: String = self::translate_expr(&children[0], span)?;
            let right: String = self::translate_expr(&children[1], span)?;

            let Some(op) = crate::extract_binary_operator(entity, &children[0], &children[1])
                .or_else(|| {
                    entity.get_range().and_then(|range| {
                        let tokens: Vec<String> = range
                            .tokenize()
                            .into_iter()
                            .map(|token| token.get_spelling())
                            .collect();
                        crate::extract_binary_operator_from_tokens(&tokens)
                    })
                })
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported binary operator.".into(),
                    None,
                    span,
                ));
            };

            if op == "," {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "The comma operator is not supported in C translation output.".into(),
                    None,
                    span,
                ));
            }

            Ok(format!("{left} {op} {right}"))
        }

        clang::EntityKind::CStyleCastExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            let Some(value_node) = children
                .iter()
                .rev()
                .find(|c| crate::is_supported_expr_kind(c.get_kind()))
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported cast expression.".into(),
                    None,
                    span,
                ));
            };

            let value: String = self::translate_expr(value_node, span)?;

            let Some(to_ty) = entity.get_type() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing cast type.".into(),
                    None,
                    span,
                ));
            };

            let to_ty_text: String = crate::format_clang_type_thrust(&to_ty).map_err(|msg| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported cast target type: {msg}"),
                    None,
                    span,
                )
            })?;

            Ok(format!("{value} as {to_ty_text}"))
        }

        clang::EntityKind::ConditionalOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 3 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed conditional operator.".into(),
                    None,
                    span,
                ));
            }

            let cond: String = self::translate_condition_expr(&children[0], span)?;
            let then_expr: String = self::translate_expr(&children[1], span)?;
            let else_expr: String = self::translate_expr(&children[2], span)?;

            Ok(format!(
                "if {cond} {{ {then_expr} }} else {{ {else_expr} }}"
            ))
        }

        clang::EntityKind::CompoundLiteralExpr => {
            let Some(compound_type) = entity.get_type() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Compound literal is missing its type.".into(),
                    None,
                    span,
                ));
            };

            crate::top_level::translate_global_initializer(entity, &compound_type, span)
        }

        clang::EntityKind::InitListExpr => {
            let Some(init_type) = entity.get_type() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Initializer list is missing its type.".into(),
                    None,
                    span,
                ));
            };

            crate::top_level::translate_global_initializer(entity, &init_type, span)
        }

        clang::EntityKind::ArraySubscriptExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed array subscript expression.".into(),
                    None,
                    span,
                ));
            }

            let base_node: &clang::Entity<'_> = &children[0];
            let index_node: &clang::Entity<'_> = &children[1];

            if let Some(linearized) = self::try_translate_linearized_array_subscript(entity, span)? {
                return Ok(linearized);
            }

            let base: String = self::translate_expr(base_node, span)?;
            let base_type: Option<clang::Type<'_>> = self::resolve_expression_type(base_node);

            let index: String = self::translate_expr(index_node, span)?;

            let uses_place_index: bool = if let Some(base_type) = base_type {
                let canonical_type: clang::Type<'_> = base_type.get_canonical_type();

                if canonical_type.get_kind() == clang::TypeKind::Pointer {
                    if let Some(pointee_type) = canonical_type.get_pointee_type() {
                        let pointee_type: clang::Type<'_> = pointee_type.get_canonical_type();

                        matches!(
                            pointee_type.get_kind(),
                            clang::TypeKind::Record
                                | clang::TypeKind::ConstantArray
                                | clang::TypeKind::IncompleteArray
                        )
                    } else {
                        false
                    }
                } else if matches!(
                    canonical_type.get_kind(),
                    clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
                ) {
                    if let Some(element_type) = canonical_type.get_element_type() {
                        let element_type: clang::Type<'_> = element_type.get_canonical_type();

                        matches!(
                            element_type.get_kind(),
                            clang::TypeKind::Record
                                | clang::TypeKind::ConstantArray
                                | clang::TypeKind::IncompleteArray
                        )
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if uses_place_index {
                Ok(format!("{base}[{index}]"))
            } else {
                Ok(format!("{base}->[{index}]"))
            }
        }

        other => Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Unsupported expression kind: {other:?}"),
            None,
            span,
        )),
    }
}

fn translate_decl_ref_expr(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let Some(range) = entity.get_range() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!(
                "Missing source range for expression kind {:?}.",
                entity.get_kind()
            ),
            None,
            span,
        ));
    };

    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
    let fallback: String = crate::tokens_to_thrust_source(&tokens);

    let Some(reference) = entity.get_reference() else {
        return Ok(fallback);
    };

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

    let module_name: String = crate::sanitize_identifier_for_thrust(&module_stem.to_string_lossy());
    let symbol_name: String = crate::sanitize_identifier_for_thrust(&reference_name);

    Ok(format!("{module_name}::{symbol_name}"))
}

fn coerce_call_argument(
    entity: &clang::Entity<'_>,
    translated: String,
    expected_type: &clang::Type<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    let thrust_type: String = crate::format_clang_type_thrust(expected_type).map_err(|msg| {
        CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Unsupported call parameter type: {msg}"),
            None,
            span,
        )
    })?;

    let mut argument_type: Option<clang::Type<'_>> = entity.get_type();
    let mut probe: clang::Entity<'_> = *entity;

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

    let expected_canonical: clang::TypeKind = expected_type.get_canonical_type().get_kind();

    let mut stack: Vec<clang::Entity<'_>> = vec![*entity];
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

    if is_string_literal_like {
        return Ok(format!("({translated}) as {thrust_type}"));
    }

    if let Some(argument_type) = argument_type {
        let argument_canonical: clang::TypeKind = argument_type.get_canonical_type().get_kind();

        if expected_canonical == clang::TypeKind::Pointer
            && matches!(
                argument_canonical,
                clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
            )
        {
            return Ok(format!("({translated}) as {thrust_type}"));
        }
    }

    let needs_cast: bool = match argument_type {
        Some(argument_type) => {
            let match_: bool = {
                let argument_text: Result<String, String> =
                    crate::format_clang_type_thrust(&argument_type);
                let expected_text: Result<String, String> =
                    crate::format_clang_type_thrust(expected_type);

                match (argument_text, expected_text) {
                    (Ok(argument_text), Ok(expected_text)) => argument_text == expected_text,
                    _ => false,
                }
            };

            !match_
        }
        None => true,
    };

    if !needs_cast {
        return Ok(translated);
    }

    Ok(format!("({translated}) as {thrust_type}"))
}

fn translate_pointer_target_expr(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<Option<String>, CompilationIssue> {
    if matches!(
        entity.get_kind(),
        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
    ) {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.len() == 1 && crate::is_supported_expr_kind(children[0].get_kind()) {
            return self::translate_pointer_target_expr(&children[0], span);
        }
    }

    if entity.get_kind() == clang::EntityKind::BinaryOperator {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.len() >= 2
            && let Some(op) = crate::extract_binary_operator(entity, &children[0], &children[1])
            && op == "+"
        {
            let base: String = self::translate_expr(&children[0], span)?;
            let index: String = self::translate_expr(&children[1], span)?;

            return Ok(Some(format!("{base}->[{index}]")));
        }
    }

    let base: String = self::translate_expr(entity, span)?;

    Ok(Some(format!("{base}->[0]")))
}

fn try_translate_linearized_array_subscript(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<Option<String>, CompilationIssue> {
    let mut indices_rev: Vec<clang::Entity<'_>> = Vec::new();
    let mut probe: clang::Entity<'_> = self::peel_expression_wrappers(entity);

    while probe.get_kind() == clang::EntityKind::ArraySubscriptExpr {
        let children: Vec<clang::Entity<'_>> = probe.get_children();

        if children.len() < 2 {
            return Ok(None);
        }

        indices_rev.push(children[1]);
        probe = self::peel_expression_wrappers(&children[0]);
    }

    if indices_rev.len() < 2 {
        return Ok(None);
    }

    indices_rev.reverse();

    let Some(source_type) = self::resolve_expression_type(&probe) else {
        return Ok(None);
    };

    let Some((pointer_root, extents)) = self::analyze_nested_scalar_array_type(&source_type) else {
        return Ok(None);
    };

    let source_canonical: clang::Type<'_> = source_type.get_canonical_type();
    let parameter_array_root: bool = !pointer_root
        && matches!(
            source_canonical.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        )
        && probe
            .get_reference()
            .is_some_and(|reference| reference.get_kind() == clang::EntityKind::ParmDecl);

    if ((!pointer_root && !parameter_array_root) && extents.len() != indices_rev.len())
        || (parameter_array_root && extents.len() != indices_rev.len())
        || (pointer_root && extents.len().saturating_add(1) != indices_rev.len())
    {
        return Ok(None);
    }

    let mut root_text: String = self::translate_expr(&probe, span)?;

    if !pointer_root && !parameter_array_root {
        root_text = format!("ref {root_text}");

        for _ in 0..extents.len() {
            root_text.push_str("[0]");
        }
    }

    let mut terms: Vec<String> = Vec::with_capacity(indices_rev.len());

    for (index_position, index_node) in indices_rev.iter().enumerate() {
        let index_text: String = self::translate_expr(index_node, span)?;

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
        return Ok(Some(format!("{root_text}->[{}]", terms.join(" + "))));
    }

    Ok(Some(format!("({root_text})->[{}]", terms.join(" + "))))
}

fn analyze_nested_scalar_array_type(ty: &clang::Type<'_>) -> Option<(bool, Vec<usize>)> {
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    let (pointer_root, mut current): (bool, clang::Type<'_>) = if canonical.get_kind()
        == clang::TypeKind::Pointer
    {
        (true, canonical.get_pointee_type()?.get_canonical_type())
    } else {
        (false, canonical)
    };

    let mut extents: Vec<usize> = Vec::new();

    while matches!(
        current.get_kind(),
        clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
    ) {
        let size: usize = current.get_size()?;

        extents.push(size);
        current = current.get_element_type()?.get_canonical_type();
    }

    if extents.is_empty()
        || matches!(
            current.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray | clang::TypeKind::Record
        )
    {
        return None;
    }

    Some((pointer_root, extents))
}

fn resolve_expression_type<'tu>(entity: &clang::Entity<'tu>) -> Option<clang::Type<'tu>> {
    let mut resolved: Option<clang::Type<'tu>> = entity.get_type();
    let mut probe: clang::Entity<'tu> = *entity;

    loop {
        if !matches!(
            probe.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            break;
        }

        let children: Vec<clang::Entity<'tu>> = probe.get_children();

        if children.len() != 1 {
            break;
        }

        probe = children[0];

        if let Some(probe_type) = probe.get_type() {
            let canonical_type: clang::Type<'tu> = probe_type.get_canonical_type();

            if matches!(
                canonical_type.get_kind(),
                clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
                    | clang::TypeKind::Record
            ) {
                return Some(probe_type);
            }

            resolved = Some(probe_type);
        }
    }

    resolved
}

fn peel_expression_wrappers<'tu>(entity: &clang::Entity<'tu>) -> clang::Entity<'tu> {
    let mut probe: clang::Entity<'tu> = *entity;

    loop {
        if !matches!(
            probe.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            break;
        }

        let children: Vec<clang::Entity<'tu>> = probe.get_children();

        if children.len() != 1 || !crate::is_supported_expr_kind(children[0].get_kind()) {
            break;
        }

        probe = children[0];
    }

    probe
}

pub(crate) fn translate_condition_expr(
    entity: &clang::Entity<'_>,
    span: Span,
) -> Result<String, CompilationIssue> {
    match entity.get_kind() {
        clang::EntityKind::ParenExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(child) = children
                .iter()
                .find(|candidate| crate::is_supported_expr_kind(candidate.get_kind()))
            {
                let inner: String = self::translate_condition_expr(child, span)?;
                return Ok(format!("({inner})"));
            }

            let value: String = self::translate_expr(entity, span)?;

            Ok(format!("({value}) != 0"))
        }

        clang::EntityKind::UnexposedExpr => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() == 1 && crate::is_supported_expr_kind(children[0].get_kind()) {
                return self::translate_condition_expr(&children[0], span);
            }

            let value: String = self::translate_expr(entity, span)?;

            Ok(format!("({value}) != 0"))
        }

        clang::EntityKind::UnaryOperator => {
            let Some(range) = entity.get_range() else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Missing source range for unary operator.".into(),
                    None,
                    span,
                ));
            };

            let tokens: Vec<String> = range
                .tokenize()
                .into_iter()
                .map(|token| token.get_spelling())
                .collect();

            if tokens.contains(&"!".to_string()) {
                let children: Vec<clang::Entity<'_>> = entity.get_children();
                let Some(operand) = children.first() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Malformed unary operator.".into(),
                        None,
                        span,
                    ));
                };

                let operand_text: String = self::translate_condition_expr(operand, span)?;

                return Ok(format!("!({operand_text})"));
            }

            let value: String = self::translate_expr(entity, span)?;

            Ok(format!("({value}) != 0"))
        }

        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed binary operator.".into(),
                    None,
                    span,
                ));
            }

            let Some(op) = crate::extract_binary_operator(entity, &children[0], &children[1])
                .or_else(|| {
                    entity.get_range().and_then(|range| {
                        let tokens: Vec<String> = range
                            .tokenize()
                            .into_iter()
                            .map(|token| token.get_spelling())
                            .collect();
                        crate::extract_binary_operator_from_tokens(&tokens)
                    })
                })
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported binary operator.".into(),
                    None,
                    span,
                ));
            };

            if matches!(op, "&&" | "||") {
                let left: String = self::translate_condition_expr(&children[0], span)?;
                let right: String = self::translate_condition_expr(&children[1], span)?;

                return Ok(format!("({left}) {op} ({right})"));
            }

            if matches!(op, "==" | "!=" | "<" | "<=" | ">" | ">=") {
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

                let left_value: String = self::translate_expr(&children[0], span)?;
                let right_value: String = self::translate_expr(&children[1], span)?;

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

            let value: String = self::translate_expr(entity, span)?;

            Ok(format!("({value}) != 0"))
        }

        _ => {
            let value: String = self::translate_expr(entity, span)?;

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
