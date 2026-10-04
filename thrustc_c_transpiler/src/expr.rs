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

                return Ok(self::normalize_literal_spelling(
                    entity.get_kind(),
                    spelling,
                ));
            }

            Ok(String::new())
        }

        clang::EntityKind::DeclRefExpr => self::translate_decl_ref_expr(entity, span),

        clang::EntityKind::MemberRefExpr => {
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
            Ok(crate::tokens_to_thrust_source(&tokens))
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

            if children.len() == 1 && crate::is_supported_expr_kind(children[0].get_kind()) {
                return self::translate_expr(&children[0], span);
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
            let expected_parameter_types: Vec<clang::Type<'_>> =
                self::resolve_call_parameter_types(&children[0]);
            let mut args: Vec<String> = Vec::new();

            for (index, arg) in children.iter().skip(1).enumerate() {
                let translated: String = self::translate_expr(arg, span)?;

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
                    if operand.get_kind() == clang::EntityKind::ArraySubscriptExpr {
                        let children: Vec<clang::Entity<'_>> = operand.get_children();

                        if children.len() >= 2 {
                            let base: String = self::translate_expr(&children[0], span)?;
                            let index: String = self::translate_expr(&children[1], span)?;
                            Ok(format!("{base}[{index}]"))
                        } else {
                            Ok(operand_text)
                        }
                    } else {
                        Ok(format!("ref {operand_text}"))
                    }
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

            let cond: String = self::translate_expr(&children[0], span)?;
            let then_expr: String = self::translate_expr(&children[1], span)?;
            let else_expr = self::translate_expr(&children[2], span)?;

            Ok(format!(
                "if {cond} {{ {then_expr} }} else {{ {else_expr} }}"
            ))
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

            let base: String = if let Some(range) = base_node.get_range() {
                let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

                if tokens.iter().any(|token| token.get_spelling() == ".") {
                    self::tokens_to_thrust_source_preserve_member_dot(&tokens)
                } else {
                    self::translate_expr(base_node, span)?
                }
            } else {
                self::translate_expr(base_node, span)?
            };
            let index: String = self::translate_expr(index_node, span)?;

            Ok(format!("{base}->[{index}]"))
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

fn resolve_call_parameter_types<'clang>(
    entity: &clang::Entity<'clang>,
) -> Vec<clang::Type<'clang>> {
    if matches!(
        entity.get_kind(),
        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
    ) {
        let children: Vec<clang::Entity<'clang>> = entity.get_children();

        if children.len() == 1 {
            return self::resolve_call_parameter_types(&children[0]);
        }
    }

    let Some(reference) = entity.get_reference() else {
        return Vec::new();
    };

    let Some(arguments) = reference.get_arguments() else {
        return Vec::new();
    };

    arguments
        .into_iter()
        .filter_map(|argument| argument.get_type())
        .collect()
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

    if self::is_string_literal_like(entity) {
        return Ok(format!("({translated}) as {thrust_type}"));
    }

    let needs_cast: bool = match entity.get_type() {
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

fn is_string_literal_like(entity: &clang::Entity<'_>) -> bool {
    match entity.get_kind() {
        clang::EntityKind::StringLiteral => true,
        clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr => entity
            .get_children()
            .into_iter()
            .any(|child| self::is_string_literal_like(&child)),
        _ => false,
    }
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

fn tokens_to_thrust_source_preserve_member_dot(tokens: &[clang::token::Token<'_>]) -> String {
    let mut out: String = String::new();
    let mut previous: Option<String> = None;

    for token in tokens.iter() {
        let raw: String = token.get_spelling();
        let current: String = match token.get_kind() {
            clang::token::TokenKind::Identifier => crate::sanitize_identifier_for_thrust(&raw),
            clang::token::TokenKind::Literal => crate::normalize_literal_token_spelling(&raw),
            _ => raw,
        };

        if let Some(prev) = previous.as_deref() {
            if crate::needs_space_between_tokens(prev, &current) {
                out.push(' ');
            }
        }

        out.push_str(&current);
        previous = Some(current);
    }

    out
}

fn normalize_literal_spelling(kind: clang::EntityKind, spelling: String) -> String {
    match kind {
        clang::EntityKind::IntegerLiteral => {
            spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string()
        }
        clang::EntityKind::FloatingLiteral => {
            spelling.trim_end_matches(['f', 'F', 'l', 'L']).to_string()
        }
        _ => spelling,
    }
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
                let left: String = self::translate_expr(&children[0], span)?;
                let right: String = self::translate_expr(&children[1], span)?;
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
