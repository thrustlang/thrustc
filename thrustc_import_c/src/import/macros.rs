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

use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportState;
use crate::model::CImportedConstant;

pub fn import_macros<'clang>(
    macro_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    for entity in macro_decls.iter() {
        let Some(macro_name) = entity.get_name() else {
            continue;
        };

        let Some(range) = entity.get_range() else {
            continue;
        };

        let mut tokens: Vec<clang::token::Token<'_>> = range.tokenize();

        if tokens
            .last()
            .is_some_and(|token| token.get_spelling() == "#")
        {
            tokens.pop();
        }

        if tokens.is_empty() {
            continue;
        }

        let Some(name_index) = tokens
            .iter()
            .position(|token| token.get_spelling() == macro_name)
        else {
            continue;
        };

        let mut body: Vec<String> = tokens
            .iter()
            .skip(name_index.saturating_add(1))
            .map(|token| token.get_spelling())
            .collect();

        // Token heuristics misclassify object-like macros such as `#define FOO (1)` as function-like.
        // Use libclang's cursor query directly so we only reject real function-like macros.
        if unsafe { entity.is_function_like_macro_unchecked() } {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("macro '{macro_name}' skipped: function-like macros are not supported"),
            ));

            continue;
        }

        if let Some(result) = entity.evaluate() {
            match result {
                clang::EvaluationResult::SignedInteger(value) => {
                    state.constants_mut().push(CImportedConstant::new(
                        macro_name,
                        Type::S64 { span },
                        BuiltinValue::Integer(value as u64),
                    ));

                    continue;
                }
                clang::EvaluationResult::UnsignedInteger(value) => {
                    state.constants_mut().push(CImportedConstant::new(
                        macro_name,
                        Type::U64 { span },
                        BuiltinValue::Integer(value),
                    ));

                    continue;
                }
                clang::EvaluationResult::Float(value) => {
                    state.constants_mut().push(CImportedConstant::new(
                        macro_name,
                        Type::F64 { span },
                        BuiltinValue::Float(value),
                    ));

                    continue;
                }
                clang::EvaluationResult::String(value) | clang::EvaluationResult::ObjCString(value) => {
                    state.constants_mut().push(CImportedConstant::new(
                        macro_name,
                        Type::Array {
                            base_type: Box::new(Type::Char { span }),
                            infered_type: None,
                            metadata: ArrayTypeMetadata::new(None, None),
                            span,
                        },
                        BuiltinValue::CString(value.to_bytes().to_vec()),
                    ));

                    continue;
                }
                _ => {}
            }
        }

        while body.len() >= 2 && body[0] == "(" && body[body.len() - 1] == ")" {
            body.remove(0);
            body.pop();
        }

        let mut is_negative: bool = false;

        if let Some(first) = body.first() {
            if first == "-" {
                is_negative = true;
                body.remove(0);
            } else if first == "+" {
                body.remove(0);
            }
        }

        let min_expression_value: Option<u64> = if is_negative
            && body.len() == 3
            && body[1] == "-"
            && body[2] == "1"
        {
            let head: &str = &body[0];

            let existing_head_value: Option<u64> = state
                .constants_mut()
                .iter()
                .find(|constant| constant.name() == head)
                .and_then(|constant| match constant.value() {
                    BuiltinValue::Integer(value) => Some(*value),
                    _ => None,
                });

            let parsed_head_value: Option<u64> = if let Some(existing_head_value) = existing_head_value {
                Some(existing_head_value)
            } else {
                let cleaned: String = head.trim_end_matches(['u', 'U', 'l', 'L']).to_string();

                if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
                    u64::from_str_radix(
                        cleaned.trim_start_matches("0x").trim_start_matches("0X"),
                        16,
                    )
                    .ok()
                } else {
                    cleaned.parse::<u64>().ok()
                }
            };

            parsed_head_value.and_then(|value| value.checked_add(1))
        } else {
            None
        };

        if let Some(min_expression_value) = min_expression_value {
            state.constants_mut().push(CImportedConstant::new(
                macro_name,
                Type::S64 { span },
                BuiltinValue::Integer(min_expression_value.wrapping_neg()),
            ));

            continue;
        }

        if body.len() != 1 {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("macro '{macro_name}' skipped: unsupported macro body"),
            ));

            continue;
        }

        let token: &str = &body[0];
        let mut numeric_token: &str = token;

        if !token.starts_with('"') && !token.starts_with('\'') && token.len() > 1 {
            if let Some(stripped) = token.strip_prefix('-') {
                is_negative = true;
                numeric_token = stripped;
            } else if let Some(stripped) = token.strip_prefix('+') {
                numeric_token = stripped;
            }
        }

        let existing_constant: Option<(Type, BuiltinValue)> = state
            .constants_mut()
            .iter()
            .find(|constant| constant.name() == token)
            .map(|constant| (constant.kind().clone(), constant.value().clone()));

        if let Some((kind, value)) = existing_constant {
            state
                .constants_mut()
                .push(CImportedConstant::new(macro_name, kind, value));

            continue;
        }

        {
            let cleaned: String = numeric_token.trim_end_matches(['u', 'U', 'l', 'L']).to_string();

            let parsed_int: Option<u64> = if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
                u64::from_str_radix(
                    cleaned.trim_start_matches("0x").trim_start_matches("0X"),
                    16,
                )
                .ok()
            } else {
                cleaned.parse::<u64>().ok()
            };

            if let Some(value) = parsed_int {
                let kind: Type = if is_negative {
                    Type::S64 { span: state.span() }
                } else {
                    Type::U64 { span: state.span() }
                };

                let integer_value: u64 = if is_negative {
                    value.wrapping_neg()
                } else {
                    value
                };

                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    kind,
                    BuiltinValue::Integer(integer_value),
                ));

                continue;
            }
        }

        {
            let cleaned: String = numeric_token.trim_end_matches(['f', 'F', 'l', 'L']).to_string();

            if let Ok(mut value) = cleaned.parse::<f64>() {
                if is_negative {
                    value = -value;
                }

                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    Type::F64 { span },
                    BuiltinValue::Float(value),
                ));

                continue;
            }
        }

        if token.starts_with('"') || token.starts_with('L') {
            let is_wide: bool = token.starts_with('L');

            let literal: &str = if is_wide { &token[1..] } else { token };

            if !literal.starts_with('"') || !literal.ends_with('"') || literal.len() < 2 {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("macro '{macro_name}' skipped: unsupported string literal"),
                ));

                continue;
            }

            let inner: &str = &literal[1..literal.len() - 1];
            let source: &[u8] = inner.as_bytes();

            let mut bytes: Vec<u8> = Vec::with_capacity(source.len());
            let mut idx: usize = 0;
            let mut invalid_escape: bool = false;

            while idx < source.len() {
                let byte: u8 = source[idx];

                if byte == b'\\' {
                    idx = idx.saturating_add(1);

                    match source.get(idx) {
                        Some(b'n') => bytes.push(b'\n'),
                        Some(b't') => bytes.push(b'\t'),
                        Some(b'r') => bytes.push(b'\r'),
                        Some(b'\\') => bytes.push(b'\\'),
                        Some(b'0') => bytes.push(0),
                        Some(b'\'') => bytes.push(b'\''),
                        Some(b'"') => bytes.push(b'"'),
                        _ => {
                            invalid_escape = true;
                            break;
                        }
                    }

                    idx = idx.saturating_add(1);
                    continue;
                }

                bytes.push(byte);
                idx = idx.saturating_add(1);
            }

            if invalid_escape {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!(
                        "macro '{macro_name}' skipped: unsupported escape sequence in string literal"
                    ),
                ));

                continue;
            }

            let kind: Type = Type::Array {
                base_type: Box::new(Type::Char { span }),
                infered_type: None,
                metadata: ArrayTypeMetadata::new(None, None),
                span,
            };

            if is_wide {
                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    kind,
                    BuiltinValue::CNString(bytes),
                ));
            } else {
                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    kind,
                    BuiltinValue::CString(bytes),
                ));
            }

            continue;
        }

        if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 3 {
            let inner: &str = &token[1..token.len() - 1];

            let byte: Option<u8> = if inner.len() == 1 {
                inner.as_bytes().first().copied()
            } else if inner.starts_with("\\") && inner.len() == 2 {
                let escaped: u8 = inner.as_bytes()[1];

                match escaped {
                    b'0' => Some(0),
                    b'n' => Some(b'\n'),
                    b'r' => Some(b'\r'),
                    b't' => Some(b'\t'),
                    b'\\' => Some(b'\\'),
                    b'\'' => Some(b'\''),
                    b'"' => Some(b'"'),
                    _ => None,
                }
            } else {
                None
            };

            if let Some(byte) = byte {
                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    Type::Char { span },
                    BuiltinValue::Char(byte),
                ));

                continue;
            }
        }

        state.diagnostics_mut().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedDeclaration,
            format!("macro '{macro_name}' skipped: unsupported macro body"),
        ));
    }
}
