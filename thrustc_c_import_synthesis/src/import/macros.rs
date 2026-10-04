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
    let span = state.span();

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

        let mut name_index: Option<usize> = None;

        for (idx, token) in tokens.iter().enumerate() {
            if token.get_spelling() == macro_name {
                name_index = Some(idx);
                break;
            }
        }

        let Some(name_index) = name_index else {
            continue;
        };

        let mut body: Vec<String> = tokens
            .iter()
            .skip(name_index.saturating_add(1))
            .map(|token| token.get_spelling())
            .collect();

        if body.first().is_some_and(|token| token == "(") {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("macro '{macro_name}' skipped: function-like macros are not supported"),
            ));

            continue;
        }

        loop {
            if body.len() >= 2 && body[0] == "(" && body[body.len() - 1] == ")" {
                body.remove(0);
                body.pop();
                continue;
            }

            break;
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

        if body.len() != 1 {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("macro '{macro_name}' skipped: unsupported macro body"),
            ));

            continue;
        }

        let token: &str = &body[0];

        {
            let cleaned: String = token.trim_end_matches(['u', 'U', 'l', 'L']).to_string();

            let parsed_int: Option<u64> = if cleaned.starts_with("0x") || cleaned.starts_with("0X")
            {
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

                state.constants_mut().push(CImportedConstant::new(
                    macro_name,
                    kind,
                    BuiltinValue::Integer(value),
                ));

                continue;
            }
        }

        {
            let cleaned: String = token.trim_end_matches(['f', 'F', 'l', 'L']).to_string();

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
            let mut ok: bool = true;

            while idx < source.len() {
                let byte = source[idx];

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
                            ok = false;
                            break;
                        }
                    }

                    idx = idx.saturating_add(1);
                    continue;
                }

                bytes.push(byte);
                idx = idx.saturating_add(1);
            }

            if !ok {
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
                match inner.as_bytes()[1] {
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
            format!("macro '{macro_name}' skipped: unsupported literal"),
        ));
    }
}
