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

#![allow(clippy::collapsible_if)]

use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportC;
use crate::model::CImportedConstant;

use thrustc_c_transpiler_macro_api::diagnostics::Diagnostics;
use thrustc_c_transpiler_macro_api::options::Options;
use thrustc_c_transpiler_macro_api::traits::MacroDiagnostic;
use thrustc_c_transpiler_macro_api::translate;
use thrustc_c_transpiler_macro_api::types::Entity;

pub fn import_macros<'clang>(
    macro_decls: &[clang::Entity<'clang>],
    import_c: &mut ImportC<'clang>,
) {
    let span: thrustc_code_location::Span = import_c.get_span();

    let function_like_macros: Vec<clang::Entity<'clang>> = macro_decls
        .iter()
        .filter(|entity| unsafe { entity.is_function_like_macro_unchecked() })
        .copied()
        .collect();

    let mut macro_diagnostics: Diagnostics = Diagnostics::new();

    let translated: Vec<Entity> = translate::translate_function_macros(
        &function_like_macros,
        span,
        &Options::new(),
        &mut macro_diagnostics,
    );

    for entity in translated.into_iter() {
        if let Entity::Macro(macro_definition) = entity {
            import_c.get_mut_macros().push(macro_definition);
        }
    }

    for diagnostic in macro_diagnostics.take_diagnostics() {
        import_c
            .get_mut_diagnostics()
            .push(CImportDiagnostic::from_compilation_issue(
                CImportDiagnosticKind::UnsupportedMacro,
                diagnostic,
            ));
    }

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

        // Function-like macros are translated by the high-level macro API.
        if unsafe { entity.is_function_like_macro_unchecked() } {
            continue;
        }

        if let Some(Entity::Macro(type_macro)) = translate::translate_type_macro(entity, span) {
            import_c.get_mut_macros().push(type_macro);

            continue;
        }

        if self::try_to_evaluate_macro_constant(entity, &macro_name, span, import_c) {
            continue;
        }

        // C object macros are often wrapped in redundant parentheses, strip the outer pair until the bare literal is exposed for the sign and number parsing below.
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

        if let Some(min_expression_value) =
            self::try_to_parse_min_expression(is_negative, &body, import_c)
        {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name,
                Type::S64 { span },
                BuiltinValue::Integer(min_expression_value.wrapping_neg()),
            ));

            continue;
        }

        if body.len() != 1 {
            if let Some(Entity::Macro(object_macro)) = translate::translate_object_macro(entity) {
                import_c.get_mut_macros().push(object_macro);

                continue;
            }

            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::UnsupportedMacro,
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

        let existing_constant: Option<(Type, BuiltinValue)> = import_c
            .get_mut_constants()
            .iter()
            .find(|constant| constant.name() == token)
            .map(|constant| (constant.kind().clone(), constant.value().clone()));

        if let Some((kind, value)) = existing_constant {
            import_c
                .get_mut_constants()
                .push(CImportedConstant::new(macro_name, kind, value));

            continue;
        }

        if let Some((kind, value)) =
            self::try_to_parse_integer_literal(numeric_token, is_negative, import_c)
        {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name,
                kind,
                BuiltinValue::Integer(value),
            ));

            continue;
        }

        if let Some((kind, value)) =
            self::try_to_parse_float_literal(numeric_token, is_negative, span)
        {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name,
                kind,
                BuiltinValue::Float(value),
            ));

            continue;
        }

        if token.starts_with('"') || token.starts_with('L') {
            let Ok((kind, value)) =
                self::try_to_parse_string_literal(token, &macro_name, span, import_c)
            else {
                continue;
            };

            import_c
                .get_mut_constants()
                .push(CImportedConstant::new(macro_name, kind, value));

            continue;
        }

        if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 3 {
            if let Some(byte) = self::try_to_parse_char_literal(token) {
                import_c.get_mut_constants().push(CImportedConstant::new(
                    macro_name,
                    Type::Char { span },
                    BuiltinValue::Char(byte),
                ));

                continue;
            }
        }

        if let Some(Entity::Macro(object_macro)) = translate::translate_object_macro(entity) {
            import_c.get_mut_macros().push(object_macro);

            continue;
        }

        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::UnsupportedMacro,
            format!("macro '{macro_name}' skipped: unsupported macro body"),
        ));
    }
}

fn try_to_parse_string_literal<'clang>(
    token: &str,
    macro_name: &str,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> Result<(Type, BuiltinValue), ()> {
    let is_wide: bool = token.starts_with('L');

    let literal: &str = if is_wide { &token[1..] } else { token };

    if !literal.starts_with('"') || !literal.ends_with('"') || literal.len() < 2 {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::UnsupportedMacro,
            format!("macro '{macro_name}' skipped: unsupported string literal"),
        ));

        return Err(());
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
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::UnsupportedMacro,
            format!("macro '{macro_name}' skipped: unsupported escape sequence in string literal"),
        ));

        return Err(());
    }

    let kind: Type = Type::Array {
        base_type: Box::new(Type::Char { span }),
        infered_type: None,
        metadata: ArrayTypeMetadata::new(None, None),
        span,
    };

    let value: BuiltinValue = if is_wide {
        BuiltinValue::CNString(bytes)
    } else {
        BuiltinValue::CString(bytes)
    };

    Ok((kind, value))
}

fn try_to_evaluate_macro_constant<'clang>(
    entity: &clang::Entity<'clang>,
    macro_name: &str,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> bool {
    let Some(result) = entity.evaluate() else {
        return false;
    };

    match result {
        clang::EvaluationResult::SignedInteger(value) => {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name.to_string(),
                Type::S64 { span },
                BuiltinValue::Integer(value as u64),
            ));

            true
        }

        clang::EvaluationResult::UnsignedInteger(value) => {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name.to_string(),
                Type::U64 { span },
                BuiltinValue::Integer(value),
            ));

            true
        }

        clang::EvaluationResult::Float(value) => {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name.to_string(),
                Type::F64 { span },
                BuiltinValue::Float(value),
            ));

            true
        }

        clang::EvaluationResult::String(value) | clang::EvaluationResult::ObjCString(value) => {
            import_c.get_mut_constants().push(CImportedConstant::new(
                macro_name.to_string(),
                Type::Array {
                    base_type: Box::new(Type::Char { span }),
                    infered_type: None,
                    metadata: ArrayTypeMetadata::new(None, None),
                    span,
                },
                BuiltinValue::CString(value.to_bytes().to_vec()),
            ));

            true
        }

        _ => false,
    }
}

fn try_to_parse_integer_literal(
    numeric_token: &str,
    is_negative: bool,
    import_c: &ImportC<'_>,
) -> Option<(Type, u64)> {
    let cleaned: String = numeric_token
        .trim_end_matches(['u', 'U', 'l', 'L'])
        .to_string();

    let parsed_int: Option<u64> = if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
        u64::from_str_radix(
            cleaned.trim_start_matches("0x").trim_start_matches("0X"),
            16,
        )
        .ok()
    } else {
        cleaned.parse::<u64>().ok()
    };

    let value: u64 = parsed_int?;

    let kind: Type = if is_negative {
        Type::S64 {
            span: import_c.get_span(),
        }
    } else {
        Type::U64 {
            span: import_c.get_span(),
        }
    };

    let integer_value: u64 = if is_negative {
        value.wrapping_neg()
    } else {
        value
    };

    Some((kind, integer_value))
}

fn try_to_parse_min_expression(
    is_negative: bool,
    body: &[String],
    import_c: &mut ImportC<'_>,
) -> Option<u64> {
    if !is_negative || body.len() != 3 || body[1] != "-" || body[2] != "1" {
        return None;
    }

    let head: &str = &body[0];

    let existing_head_value: Option<u64> = import_c
        .get_mut_constants()
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
}

fn try_to_parse_char_literal(token: &str) -> Option<u8> {
    let inner: &str = &token[1..token.len() - 1];

    if inner.len() == 1 {
        return inner.as_bytes().first().copied();
    }

    if inner.starts_with("\\") && inner.len() == 2 {
        let escaped: u8 = inner.as_bytes()[1];

        return match escaped {
            b'0' => Some(0),
            b'n' => Some(b'\n'),
            b'r' => Some(b'\r'),
            b't' => Some(b'\t'),
            b'\\' => Some(b'\\'),
            b'\'' => Some(b'\''),
            b'"' => Some(b'"'),
            _ => None,
        };
    }

    None
}

fn try_to_parse_float_literal(
    numeric_token: &str,
    is_negative: bool,
    span: thrustc_code_location::Span,
) -> Option<(Type, f64)> {
    let cleaned: String = numeric_token
        .trim_end_matches(['f', 'F', 'l', 'L'])
        .to_string();

    let Ok(mut value) = cleaned.parse::<f64>() else {
        return None;
    };

    if is_negative {
        value = -value;
    }

    Some((Type::F64 { span }, value))
}
