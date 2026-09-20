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

use serde_json::Value;

use crate::analysis::{Analysis, CompletionKind};
use crate::documents::Documents;

pub fn hover(documents: &Documents, analysis: &Analysis, payload: &Value) -> Value {
    let mut uri: Option<&str> = None;
    let mut line: Option<u64> = None;
    let mut character: Option<u64> = None;

    if let Some(params) = payload.get("params") {
        if let Some(document) = params.get("textDocument") {
            let uri_value: Option<&Value> = document.get("uri");

            uri = uri_value.and_then(Value::as_str);
        }

        if let Some(position) = params.get("position") {
            let line_value: Option<&Value> = position.get("line");
            let character_value: Option<&Value> = position.get("character");

            line = line_value.and_then(Value::as_u64);
            character = character_value.and_then(Value::as_u64);
        }
    }

    let Some(uri) = uri else {
        return Value::Null;
    };

    let Some(line) = line else {
        return Value::Null;
    };

    let Some(character) = character else {
        return Value::Null;
    };

    let Some(document) = documents.get(uri) else {
        return Value::Null;
    };

    let Some(document_analysis) = analysis.get_document(uri) else {
        return Value::Null;
    };

    let line_index: usize = line.try_into().unwrap_or(usize::MAX);
    let character_index: usize = character.try_into().unwrap_or(usize::MAX);
    let Some(source_line) = document.get_text().lines().nth(line_index) else {
        return Value::Null;
    };

    let chars: Vec<char> = source_line.chars().collect();

    if chars.is_empty() {
        return Value::Null;
    }

    let mut start: usize = character_index.min(chars.len().saturating_sub(1));
    let mut end: usize = character_index.min(chars.len());

    if start < chars.len() && !(chars[start] == '_' || chars[start].is_ascii_alphanumeric()) {
        if start > 0 && (chars[start - 1] == '_' || chars[start - 1].is_ascii_alphanumeric()) {
            start = start.saturating_sub(1);
            end = start.saturating_add(1);
        } else {
            return Value::Null;
        }
    }

    while start > 0
        && (chars[start - 1] == '_'
            || chars[start - 1].is_ascii_alphanumeric()
            || chars[start - 1] == ':')
    {
        start = start.saturating_sub(1);
    }

    while end < chars.len()
        && (chars[end] == '_' || chars[end].is_ascii_alphanumeric() || chars[end] == ':')
    {
        end = end.saturating_add(1);
    }

    let reference: String = chars[start..end].iter().collect();
    let mut detail: Option<String> = None;

    if let Some((module_name, symbol_name)) = reference.rsplit_once("::") {
        for module in document_analysis.get_modules() {
            if module.get_name() != module_name {
                continue;
            }

            for symbol in module.get_symbols() {
                if symbol.get_name() != symbol_name {
                    continue;
                }

                if symbol.get_kind() != CompletionKind::Function {
                    continue;
                }

                detail = Some(symbol.get_detail().to_string());

                break;
            }
        }
    } else {
        for symbol in document_analysis.get_symbols().iter().rev() {
            if symbol.get_name() != reference {
                continue;
            }

            if symbol.get_kind() != CompletionKind::Function {
                continue;
            }

            if !symbol.is_visible_at(line) {
                continue;
            }

            detail = Some(symbol.get_detail().to_string());

            break;
        }
    }

    let Some(detail) = detail else {
        return Value::Null;
    };

    serde_json::json!({
        "contents": {
            "kind": "markdown",
            "value": format!("```thrust\n{}\n```", detail)
        }
    })
}