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

use crate::analysis::{Analysis, Symbol};
use crate::documents::Documents;

pub fn definition(documents: &Documents, analysis: &Analysis, payload: &Value) -> Value {
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

    let lines: Vec<&str> = document.get_text().lines().collect();
    let line_index: usize = line.try_into().unwrap_or(usize::MAX);
    let character_index: usize = character.try_into().unwrap_or(usize::MAX);
    let Some(source_line) = lines.get(line_index).copied() else {
        return Value::Null;
    };
    let chars: Vec<char> = source_line.chars().collect();

    if chars.is_empty() {
        return Value::Null;
    }

    let mut start: usize = character_index.min(chars.len());

    while start > 0 && (chars[start - 1] == '_' || chars[start - 1].is_ascii_alphanumeric()) {
        start = start.saturating_sub(1);
    }

    let mut end: usize = start;

    while end < chars.len() && (chars[end] == '_' || chars[end].is_ascii_alphanumeric()) {
        end = end.saturating_add(1);
    }

    if start == end {
        return Value::Null;
    }

    let reference: String = chars[start..end].iter().collect();

    for symbol in document_analysis.get_symbols().iter().rev() {
        if symbol.get_name() != reference {
            continue;
        }

        if symbol.get_declaration_line() > line {
            continue;
        }

        return self::symbol_location(uri, symbol, &lines);
    }

    for (candidate_line_index, candidate_line) in
        lines.iter().enumerate().take(line_index + 1).rev()
    {
        let trimmed: &str = candidate_line.trim_start();
        let is_declaration: bool = trimmed.starts_with("var ")
            || trimmed.starts_with("const ")
            || trimmed.starts_with("static ")
            || trimmed.starts_with("fn ")
            || trimmed.starts_with("struct ")
            || trimmed.starts_with("enum ")
            || trimmed.starts_with("type ")
            || trimmed.starts_with("import ");

        if !is_declaration {
            continue;
        }

        let Some(start) = candidate_line.find(&reference) else {
            continue;
        };
        let end: usize = start.saturating_add(reference.len());
        let candidate_line: u64 = candidate_line_index.try_into().unwrap_or(u64::MAX);

        return serde_json::json!({
            "uri": uri,
            "range": {
                "start": {
                    "line": candidate_line,
                    "character": start
                },
                "end": {
                    "line": candidate_line,
                    "character": end
                }
            }
        });
    }

    Value::Null
}

fn symbol_location(uri: &str, symbol: &Symbol, lines: &[&str]) -> Value {
    let line: u64 = symbol.get_declaration_line();
    let line_index: usize = line.try_into().unwrap_or(usize::MAX);
    let source_line: &str = lines.get(line_index).copied().unwrap_or_default();
    let start: usize = source_line.find(symbol.get_name()).unwrap_or(0);
    let end: usize = start.saturating_add(symbol.get_name().len());

    serde_json::json!({
        "uri": uri,
        "range": {
            "start": {
                "line": line,
                "character": start
            },
            "end": {
                "line": line,
                "character": end
            }
        }
    })
}
