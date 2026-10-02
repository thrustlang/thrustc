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

use crate::analysis::{Analysis, CompletionKind, Symbol};
use crate::documents::Documents;

pub fn document_symbols(documents: &Documents, analysis: &Analysis, payload: &Value) -> Value {
    let mut uri: Option<&str> = None;

    if let Some(params) = payload.get("params") {
        if let Some(document) = params.get("textDocument") {
            let uri_value: Option<&Value> = document.get("uri");

            uri = uri_value.and_then(Value::as_str);
        }
    }

    let Some(uri) = uri else {
        return Value::Array(Vec::with_capacity(0));
    };

    let Some(document) = documents.get(uri) else {
        return Value::Array(Vec::with_capacity(0));
    };

    let Some(document_analysis) = analysis.get_document(uri) else {
        return Value::Array(Vec::with_capacity(0));
    };

    let lines: Vec<&str> = document.get_text().lines().collect();
    let mut symbols: Vec<Value> = Vec::with_capacity(document_analysis.get_symbols().len());

    for symbol in document_analysis.get_symbols() {
        if !symbol.is_global() && symbol.get_kind() != CompletionKind::Field {
            continue;
        }

        let mut children: Vec<Value> = Vec::with_capacity(8);

        match symbol.get_kind() {
            CompletionKind::Struct => {
                if let Some(structure) = document_analysis
                    .get_structures()
                    .iter()
                    .find(|structure| structure.get_name() == symbol.get_name())
                {
                    for field in structure.get_fields() {
                        children.push(self::symbol_to_document_symbol(
                            field,
                            &lines,
                            Vec::with_capacity(0),
                        ));
                    }
                }
            }
            CompletionKind::Enum => {
                if let Some(enumeration) = document_analysis
                    .get_enumerations()
                    .iter()
                    .find(|enumeration| enumeration.get_name() == symbol.get_name())
                {
                    for value in enumeration.get_values() {
                        children.push(self::symbol_to_document_symbol(
                            value,
                            &lines,
                            Vec::with_capacity(0),
                        ));
                    }
                }
            }
            CompletionKind::Function => {
                if let Some(function) = document_analysis
                    .get_functions()
                    .iter()
                    .find(|function| function.get_name() == symbol.get_name())
                {
                    for parameter in function.get_parameters() {
                        children.push(self::symbol_to_document_symbol(
                            parameter,
                            &lines,
                            Vec::with_capacity(0),
                        ));
                    }
                }
            }
            _ => {}
        }

        let item: Value = self::symbol_to_document_symbol(symbol, &lines, children);

        symbols.push(item);
    }

    Value::Array(symbols)
}

fn symbol_to_document_symbol(symbol: &Symbol, lines: &[&str], children: Vec<Value>) -> Value {
    let line: u64 = symbol.get_declaration_line();
    let line_index: usize = line.try_into().unwrap_or(usize::MAX);
    let source_line: &str = lines.get(line_index).copied().unwrap_or_default();
    let fallback_start: u64 = source_line
        .find(symbol.get_name())
        .unwrap_or(0)
        .try_into()
        .unwrap_or(u64::MAX);
    let start: u64 = if symbol.get_declaration_start() == 0 && fallback_start != 0 {
        fallback_start
    } else {
        symbol.get_declaration_start()
    };
    let end: u64 = if symbol.get_declaration_end() == 0 {
        start.saturating_add(symbol.get_name().len().try_into().unwrap_or(u64::MAX))
    } else {
        symbol.get_declaration_end()
    };
    let kind: u64 = match symbol.get_kind() {
        CompletionKind::Function => 12,
        CompletionKind::Field => 8,
        CompletionKind::Variable => 13,
        CompletionKind::Module => 2,
        CompletionKind::Enum => 10,
        CompletionKind::Keyword => 14,
        CompletionKind::Snippet => 13,
        CompletionKind::EnumMember => 22,
        CompletionKind::Constant => 14,
        CompletionKind::Struct => 23,
        CompletionKind::TypeParameter => 26,
    };

    let mut item: Value = serde_json::json!({
        "name": symbol.get_name(),
        "kind": kind,
        "detail": symbol.get_detail(),
        "range": {
            "start": {
                "line": line,
                "character": 0
            },
            "end": {
                "line": line,
                "character": source_line.len()
            }
        },
        "selectionRange": {
            "start": {
                "line": line,
                "character": start
            },
            "end": {
                "line": line,
                "character": end
            }
        }
    });

    if !children.is_empty() {
        item["children"] = Value::Array(children);
    }

    item
}
