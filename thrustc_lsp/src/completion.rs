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

#![allow(clippy::if_same_then_else)]

use std::collections::HashSet;

use serde_json::Value;
use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_preprocessor::signatures::{Signature, Variant};

use crate::analysis::{
    Analysis, CompletionKind, DocumentAnalysis, Function, ImportedModule, Symbol,
};
use crate::documents::Documents;

pub fn complete(documents: &Documents, analysis: &Analysis, payload: &Value) -> Vec<Value> {
    let mut items: Vec<Value> = Vec::with_capacity(u8::MAX as usize);
    let mut seen: HashSet<String> = HashSet::with_capacity(u8::MAX as usize);
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
        self::push_global_items(&mut items, &mut seen);
        return items;
    };

    let Some(line) = line else {
        self::push_global_items(&mut items, &mut seen);
        return items;
    };

    let Some(character) = character else {
        self::push_global_items(&mut items, &mut seen);
        return items;
    };

    let Some(document) = documents.get(uri) else {
        self::push_global_items(&mut items, &mut seen);
        return items;
    };

    let text: &str = document.get_text();
    let line_index: usize = line.try_into().unwrap_or(usize::MAX);
    let character_index: usize = character.try_into().unwrap_or(usize::MAX);
    let prefix: String = if let Some(source_line) = text.lines().nth(line_index) {
        source_line.chars().take(character_index).collect()
    } else {
        String::new()
    };
    let document_analysis: Option<&DocumentAnalysis> = analysis.get_document(uri);

    let trimmed: &str = prefix.trim_end();
    let last_word: Option<&str> = trimmed.rsplit(char::is_whitespace).next();

    if self::try_import_completion(&mut items, &mut seen, &prefix) {
        return items;
    }

    if trimmed.ends_with('@') || last_word.is_some_and(|last_word| last_word.starts_with('@')) {
        self::push_attributes(&mut items, &mut seen);
        return items;
    }

    let is_type_context: bool = self::is_type_context(&prefix, trimmed);

    if is_type_context {
        if let Some(document_analysis) = document_analysis {
            if self::try_module_type_completion(&mut items, &mut seen, document_analysis, &prefix) {
                return items;
            }
        }

        self::push_types(&mut items, &mut seen);

        if let Some(document_analysis) = document_analysis {
            self::push_type_symbols(&mut items, &mut seen, document_analysis);
        }

        return items;
    }

    if let Some(document_analysis) = document_analysis {
        if self::try_call_argument_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }

        if self::try_module_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }

        if self::try_member_completion(&mut items, &mut seen, document_analysis, &prefix, line) {
            return items;
        }

        if self::try_enum_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }
    }

    if let Some(document_analysis) = document_analysis {
        let depth: u64 = document_analysis.get_line_depth(line);

        if depth == 0 {
            self::push_top_level_keywords(&mut items, &mut seen);
            self::push_type_symbols(&mut items, &mut seen, document_analysis);
            self::push_global_symbols(&mut items, &mut seen, document_analysis);
            self::push_top_level_templates(&mut items, &mut seen);

            return items;
        }

        self::push_visible_symbols(&mut items, &mut seen, document_analysis, line);
        self::push_statement_keywords(&mut items, &mut seen);
        self::push_expression_keywords(&mut items, &mut seen);
        self::push_builtins(&mut items, &mut seen);
        self::push_statement_templates(&mut items, &mut seen);

        return items;
    }

    self::push_global_items(&mut items, &mut seen);

    items
}

fn is_type_context(prefix: &str, trimmed: &str) -> bool {
    if trimmed.ends_with(':') || trimmed.ends_with(" as") || trimmed.ends_with(" as ") {
        return true;
    }

    let line: &str = prefix.trim_start();
    let words: Vec<&str> = trimmed.split_whitespace().collect();

    if let Some(last) = words.last() {
        if matches!(*last, "ptr" | "array" | "fixed" | "Fn" | "const") {
            return true;
        }
    }

    if line.starts_with("type ") {
        if let Some(eq_index) = line.rfind('=') {
            let after_eq: &str = line[eq_index.saturating_add(1)..].trim();

            if !after_eq.contains(';') && !after_eq.contains('{') {
                return true;
            }
        }
    }

    if line.starts_with("fn ") {
        if let Some(close_index) = line.rfind(')') {
            let after_close: &str = line[close_index.saturating_add(1)..].trim();

            if !after_close.contains('{')
                && !after_close.contains(';')
                && !after_close.contains('@')
            {
                return true;
            }
        }
    }

    if let Some(as_index) = line.rfind(" as ") {
        let after_as: &str = line[as_index.saturating_add(" as ".len())..].trim();

        if !after_as.contains(';') && !after_as.contains('{') && !after_as.contains(')') {
            return true;
        }
    }

    if let Some(colon_index) = line.rfind(':') {
        let before_colon: &str = &line[..colon_index];
        let after_colon: &str = line[colon_index.saturating_add(1)..].trim();
        let previous_is_colon: bool = before_colon.ends_with(':');
        let next_is_colon: bool = line[colon_index.saturating_add(1)..].starts_with(':');

        if !previous_is_colon
            && !next_is_colon
            && !after_colon.contains('=')
            && !after_colon.contains(';')
            && !after_colon.contains('{')
            && !after_colon.contains(')')
        {
            return true;
        }
    }

    if let Some(open_index) = line.rfind('[') {
        if line[open_index.saturating_add(1)..].contains(']') {
            return false;
        }

        let before_open: &str = line[..open_index].trim_end();
        let type_constructor: &str = before_open
            .rsplit(|ch: char| !(ch == '_' || ch.is_ascii_alphanumeric()))
            .next()
            .unwrap_or_default();

        if matches!(type_constructor, "ptr" | "array" | "Fn") {
            return true;
        }
    }

    false
}

fn try_import_completion(items: &mut Vec<Value>, seen: &mut HashSet<String>, prefix: &str) -> bool {
    let line: &str = prefix.trim_start();
    let Some(source) = line.strip_prefix("import") else {
        return false;
    };

    if source.chars().next().is_some_and(|ch| !ch.is_whitespace()) {
        return false;
    }

    let mut source: &str = source.trim_start();

    if source.starts_with('"') {
        return true;
    }

    if let Some(only_index) = source.find(" only ") {
        let only_source: &str = &source[only_index + " only ".len()..];
        let Some(open_index) = only_source.rfind('{') else {
            return true;
        };

        let after_open: &str = &only_source[open_index.saturating_add(1)..];

        if after_open.contains('}') {
            return true;
        }

        let module_source: &str = source[..only_index].trim();

        if !module_source.starts_with("std") {
            return true;
        }

        let access: Vec<String> = module_source
            .split("::")
            .filter(|part| !part.is_empty())
            .map(str::to_string)
            .collect();

        if access.first().map(String::as_str) != Some("std") {
            return true;
        }

        let options: thrustc_options::CompilerOptions = thrustc_options::CompilerOptions::new();

        let Ok(module) = thrustc_preprocessor::std_library::find_std_module(&access, &options)
        else {
            return true;
        };

        let mut used: HashSet<String> = HashSet::with_capacity(8);
        let mut partial: &str = after_open.trim();

        if let Some(comma_index) = partial.rfind(',') {
            for name in partial[..comma_index].split(',') {
                let name: &str = name.trim();

                if !name.is_empty() {
                    used.insert(name.to_string());
                }
            }

            partial = partial[comma_index.saturating_add(1)..].trim();
        }

        for symbol in module.get_symbols() {
            if used.contains(&symbol.name) {
                continue;
            }

            if !partial.is_empty() && !symbol.name.starts_with(partial) {
                continue;
            }

            let Some(symbol) = self::convert_import_symbol(symbol) else {
                continue;
            };

            self::push_item(
                items,
                seen,
                symbol.get_name(),
                symbol.get_kind(),
                symbol.get_detail(),
                None,
            );
        }

        return true;
    }

    for marker in [" only ", " as ", ";"] {
        if let Some(index) = source.find(marker) {
            source = &source[..index];
        }
    }

    let source: &str = source.trim();

    if source.is_empty() || "std".starts_with(source) {
        self::push_item(items, seen, "std", CompletionKind::Module, "module", None);
        return true;
    }

    if !source.starts_with("std") {
        return true;
    }

    let ends_with_separator: bool = source.ends_with("::");
    let mut parts: Vec<&str> = source.split("::").filter(|part| !part.is_empty()).collect();

    let partial: &str = if ends_with_separator {
        ""
    } else {
        parts.pop().unwrap_or_default()
    };

    if parts.is_empty() {
        if "std".starts_with(partial) {
            self::push_item(items, seen, "std", CompletionKind::Module, "module", None);
        }

        return true;
    }

    if parts.first().copied() != Some("std") {
        return true;
    }

    let options: thrustc_options::CompilerOptions = thrustc_options::CompilerOptions::new();
    let access: Vec<String> = parts.iter().map(|part| (*part).to_string()).collect();

    let Ok(module) = thrustc_preprocessor::std_library::find_std_module(&access, &options) else {
        return true;
    };

    for submodule in module.get_submodules() {
        if !partial.is_empty() && !submodule.get_name().starts_with(partial) {
            continue;
        }

        self::push_item(
            items,
            seen,
            submodule.get_name(),
            CompletionKind::Module,
            "module",
            None,
        );
    }

    true
}

fn try_module_completion(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    prefix: &str,
) -> bool {
    let prefix: &str = prefix.trim_end();
    let Some(index) = prefix.rfind("::") else {
        return false;
    };
    let tail: &str = &prefix[index.saturating_add(2)..];

    if tail
        .chars()
        .any(|ch| !(ch == '_' || ch.is_ascii_alphanumeric() || ch == ':'))
    {
        return false;
    }

    let source: &str = &prefix[..index];
    let mut chars: Vec<char> = Vec::with_capacity(32);

    for ch in source.chars().rev() {
        if ch == '_' || ch.is_ascii_alphanumeric() || ch == ':' {
            chars.push(ch);
            continue;
        }

        if !chars.is_empty() {
            break;
        }
    }

    if chars.is_empty() {
        return false;
    }

    chars.reverse();

    let receiver: String = chars.into_iter().collect();

    for module in document_analysis.get_modules() {
        let Some(module) = self::find_imported_module(module, &receiver) else {
            continue;
        };

        for submodule in module.get_submodules() {
            self::push_item(
                items,
                seen,
                submodule.get_name(),
                CompletionKind::Module,
                "module",
                None,
            );
        }

        for symbol in module.get_symbols() {
            self::push_symbol(items, seen, symbol);
        }

        return true;
    }

    false
}

fn try_module_type_completion(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    prefix: &str,
) -> bool {
    let prefix: &str = prefix.trim_end();
    let Some(index) = prefix.rfind("::") else {
        return false;
    };
    let tail: &str = &prefix[index.saturating_add(2)..];

    if tail
        .chars()
        .any(|ch| !(ch == '_' || ch.is_ascii_alphanumeric() || ch == ':'))
    {
        return false;
    }

    let source: &str = &prefix[..index];
    let mut chars: Vec<char> = Vec::with_capacity(32);

    for ch in source.chars().rev() {
        if ch == '_' || ch.is_ascii_alphanumeric() || ch == ':' {
            chars.push(ch);
            continue;
        }

        if !chars.is_empty() {
            break;
        }
    }

    if chars.is_empty() {
        return false;
    }

    chars.reverse();

    let receiver: String = chars.into_iter().collect();

    for module in document_analysis.get_modules() {
        let Some(module) = self::find_imported_module(module, &receiver) else {
            continue;
        };

        for symbol in module.get_symbols() {
            if !self::is_type_completion_kind(symbol.get_kind()) {
                continue;
            }

            self::push_symbol(items, seen, symbol);
        }

        return true;
    }

    false
}

fn try_member_completion(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    prefix: &str,
    line: u64,
) -> bool {
    let prefix: &str = prefix.trim_end();
    let receiver: Option<String> = if prefix.ends_with("->") {
        let source: &str = prefix.trim_end_matches("->").trim_end();

        self::previous_identifier(source)
    } else if prefix.ends_with('.') {
        let source: &str = prefix.trim_end_matches('.').trim_end();

        self::previous_identifier(source)
    } else {
        None
    };

    let Some(receiver) = receiver else {
        return false;
    };

    let Some(type_name) = self::resolve_symbol_type(document_analysis, &receiver, line) else {
        return false;
    };

    let mut base_type: &str = type_name.trim();

    if base_type.starts_with("ptr[") && base_type.ends_with(']') {
        base_type = &base_type[4..base_type.len().saturating_sub(1)];
    }

    if let Some(index) = base_type.find('[') {
        base_type = &base_type[..index];
    }

    let base_type: String = base_type.trim().to_string();

    for structure in document_analysis.get_structures() {
        if structure.get_name() != base_type {
            continue;
        }

        for field in structure.get_fields() {
            self::push_symbol(items, seen, field);
        }

        return true;
    }

    false
}

fn try_enum_completion(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    prefix: &str,
) -> bool {
    let prefix: &str = prefix.trim_end();

    if !prefix.ends_with("=>") {
        return false;
    }

    let source: &str = prefix.trim_end_matches("=>").trim_end();
    let Some(enum_name) = self::previous_identifier(source) else {
        return false;
    };

    for enumeration in document_analysis.get_enumerations() {
        if enumeration.get_name() != enum_name {
            continue;
        }

        for value in enumeration.get_values() {
            self::push_symbol(items, seen, value);
        }

        return true;
    }

    false
}

fn try_call_argument_completion(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    prefix: &str,
) -> bool {
    let Some(open_index) = self::find_active_call_open(prefix) else {
        return false;
    };
    let mut source: &str = prefix[..open_index].trim_end();

    source = self::strip_trailing_generic_args(source).trim_end();

    let Some(function_path) = self::previous_path(source) else {
        return false;
    };
    let arguments: &str = &prefix[open_index.saturating_add(1)..];
    let split_arguments: Vec<&str> = self::split_top_level_arguments(arguments);
    let current_argument_index: usize = split_arguments.len().saturating_sub(1);
    let mut used_named_arguments: HashSet<String> = HashSet::with_capacity(8);

    for argument in &split_arguments {
        let Some(eq_index) = argument.find('=') else {
            continue;
        };

        let mut chars: Vec<char> = Vec::with_capacity(32);

        for ch in argument[..eq_index].trim().chars().rev() {
            if ch == '_' || ch.is_ascii_alphanumeric() {
                chars.push(ch);
                continue;
            }

            if !chars.is_empty() {
                break;
            }
        }

        chars.reverse();

        let name: String = chars.into_iter().collect();

        if !name.is_empty() {
            used_named_arguments.insert(name);
        }
    }

    if let Some((module_path, function_name)) = function_path.rsplit_once("::") {
        for module in document_analysis.get_modules() {
            let Some(module) = self::find_imported_module(module, module_path) else {
                continue;
            };

            for function in module.get_functions() {
                if function.get_name() != function_name {
                    continue;
                }

                return self::push_call_argument_completions(
                    items,
                    seen,
                    function,
                    current_argument_index,
                    &used_named_arguments,
                );
            }
        }

        return false;
    }

    for function in document_analysis.get_functions() {
        if function.get_name() != function_path {
            continue;
        }

        return self::push_call_argument_completions(
            items,
            seen,
            function,
            current_argument_index,
            &used_named_arguments,
        );
    }

    false
}

fn push_call_argument_completions(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    function: &Function,
    current_argument_index: usize,
    used_named_arguments: &HashSet<String>,
) -> bool {
    let parameters: &[Symbol] = function.get_parameters();

    if function.is_variadic()
        && (current_argument_index >= parameters.len()
            || parameters
                .iter()
                .all(|parameter| used_named_arguments.contains(parameter.get_name())))
    {
        return false;
    }

    if !function.is_variadic() && current_argument_index >= parameters.len() {
        return true;
    }

    for (index, parameter) in parameters.iter().enumerate() {
        if index < current_argument_index {
            continue;
        }

        let label: &str = parameter.get_name();

        if used_named_arguments.contains(label) {
            continue;
        }

        let insert_text: String = format!("{}= ", label);
        self::push_item(
            items,
            seen,
            label,
            CompletionKind::Variable,
            parameter.get_detail(),
            Some(&insert_text),
        );
    }

    true
}

fn push_visible_symbols(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
    line: u64,
) {
    for symbol in document_analysis.get_symbols() {
        if !symbol.is_visible_at(line) {
            continue;
        }

        self::push_symbol(items, seen, symbol);
    }
}

fn push_global_symbols(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
) {
    for symbol in document_analysis.get_symbols() {
        if !symbol.is_global() {
            continue;
        }

        self::push_symbol(items, seen, symbol);
    }
}

fn push_type_symbols(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    document_analysis: &DocumentAnalysis,
) {
    for symbol in document_analysis.get_symbols() {
        let kind: CompletionKind = symbol.get_kind();

        if !self::is_type_completion_kind(kind) {
            continue;
        }

        self::push_symbol(items, seen, symbol);
    }
}

#[inline]
fn is_type_completion_kind(kind: CompletionKind) -> bool {
    matches!(
        kind,
        CompletionKind::Enum | CompletionKind::Struct | CompletionKind::TypeParameter
    )
}

#[inline]
fn push_global_items(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_keywords(items, seen);
    self::push_types(items, seen);
    self::push_builtins(items, seen);
    self::push_top_level_templates(items, seen);
    self::push_statement_templates(items, seen);
}

fn push_top_level_keywords(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "fn",
        CompletionKind::Keyword,
        "function declaration",
        None,
    );
    self::push_item(
        items,
        seen,
        "struct",
        CompletionKind::Keyword,
        "structure declaration",
        None,
    );
    self::push_item(
        items,
        seen,
        "enum",
        CompletionKind::Keyword,
        "enum declaration",
        None,
    );
    self::push_item(
        items,
        seen,
        "type",
        CompletionKind::Keyword,
        "type alias",
        None,
    );
    self::push_item(
        items,
        seen,
        "const",
        CompletionKind::Keyword,
        "constant declaration",
        None,
    );
    self::push_item(
        items,
        seen,
        "static",
        CompletionKind::Keyword,
        "static declaration",
        None,
    );
    self::push_item(
        items,
        seen,
        "import",
        CompletionKind::Keyword,
        "module import",
        None,
    );
    self::push_item(
        items,
        seen,
        "intrinsic",
        CompletionKind::Keyword,
        "compiler intrinsic",
        None,
    );
    self::push_item(
        items,
        seen,
        "directive",
        CompletionKind::Keyword,
        "compiler directive",
        None,
    );
    self::push_item(
        items,
        seen,
        "@if",
        CompletionKind::Keyword,
        "compile-time conditional",
        Some("@if(${1:condition}) $0"),
    );
}

fn push_statement_keywords(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(items, seen, "var", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "const",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "static",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "struct",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "enum",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "type",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "return",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(items, seen, "if", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "elif",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "else",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(items, seen, "for", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "while",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "loop",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "break",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "breakall",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "continue",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "continueall",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "defer",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "@if",
        CompletionKind::Keyword,
        "compile-time conditional",
        Some("@if(${1:condition}) $0"),
    );
}

fn push_expression_keywords(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "true",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "false",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "nullptr",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "deref",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "load",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(items, seen, "ref", CompletionKind::Keyword, "keyword", None);
    self::push_item(items, seen, "new", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "fixed",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "unreachable",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
}

fn push_keywords(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_statement_keywords(items, seen);
    self::push_expression_keywords(items, seen);
    self::push_item(items, seen, "fn", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "import",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "only",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(items, seen, "as", CompletionKind::Keyword, "keyword", None);
    self::push_item(items, seen, "mut", CompletionKind::Keyword, "keyword", None);
    self::push_item(
        items,
        seen,
        "intrinsic",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "directive",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
}

fn push_types(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "const",
        CompletionKind::Keyword,
        "type qualifier",
        None,
    );
    self::push_item(
        items,
        seen,
        "s8",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "s16",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "s32",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "s64",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "ssize",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "u8",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "u16",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "u32",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "u64",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "u128",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "usize",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "f32",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "f64",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "f128",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "f80",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "fppc_128",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "bool",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "char",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "CString",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "CNString",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "ptr",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "void",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "addr",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "array",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "Fn",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
}

fn push_builtins(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "halloc",
        CompletionKind::Function,
        "builtin",
        Some("halloc($0)"),
    );
    self::push_item(
        items,
        seen,
        "abiSizeOf",
        CompletionKind::Function,
        "builtin",
        Some("abiSizeOf($0)"),
    );
    self::push_item(
        items,
        seen,
        "bitSizeOf",
        CompletionKind::Function,
        "builtin",
        Some("bitSizeOf($0)"),
    );
    self::push_item(
        items,
        seen,
        "abiAlignOf",
        CompletionKind::Function,
        "builtin",
        Some("abiAlignOf($0)"),
    );
    self::push_item(
        items,
        seen,
        "memcpy",
        CompletionKind::Function,
        "builtin",
        Some("memcpy($0)"),
    );
    self::push_item(
        items,
        seen,
        "memmove",
        CompletionKind::Function,
        "builtin",
        Some("memmove($0)"),
    );
    self::push_item(
        items,
        seen,
        "memset",
        CompletionKind::Function,
        "builtin",
        Some("memset($0)"),
    );
    self::push_item(
        items,
        seen,
        "arbitraryArg",
        CompletionKind::Function,
        "builtin",
        Some("arbitraryArg($0)"),
    );
    self::push_item(
        items,
        seen,
        "arbitraryArgs",
        CompletionKind::Function,
        "builtin",
        Some("arbitraryArgs($0)"),
    );

    for builtin in [
        "sizeOf",
        "alignOf",
        "file",
        "fileLine",
        "currentFuncName",
        "staticAssert",
        "compileError",
        "compileWarning",
        "isSigned",
        "isUnsigned",
        "isInteger",
        "isFloat",
        "isBool",
        "isChar",
        "isPointer",
        "isArray",
        "isFixedArray",
        "isStruct",
        "isVoid",
        "isConst",
        "isNumeric",
        "isFunction",
        "typeWidth",
        "fieldCount",
        "fixedArraySize",
        "isSameType",
        "isPtrLike",
        "isFixedArrayOfSize",
        "compilerVersion",
        "debugBuild",
        "stringLength",
        "targetOS",
        "targetArch",
        "targetVendor",
        "targetAbi",
        "targetTriple",
        "isLinux",
        "isWindows",
        "isDarwin",
        "isApple",
        "isAix",
        "is64Bit",
        "is32Bit",
        "isBigEndian",
        "isLittleEndian",
        "isX86",
        "isX8664",
        "isArm",
        "isAarch64",
        "isRiscv64",
        "isPpc",
        "isPpc64",
        "isMips64",
        "isSystemz",
        "isLoongarch64",
        "isWasm",
        "isElf",
        "isMachO",
        "isCoff",
        "hasPosixThreads",
        "hasSysvAbi",
        "pointerWidth",
        "isizeWidth",
        "usizeWidth",
        "pointerAlign",
        "maxAlignment",
        "targetCPU",
        "targetCpuFeatures",
        "hasFeature",
        "hostOsName",
        "hostArch",
        "hostEndian",
        "currentTimestamp",
        "processorCount",
        "pageSize",
        "cpuCacheLineSize",
        "hostName",
    ] {
        self::push_item(
            items,
            seen,
            builtin,
            CompletionKind::Function,
            "compiler builtin",
            Some(&format!("{}($0)", builtin)),
        );
    }
}

fn push_top_level_templates(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "main-fn",
        CompletionKind::Snippet,
        "template",
        Some("fn main() s32 @public {\n    $0\n\n    return 0;\n}"),
    );
    self::push_item(
        items,
        seen,
        "fn-template",
        CompletionKind::Snippet,
        "template",
        Some("fn ${1:name}(${2:args}) ${3:void} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "public-fn",
        CompletionKind::Snippet,
        "template",
        Some("fn ${1:name}(${2:args}) ${3:void} @public {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "struct-template",
        CompletionKind::Snippet,
        "template",
        Some("struct ${1:Name} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "generic-struct",
        CompletionKind::Snippet,
        "template",
        Some("struct ${1:Name} [${2:T}] {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "enum-template",
        CompletionKind::Snippet,
        "template",
        Some("enum ${1:Name} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "type-alias",
        CompletionKind::Snippet,
        "template",
        Some("type ${1:Name} = ${2:u32};"),
    );
    self::push_item(
        items,
        seen,
        "const-template",
        CompletionKind::Snippet,
        "template",
        Some("const ${1:NAME}: ${2:type} = ${3:value};"),
    );
    self::push_item(
        items,
        seen,
        "static-template",
        CompletionKind::Snippet,
        "template",
        Some("static ${1:name}: ${2:type} = ${3:value};"),
    );
    self::push_item(
        items,
        seen,
        "import-as",
        CompletionKind::Snippet,
        "template",
        Some("import ${1:std::mem} as ${2:mem};"),
    );
    self::push_item(
        items,
        seen,
        "import-only",
        CompletionKind::Snippet,
        "template",
        Some("import ${1:std::mem} only { ${2:symbol} };"),
    );
    self::push_item(
        items,
        seen,
        "directive-template",
        CompletionKind::Snippet,
        "template",
        Some("directive \"${1:--disable-warnings=W0000}\";"),
    );
    self::push_item(
        items,
        seen,
        "compiletime-if",
        CompletionKind::Snippet,
        "template",
        Some("@if(${1:condition}) {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "compiletime-if-else",
        CompletionKind::Snippet,
        "template",
        Some("@if(${1:condition}) {\n    $2\n} @else {\n    $0\n}"),
    );
}

fn push_statement_templates(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "for-loop",
        CompletionKind::Snippet,
        "template",
        Some("for var ${1:i}: usize = 0; ${1:i} < ${2:limit}; ${1:i} = ${1:i} + 1; {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "while-loop",
        CompletionKind::Snippet,
        "template",
        Some("while ${1:condition} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "loop-block",
        CompletionKind::Snippet,
        "template",
        Some("loop {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "if-block",
        CompletionKind::Snippet,
        "template",
        Some("if ${1:condition} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "if-else",
        CompletionKind::Snippet,
        "template",
        Some("if ${1:condition} {\n    $2\n} else {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "else-if-block",
        CompletionKind::Snippet,
        "template",
        Some("else if ${1:condition} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "elif-block",
        CompletionKind::Snippet,
        "template",
        Some("elif ${1:condition} {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "defer-block",
        CompletionKind::Snippet,
        "template",
        Some("defer {\n    $0\n}"),
    );
    self::push_item(
        items,
        seen,
        "var-template",
        CompletionKind::Snippet,
        "template",
        Some("var ${1:name}: ${2:type} = ${3:value};"),
    );
    self::push_item(
        items,
        seen,
        "infer-var",
        CompletionKind::Snippet,
        "template",
        Some("var ${1:name} := ${2:value};"),
    );
    self::push_item(
        items,
        seen,
        "compiletime-if",
        CompletionKind::Snippet,
        "template",
        Some("@if(${1:condition}) {\n    $0\n}"),
    );
}

fn push_attributes(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(
        items,
        seen,
        "@heap",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@dealloc",
        CompletionKind::Keyword,
        "attribute",
        Some("@dealloc"),
    );
    self::push_item(
        items,
        seen,
        "@deallocator",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@extern",
        CompletionKind::Keyword,
        "attribute",
        Some("@extern(\"$0\")"),
    );
    self::push_item(
        items,
        seen,
        "@arbitraryArgs",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@public",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@entrypoint",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@minSize",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@noInline",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@alwaysInline",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@inline",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@hot",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@safeStack",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@weakStack",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@strongStack",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@preciseFloatingPoint",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@convention",
        CompletionKind::Keyword,
        "attribute",
        Some("@convention(\"$0\")"),
    );
    self::push_item(
        items,
        seen,
        "@noUnwind",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@noReturn",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@packed",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@optFuzzing",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@align",
        CompletionKind::Keyword,
        "attribute",
        Some("@align($0)"),
    );
    self::push_item(
        items,
        seen,
        "@linkage",
        CompletionKind::Keyword,
        "attribute",
        Some("@linkage(\"$0\")"),
    );
    self::push_item(
        items,
        seen,
        "@pure",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@cuda",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@thunk",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@constructor",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@destructor",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
}

#[inline]
fn push_symbol(items: &mut Vec<Value>, seen: &mut HashSet<String>, symbol: &Symbol) {
    self::push_item(
        items,
        seen,
        symbol.get_name(),
        symbol.get_kind(),
        symbol.get_detail(),
        symbol.get_insert_text(),
    );
}

fn convert_import_symbol(symbol: &thrustc_preprocessor::signatures::Symbol) -> Option<Symbol> {
    let kind: CompletionKind = match symbol.variant {
        Variant::Function | Variant::CompilerIntrinsic => CompletionKind::Function,
        Variant::Constant => CompletionKind::Constant,
        Variant::Static => CompletionKind::Variable,
        Variant::Struct => CompletionKind::Struct,
        Variant::Enum => CompletionKind::Enum,
        Variant::CustomType => CompletionKind::TypeParameter,
    };

    let mut detail: String = String::with_capacity(64);
    let mut insert_text: Option<String> = None;

    match &symbol.signature {
        Signature::Function {
            kind,
            parameters,
            attributes,
            ..
        } => {
            if !attributes.has_public_attribute() {
                return None;
            }

            detail.push_str("fn ");
            detail.push_str(&symbol.name);
            detail.push('(');

            for (index, (name, ty, _)) in parameters.iter().enumerate() {
                if index > 0 {
                    detail.push_str(", ");
                }

                detail.push_str(name);
                detail.push_str(": ");
                detail.push_str(&ty.to_string());
            }

            detail.push(')');

            if !kind.to_string().is_empty() {
                detail.push(' ');
                detail.push_str(&kind.to_string());
            }

            insert_text = Some(format!("{}($0)", symbol.name));
        }
        Signature::CompilerIntrinsic {
            kind,
            parameters,
            attributes,
            ..
        } => {
            if !attributes.has_public_attribute() {
                return None;
            }

            detail.push_str("intrinsic ");
            detail.push_str(&symbol.name);
            detail.push('(');

            for (index, (name, ty, _)) in parameters.iter().enumerate() {
                if index > 0 {
                    detail.push_str(", ");
                }

                detail.push_str(name);
                detail.push_str(": ");
                detail.push_str(&ty.to_string());
            }

            detail.push(')');

            if !kind.to_string().is_empty() {
                detail.push(' ');
                detail.push_str(&kind.to_string());
            }

            insert_text = Some(format!("{}($0)", symbol.name));
        }
        Signature::Constant {
            kind, attributes, ..
        } => {
            if !attributes.has_public_attribute() {
                return None;
            }

            detail.push_str("const: ");
            detail.push_str(&kind.to_string());
        }
        Signature::Static {
            kind, attributes, ..
        } => {
            if !attributes.has_public_attribute() {
                return None;
            }

            detail.push_str("static: ");
            detail.push_str(&kind.to_string());
        }
        Signature::Struct { kind, .. } => {
            detail.push_str("struct: ");
            detail.push_str(&kind.to_string());
        }
        Signature::Enum { fields, .. } => {
            detail.push_str("enum ");
            detail.push_str(&symbol.name);
            detail.push_str(" fields: ");
            detail.push_str(&fields.len().to_string());
        }
        Signature::CustomType {
            kind, attributes, ..
        } => {
            if !attributes.has_public_attribute() {
                return None;
            }

            detail.push_str("type: ");
            detail.push_str(&kind.to_string());
        }
    }

    Some(Symbol::new(
        symbol.name.clone(),
        kind,
        detail,
        insert_text,
        None,
        0,
        u64::MAX,
        0,
    ))
}

#[inline]
fn push_item(
    items: &mut Vec<Value>,
    seen: &mut HashSet<String>,
    label: &str,
    kind: CompletionKind,
    detail: &str,
    insert_text: Option<&str>,
) {
    if !seen.insert(label.to_string()) {
        return;
    }

    let mut item: Value = serde_json::json!({
        "label": label,
        "kind": kind.as_lsp_value(),
        "detail": detail
    });

    if let Some(insert_text) = insert_text {
        item["insertText"] = Value::String(insert_text.to_string());
        item["insertTextFormat"] = Value::Number(2.into());
    }

    if kind == CompletionKind::Snippet {
        item["sortText"] = Value::String(format!("zzzz_{}", label));
    }

    items.push(item);
}

fn resolve_symbol_type(
    document_analysis: &DocumentAnalysis,
    name: &str,
    line: u64,
) -> Option<String> {
    for symbol in document_analysis.get_symbols().iter().rev() {
        if symbol.get_name() != name {
            continue;
        }

        if !symbol.is_visible_at(line) {
            continue;
        }

        if let Some(type_name) = symbol.get_type_name() {
            return Some(type_name.to_string());
        }
    }

    None
}

fn find_imported_module<'a>(
    module: &'a ImportedModule,
    receiver: &str,
) -> Option<&'a ImportedModule> {
    if module.get_name() == receiver {
        return Some(module);
    }

    let parts: Vec<&str> = receiver
        .split("::")
        .filter(|part| !part.is_empty())
        .collect();

    self::find_imported_module_parts(module, &parts)
}

fn find_imported_module_parts<'a>(
    module: &'a ImportedModule,
    parts: &[&str],
) -> Option<&'a ImportedModule> {
    if parts.is_empty() {
        return Some(module);
    }

    if module.get_name() != parts[0] {
        return None;
    }

    if parts.len() == 1 {
        return Some(module);
    }

    for submodule in module.get_submodules() {
        if let Some(module) = self::find_imported_module_parts(submodule, &parts[1..]) {
            return Some(module);
        }
    }

    None
}

fn find_active_call_open(prefix: &str) -> Option<usize> {
    let mut stack: Vec<usize> = Vec::with_capacity(8);
    let mut in_string: bool = false;
    let mut in_char: bool = false;
    let mut escaped: bool = false;

    for (index, ch) in prefix.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }

        if (in_string || in_char) && ch == '\\' {
            escaped = true;
            continue;
        }

        if ch == '"' && !in_char {
            in_string = !in_string;
            continue;
        }

        if ch == '\'' && !in_string {
            in_char = !in_char;
            continue;
        }

        if in_string || in_char {
            continue;
        }

        if ch == '(' {
            stack.push(index);
        } else if ch == ')' {
            stack.pop();
        }
    }

    stack.pop()
}

fn split_top_level_arguments(arguments: &str) -> Vec<&str> {
    let mut result: Vec<&str> = Vec::with_capacity(8);
    let mut start: usize = 0;
    let mut paren_depth: usize = 0;
    let mut bracket_depth: usize = 0;
    let mut brace_depth: usize = 0;
    let mut in_string: bool = false;
    let mut in_char: bool = false;
    let mut escaped: bool = false;

    for (index, ch) in arguments.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }

        if (in_string || in_char) && ch == '\\' {
            escaped = true;
            continue;
        }

        if ch == '"' && !in_char {
            in_string = !in_string;
            continue;
        }

        if ch == '\'' && !in_string {
            in_char = !in_char;
            continue;
        }

        if in_string || in_char {
            continue;
        }

        match ch {
            '(' => paren_depth = paren_depth.saturating_add(1),
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth = bracket_depth.saturating_add(1),
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '{' => brace_depth = brace_depth.saturating_add(1),
            '}' => brace_depth = brace_depth.saturating_sub(1),
            ',' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                result.push(arguments[start..index].trim());
                start = index.saturating_add(1);
            }
            _ => {}
        }
    }

    result.push(arguments[start..].trim());

    result
}

fn strip_trailing_generic_args(source: &str) -> &str {
    let source: &str = source.trim_end();

    if !source.ends_with(']') {
        return source;
    }

    let mut depth: usize = 0;

    for (index, ch) in source.char_indices().rev() {
        if ch == ']' {
            depth = depth.saturating_add(1);
            continue;
        }

        if ch == '[' {
            depth = depth.saturating_sub(1);

            if depth == 0 {
                return &source[..index];
            }
        }
    }

    source
}

fn previous_path(source: &str) -> Option<String> {
    let mut chars: Vec<char> = Vec::with_capacity(32);

    for ch in source.chars().rev() {
        if ch == '_' || ch.is_ascii_alphanumeric() || ch == ':' {
            chars.push(ch);
            continue;
        }

        if !chars.is_empty() {
            break;
        }
    }

    if chars.is_empty() {
        return None;
    }

    chars.reverse();

    let path: String = chars.into_iter().collect();
    let path: &str = path.trim_matches(':');

    if path.is_empty() {
        None
    } else {
        Some(path.to_string())
    }
}

#[inline]
fn previous_identifier(source: &str) -> Option<String> {
    let mut chars: Vec<char> = Vec::with_capacity(32);

    for ch in source.chars().rev() {
        if ch == '_' || ch.is_ascii_alphanumeric() {
            chars.push(ch);
            continue;
        }

        if !chars.is_empty() {
            break;
        }
    }

    if chars.is_empty() {
        return None;
    }

    chars.reverse();

    Some(chars.into_iter().collect())
}
