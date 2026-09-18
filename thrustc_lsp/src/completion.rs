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

use crate::analysis::{Analysis, CompletionKind, DocumentAnalysis, Symbol};
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

    if trimmed.ends_with('@') || last_word.is_some_and(|last_word| last_word.starts_with('@')) {
        self::push_attributes(&mut items, &mut seen);
        return items;
    }

    if let Some(document_analysis) = document_analysis {
        if self::try_module_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }

        if self::try_member_completion(&mut items, &mut seen, document_analysis, &prefix, line) {
            return items;
        }

        if self::try_enum_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }

        if self::try_call_argument_completion(&mut items, &mut seen, document_analysis, &prefix) {
            return items;
        }
    }

    let words: Vec<&str> = trimmed.split_whitespace().collect();
    let is_type_context: bool = if trimmed.ends_with("::") {
        false
    } else if trimmed.ends_with(':') {
        true
    } else if trimmed.ends_with(" as") || trimmed.ends_with(" as ") {
        true
    } else if words.is_empty() {
        false
    } else {
        let last: &str = words[words.len().saturating_sub(1)];

        last == "ptr" || last == "array" || last == "fixed" || last == "fnref"
    };

    if is_type_context {
        self::push_types(&mut items, &mut seen);

        if let Some(document_analysis) = document_analysis {
            self::push_type_symbols(&mut items, &mut seen, document_analysis);
        }

        return items;
    }

    if let Some(document_analysis) = document_analysis {
        let depth: u64 = document_analysis.get_line_depth(line);

        if depth == 0 {
            self::push_top_level_keywords(&mut items, &mut seen);
            self::push_type_symbols(&mut items, &mut seen, document_analysis);
            self::push_global_symbols(&mut items, &mut seen, document_analysis);
            self::push_templates(&mut items, &mut seen);

            return items;
        }

        self::push_visible_symbols(&mut items, &mut seen, document_analysis, line);
    }

    self::push_global_items(&mut items, &mut seen);

    items
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
        if module.get_name() != receiver {
            continue;
        }

        for symbol in module.get_symbols() {
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
    let Some(open_index) = prefix.rfind('(') else {
        return false;
    };
    let source: &str = &prefix[..open_index];
    let Some(function_name) = self::previous_identifier(source) else {
        return false;
    };

    for function in document_analysis.get_functions() {
        if function.get_name() != function_name {
            continue;
        }

        for parameter in function.get_parameters() {
            let label: &str = parameter.get_name();
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

        return true;
    }

    false
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

        if !matches!(
            kind,
            CompletionKind::Enum | CompletionKind::Struct | CompletionKind::TypeParameter
        ) {
            continue;
        }

        self::push_symbol(items, seen, symbol);
    }
}

#[inline]
fn push_global_items(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_keywords(items, seen);
    self::push_types(items, seen);
    self::push_builtins(items, seen);
    self::push_templates(items, seen);
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
        "importc",
        CompletionKind::Keyword,
        "C import",
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
        "asmfn",
        CompletionKind::Keyword,
        "assembler function",
        None,
    );
}

fn push_keywords(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
    self::push_item(items, seen, "fn", CompletionKind::Keyword, "keyword", None);
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
        "continue",
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
        "import",
        CompletionKind::Keyword,
        "keyword",
        None,
    );
    self::push_item(
        items,
        seen,
        "importc",
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
    self::push_item(items, seen, "ref", CompletionKind::Keyword, "keyword", None);
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
}

fn push_types(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
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
        "fx8680",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "fppc128",
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
        "cstring",
        CompletionKind::TypeParameter,
        "type",
        None,
    );
    self::push_item(
        items,
        seen,
        "cnstring",
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
        "fnref",
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
        "memCpy",
        CompletionKind::Function,
        "builtin",
        Some("memCpy($0)"),
    );
    self::push_item(
        items,
        seen,
        "memMove",
        CompletionKind::Function,
        "builtin",
        Some("memMove($0)"),
    );
    self::push_item(
        items,
        seen,
        "memSet",
        CompletionKind::Function,
        "builtin",
        Some("memSet($0)"),
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
}

fn push_templates(items: &mut Vec<Value>, seen: &mut HashSet<String>) {
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
        "for-loop",
        CompletionKind::Snippet,
        "template",
        Some("for var ${1:i}: usize = 0; ${1:i} < ${2:limit}; ${1:i} = ${1:i} + 1 {\n    $0\n}"),
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
        "@ignore",
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
        "@entryPoint",
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
        "@inlineHint",
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
        "@preciseFloats",
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
        "@asmAlignStack",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@asmSyntax",
        CompletionKind::Keyword,
        "attribute",
        Some("@asmSyntax(\"$0\")"),
    );
    self::push_item(
        items,
        seen,
        "@asmThrow",
        CompletionKind::Keyword,
        "attribute",
        None,
    );
    self::push_item(
        items,
        seen,
        "@asmSideEffects",
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
    self::push_item(
        items,
        seen,
        "@promote",
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
