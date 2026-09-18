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

use std::collections::HashMap;
use std::path::PathBuf;

use serde_json::Value;
use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_builtins::BuiltinRegistry;
use thrustc_lexer::Lexer;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_preprocessor::Preprocessor;
use thrustc_preprocessor::signatures::{Signature, Variant};
use thrustc_typesystem::type_layout::TargetInfo;

use crate::documents::Document;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompletionKind {
    Function,
    Field,
    Variable,
    Module,
    Enum,
    Keyword,
    EnumMember,
    Constant,
    Struct,
    TypeParameter,
}

impl CompletionKind {
    #[inline]
    pub fn as_lsp_value(self) -> u64 {
        match self {
            CompletionKind::Function => 3,
            CompletionKind::Field => 5,
            CompletionKind::Variable => 6,
            CompletionKind::Module => 9,
            CompletionKind::Enum => 13,
            CompletionKind::Keyword => 14,
            CompletionKind::EnumMember => 20,
            CompletionKind::Constant => 21,
            CompletionKind::Struct => 22,
            CompletionKind::TypeParameter => 25,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Symbol {
    name: String,
    kind: CompletionKind,
    detail: String,
    insert_text: Option<String>,
    type_name: Option<String>,
    scope_start: u64,
    scope_end: u64,
    declaration_line: u64,
}

#[derive(Clone, Debug)]
pub struct Structure {
    name: String,
    fields: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct Enumeration {
    name: String,
    values: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    parameters: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct ImportedModule {
    name: String,
    symbols: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct DocumentAnalysis {
    symbols: Vec<Symbol>,
    structures: Vec<Structure>,
    enumerations: Vec<Enumeration>,
    functions: Vec<Function>,
    modules: Vec<ImportedModule>,
    line_depths: Vec<u64>,
}

#[derive(Debug)]
pub struct Analysis {
    documents: HashMap<String, DocumentAnalysis>,
    analyzed_documents: usize,
}

impl Analysis {
    #[inline]
    pub fn new() -> Self {
        Self {
            documents: HashMap::with_capacity(u8::MAX as usize),
            analyzed_documents: 0,
        }
    }

    pub fn analyze_document(&mut self, document: Option<&Document>) -> Vec<Value> {
        let Some(document) = document else {
            return Vec::with_capacity(0);
        };

        let uri: &str = document.get_uri();
        let text: &str = document.get_text();
        let document_analysis: DocumentAnalysis = self::analyze_text(uri, text);

        self.documents.insert(uri.to_string(), document_analysis);
        self.analyzed_documents = self.analyzed_documents.saturating_add(1);

        Vec::with_capacity(0)
    }
}

impl Analysis {
    #[inline]
    pub fn get_document(&self, uri: &str) -> Option<&DocumentAnalysis> {
        self.documents.get(uri)
    }

    #[inline]
    pub fn remove_document(&mut self, uri: &str) {
        self.documents.remove(uri);
    }
}

impl DocumentAnalysis {
    #[inline]
    pub fn get_symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    #[inline]
    pub fn get_structures(&self) -> &[Structure] {
        &self.structures
    }

    #[inline]
    pub fn get_enumerations(&self) -> &[Enumeration] {
        &self.enumerations
    }

    #[inline]
    pub fn get_functions(&self) -> &[Function] {
        &self.functions
    }

    #[inline]
    pub fn get_modules(&self) -> &[ImportedModule] {
        &self.modules
    }

    #[inline]
    pub fn get_line_depth(&self, line: u64) -> u64 {
        let line: usize = line.try_into().unwrap_or(usize::MAX);

        self.line_depths.get(line).copied().unwrap_or(0)
    }
}

impl Symbol {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_kind(&self) -> CompletionKind {
        self.kind
    }

    #[inline]
    pub fn get_detail(&self) -> &str {
        &self.detail
    }

    #[inline]
    pub fn get_insert_text(&self) -> Option<&str> {
        self.insert_text.as_deref()
    }

    #[inline]
    pub fn get_type_name(&self) -> Option<&str> {
        self.type_name.as_deref()
    }

    #[inline]
    pub fn is_visible_at(&self, line: u64) -> bool {
        line >= self.scope_start && line <= self.scope_end && line >= self.declaration_line
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        self.scope_start == 0 && self.scope_end != 0
    }
}

impl Structure {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_fields(&self) -> &[Symbol] {
        &self.fields
    }
}

impl Enumeration {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_values(&self) -> &[Symbol] {
        &self.values
    }
}

impl Function {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_parameters(&self) -> &[Symbol] {
        &self.parameters
    }
}

impl ImportedModule {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_symbols(&self) -> &[Symbol] {
        &self.symbols
    }
}

fn analyze_text(uri: &str, text: &str) -> DocumentAnalysis {
    let lines: Vec<&str> = text.lines().collect();
    let line_depths: Vec<u64> = self::build_line_depths(&lines);
    let total_lines: u64 = lines.len().try_into().unwrap_or(u64::MAX);

    let mut symbols: Vec<Symbol> = Vec::with_capacity(u8::MAX as usize);
    let mut structures: Vec<Structure> = Vec::with_capacity(u8::MAX as usize);
    let mut enumerations: Vec<Enumeration> = Vec::with_capacity(u8::MAX as usize);
    let mut functions: Vec<Function> = Vec::with_capacity(u8::MAX as usize);
    let modules: Vec<ImportedModule> = self::analyze_imported_modules(uri, text);

    let mut line_index: usize = 0;
    let mut block_comment: bool = false;

    while line_index < lines.len() {
        let raw_line: &str = lines[line_index];
        let line: String = self::clean_line(raw_line, &mut block_comment);
        let trimmed: &str = line.trim();
        let line_number: u64 = line_index.try_into().unwrap_or(u64::MAX);

        if trimmed.is_empty() {
            line_index = line_index.saturating_add(1);
            continue;
        }

        if trimmed.starts_with("struct ") {
            let end_line: u64 = self::find_block_end(&lines, line_index);
            let name: String = self::parse_named_declaration(trimmed, "struct");

            if !name.is_empty() {
                let fields: Vec<Symbol> = self::parse_struct_fields(&lines, line_index, end_line);
                let symbol: Symbol = Symbol {
                    name: name.clone(),
                    kind: CompletionKind::Struct,
                    detail: "struct".into(),
                    insert_text: None,
                    type_name: Some(name.clone()),
                    scope_start: 0,
                    scope_end: total_lines,
                    declaration_line: line_number,
                };

                symbols.push(symbol);
                structures.push(Structure { name, fields });
            }
        }

        if trimmed.starts_with("enum ") {
            let end_line: u64 = self::find_block_end(&lines, line_index);
            let name: String = self::parse_named_declaration(trimmed, "enum");

            if !name.is_empty() {
                let values: Vec<Symbol> = self::parse_enum_values(&lines, line_index, end_line);
                let symbol: Symbol = Symbol {
                    name: name.clone(),
                    kind: CompletionKind::Enum,
                    detail: "enum".into(),
                    insert_text: None,
                    type_name: Some(name.clone()),
                    scope_start: 0,
                    scope_end: total_lines,
                    declaration_line: line_number,
                };

                symbols.push(symbol);
                enumerations.push(Enumeration { name, values });
            }
        }

        if trimmed.starts_with("fn ") {
            let end_line: u64 = self::find_block_end(&lines, line_index);
            let name: String = self::parse_named_declaration(trimmed, "fn");

            if !name.is_empty() {
                let parameters: Vec<Symbol> =
                    self::parse_function_parameters(trimmed, line_number, end_line);
                let mut detail: String = String::with_capacity(trimmed.len());

                for ch in trimmed.chars() {
                    if ch == '{' || ch == '@' || ch == ';' {
                        break;
                    }

                    detail.push(ch);
                }

                let detail: String = detail.trim().to_string();
                let insert_text: String = format!("{}($0)", name);
                let symbol: Symbol = Symbol {
                    name: name.clone(),
                    kind: CompletionKind::Function,
                    detail,
                    insert_text: Some(insert_text),
                    type_name: None,
                    scope_start: 0,
                    scope_end: total_lines,
                    declaration_line: line_number,
                };

                symbols.push(symbol);

                for parameter in parameters.iter() {
                    symbols.push(parameter.clone());
                }

                functions.push(Function { name, parameters });
            }
        }

        if trimmed.starts_with("type ") {
            let name: String = self::parse_named_declaration(trimmed, "type");
            let aliased_type: String = self::parse_type_after_colon(trimmed);

            if !name.is_empty() {
                symbols.push(Symbol {
                    name,
                    kind: CompletionKind::TypeParameter,
                    detail: aliased_type,
                    insert_text: None,
                    type_name: None,
                    scope_start: 0,
                    scope_end: total_lines,
                    declaration_line: line_number,
                });
            }
        }

        if trimmed.starts_with("const ")
            || trimmed.starts_with("static ")
            || trimmed.starts_with("var ")
        {
            let depth: u64 = line_depths.get(line_index).copied().unwrap_or(0);
            let keyword: &str = if trimmed.starts_with("const ") {
                "const"
            } else if trimmed.starts_with("static ") {
                "static"
            } else {
                "var"
            };
            let name: String = self::parse_named_declaration(trimmed, keyword);
            let type_name: String = self::parse_type_after_colon(trimmed);
            let mut scope_start: u64 = 0;
            let mut scope_end: u64 = total_lines;

            if depth > 0 || keyword == "var" {
                scope_start = line_number;
                scope_end = self::find_scope_end(&line_depths, line_index, depth);
            }

            if !name.is_empty() {
                let kind: CompletionKind = if keyword == "const" {
                    CompletionKind::Constant
                } else {
                    CompletionKind::Variable
                };
                let detail: String = if type_name.is_empty() {
                    keyword.to_string()
                } else {
                    format!("{}: {}", keyword, type_name)
                };

                symbols.push(Symbol {
                    name,
                    kind,
                    detail,
                    insert_text: None,
                    type_name: Some(type_name),
                    scope_start,
                    scope_end,
                    declaration_line: line_number,
                });
            }
        }

        if trimmed.starts_with("import ") {
            let source: &str = trimmed.trim_start_matches("import").trim();
            let source: &str = source.trim_end_matches(';').trim();
            let source: &str = source.split(" only ").next().unwrap_or(source);
            let source: &str = source.split(" as ").last().unwrap_or(source);
            let imported: String = source
                .split("::")
                .last()
                .unwrap_or(source)
                .trim()
                .to_string();

            if !imported.is_empty() {
                symbols.push(Symbol {
                    name: imported.clone(),
                    kind: CompletionKind::Module,
                    detail: "module".into(),
                    insert_text: None,
                    type_name: Some(imported),
                    scope_start: 0,
                    scope_end: total_lines,
                    declaration_line: line_number,
                });
            }
        }

        line_index = line_index.saturating_add(1);
    }

    DocumentAnalysis {
        symbols,
        structures,
        enumerations,
        functions,
        modules,
        line_depths,
    }
}

fn analyze_imported_modules(uri: &str, text: &str) -> Vec<ImportedModule> {
    let mut modules: Vec<ImportedModule> = Vec::with_capacity(u8::MAX as usize);
    let path: PathBuf = self::uri_to_path(uri);
    let name: String = path.file_name().map_or_else(
        || "memory.thrust".to_string(),
        |name| name.to_string_lossy().to_string(),
    );
    let base_name: String = path.file_stem().map_or_else(
        || "memory".to_string(),
        |name| name.to_string_lossy().to_string(),
    );

    let options: CompilerOptions = CompilerOptions::new();
    let target_info: TargetInfo = TargetInfo::new(
        options
            .get_llvm_backend()
            .get_target()
            .get_normalized_target_triple()
            .clone(),
    );
    let builtins: BuiltinRegistry = thrustc_builtins::default_registry(target_info);
    let file: CompilationUnit = CompilationUnit::new(name, path, text.to_string(), base_name);
    let Ok(tokens) = Lexer::lex_for_preprocessor(&file, &options) else {
        return modules;
    };
    let Ok(directives) = thrustc_directive::apply_file_directives(&tokens) else {
        return modules;
    };
    let file_options: thrustc_directive::FileOptions =
        thrustc_directive::FileOptions::new(&options, &directives);
    let mut preprocessor: Preprocessor = Preprocessor::new();
    let Ok(imported) = preprocessor.generate_modules(&tokens, &file_options, &file, &builtins)
    else {
        return modules;
    };

    for module in imported {
        let mut symbols: Vec<Symbol> = Vec::with_capacity(u8::MAX as usize);

        for symbol in module.get_symbols() {
            if let Some(only) = module.get_only() {
                if !only.contains(&symbol.name) {
                    continue;
                }
            }

            let Some(converted) = self::convert_module_symbol(symbol) else {
                continue;
            };

            symbols.push(converted);
        }

        let module_name: String = if let Some(alias) = module.get_alias() {
            if alias.is_empty() {
                module.get_name().to_string()
            } else {
                alias.join("::")
            }
        } else {
            module.get_name().to_string()
        };

        modules.push(ImportedModule {
            name: module_name,
            symbols,
        });
    }

    modules
}

fn convert_module_symbol(symbol: &thrustc_preprocessor::signatures::Symbol) -> Option<Symbol> {
    let kind: CompletionKind = match symbol.variant {
        Variant::Function => CompletionKind::Function,
        Variant::Constant => CompletionKind::Constant,
        Variant::Static => CompletionKind::Variable,
        Variant::Struct => CompletionKind::Struct,
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

    Some(Symbol {
        name: symbol.name.clone(),
        kind,
        detail,
        insert_text,
        type_name: None,
        scope_start: 0,
        scope_end: u64::MAX,
        declaration_line: 0,
    })
}

fn uri_to_path(uri: &str) -> PathBuf {
    let uri: &str = uri.strip_prefix("file://").unwrap_or(uri);
    let mut result: String = String::with_capacity(uri.len());
    let bytes: &[u8] = uri.as_bytes();
    let mut index: usize = 0;

    while index < bytes.len() {
        if bytes[index] == b'%' && index + 2 < bytes.len() {
            let first: u8 = bytes[index + 1];
            let second: u8 = bytes[index + 2];
            let high: Option<u8> = if first.is_ascii_digit() {
                Some(first - b'0')
            } else if (b'a'..=b'f').contains(&first) {
                Some(first - b'a' + 10)
            } else if (b'A'..=b'F').contains(&first) {
                Some(first - b'A' + 10)
            } else {
                None
            };
            let low: Option<u8> = if second.is_ascii_digit() {
                Some(second - b'0')
            } else if (b'a'..=b'f').contains(&second) {
                Some(second - b'a' + 10)
            } else if (b'A'..=b'F').contains(&second) {
                Some(second - b'A' + 10)
            } else {
                None
            };

            if let (Some(high), Some(low)) = (high, low) {
                result.push((high << 4 | low) as char);
                index = index.saturating_add(3);
                continue;
            }
        }

        result.push(bytes[index] as char);
        index = index.saturating_add(1);
    }

    PathBuf::from(result)
}

fn parse_struct_fields(lines: &[&str], start: usize, end: u64) -> Vec<Symbol> {
    let mut fields: Vec<Symbol> = Vec::with_capacity(u8::MAX as usize);
    let mut line_index: usize = start.saturating_add(1);
    let end: usize = end.try_into().unwrap_or(usize::MAX);
    let mut block_comment: bool = false;

    while line_index <= end && line_index < lines.len() {
        let raw_line: &str = lines[line_index];
        let line: String = self::clean_line(raw_line, &mut block_comment);
        let trimmed: &str = line.trim();
        let line_number: u64 = line_index.try_into().unwrap_or(u64::MAX);

        if !trimmed.contains(':') || trimmed.starts_with("fn ") {
            line_index = line_index.saturating_add(1);
            continue;
        }

        let Some((name, ty)) = trimmed.split_once(':') else {
            line_index = line_index.saturating_add(1);
            continue;
        };

        let name: String = self::clean_identifier(name);
        let type_name: String = self::clean_type(ty);

        if !name.is_empty() {
            fields.push(Symbol {
                name,
                kind: CompletionKind::Field,
                detail: type_name.clone(),
                insert_text: None,
                type_name: Some(type_name),
                scope_start: 0,
                scope_end: u64::MAX,
                declaration_line: line_number,
            });
        }

        line_index = line_index.saturating_add(1);
    }

    fields
}

fn parse_enum_values(lines: &[&str], start: usize, end: u64) -> Vec<Symbol> {
    let mut values: Vec<Symbol> = Vec::with_capacity(u8::MAX as usize);
    let mut line_index: usize = start.saturating_add(1);
    let end: usize = end.try_into().unwrap_or(usize::MAX);
    let mut block_comment: bool = false;

    while line_index <= end && line_index < lines.len() {
        let raw_line: &str = lines[line_index];
        let line: String = self::clean_line(raw_line, &mut block_comment);
        let trimmed: &str = line.trim();
        let line_number: u64 = line_index.try_into().unwrap_or(u64::MAX);

        if trimmed.is_empty() || trimmed.starts_with('}') {
            line_index = line_index.saturating_add(1);
            continue;
        }

        let left: &str = trimmed.split(':').next().unwrap_or(trimmed);
        let left: &str = left.split('=').next().unwrap_or(left);
        let left: &str = left.split(';').next().unwrap_or(left);
        let name: String = self::clean_identifier(left);

        if !name.is_empty() {
            values.push(Symbol {
                name,
                kind: CompletionKind::EnumMember,
                detail: "enum value".into(),
                insert_text: None,
                type_name: None,
                scope_start: 0,
                scope_end: u64::MAX,
                declaration_line: line_number,
            });
        }

        line_index = line_index.saturating_add(1);
    }

    values
}

fn parse_function_parameters(line: &str, declaration_line: u64, scope_end: u64) -> Vec<Symbol> {
    let mut parameters: Vec<Symbol> = Vec::with_capacity(16);
    let Some(open) = line.find('(') else {
        return parameters;
    };
    let Some(close) = line[open.saturating_add(1)..].find(')') else {
        return parameters;
    };

    let close: usize = open.saturating_add(1).saturating_add(close);
    let text: &str = &line[open.saturating_add(1)..close];

    for parameter in text.split(',') {
        let parameter: &str = parameter.trim();

        if parameter.is_empty() {
            continue;
        }

        let Some((name, ty)) = parameter.split_once(':') else {
            continue;
        };

        let name: String = self::clean_identifier(name);
        let type_name: String = self::clean_type(ty);

        if name.is_empty() {
            continue;
        }

        parameters.push(Symbol {
            name,
            kind: CompletionKind::Variable,
            detail: type_name.clone(),
            insert_text: None,
            type_name: Some(type_name),
            scope_start: declaration_line,
            scope_end,
            declaration_line,
        });
    }

    parameters
}

fn build_line_depths(lines: &[&str]) -> Vec<u64> {
    let mut line_depths: Vec<u64> = Vec::with_capacity(lines.len());
    let mut depth: u64 = 0;
    let mut block_comment: bool = false;

    for line in lines {
        let line: String = self::clean_line(line, &mut block_comment);
        line_depths.push(depth);

        for ch in line.chars() {
            if ch == '{' {
                depth = depth.saturating_add(1);
            }

            if ch == '}' {
                depth = depth.saturating_sub(1);
            }
        }
    }

    line_depths
}

fn clean_line(line: &str, block_comment: &mut bool) -> String {
    let mut result: String = String::with_capacity(line.len());
    let chars: Vec<char> = line.chars().collect();
    let mut index: usize = 0;
    let mut in_string: bool = false;
    let mut in_char: bool = false;

    while index < chars.len() {
        let ch: char = chars[index];
        let next: char = chars.get(index.saturating_add(1)).copied().unwrap_or('\0');

        if *block_comment {
            if ch == '*' && next == '/' {
                *block_comment = false;
                index = index.saturating_add(2);
                continue;
            }

            index = index.saturating_add(1);
            continue;
        }

        if !in_string && !in_char && ch == '/' && next == '/' {
            break;
        }

        if !in_string && !in_char && ch == '/' && next == '*' {
            *block_comment = true;
            index = index.saturating_add(2);
            continue;
        }

        if ch == '"' && !in_char {
            in_string = !in_string;
        }

        if ch == '\'' && !in_string {
            in_char = !in_char;
        }

        result.push(ch);
        index = index.saturating_add(1);
    }

    result
}

fn find_block_end(lines: &[&str], start: usize) -> u64 {
    let mut depth: u64 = 0;
    let mut saw_open: bool = false;
    let mut line_index: usize = start;
    let mut block_comment: bool = false;

    while line_index < lines.len() {
        let line: String = self::clean_line(lines[line_index], &mut block_comment);

        for ch in line.chars() {
            if ch == '{' {
                saw_open = true;
                depth = depth.saturating_add(1);
            }

            if ch == '}' {
                depth = depth.saturating_sub(1);

                if saw_open && depth == 0 {
                    return line_index.try_into().unwrap_or(u64::MAX);
                }
            }
        }

        if saw_open {
            line_index = line_index.saturating_add(1);
            continue;
        }

        if line.contains(';') {
            return line_index.try_into().unwrap_or(u64::MAX);
        }

        line_index = line_index.saturating_add(1);
    }

    lines.len().try_into().unwrap_or(u64::MAX)
}

fn find_scope_end(line_depths: &[u64], start: usize, depth: u64) -> u64 {
    let mut line_index: usize = start.saturating_add(1);

    while line_index < line_depths.len() {
        let line_depth: u64 = line_depths[line_index];

        if line_depth < depth {
            return line_index.try_into().unwrap_or(u64::MAX);
        }

        line_index = line_index.saturating_add(1);
    }

    line_depths.len().try_into().unwrap_or(u64::MAX)
}

fn parse_named_declaration(line: &str, keyword: &str) -> String {
    let mut source: &str = line.trim();

    if let Some(rest) = source.strip_prefix(keyword) {
        source = rest.trim_start();
    }

    let mut name: String = String::with_capacity(32);

    for ch in source.chars() {
        if ch == '_' || ch.is_ascii_alphanumeric() {
            name.push(ch);
            continue;
        }

        break;
    }

    name
}

#[inline]
fn parse_type_after_colon(line: &str) -> String {
    let Some((_, right)) = line.split_once(':') else {
        return String::new();
    };

    self::clean_type(right)
}

#[inline]
fn clean_type(value: &str) -> String {
    let mut result: String = String::with_capacity(value.len());

    for ch in value.trim().chars() {
        if ch == '=' || ch == ',' || ch == ';' || ch == '{' || ch == '@' {
            break;
        }

        result.push(ch);
    }

    result.trim().to_string()
}

#[inline]
fn clean_identifier(value: &str) -> String {
    let mut result: String = String::with_capacity(value.len());

    for ch in value.trim().chars() {
        if ch == '_' || ch.is_ascii_alphanumeric() {
            result.push(ch);
            continue;
        }

        break;
    }

    result
}
