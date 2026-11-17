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

use std::path::PathBuf;

use serde_json::Value;
use thrustc_code_location::Span;
use thrustc_lexer::Lexer;
use thrustc_options::CompilationUnit;
use thrustc_preprocessor::Preprocessor;
use thrustc_typesystem::type_layout::TargetInfo;

use crate::analysis::{Analysis, DocumentAnalysis, Symbol};
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

    let mut qualified_start: usize = start;

    while qualified_start >= 2
        && chars[qualified_start - 1] == ':'
        && chars[qualified_start - 2] == ':'
    {
        let segment_end: usize = qualified_start.saturating_sub(2);

        let mut segment_start: usize = segment_end;

        while segment_start > 0
            && (chars[segment_start - 1] == '_' || chars[segment_start - 1].is_ascii_alphanumeric())
        {
            segment_start = segment_start.saturating_sub(1);
        }

        if segment_start == segment_end {
            break;
        }

        qualified_start = segment_start;
    }

    let receiver_text: String = chars[qualified_start..start].iter().collect();

    let receiver_parts: Vec<String> = receiver_text
        .split("::")
        .filter(|part| !part.is_empty())
        .map(|part| part.to_string())
        .collect();

    let target_name: String = reference.clone();

    let origin_start: u64 = start.try_into().unwrap_or(u64::MAX);

    let origin_end: u64 = end.try_into().unwrap_or(u64::MAX);

    for symbol in document_analysis.get_symbols().iter().rev() {
        if symbol.get_name() != reference {
            continue;
        }

        if symbol.get_declaration_line() > line {
            continue;
        }

        return self::symbol_location(uri, symbol, &lines);
    }

    let compiler_options: &thrustc_options::CompilerOptions = analysis.get_options();

    let current_path: PathBuf = url::Url::parse(uri)
        .ok()
        .and_then(|parsed| parsed.to_file_path().ok())
        .unwrap_or_else(|| PathBuf::from(uri));

    let current_name: String = current_path
        .file_name()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| "memory.thrust".to_string());

    let current_base: String = current_path
        .file_stem()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| "memory".to_string());

    let document_text: String = document.get_text().to_string();

    let target_info: TargetInfo = TargetInfo::new(
        compiler_options
            .get_llvm_backend()
            .get_target()
            .get_normalized_target_triple()
            .clone(),
    );

    let registry: thrustc_builtins::BuiltinRegistry =
        thrustc_builtins::default_registry(target_info);

    let current_file: CompilationUnit =
        CompilationUnit::new(current_name, current_path, document_text, current_base);

    let lexed_tokens: Option<Vec<thrustc_token::Token>> =
        Lexer::lex_for_preprocessor(&current_file, compiler_options).ok();

    let cross_file_link: Option<Value> = lexed_tokens.as_ref().and_then(|tokens| {
        let directives: thrustc_directive::FileDirectives =
            match thrustc_directive::apply_file_directives(
                tokens,
                compiler_options.get_compiler_features(),
            ) {
                Ok(directives) => directives,
                Err(_) => return None,
            };

        let file_options: thrustc_directive::FileOptions =
            thrustc_directive::FileOptions::new(compiler_options, &directives);

        let mut preprocessor: Preprocessor = Preprocessor::new();

        let modules: &[thrustc_preprocessor::module::Module] =
            match preprocessor.generate_modules(tokens, &file_options, &current_file, &registry) {
                Ok(modules) => modules,
                Err(_) => return None,
            };

        let matched_symbol: Option<(std::path::PathBuf, Span)> = if receiver_parts.is_empty() {
            modules.iter().find_map(|module| {
                let direct_hit: Option<(std::path::PathBuf, Span)> = module
                    .get_symbols()
                    .iter()
                    .find(|symbol| symbol.name == target_name && symbol.public)
                    .or_else(|| {
                        module
                            .get_symbols()
                            .iter()
                            .find(|symbol| symbol.name == target_name)
                    })
                    .map(|symbol| {
                        (
                            module.get_path().to_path_buf(),
                            symbol.signature.get_code_location(),
                        )
                    });

                if direct_hit.is_some() {
                    return direct_hit;
                }

                module.get_submodules().iter().find_map(|submodule| {
                    submodule
                        .get_symbols()
                        .iter()
                        .find(|symbol| symbol.name == target_name && symbol.public)
                        .or_else(|| {
                            submodule
                                .get_symbols()
                                .iter()
                                .find(|symbol| symbol.name == target_name)
                        })
                        .map(|symbol| {
                            (
                                submodule.get_path().to_path_buf(),
                                symbol.signature.get_code_location(),
                            )
                        })
                })
            })
        } else {
            modules.iter().find_map(|module| {
                let top_matches: bool = module.get_name() == receiver_parts[0]
                    || module
                        .get_alias()
                        .map(|alias| {
                            alias
                                .first()
                                .map(|first| first == &receiver_parts[0])
                                .unwrap_or(false)
                        })
                        .unwrap_or(false);

                if !top_matches {
                    return None;
                }

                let resolved_module: &thrustc_preprocessor::module::Module =
                    if receiver_parts.len() == 1 {
                        module
                    } else {
                        match module.find_submodule(receiver_parts[1..].to_vec()) {
                            Some(found) => found,
                            None => return None,
                        }
                    };

                resolved_module
                    .get_symbols()
                    .iter()
                    .find(|symbol| symbol.name == target_name && symbol.public)
                    .or_else(|| {
                        resolved_module
                            .get_symbols()
                            .iter()
                            .find(|symbol| symbol.name == target_name)
                    })
                    .map(|symbol| {
                        (
                            resolved_module.get_path().to_path_buf(),
                            symbol.signature.get_code_location(),
                        )
                    })
            })
        };

        let (target_path, target_span) = matched_symbol?;

        let target_uri: String = match url::Url::from_file_path(&target_path) {
            Ok(uri) => uri.to_string(),
            Err(_) => return None,
        };

        let target_line: u64 = target_span.get_line().saturating_sub(1).into();

        let target_start: u64 = target_span.get_span_start().into();

        let raw_end: u64 = target_span.get_span_end().into();

        let target_end: u64 = if raw_end > target_start {
            raw_end
        } else {
            target_start.saturating_add(target_name.len() as u64)
        };

        Some(serde_json::json!({
            "originSelectionRange": {
                "start": {"line": line, "character": origin_start},
                "end": {"line": line, "character": origin_end}
            },
            "targetUri": target_uri,
            "targetRange": {
                "start": {"line": target_line, "character": target_start},
                "end": {"line": target_line, "character": target_end}
            },
            "targetSelectionRange": {
                "start": {"line": target_line, "character": target_start},
                "end": {"line": target_line, "character": target_end}
            }
        }))
    });

    if let Some(link) = cross_file_link {
        return link;
    }

    if let Some(symbol) = self::find_field_symbol(document_analysis, &reference) {
        return self::symbol_location(uri, symbol, &lines);
    }

    if let Some(symbol) = self::find_enum_value_symbol(document_analysis, &reference) {
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

fn find_field_symbol<'analysis>(
    document_analysis: &'analysis DocumentAnalysis,
    reference: &str,
) -> Option<&'analysis Symbol> {
    for structure in document_analysis.get_structures() {
        let Some(field) = structure
            .get_fields()
            .iter()
            .find(|field| field.get_name() == reference)
        else {
            continue;
        };

        return Some(field);
    }

    None
}

fn find_enum_value_symbol<'analysis>(
    document_analysis: &'analysis DocumentAnalysis,
    reference: &str,
) -> Option<&'analysis Symbol> {
    for enumeration in document_analysis.get_enumerations() {
        let Some(value) = enumeration
            .get_values()
            .iter()
            .find(|value| value.get_name() == reference)
        else {
            continue;
        };

        return Some(value);
    }

    None
}

fn symbol_location(uri: &str, symbol: &Symbol, lines: &[&str]) -> Value {
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
