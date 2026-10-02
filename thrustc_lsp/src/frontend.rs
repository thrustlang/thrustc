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
use thrustc_builtins::BuiltinRegistry;
use thrustc_errors::CompilationIssue;
use thrustc_lexer::Lexer;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_parser::Parser;
use thrustc_preprocessor::Preprocessor;
use thrustc_typesystem::type_layout::TargetInfo;

pub fn diagnostics(uri: &str, text: &str) -> Vec<Value> {
    let mut diagnostics: Vec<Value> = self::compiler_diagnostics(uri, text);

    if diagnostics.is_empty() {
        diagnostics = self::lightweight_diagnostics(text);
    }

    diagnostics
}

fn compiler_diagnostics(uri: &str, text: &str) -> Vec<Value> {
    let mut diagnostics: Vec<Value> = Vec::with_capacity(16);
    let path: PathBuf = url::Url::parse(uri)
        .ok()
        .and_then(|uri| uri.to_file_path().ok())
        .unwrap_or_else(|| PathBuf::from(uri));
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
    let mut builtins: BuiltinRegistry = thrustc_builtins::default_registry(target_info);
    let file: CompilationUnit = CompilationUnit::new(name, path, text.to_string(), base_name);

    let tokens = match Lexer::lex_for_lsp(&file, &options) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for error in errors {
                diagnostics.push(self::compilation_issue_to_diagnostic(&error));
            }

            return diagnostics;
        }
    };

    let directives: thrustc_directive::FileDirectives =
        match thrustc_directive::apply_file_directives(&tokens) {
            Ok(directives) => directives,
            Err(error) => {
                diagnostics.push(self::compilation_issue_to_diagnostic(&error));

                return diagnostics;
            }
        };
    let file_options: thrustc_directive::FileOptions =
        thrustc_directive::FileOptions::new(&options, &directives);
    let mut preprocessor: Preprocessor = Preprocessor::new();
    let Ok(modules) = preprocessor.generate_modules(&tokens, &file_options, &file, &builtins)
    else {
        return diagnostics;
    };
    let parser_result = Parser::parse_for_lsp(
        &tokens,
        modules,
        &file,
        &options,
        &file_options,
        &mut builtins,
    );
    let parser_context: thrustc_parser::ParserContext<'_> = parser_result.0;

    for error in parser_context.get_errors() {
        diagnostics.push(self::compilation_issue_to_diagnostic(error));
    }

    for bug in parser_context.get_bugs() {
        diagnostics.push(self::compilation_issue_to_diagnostic(bug));
    }

    for warning in parser_context.get_warnings() {
        diagnostics.push(self::compilation_issue_to_diagnostic(warning));
    }

    diagnostics
}

fn compilation_issue_to_diagnostic(issue: &CompilationIssue) -> Value {
    match issue {
        CompilationIssue::Error(code, message, help, note, span) => {
            let line: u32 = span.get_line().saturating_sub(1);
            let start: u32 = span.get_span_start();
            let end: u32 = span.get_span_end().max(start.saturating_add(1));
            let mut diagnostic_message: String =
                String::with_capacity(message.len() + help.len() + 32);

            diagnostic_message.push_str(message);

            if !help.is_empty() {
                diagnostic_message.push('\n');
                diagnostic_message.push_str(help);
            }

            if let Some(note) = note {
                diagnostic_message.push('\n');
                diagnostic_message.push_str(note);
            }

            serde_json::json!({
                "range": {
                    "start": {
                        "line": line,
                        "character": start
                    },
                    "end": {
                        "line": line,
                        "character": end
                    }
                },
                "severity": 1,
                "code": format!("{:?}", code),
                "source": "thrustc",
                "message": diagnostic_message
            })
        }
        CompilationIssue::Warning(code, message, span) => {
            let line: u32 = span.get_line().saturating_sub(1);
            let start: u32 = span.get_span_start();
            let end: u32 = span.get_span_end().max(start.saturating_add(1));

            serde_json::json!({
                "range": {
                    "start": {
                        "line": line,
                        "character": start
                    },
                    "end": {
                        "line": line,
                        "character": end
                    }
                },
                "severity": 2,
                "code": format!("{:?}", code),
                "source": "thrustc",
                "message": message
            })
        }
        CompilationIssue::FrontendBug(message, help, span, ..)
        | CompilationIssue::BackendBug(message, help, span, ..) => {
            let line: u32 = span.get_line().saturating_sub(1);
            let start: u32 = span.get_span_start();
            let end: u32 = span.get_span_end().max(start.saturating_add(1));
            let mut diagnostic_message: String =
                String::with_capacity(message.len() + help.len() + 1);

            diagnostic_message.push_str(message);

            if !help.is_empty() {
                diagnostic_message.push('\n');
                diagnostic_message.push_str(help);
            }

            serde_json::json!({
                "range": {
                    "start": {
                        "line": line,
                        "character": start
                    },
                    "end": {
                        "line": line,
                        "character": end
                    }
                },
                "severity": 1,
                "source": "thrustc",
                "message": diagnostic_message
            })
        }
    }
}

fn lightweight_diagnostics(text: &str) -> Vec<Value> {
    let mut diagnostics: Vec<Value> = Vec::with_capacity(8);
    let mut stack: Vec<(char, u64, u64)> = Vec::with_capacity(32);
    let mut block_comment_start: Option<(u64, u64)> = None;
    let mut in_string: bool = false;
    let mut in_char: bool = false;
    let mut string_start: (u64, u64) = (0, 0);
    let mut char_start: (u64, u64) = (0, 0);

    for (line_index, line) in text.lines().enumerate() {
        let line_number: u64 = line_index.try_into().unwrap_or(u64::MAX);
        let chars: Vec<char> = line.chars().collect();
        let mut character_index: usize = 0;

        while character_index < chars.len() {
            let ch: char = chars[character_index];
            let next: char = chars
                .get(character_index.saturating_add(1))
                .copied()
                .unwrap_or('\0');
            let character: u64 = character_index.try_into().unwrap_or(u64::MAX);

            if block_comment_start.is_some() {
                if ch == '*' && next == '/' {
                    block_comment_start = None;
                    character_index = character_index.saturating_add(2);

                    continue;
                }

                character_index = character_index.saturating_add(1);

                continue;
            }

            if !in_string && !in_char && ch == '/' && next == '/' {
                break;
            }

            if !in_string && !in_char && ch == '/' && next == '*' {
                block_comment_start = Some((line_number, character));
                character_index = character_index.saturating_add(2);

                continue;
            }

            if !in_char && ch == '"' {
                if in_string {
                    in_string = false;
                } else {
                    in_string = true;
                    string_start = (line_number, character);
                }

                character_index = character_index.saturating_add(1);

                continue;
            }

            if !in_string && ch == '\'' {
                if in_char {
                    in_char = false;
                } else {
                    in_char = true;
                    char_start = (line_number, character);
                }

                character_index = character_index.saturating_add(1);

                continue;
            }

            if in_string || in_char {
                character_index = character_index.saturating_add(1);

                continue;
            }

            match ch {
                '{' | '(' | '[' => stack.push((ch, line_number, character)),
                '}' | ')' | ']' => {
                    let expected: char = match ch {
                        '}' => '{',
                        ')' => '(',
                        ']' => '[',
                        _ => '\0',
                    };

                    if stack.last().is_some_and(|(open, _, _)| *open == expected) {
                        stack.pop();
                    } else {
                        diagnostics.push(serde_json::json!({
                            "range": {
                                "start": {
                                    "line": line_number,
                                    "character": character
                                },
                                "end": {
                                    "line": line_number,
                                    "character": character.saturating_add(1)
                                }
                            },
                            "severity": 1,
                            "source": "thrustc_lsp",
                            "message": "Unmatched closing delimiter."
                        }));
                    }
                }
                _ => {}
            }

            character_index = character_index.saturating_add(1);
        }
    }

    if let Some((line, character)) = block_comment_start {
        diagnostics.push(serde_json::json!({
            "range": {
                "start": {
                    "line": line,
                    "character": character
                },
                "end": {
                    "line": line,
                    "character": character.saturating_add(2)
                }
            },
            "severity": 1,
            "source": "thrustc_lsp",
            "message": "Unterminated block comment."
        }));
    }

    if in_string {
        diagnostics.push(serde_json::json!({
            "range": {
                "start": {
                    "line": string_start.0,
                    "character": string_start.1
                },
                "end": {
                    "line": string_start.0,
                    "character": string_start.1.saturating_add(1)
                }
            },
            "severity": 1,
            "source": "thrustc_lsp",
            "message": "Unterminated string literal."
        }));
    }

    if in_char {
        diagnostics.push(serde_json::json!({
            "range": {
                "start": {
                    "line": char_start.0,
                    "character": char_start.1
                },
                "end": {
                    "line": char_start.0,
                    "character": char_start.1.saturating_add(1)
                }
            },
            "severity": 1,
            "source": "thrustc_lsp",
            "message": "Unterminated character literal."
        }));
    }

    while let Some((_, line, character)) = stack.pop() {
        diagnostics.push(serde_json::json!({
            "range": {
                "start": {
                    "line": line,
                    "character": character
                },
                "end": {
                    "line": line,
                    "character": character.saturating_add(1)
                }
            },
            "severity": 1,
            "source": "thrustc_lsp",
            "message": "Unclosed delimiter."
        }));
    }

    diagnostics
}
