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
        match thrustc_directive::apply_file_directives(&tokens, options.get_compiler_features()) {
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
