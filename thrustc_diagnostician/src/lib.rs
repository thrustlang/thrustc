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

pub mod config;
pub mod diagnostic;
pub mod errors;
mod impls;
pub mod position;
pub mod printers;
mod traits;

use thrustc_errors::CompilationIssue;
use thrustc_logging::LoggingType;
use thrustc_logging::OutputIn;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;

use crate::config::DiagnosticianConfig;
use crate::diagnostic::Diagnostic;

use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy)]
enum Notificator {
    Error,
    Warning,
    CompilerFrontendBug,
    CompilerBackendBug,
}

#[derive(Debug, Clone, Copy)]
enum DiagnosticType {
    Warning,
    Error,
    FrontendBug,
    BackendBug,
}

#[derive(Debug, Clone, Default)]
pub struct Diagnostician {
    path: PathBuf,
    base_name: String,
    code: String,
    config: DiagnosticianConfig,
}

impl Diagnostician {
    #[inline]
    pub fn new(file: &CompilationUnit, options: &CompilerOptions) -> Self {
        Self {
            path: file.get_path().to_path_buf(),
            base_name: file.get_base_name(),
            code: file.get_unit_clone(),
            config: DiagnosticianConfig::new(
                options.get_export_diagnostics_path().to_path_buf(),
                options.get_export_compiler_error_diagnostics(),
                options.get_export_compiler_warning_diagnostics(),
            ),
        }
    }
}

impl Diagnostician {
    pub fn dispatch_diagnostic(&mut self, error: &CompilationIssue, logging_type: LoggingType) {
        match error {
            CompilationIssue::Error(title, message, help, note, span) => {
                let diagnostic: Diagnostic = diagnostic::build(
                    &self.code,
                    *span,
                    message,
                    help,
                    DiagnosticType::Error,
                    Notificator::Error,
                    logging_type,
                );

                let generated_diagnostic: String = printers::print_to_string(
                    &diagnostic,
                    (
                        &title.to_title(),
                        &self.path,
                        note.as_ref().map(|x| x.as_str()),
                        logging_type,
                    ),
                );

                if self.get_config().export_errors() {
                    let base_path: PathBuf = self.get_config().export_path().join("errors");

                    std::fs::create_dir_all(&base_path).unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "Unable to create errors diagnostics path for export purposes!",
                        );
                    });

                    let full_path: PathBuf =
                        base_path.join(format!("{}.txt", self.get_base_name()));

                    if let Ok(mut file_diag) =
                        OpenOptions::new().create(true).append(true).open(full_path)
                    {
                        let _ = file_diag.write(generated_diagnostic.as_bytes());
                    }
                }

                thrustc_logging::write(OutputIn::Stderr, &generated_diagnostic);
            }

            CompilationIssue::Warning(title, message, span) => {
                let diagnostic: Diagnostic = diagnostic::build(
                    &self.code,
                    *span,
                    message,
                    "",
                    DiagnosticType::Warning,
                    Notificator::Warning,
                    logging_type,
                );

                let generated_diagnostic: String = printers::print_to_string(
                    &diagnostic,
                    (&title.to_title(), &self.path, None, logging_type),
                );

                if self.get_config().export_warnings() {
                    let base_path: PathBuf = self.get_config().export_path().join("warnings");

                    std::fs::create_dir_all(&base_path).unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "Unable to create warnings diagnostics path for export purposes!",
                        );
                    });

                    let full_path: PathBuf =
                        base_path.join(format!("{}.txt", self.get_base_name()));

                    if let Ok(mut file_diag) =
                        OpenOptions::new().create(true).append(true).open(full_path)
                    {
                        let _ = file_diag.write(generated_diagnostic.as_bytes());
                    }
                }

                thrustc_logging::write(OutputIn::Stderr, &generated_diagnostic);
            }

            CompilationIssue::FrontendBug(title, message, span, position, path, line) => {
                let diagnostic: Diagnostic = diagnostic::build(
                    &self.code,
                    *span,
                    message,
                    "",
                    DiagnosticType::FrontendBug,
                    Notificator::CompilerFrontendBug,
                    logging_type,
                );

                printers::print_compiler_frontend_bug(
                    &diagnostic,
                    (title, *position, logging_type, &self.path, path, *line),
                );
            }

            CompilationIssue::BackendBug(title, message, span, position, path, line) => {
                let diagnostic: Diagnostic = diagnostic::build(
                    &self.code,
                    *span,
                    message,
                    "",
                    DiagnosticType::BackendBug,
                    Notificator::CompilerBackendBug,
                    logging_type,
                );

                printers::print_compiler_backend_bug(
                    &diagnostic,
                    (title, *position, logging_type, &self.path, path, *line),
                );
            }
        };
    }
}

impl Diagnostician {
    #[inline]
    pub fn get_file_path(&self) -> PathBuf {
        self.path.clone()
    }

    #[inline]
    pub fn get_config(&self) -> &DiagnosticianConfig {
        &self.config
    }

    #[inline]
    pub fn get_base_name(&self) -> &str {
        &self.base_name
    }
}
