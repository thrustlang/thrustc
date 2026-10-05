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

use std::io::Write;
use std::path::PathBuf;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_logging::{LoggingType, OutputIn};
use thrustc_options::TranslateCOptions;

use crate::options::TranslateCOutput;

pub fn handle_emit_c_bindings_thrust(
    header: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
    import_opts: &thrustc_options::ImportCOptions,
) {
    let Some(header) = header else {
        return;
    };

    let mut emit_opts: crate::options::EmitCBindingsOptions =
        crate::options::EmitCBindingsOptions::new();

    if let Some(out_dir) = out_dir {
        emit_opts.set_out_dir(out_dir);
    }

    if let Some(output) = output {
        emit_opts.set_output(output);
    }

    match crate::manager::emit_c_bindings_thrust(header, import_opts, &emit_opts) {
        Ok((out_path, contents, warnings)) => {
            if let Some(parent) = out_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            let mut open_options: std::fs::OpenOptions = std::fs::File::options();

            open_options.create(true);
            open_options.truncate(true);
            open_options.write(true);

            if let Ok(mut file) = open_options.open(&out_path) {
                let _ = file.write_all(contents.as_bytes());
            } else {
                thrustc_logging::print_critical_error(
                    LoggingType::Error,
                    &format!("Unable to write output '{}'.", out_path.display()),
                );
            }

            for warning in warnings {
                if let CompilationIssue::Warning(code, message, ..) = warning {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        &format!("{}: {}", code.to_title(), message),
                    );
                }
            }

            thrustc_logging::write(
                OutputIn::Stdout,
                &format!("Emitted C bindings to '{}'.\n", out_path.display()),
            );

            std::process::exit(thrustc_constants::SUCCESFUL_CODE);
        }
        Err(message) => {
            thrustc_logging::print_critical_error(LoggingType::Error, &message);
        }
    }
}

pub fn handle_translate_c_to_thrust(inputs: &[PathBuf], translate_opts: &TranslateCOptions) {
    if inputs.is_empty() {
        return;
    }

    match self::translate_c_to_thrust(inputs, translate_opts) {
        Ok(results) => {
            for (out_path, contents, issues) in results {
                if let Some(parent) = out_path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }

                let mut open_options: std::fs::OpenOptions = std::fs::File::options();

                open_options.create(true);
                open_options.truncate(true);
                open_options.write(true);

                if let Ok(mut file) = open_options.open(&out_path) {
                    let _ = file.write_all(contents.as_bytes());
                } else {
                    thrustc_logging::print_critical_error(
                        LoggingType::Error,
                        &format!("Unable to write output '{}'.", out_path.display()),
                    );
                }

                for issue in issues {
                    match issue {
                        CompilationIssue::Warning(code, message, ..) => {
                            thrustc_logging::print_warning(
                                LoggingType::Warning,
                                &format!("{}: {}\n", code.to_title(), message),
                            );
                        }
                        CompilationIssue::Error(code, message, help, note, ..) => {
                            let mut full: String =
                                format!("{}: {}\nhelp: {}", code.to_title(), message, help);

                            if let Some(note) = note {
                                full.push_str("\nnote: ");
                                full.push_str(&note);
                            }

                            thrustc_logging::print_error(LoggingType::Error, &full);
                        }
                        _ => {}
                    }
                }
            }

            thrustc_logging::write(OutputIn::Stdout, "C successfully translated to Thrust.\n");

            std::process::exit(thrustc_constants::SUCCESFUL_CODE);
        }

        Err(issues) => {
            for issue in issues {
                match issue {
                    CompilationIssue::Warning(code, message, ..) => {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            &format!("{}: {}\n", code.to_title(), message),
                        );
                    }
                    CompilationIssue::Error(code, message, help, note, ..) => {
                        let mut full: String =
                            format!("{}: {}\nhelp: {}", code.to_title(), message, help);

                        if let Some(note) = note {
                            full.push_str("\nnote: ");
                            full.push_str(&note);
                        }

                        thrustc_logging::print_error(LoggingType::Error, &full);
                    }
                    _ => {}
                }
            }

            std::process::exit(thrustc_constants::FAILURE_CODE);
        }
    }
}

pub fn translate_c_to_thrust(
    inputs: &[PathBuf],
    translate_opts: &TranslateCOptions,
) -> Result<Vec<TranslateCOutput>, Vec<CompilationIssue>> {
    let mut outputs: Vec<TranslateCOutput> = Vec::new();

    let mut errors: Vec<CompilationIssue> = Vec::new();

    if inputs.is_empty() {
        errors.push(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "No input C files were provided.".into(),
            "Pass at least one '.c' file.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        return Err(errors);
    }

    if inputs.len() > 1 && translate_opts.output().is_some() {
        errors.push(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "'--translate-c-output' only supports a single input file.".into(),
            "Use '--translate-c-out-dir' or omit '--translate-c-output' when translating multiple inputs.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        return Err(errors);
    }

    for input in inputs {
        match crate::manager::translate_single_c_to_thrust(input, translate_opts) {
            Ok(result) => outputs.push(result),
            Err(mut issues) => errors.append(&mut issues),
        }
    }

    if errors.is_empty() {
        Ok(outputs)
    } else {
        Err(errors)
    }
}
