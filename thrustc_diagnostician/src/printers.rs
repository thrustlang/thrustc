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

use crate::diagnostic::Diagnostic;
use crate::errors::BackendError;
use crate::errors::Error;
use crate::errors::FrontendError;
use crate::traits::ErrorDisassembler;
use crate::traits::IssueDisassembler;

use thrustc_errors::CompilationPosition;
use thrustc_logging::OutputIn;
use thrustc_logging::{self, LoggingType};

use colored::Colorize;
use std::path::Path;

pub fn print_to_string(diagnostic: &Diagnostic, error: Error<'_>) -> String {
    let title: &str = error.get_title();
    let path: &Path = error.get_path();
    let note: Option<&str> = error.get_note();
    let logging_type: LoggingType = error.get_logging_type();

    let code: &str = diagnostic.code();
    let signaler: &str = diagnostic.signaler();

    let line: u32 = diagnostic.span().get_line();
    let start: u32 = diagnostic.span().get_span_start();

    let mut buffer: String = String::new();

    buffer.push_str(&format!(
        "{}:{}:{}:\n",
        format_args!(
            "{}",
            logging_type
                .text_with_color(path.to_string_lossy().as_ref())
                .underline()
        ),
        logging_type.text_with_color(&line.to_string()),
        logging_type.text_with_color(&start.to_string()),
    ));

    buffer.push_str(&format!("\n{}\n", title));
    buffer.push_str(&format!("\n{}\n{}", code, signaler));

    if let Some(note) = note {
        buffer.push_str(&format!("{} {}\n", "NOTE:".bright_blue().bold(), note));
    }

    buffer
}

pub fn print_compiler_frontend_bug(diagnostic: &Diagnostic, error: FrontendError<'_>) {
    let title: &str = error.get_title();
    let position: CompilationPosition = error.get_position();
    let compiler_line: u32 = error.get_line();
    let path: &Path = error.get_source_path();
    let compiler_source_path: &Path = error.get_compiler_source_path();
    let logging_type: LoggingType = error.get_logging_type();

    let code: &str = diagnostic.code();
    let signaler: &str = diagnostic.signaler();

    let line: u32 = diagnostic.span().get_line();
    let start: u32 = diagnostic.span().get_span_start();

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "{}:{}:{}:\n",
            format_args!(
                "{}",
                logging_type
                    .text_with_color(path.to_string_lossy().as_ref())
                    .underline()
            ),
            logging_type.text_with_color(&line.to_string()),
            logging_type.text_with_color(&start.to_string()),
        ),
    );

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "\n{} {} {} {} {}{}{}\n",
            "FRONTEND BUG".bright_red().bold(),
            title.to_uppercase(),
            "-".bold(),
            position,
            compiler_source_path.display(),
            ":".bold(),
            compiler_line.to_string().red().underline().bold()
        ),
    );

    thrustc_logging::write(OutputIn::Stderr, &format!("\n{}\n{}", code, signaler));

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "Report it in '{}'.\n",
            "https://github.com/thrustlang/thrustc/issues"
                .white()
                .bold()
                .underline()
        ),
    );
}

pub fn print_compiler_backend_bug(diagnostic: &Diagnostic, error: BackendError<'_>) {
    let title: &str = error.get_title();
    let position: CompilationPosition = error.get_position();
    let compiler_line: u32 = error.get_line();
    let path: &Path = error.get_source_path();
    let compiler_source_path: &Path = error.get_compiler_source_path();
    let logging_type: LoggingType = error.get_logging_type();

    let code: &str = diagnostic.code();
    let signaler: &str = diagnostic.signaler();

    let line: u32 = diagnostic.span().get_line();
    let start: u32 = diagnostic.span().get_span_start();

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "{} - {}:{}\n",
            format_args!(
                "{}",
                logging_type
                    .text_with_color(path.to_string_lossy().as_ref())
                    .underline()
            ),
            logging_type.text_with_color(&line.to_string()),
            logging_type.text_with_color(&start.to_string()),
        ),
    );

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "\n{} {} {} {} {}{}{}\n",
            "BACKEND BUG".bright_red().bold(),
            title.to_uppercase(),
            "-".bold(),
            position,
            compiler_source_path.display(),
            ":".bold(),
            compiler_line.to_string().red().underline().bold()
        ),
    );

    thrustc_logging::write(OutputIn::Stderr, &format!("\n{}\n{}", code, signaler));

    thrustc_logging::write(
        OutputIn::Stderr,
        &format!(
            "Report it in '{}'.\n",
            "https://github.com/thrustlang/thrustc/issues"
                .white()
                .bold()
                .underline()
        ),
    );
}
