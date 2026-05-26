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

use colored::{ColoredString, Colorize};
use std::io::{self, Write};

#[derive(Debug, Clone, Copy)]
pub enum OutputIn {
    Stdout,
    Stderr,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LoggingType {
    LLVMBackend,
    JITCompiler,
    BackendPanic,
    BackendBug,
    FrontEndPanic,
    FronteEndBug,
    Error,
    Warning,
    Panic,
    Bug,
    Debug,
}

#[inline]
pub fn print_debug(ltype: LoggingType, msg: &str) {
    let _ = io::stdout().write_all(format!("{} {}", ltype.as_styled(), msg).as_bytes());
}

#[inline]
pub fn print_warning(ltype: LoggingType, msg: &str) {
    let _ = io::stderr().write_all(format!("{} {}", ltype.as_styled(), msg).as_bytes());
}

#[inline]
pub fn print_error(ltype: LoggingType, msg: &str) {
    let _ = io::stderr().write_all(format!("{} {}\n", ltype.as_styled(), msg).as_bytes());
}

#[inline]
pub fn print_frontend_panic(ltype: LoggingType, msg: &str) -> ! {
    let _ = io::stderr().write_all(format!("\n{} {}", ltype.as_styled(), msg).as_bytes());
    std::process::exit(thrustc_constants::FAILURE_CODE);
}

#[inline]
pub fn print_critical_error(ltype: LoggingType, msg: &str) -> ! {
    let _ = io::stderr().write_all(format!("{} {}\n", ltype.as_styled(), msg).as_bytes());
    std::process::exit(thrustc_constants::FAILURE_CODE);
}

#[inline]
pub fn print_backend_panic_not_exit(ltype: LoggingType, msg: &str) {
    let _ = io::stderr().write_all(format!("\n{} {}", ltype.as_styled(), msg).as_bytes());

    let _ = io::stderr().write_all(
        format!(
            "\n\nMaybe this is a issue... Report it in: '{}'.\n",
            "https://github.com/thrustlang/thrustc/issues/"
                .bold()
                .bright_red()
                .underline()
        )
        .as_bytes(),
    );

    let _ = io::stderr().write_all(
        format!(
            "\n{} It isn't a issue if:\n• Comes from the inline assembler thing.\n\n",
            "IMPORTANT NOTE".bold().underline().bright_red()
        )
        .as_bytes(),
    );
}

#[inline]
pub fn print_backend_panic(ltype: LoggingType, msg: &str) -> ! {
    let _ = io::stderr().write_all(format!("\n{} {}", ltype.as_styled(), msg).as_bytes());

    let _ = io::stderr().write_all(
        format!(
            "\n\nMaybe this is a issue... Report it in: '{}'.\n",
            "https://github.com/thrustlang/thrustc/issues/"
                .bold()
                .bright_red()
                .underline()
        )
        .as_bytes(),
    );

    let _ = io::stderr().write_all(
        format!(
            "\n{} It isn't a issue if:\n• Comes from the inline assembler thing.\n\n",
            "IMPORTANT NOTE".bold().underline().bright_red()
        )
        .as_bytes(),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE);
}

#[inline]
pub fn print_backend_bug(ltype: LoggingType, msg: &str) -> ! {
    let _ = io::stderr().write_all(format!("{} {}\n", ltype.as_styled(), msg).as_bytes());

    let _ = io::stderr().write_all(
        format!(
            "\nThis is a {} at code generation time. Report it in: '{}'.\n",
            "critical issue".bold().bright_red().underline(),
            "https://github.com/thrustlang/thrustc/issues/"
                .bold()
                .bright_red()
                .underline(),
        )
        .as_bytes(),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE);
}

#[inline]
pub fn write(output_in: OutputIn, text: &str) {
    match output_in {
        OutputIn::Stdout => {
            let _ = io::stdout().write_all(text.as_bytes());
        }
        OutputIn::Stderr => {
            let _ = io::stderr().write_all(text.as_bytes());
        }
    };
}

impl LoggingType {
    #[inline]
    pub fn as_styled(&self) -> ColoredString {
        match self {
            LoggingType::LLVMBackend => "LLVM Backend".bright_red().bold().underline(),
            LoggingType::JITCompiler => "JIT Compiler".bright_red().bold().underline(),
            LoggingType::BackendPanic => "BACKEND PANIC".bright_red().bold(),
            LoggingType::BackendBug => "BACKEND BUG".bold().bright_red().underline(),
            LoggingType::FrontEndPanic => "FRONTEND PANIC".bright_red().bold(),
            LoggingType::FronteEndBug => "FRONTEND BUG".bright_red().bold(),
            LoggingType::Error => "ERROR".bright_red().bold(),
            LoggingType::Warning => "WARN".yellow().bold(),
            LoggingType::Panic => "PANIC".bold().bright_red().underline(),
            LoggingType::Bug => "BUG".bold().bright_red().underline(),
            LoggingType::Debug => "DEBUG".custom_color((141, 141, 142)).bold(),
        }
    }

    #[inline]
    pub fn text_with_color(&self, msg: &str) -> ColoredString {
        match self {
            LoggingType::LLVMBackend => msg.bright_red().bold(),
            LoggingType::JITCompiler => msg.bright_red().bold(),
            LoggingType::BackendPanic => msg.bright_red().bold(),
            LoggingType::BackendBug => msg.bold().bright_red().underline(),
            LoggingType::FronteEndBug => msg.bright_red().bold(),
            LoggingType::FrontEndPanic => msg.bright_red().bold(),
            LoggingType::Error => msg.bright_red().bold(),
            LoggingType::Warning => msg.yellow().bold(),
            LoggingType::Panic => msg.bright_red().underline(),
            LoggingType::Bug => msg.bold().bright_red().underline(),
            LoggingType::Debug => msg.custom_color((141, 141, 142)).bold(),
        }
    }
}
