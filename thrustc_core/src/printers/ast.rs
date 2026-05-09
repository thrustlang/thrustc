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

#![allow(unused)]

use colored::Colorize;
use thrustc_options::CompilerOptions;

use crate::Ast;

pub fn print_to_stdout(
    options: &CompilerOptions,
    ast: &[Ast],
    file_name: &str,
) -> Result<(), std::fmt::Error> {
    let json: String = serde_json::to_string(ast).map_err(|_| std::fmt::Error)?;

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!("\n{}\n\n", file_name.bright_green().bold()),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &json);
    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");

    #[cfg(feature = "extra_utilities")]
    {
        if options.need_copy_output_to_clipboard() {
            use clipboard::*;

            let ctx: Result<ClipboardContext, Box<dyn std::error::Error>> =
                ClipboardProvider::new();

            if let Ok(mut ctx) = ctx {
                ctx.set_contents(json).unwrap_or_else(|_| {
                    thrustc_logging::print_warn(
                        thrustc_logging::LoggingType::Warning,
                        "Unable to copy the tokens stream into system clipboard.",
                    );
                });
            } else {
                thrustc_logging::print_warn(
                    thrustc_logging::LoggingType::Warning,
                    "Failed to initialize clipboard processes.",
                );
            }
        }
    }

    Ok(())
}

pub fn print_to_stdout_pretty(
    options: &CompilerOptions,
    ast: &[Ast],
    file_name: &str,
) -> Result<(), std::fmt::Error> {
    let json: String = serde_json::to_string_pretty(ast).map_err(|_| std::fmt::Error)?;

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!("\n{}\n\n", file_name.bright_green().bold()),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &json);
    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");

    #[cfg(feature = "extra_utilities")]
    {
        if options.need_copy_output_to_clipboard() {
            use clipboard::*;

            let ctx: Result<ClipboardContext, Box<dyn std::error::Error>> =
                ClipboardProvider::new();

            if let Ok(mut ctx) = ctx {
                ctx.set_contents(json).unwrap_or_else(|_| {
                    thrustc_logging::print_warn(
                        thrustc_logging::LoggingType::Warning,
                        "Unable to copy the tokens stream into system clipboard.",
                    );
                });
            } else {
                thrustc_logging::print_warn(
                    thrustc_logging::LoggingType::Warning,
                    "Failed to initialize clipboard processes.",
                );
            }
        }
    }

    Ok(())
}
