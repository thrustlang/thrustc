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

use std::fmt::Write as WriteFmt;
use std::fs::File;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use colored::Colorize;
use thrustc_logging::OutputIn;
use thrustc_options::CompilerOptions;
use thrustc_token::Token;

use serde_json;

pub fn print_to_file(
    tokens: &[Token],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base_tokens_path: PathBuf = build_dir.join("emit").join("tokens");

    std::fs::create_dir_all(&base_tokens_path)?;

    let formatted_file_name = format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    );

    let file_path: PathBuf = base_tokens_path.join(formatted_file_name);
    let file: File = std::fs::File::create(file_path)?;

    let writer: io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer_pretty(writer, tokens)?;

    Ok(())
}

pub fn generate_string(tokens: &[Token]) -> Result<String, std::fmt::Error> {
    let mut buffer: String = String::with_capacity(tokens.len());

    tokens
        .iter()
        .try_for_each(|token| writeln!(buffer, "{}", token))?;

    Ok(buffer)
}

pub fn print_to_stdout_fine(
    options: &CompilerOptions,
    tokens: &[Token],
    file_name: &str,
) -> Result<(), std::fmt::Error> {
    let mut tokens_formatted: String = String::with_capacity(tokens.len());

    tokens
        .iter()
        .try_for_each(|token| writeln!(tokens_formatted, "{}", token))?;

    thrustc_logging::write(
        OutputIn::Stdout,
        &format!("\n{}\n\n", file_name.bright_green().bold()),
    );

    thrustc_logging::write(OutputIn::Stdout, &tokens_formatted);
    thrustc_logging::write(OutputIn::Stdout, "\n");

    #[cfg(feature = "utils")]
    {
        if options.need_copy_output_to_clipboard() {
            use clipboard::*;

            let ctx: Result<ClipboardContext, Box<dyn std::error::Error>> =
                ClipboardProvider::new();

            if let Ok(mut ctx) = ctx {
                ctx.set_contents(tokens_formatted.clone())
                    .unwrap_or_else(|_| {
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
