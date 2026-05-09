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

use colored::Colorize;
use thrustc_options::CompilerOptions;

use std::fmt::Write as WriteFmt;
use std::fs::File;
use std::io::Write as WriteIO;
use std::path::Path;

use crate::Ast;

pub fn print_to_file_pretty(
    ast: &[Ast],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base: std::path::PathBuf = build_dir.join("emit").join("ast");
    std::fs::create_dir_all(&base)?;

    let path: std::path::PathBuf = base.join(format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    ));

    let file: File = std::fs::File::create(path)?;
    let mut writer: std::io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer_pretty(&mut writer, ast)?;
    writer.flush()?;

    Ok(())
}

pub fn print_to_stdout_pretty(
    options: &CompilerOptions,
    ast: &[Ast],
    file_name: &str,
) -> Result<(), std::fmt::Error> {
    let mut ast_formatted: String = String::with_capacity(ast.len());

    ast.iter()
        .try_for_each(|ast| writeln!(ast_formatted, "{}", ast))?;

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!("\n{}\n\n", file_name.bright_green().bold()),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &ast_formatted);
    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");

    #[cfg(feature = "utils")]
    {
        if options.need_copy_output_to_clipboard() {
            use clipboard::*;

            let ctx: Result<ClipboardContext, Box<dyn std::error::Error>> =
                ClipboardProvider::new();

            if let Ok(mut ctx) = ctx {
                ctx.set_contents(ast_formatted.clone()).unwrap_or_else(|_| {
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
