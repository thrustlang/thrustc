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

use inkwell::module::Module;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxDefinition;
use syntect::parsing::SyntaxSetBuilder;
use syntect::util::LinesWithEndings;
use thrustc_options::CompilerOptions;

use crate::ThrustCompiler;
use crate::utils;

pub fn print_llvm_ir(
    compiler: &ThrustCompiler,
    llvm_module: &Module,
    file_name: &str,
    unoptimized: bool,
) {
    let compiler_options: &CompilerOptions = compiler.get_compilation_options();
    let obfuscate: bool = compiler_options.need_obfuscate_archive_names();

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let ir_file_name: String = if obfuscate {
        format!(
            "{}{}_{}.ll",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name
        )
    } else {
        format!("{}{}.ll", optimization_name_modifier, file_name)
    };

    let module_print: String = llvm_module.print_to_string().to_string();

    if !compiler_options.need_ansi_colors() {
        thrustc_logging::write(
            thrustc_logging::OutputIn::Stdout,
            &format!(
                "{}{}\n\n",
                "LLVM IR FILE - ".bold(),
                ir_file_name.bright_green().bold(),
            ),
        );

        thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &module_print);
        thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");
    } else {
        let mut builder: SyntaxSetBuilder = SyntaxSetBuilder::new();

        builder.add_plain_text_syntax();

        let syntax_str: &str = std::str::from_utf8(thrustc_constants::LLVM_SYNTAX_HIGHLIGHTING)
            .expect("llvm.sublime-syntax is not valid UTF-8");
        let syntax: syntect::parsing::SyntaxDefinition =
            SyntaxDefinition::load_from_str(syntax_str, true, None)
                .expect("failed to parse llvm.sublime-syntax");

        builder.add(syntax);

        let syntax_set: syntect::parsing::SyntaxSet = builder.build();

        let theme: syntect::highlighting::Theme = ThemeSet::load_from_reader(
            &mut std::io::Cursor::new(thrustc_constants::ONE_DARK_THEME),
        )
        .expect("failed to load One Dark theme");

        let syntax_ref = syntax_set
            .find_syntax_by_name("LLVM IR")
            .expect("LLVM IR syntax not found");

        let mut highlighter: HighlightLines = HighlightLines::new(syntax_ref, &theme);
        let mut colored_ir: String = String::with_capacity(module_print.len() * 2);

        for line in LinesWithEndings::from(&module_print) {
            let ranges = highlighter
                .highlight_line(line, &syntax_set)
                .expect("highlight_line failed");
            colored_ir.push_str(&syntect::util::as_24_bit_terminal_escaped(&ranges, false));
        }

        colored_ir.push_str("\x1b[0m");

        thrustc_logging::write(
            thrustc_logging::OutputIn::Stdout,
            &format!(
                "{}{}\n\n",
                "LLVM IR FILE - ".bold(),
                ir_file_name.bright_green().bold(),
            ),
        );

        thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &colored_ir);
        thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");
    }

    #[cfg(feature = "extra_utilities")]
    {
        if compiler_options.need_copy_output_to_clipboard() {
            use clipboard::*;

            let ctx: Result<ClipboardContext, Box<dyn std::error::Error>> =
                ClipboardProvider::new();

            if let Ok(mut ctx) = ctx {
                ctx.set_contents(module_print.clone()).unwrap_or_else(|_| {
                    thrustc_logging::print_warn(
                        thrustc_logging::LoggingType::Warning,
                        "Unable to copy the IR code into system clipboard.",
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
}
