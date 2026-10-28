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

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::FileType;
use inkwell::targets::TargetMachine;
use thrustc_options::CompilerOptions;

use crate::utils;
use crate::ThrustCompiler;

pub fn print_llvm_assembler(
    compiler: &ThrustCompiler,
    target_machine: &TargetMachine,
    llvm_module: &Module,
    file_name: &str,
    unoptimized: bool,
    obfuscate: bool,
) -> Result<(), LLVMString> {
    let _compiler_options: &CompilerOptions = compiler.get_compilation_options();

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let assembler_file_name: String = if obfuscate {
        format!(
            "{}{}_{}.s",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name
        )
    } else {
        format!("{}{}.s", optimization_name_modifier, file_name)
    };

    let memory_buffer: MemoryBuffer =
        target_machine.write_to_memory_buffer(llvm_module, FileType::Assembly)?;

    let assembler_in_bytes: Vec<u8> = memory_buffer.as_slice().to_vec();
    let assembler: String = unsafe { String::from_utf8_unchecked(assembler_in_bytes) };


    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!(
            "{}{}\n\n",
            "ASSEMBLER FILE - ".bold(),
            assembler_file_name.bright_green().bold(),
        ),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, &assembler);
    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "\n");

    Ok(())
}
