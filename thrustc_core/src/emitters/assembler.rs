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

use inkwell::{
    module::Module,
    targets::{FileType, TargetMachine},
};
use thrustc_backends::llvm::{LLVMBackend, target::LLVMTarget};
use thrustc_options::CompilerOptions;

use crate::{ThrustCompiler, utils};

pub fn emit_llvm_assembler(
    compiler: &ThrustCompiler,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    build_dir: &std::path::Path,
    file_name: &str,
    unoptimized: bool,
    need_obfuscation: bool,
) -> Result<(), &'static str> {
    let compiler_options: &CompilerOptions = compiler.get_compilation_options();
    let llvm_backend: &LLVMBackend = compiler_options.get_llvm_backend();
    let target: &LLVMTarget = llvm_backend.get_target();

    let is_nvidia_target: bool = target.get_normalized_target_triple().is_nvptx_arch();

    let extension: &str = if is_nvidia_target { "ptx" } else { "s" };

    let assembler_base_path: std::path::PathBuf = build_dir.join("emit").join("assembler");

    if !assembler_base_path.exists() {
        let _ = std::fs::create_dir_all(&assembler_base_path);
    }

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let assembler_file_name: String = if need_obfuscation {
        format!(
            "{}{}_{}.{}",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name,
            extension
        )
    } else {
        format!("{}{}.{}", optimization_name_modifier, file_name, extension)
    };

    let assembler_file_path: std::path::PathBuf = assembler_base_path.join(assembler_file_name);

    target_machine
        .write_to_file(llvm_module, FileType::Assembly, &assembler_file_path)
        .map_err(|_| "Failed to compile the readable assembler!")?;

    Ok(())
}
