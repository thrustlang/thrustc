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

use inkwell::{module::Module, support::LLVMString};

use crate::{ThrustCompiler, utils};

pub fn emit_llvm_ir(
    _compiler: &ThrustCompiler,
    llvm_module: &Module,
    build_dir: &std::path::Path,
    file_name: &str,
    unoptimized: bool,
    need_obfuscation: bool,
) -> Result<(), LLVMString> {
    let llvmir_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-ir");

    if !llvmir_base_path.exists() {
        let _ = std::fs::create_dir_all(&llvmir_base_path);
    }

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let llvmir_file_name: String = if need_obfuscation {
        format!(
            "{}{}_{}.ll",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name
        )
    } else {
        format!("{}{}.ll", optimization_name_modifier, file_name)
    };

    let llvmir_file_path: std::path::PathBuf = llvmir_base_path.join(llvmir_file_name);

    llvm_module.print_to_file(&llvmir_file_path)?;

    Ok(())
}
