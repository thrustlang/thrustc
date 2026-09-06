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

use inkwell::module::Module;

use crate::{ThrustCompiler, utils};

pub fn emit_llvm_bitcode(
    _compiler: &ThrustCompiler,
    llvm_module: &Module,
    build_dir: &std::path::Path,
    file_name: &str,
    unoptimized: bool,
    need_obfuscation: bool,
) -> bool {
    let bitcode_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-bitcode");

    if !bitcode_base_path.exists() {
        let _ = std::fs::create_dir_all(&bitcode_base_path);
    }

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let bitcode_file_name: String = if need_obfuscation {
        format!(
            "{}{}_{}.bc",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name
        )
    } else {
        format!("{}{}.bc", optimization_name_modifier, file_name)
    };

    let bitcode_file_path: std::path::PathBuf = bitcode_base_path.join(bitcode_file_name);

    llvm_module.write_bitcode_to_path(&bitcode_file_path)
}
