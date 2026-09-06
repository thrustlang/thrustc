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

use crate::{ThrustCompiler, utils};

pub fn emit_llvm_object(
    _compiler: &ThrustCompiler,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    build_dir: &std::path::Path,
    file_name: &str,
    unoptimized: bool,
    need_obfuscation: bool,
) -> Result<(), &'static str> {
    let objects_base_path: std::path::PathBuf = build_dir.join("emit").join("obj");

    if !objects_base_path.exists() {
        let _ = std::fs::create_dir_all(&objects_base_path);
    }

    let optimization_name_modifier: &str = if unoptimized { "unopt_" } else { "" };

    let object_file_name: String = if need_obfuscation {
        format!(
            "{}{}_{}.o",
            optimization_name_modifier,
            utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
            file_name,
        )
    } else {
        format!("{}{}.o", optimization_name_modifier, file_name)
    };

    let object_file_path: std::path::PathBuf = objects_base_path.join(object_file_name);

    target_machine
        .write_to_file(llvm_module, FileType::Object, &object_file_path)
        .map_err(|_| "Failed to compile to machine core representation")?;

    Ok(())
}
