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

#![allow(clippy::result_unit_err)]

use colored::Colorize;

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::FileType;
use inkwell::targets::TargetMachine;

use thrustc_options::CompilationUnit;

use crate::ThrustCompiler;
use crate::utils;

#[inline]
pub fn archive_compilation(
    compiler: &mut ThrustCompiler,
    file_time: std::time::Instant,
    file: &CompilationUnit,
) -> Result<(), ()> {
    compiler.thrustc_time = compiler.thrustc_time.saturating_add(file_time.elapsed());

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!(
            "{} {} {}\n",
            "Compilation".custom_color((141, 141, 142)).bold(),
            "FINISHED".bright_green().bold(),
            file.get_path().to_string_lossy()
        ),
    );

    Ok(())
}

#[inline]
pub fn archive_compilation_module_jit(
    compiler: &mut ThrustCompiler,
    file_time: std::time::Instant,
    file: &CompilationUnit,
) -> Result<either::Either<MemoryBuffer, ()>, ()> {
    compiler.thrustc_time = compiler.thrustc_time.saturating_add(file_time.elapsed());

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!(
            "{} {} {}\n",
            "Compilation".custom_color((141, 141, 142)).bold(),
            "FINISHED".bright_green().bold(),
            file.get_path().to_string_lossy()
        ),
    );

    Ok(either::Either::Right(()))
}

#[inline]
pub fn llvm_obj_compilation(
    llvm_module: &Module,
    target_machine: &TargetMachine,
    build_dir: &std::path::Path,
    file_name: &str,
) -> std::path::PathBuf {
    let path: std::path::PathBuf = build_dir.join("obj");

    if !path.exists() {
        std::fs::create_dir_all(&path).unwrap_or_else(|_| {
            thrustc_logging::print_critical_error(
                thrustc_logging::LoggingType::Error,
                &format!(
                    "Cannot create directory '{}' for object files compilation.",
                    path.display()
                ),
            )
        });
    }

    let obj_file_path: std::path::PathBuf = path.join(format!(
        "{}_{}.o",
        utils::generate_random_string(thrustc_constants::COMPILER_HARD_OBFUSCATION_LEVEL),
        file_name
    ));

    target_machine
        .write_to_file(llvm_module, FileType::Object, &obj_file_path)
        .unwrap_or_else(|error| {
            thrustc_logging::print_backend_panic(
                thrustc_logging::LoggingType::BackendPanic,
                &format!(
                    "'{}' cannot be emited as object file '{}'.",
                    obj_file_path.display(),
                    error
                ),
            );
        });

    obj_file_path
}
