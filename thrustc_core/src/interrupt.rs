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
use thrustc_logging::LoggingType;
use thrustc_options::CompilationUnit;

use crate::ThrustCompiler;

#[inline]
pub fn archive_compilation_module(
    compiler: &mut ThrustCompiler,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<(), ()> {
    compiler.update_thrustc_time(file_time.elapsed());

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "Compilation".custom_color((141, 141, 142)).bold(),
            "FAILED".bright_red().bold(),
            &file.get_path().to_string_lossy()
        ),
    );

    Err(())
}

#[inline]
pub fn archive_compilation_module_jit(
    compiler: &mut ThrustCompiler,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<either::Either<MemoryBuffer, ()>, ()> {
    compiler.update_thrustc_time(file_time.elapsed());

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "Compilation".custom_color((141, 141, 142)).bold(),
            "FAILED".bright_red().bold(),
            &file.get_path().to_string_lossy()
        ),
    );

    Err(())
}

#[inline]
pub fn archive_compilation_unit_with_message(
    compiler: &mut ThrustCompiler,
    log_type: LoggingType,
    msg: &str,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<(), ()> {
    thrustc_logging::print_error(log_type, msg);

    compiler.update_thrustc_time(file_time.elapsed());

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "Compilation".custom_color((141, 141, 142)).bold(),
            "FAILED".bright_red().bold(),
            &file.get_path().to_string_lossy()
        ),
    );

    Err(())
}
