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

use inkwell::module::Module;
use inkwell::targets::TargetMachine;

use thrustc_directive::FileOptions;
use thrustc_options::{CompilationUnit, Emited, PrintableUnit};

use crate::{ThrustCompiler, interrupt, printers};

#[inline]
pub fn llvm_before_optimization(
    compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<bool, ()> {
    if compiler_options.contains_printable(PrintableUnit::UnOptLLVMIR) {
        printers::llvmir::print_llvm_ir(
            compiler,
            llvm_module,
            file.get_name(),
            true,
            compiler_options.obfuscate_archive_names(),
        );
        return Ok(true);
    }

    if compiler_options.contains_printable(PrintableUnit::UnOptAssembly) {
        if let Err(error) = printers::assembler::print_llvm_assembler(
            compiler,
            target_machine,
            llvm_module,
            file.get_name(),
            true,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, &error.to_string());
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        return Ok(true);
    }

    Ok(false)
}

#[inline]
pub fn llvm_after_optimization(
    compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<bool, ()> {
    if compiler_options.contains_printable(PrintableUnit::LLVMIR) {
        printers::llvmir::print_llvm_ir(
            compiler,
            llvm_module,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        );
        return Ok(true);
    }

    if compiler_options.contains_printable(PrintableUnit::Assembly) {
        if let Err(error) = printers::assembler::print_llvm_assembler(
            compiler,
            target_machine,
            llvm_module,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, &error.to_string());
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        return Ok(true);
    }

    Ok(false)
}

#[inline]
pub fn before_frontend(
    _compiler: &mut ThrustCompiler,
    options: &FileOptions<'_, '_>,
    file: &CompilationUnit,
    emited: Emited,
) -> bool {
    if options.contains_printable(PrintableUnit::TokensPretty) {
        if let Emited::Tokens(tokens) = emited {
            if printers::tokens::print_to_stdout_pretty(options.global(), tokens, file.get_name())
                .is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::Tokens) {
        if let Emited::Tokens(tokens) = emited {
            if printers::tokens::print_to_stdout(options.global(), tokens, file.get_name()).is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::UnCheckedAstPretty) {
        if let Emited::Ast(ast) = emited {
            if printers::ast::print_to_stdout_pretty(options.global(), ast, file.get_name())
                .is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::UnCheckedAst) {
        if let Emited::Ast(ast) = emited {
            if printers::ast::print_to_stdout(options.global(), ast, file.get_name()).is_err() {
                return false;
            }

            return true;
        }
    }

    false
}

#[inline]
pub fn after_frontend(
    _compiler: &mut ThrustCompiler,
    options: &FileOptions<'_, '_>,
    file: &CompilationUnit,
    emited: Emited,
) -> bool {
    if options.contains_printable(PrintableUnit::TokensPretty) {
        if let Emited::Tokens(tokens) = emited {
            if printers::tokens::print_to_stdout_pretty(options.global(), tokens, file.get_name())
                .is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::Tokens) {
        if let Emited::Tokens(tokens) = emited {
            if printers::tokens::print_to_stdout(options.global(), tokens, file.get_name()).is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::AstPretty) {
        if let Emited::Ast(ast) = emited {
            if printers::ast::print_to_stdout_pretty(options.global(), ast, file.get_name())
                .is_err()
            {
                return false;
            }

            return true;
        }
    }

    if options.contains_printable(PrintableUnit::Ast) {
        if let Emited::Ast(ast) = emited {
            if printers::ast::print_to_stdout(options.global(), ast, file.get_name()).is_err() {
                return false;
            }

            return true;
        }
    }

    false
}
