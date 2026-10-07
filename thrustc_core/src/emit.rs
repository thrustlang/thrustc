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
use thrustc_options::{CompilationUnit, EmitableUnit, Emited};

use crate::{emitters, interrupt, ThrustCompiler};

pub fn llvm_after_optimization(
    compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    build_dir: &std::path::Path,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<bool, ()> {
    if compiler_options.contains_emitable(EmitableUnit::LLVMBitcode) {
        let bitcode_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-bitcode");

        if !emitters::llvmbitcode::emit_llvm_bitcode(
            compiler,
            llvm_module,
            build_dir,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Error,
                &format!(
                    "Failed t emit LLVM bitcode for file '{}'.",
                    file.get_path().display()
                ),
            );

            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "LLVM bitcode after optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    bitcode_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    if compiler_options.contains_emitable(EmitableUnit::LLVMIR) {
        let llvmir_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-ir");

        if let Err(error) = emitters::llvmir::emit_llvm_ir(
            compiler,
            llvm_module,
            build_dir,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, &error.to_string());
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "LLVM IR after optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    llvmir_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    if compiler_options.contains_emitable(EmitableUnit::Assembly) {
        let assembler_base_path: std::path::PathBuf = build_dir.join("emit").join("assembler");

        if let Err(error) = emitters::assembler::emit_llvm_assembler(
            compiler,
            llvm_module,
            target_machine,
            build_dir,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, error);
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        };

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "Assembly after optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    assembler_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    if compiler_options.contains_emitable(EmitableUnit::Object) {
        let objects_base_path: std::path::PathBuf = build_dir.join("emit").join("obj");

        if let Err(error) = emitters::objfile::emit_llvm_object(
            compiler,
            llvm_module,
            target_machine,
            build_dir,
            file.get_name(),
            false,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, error);
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "Object file after optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    objects_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    Ok(false)
}

pub fn llvm_before_optimization(
    compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    llvm_module: &Module,
    target_machine: &TargetMachine,
    build_dir: &std::path::Path,
    file: &CompilationUnit,
    file_time: std::time::Instant,
) -> Result<bool, ()> {
    if compiler_options.contains_emitable(EmitableUnit::UnOptLLVMIR) {
        let llvmir_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-ir");

        if let Err(error) = emitters::llvmir::emit_llvm_ir(
            compiler,
            llvm_module,
            build_dir,
            file.get_name(),
            true,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, &error.to_string());
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "LLVM IR before optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    llvmir_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    if compiler_options.contains_emitable(EmitableUnit::UnOptLLVMBitcode) {
        let bitcode_base_path: std::path::PathBuf = build_dir.join("emit").join("llvm-bitcode");

        if !emitters::llvmbitcode::emit_llvm_bitcode(
            compiler,
            llvm_module,
            build_dir,
            file.get_name(),
            true,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Error,
                &format!(
                    "Failed to emit LLVM bitcode for file '{}'.",
                    file.get_path().display()
                ),
            );
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "LLVM bitcode before optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    bitcode_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    if compiler_options.contains_emitable(EmitableUnit::UnOptAssembly) {
        let assembler_base_path: std::path::PathBuf = build_dir.join("emit").join("assembler");

        if let Err(error) = emitters::assembler::emit_llvm_assembler(
            compiler,
            llvm_module,
            target_machine,
            build_dir,
            file.get_name(),
            true,
            compiler_options.obfuscate_archive_names(),
        ) {
            thrustc_logging::print_error(thrustc_logging::LoggingType::Error, error);
            interrupt::archive_compilation_module(compiler, file, file_time)?;
        }

        if !compiler_options.global().quiet() {
            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Debug,
                &format!(
                    "Assembler before optimization emitted for '{}' in '{}'.",
                    file.get_path().display(),
                    assembler_base_path.display()
                ),
            );
        }

        return Ok(true);
    }

    Ok(false)
}

pub fn before_frontend(
    _compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    build_dir: &std::path::Path,
    file: &CompilationUnit,
    emited: Emited,
) -> Result<bool, ()> {
    if compiler_options.contains_emitable(EmitableUnit::TokensPretty) {
        if let Emited::Tokens(tokens) = emited {
            let base_tokens_path: std::path::PathBuf = build_dir.join("emit").join("tokens");

            if emitters::tokens::to_file_pretty(tokens, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit pretty tokens for '{}'.",
                        file.get_path().display()
                    ),
                );
                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "Tokens before the frontend process pipeline emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_tokens_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::Tokens) {
        let base_tokens_path: std::path::PathBuf = build_dir.join("emit").join("tokens");

        if let Emited::Tokens(tokens) = emited {
            if emitters::tokens::to_file(tokens, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!("Failed to emit tokens for '{}'.", file.get_path().display()),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "Tokens before the frontend process pipeline emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_tokens_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::UnCheckedAstPretty) {
        if let Emited::Ast(ast) = emited {
            let base_ast_path: std::path::PathBuf = build_dir.join("emit").join("ast");

            if emitters::ast::to_file_pretty(ast, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit the pretty AST for '{}'.",
                        file.get_path().display()
                    ),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "AST before the frontend process pipeline validation emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_ast_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::UnCheckedAst) {
        if let Emited::Ast(ast) = emited {
            let base_ast_path: std::path::PathBuf = build_dir.join("emit").join("ast");

            if emitters::ast::to_file(ast, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit the AST for '{}'.",
                        file.get_path().display()
                    ),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "AST before the frontend process pipeline validation emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_ast_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    Ok(false)
}

pub fn after_frontend(
    _compiler: &mut ThrustCompiler,
    compiler_options: &FileOptions<'_, '_>,
    build_dir: &std::path::Path,
    file: &CompilationUnit,
    emited: Emited,
) -> Result<bool, ()> {
    if compiler_options.contains_emitable(EmitableUnit::TokensPretty) {
        if let Emited::Tokens(tokens) = emited {
            let base_tokens_path: std::path::PathBuf = build_dir.join("emit").join("tokens");

            if emitters::tokens::to_file_pretty(tokens, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit pretty tokens for '{}'.",
                        file.get_path().display()
                    ),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "Tokens after the frontend process pipeline emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_tokens_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::Tokens) {
        if let Emited::Tokens(tokens) = emited {
            let base_tokens_path: std::path::PathBuf = build_dir.join("emit").join("tokens");

            if emitters::tokens::to_file(tokens, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!("Failed to emit tokens for '{}'.", file.get_path().display()),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "Tokens after the frontend process pipeline emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_tokens_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::AstPretty) {
        if let Emited::Ast(ast) = emited {
            let base_ast_path: std::path::PathBuf = build_dir.join("emit").join("ast");

            if emitters::ast::to_file_pretty(ast, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit the pretty AST for '{}'.",
                        file.get_path().display()
                    ),
                );

                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "AST after the frontend process pipeline validation emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_ast_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    if compiler_options.contains_emitable(EmitableUnit::Ast) {
        if let Emited::Ast(ast) = emited {
            let base_ast_path: std::path::PathBuf = build_dir.join("emit").join("ast");

            if emitters::ast::to_file(ast, build_dir, file.get_name()).is_err() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    &format!(
                        "Failed to emit the AST for '{}'.",
                        file.get_path().display()
                    ),
                );
                return Err(());
            }

            if !compiler_options.global().quiet() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Debug,
                    &format!(
                        "AST after the frontend process pipeline validation emitted for '{}' in '{}'.",
                        file.get_path().display(),
                        base_ast_path.display()
                    ),
                );
            }

            return Ok(true);
        }
    }

    Ok(false)
}
