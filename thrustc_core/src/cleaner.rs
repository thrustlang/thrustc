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

use thrustc_options::CompilerOptions;

pub fn auto_clean(options: &CompilerOptions) {
    if options.get_clean_build() {
        self::clean_build(options);
    }

    if options.clean_exported_compiler_diagnostics() {
        self::clean_exported_diagnostics(options)
    }

    if options.get_clean_assembler() {
        self::clean_assembler(options);
    }

    if options.get_clean_llvm_ir() {
        self::clean_llvm_ir(options);
    }

    if options.get_clean_llvm_bitcode() {
        self::clean_llvm_bitcode(options);
    }

    if options.get_clean_object() {
        self::clean_objects(options);
    }

    if options.get_clean_tokens() {
        self::clean_tokens(options);
    }
}

#[inline]
fn clean_exported_diagnostics(options: &CompilerOptions) {
    let _ = std::fs::remove_dir_all(options.get_export_diagnostics_path());
    let _ = std::fs::create_dir_all(options.get_export_diagnostics_path());
}

#[inline]
fn clean_build(options: &CompilerOptions) {
    let build_path: &std::path::Path = options.get_build_dir();

    let _ = std::fs::remove_dir_all(build_path);
    let _ = std::fs::create_dir_all(build_path);
}

#[inline]
fn clean_assembler(options: &CompilerOptions) {
    let assembler_path: std::path::PathBuf = options.get_build_dir().join("emit").join("assembler");
    let _ = std::fs::remove_dir_all(&assembler_path);
}

#[inline]
fn clean_llvm_ir(options: &CompilerOptions) {
    let llvm_ir_path: std::path::PathBuf = options.get_build_dir().join("emit").join("llvm-ir");
    let _ = std::fs::remove_dir_all(&llvm_ir_path);
}

#[inline]
fn clean_llvm_bitcode(options: &CompilerOptions) {
    let llvm_bitcode_path: std::path::PathBuf =
        options.get_build_dir().join("emit").join("llvm-bitcode");
    let _ = std::fs::remove_dir_all(&llvm_bitcode_path);
}

#[inline]
fn clean_objects(options: &CompilerOptions) {
    let objects_path: std::path::PathBuf = options.get_build_dir().join("emit").join("obj");
    let _ = std::fs::remove_dir_all(&objects_path);
}

#[inline]
fn clean_tokens(options: &CompilerOptions) {
    let tokens_path: std::path::PathBuf = options.get_build_dir().join("emit").join("tokens");
    let _ = std::fs::remove_dir_all(&tokens_path);
}
