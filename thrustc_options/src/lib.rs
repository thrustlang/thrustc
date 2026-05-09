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

pub mod linkage;

use crate::linkage::LinkingCompilersConfiguration;
use thrustc_backends::llvm::LLVMBackend;

use thrustc_ast::Ast;
use thrustc_logging::{self, LoggingType};
use thrustc_token::Token;

use std::path::Path;
use std::path::PathBuf;

#[derive(Debug)]
pub struct CompilerOptions {
    compiler_tools_path: PathBuf,

    llvm: bool,
    llvm_backend: LLVMBackend,
    files: Vec<CompilationUnit>,
    build_dir: PathBuf,

    disable_all_warnings: bool,

    emit: Vec<EmitableUnit>,
    printable: Vec<PrintableUnit>,

    enable_ansi_colors: bool,
    omit_default_optimizations: bool,

    export_diagnostics_path: PathBuf,
    export_compiler_error_diagnostics: bool,
    export_compiler_warning_diagnostics: bool,
    compiler_export_diagnostics_clean: bool,

    copy_output_to_clipboard: bool,
    clean_tokens: bool,
    clean_assembler: bool,
    clean_object: bool,
    clean_llvm_ir: bool,
    clean_llvm_bitcode: bool,
    clean_build: bool,
    obfuscate_archive_names: bool,
    obfuscate_ir: bool,

    linking_compilers_config: LinkingCompilersConfiguration,
    build_id: uuid::Uuid,
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    name: String,
    base_name: String,
    path: PathBuf,
    content: String,
}

#[derive(Debug, PartialEq)]
pub enum EmitableUnit {
    UnOptLLVMIR,
    UnOptLLVMBitcode,
    LLVMBitcode,
    LLVMIR,
    Object,
    UnOptAssembly,
    Assembly,
    UnCheckedAstPretty,
    AstPretty,
    Ast,
    UnCheckedAst,
    TokensPretty,
    Tokens,
}

#[derive(Debug, PartialEq)]
pub enum PrintableUnit {
    UnOptLLVMIR,
    LLVMIR,
    UnOptAssembly,
    Assembly,
    TokensPretty,
    Tokens,
    UnCheckedAstPretty,
    AstPretty,
    Ast,
    UnCheckedAst,
}

#[derive(Debug)]
pub enum Emited<'emited> {
    Tokens(&'emited Vec<Token>),
    Ast(&'emited [Ast<'emited>]),
}

impl CompilationUnit {
    #[inline]
    pub fn new(name: String, path: PathBuf, content: String, base_name: String) -> Self {
        Self {
            name,
            path,
            content,
            base_name,
        }
    }
}

impl CompilerOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            compiler_tools_path: PathBuf::new(),

            llvm: true,
            llvm_backend: LLVMBackend::new(),
            files: Vec::with_capacity(u8::MAX as usize),

            emit: Vec::with_capacity(u8::MAX as usize),
            printable: Vec::with_capacity(u8::MAX as usize),

            build_dir: "build".into(),

            disable_all_warnings: false,

            enable_ansi_colors: false,
            omit_default_optimizations: false,

            export_diagnostics_path: "diagnostics".into(),
            export_compiler_error_diagnostics: false,
            export_compiler_warning_diagnostics: false,
            compiler_export_diagnostics_clean: false,

            copy_output_to_clipboard: false,
            clean_tokens: false,
            clean_assembler: false,
            clean_object: false,
            clean_llvm_ir: false,
            clean_llvm_bitcode: false,
            clean_build: false,
            obfuscate_archive_names: true,
            obfuscate_ir: true,

            linking_compilers_config: LinkingCompilersConfiguration::new(),
            build_id: uuid::Uuid::new_v4(),
        }
    }
}

impl CompilerOptions {
    #[inline]
    pub fn add_compilation_unit(
        &mut self,
        name: String,
        path: PathBuf,
        content: String,
        base_name: String,
    ) {
        if self.files.iter().any(|file| file.path == path) {
            thrustc_logging::print_warn(
                LoggingType::Warning,
                &format!("File skipped due to repetition '{}'.", path.display()),
            );
        } else {
            self.files
                .push(CompilationUnit::new(name, path, content, base_name));
        }
    }
}

impl CompilerOptions {
    #[inline]
    pub fn set_use_llvm_backend(&mut self, value: bool) {
        self.llvm = value;
    }

    #[inline]
    pub fn set_build_dir(&mut self, build_dir: PathBuf) {
        self.build_dir = build_dir;
    }

    #[inline]
    pub fn set_disable_all_warnings(&mut self) {
        self.disable_all_warnings = true;
    }

    #[inline]
    pub fn set_clean_tokens(&mut self) {
        self.clean_tokens = true;
    }

    #[inline]
    pub fn set_clean_assembler(&mut self) {
        self.clean_assembler = true;
    }

    #[inline]
    pub fn set_clean_object(&mut self) {
        self.clean_object = true;
    }

    #[inline]
    pub fn set_clean_llvm_ir(&mut self) {
        self.clean_llvm_ir = true;
    }

    #[inline]
    pub fn set_clean_llvm_bitcode(&mut self) {
        self.clean_llvm_bitcode = true;
    }

    #[inline]
    pub fn set_clean_build(&mut self) {
        self.clean_build = true;
    }

    #[inline]
    pub fn set_omit_default_optimizations(&mut self) {
        self.omit_default_optimizations = true;
    }

    #[inline]
    pub fn set_no_obfuscate_archive_names(&mut self) {
        self.obfuscate_archive_names = false;
    }

    #[inline]
    pub fn set_no_obfuscate_ir(&mut self) {
        self.obfuscate_ir = false;
    }

    #[inline]
    pub fn set_enable_ansi_colors(&mut self) {
        self.enable_ansi_colors = true;
    }

    #[inline]
    pub fn set_export_diagnostic_path(&mut self, export_diagnostics_path: PathBuf) {
        self.export_diagnostics_path = export_diagnostics_path;
    }

    #[inline]
    pub fn set_export_compiler_error_diagnostics(&mut self) {
        self.export_compiler_error_diagnostics = true;
    }

    #[inline]
    pub fn set_export_compiler_warning_diagnostics(&mut self) {
        self.export_compiler_warning_diagnostics = true;
    }

    #[inline]
    pub fn set_compiler_exported_diagnostics_clean(&mut self) {
        self.compiler_export_diagnostics_clean = true;
    }

    #[inline]
    pub fn set_copy_output_to_clipboard(&mut self) {
        self.copy_output_to_clipboard = true;
    }

    #[inline]
    pub fn set_compiler_tools_path(&mut self, path: PathBuf) {
        self.compiler_tools_path = path;
    }

    #[inline]
    pub fn add_emit_option(&mut self, emit: EmitableUnit) {
        self.emit.push(emit);
    }

    #[inline]
    pub fn add_print_option(&mut self, printable: PrintableUnit) {
        self.printable.push(printable);
    }
}

impl CompilerOptions {
    #[inline]
    pub fn llvm(&self) -> bool {
        self.llvm
    }

    #[inline]
    pub fn get_units(&self) -> &[CompilationUnit] {
        self.files.as_slice()
    }

    #[inline]
    pub fn get_llvm_backend(&self) -> &LLVMBackend {
        &self.llvm_backend
    }

    #[inline]
    pub fn get_build_dir(&self) -> &PathBuf {
        if !self.build_dir.exists() {
            std::fs::create_dir_all(&self.build_dir).unwrap_or_else(|_| {
                thrustc_logging::print_critical_error(
                    LoggingType::Panic,
                    "The compiler build directory couldn't be created automatically.",
                );
            });
        }

        &self.build_dir
    }

    #[inline]
    pub fn get_clean_tokens(&self) -> bool {
        self.clean_tokens
    }

    #[inline]
    pub fn get_clean_assembler(&self) -> bool {
        self.clean_assembler
    }

    #[inline]
    pub fn get_clean_object(&self) -> bool {
        self.clean_object
    }

    #[inline]
    pub fn get_clean_llvm_ir(&self) -> bool {
        self.clean_llvm_ir
    }

    #[inline]
    pub fn get_clean_llvm_bitcode(&self) -> bool {
        self.clean_llvm_bitcode
    }

    #[inline]
    pub fn get_clean_build(&self) -> bool {
        self.clean_build
    }

    #[inline]
    pub fn get_compiler_tools_path(&self) -> &Path {
        &self.compiler_tools_path
    }

    #[inline]
    pub fn need_obfuscate_archive_names(&self) -> bool {
        self.obfuscate_archive_names
    }

    #[inline]
    pub fn need_obfuscate_ir(&self) -> bool {
        self.obfuscate_ir
    }

    #[inline]
    pub fn need_ansi_colors(&self) -> bool {
        self.enable_ansi_colors
    }

    #[inline]
    pub fn need_copy_output_to_clipboard(&self) -> bool {
        self.copy_output_to_clipboard
    }

    #[inline]
    pub fn get_export_diagnostics_path(&self) -> &Path {
        &self.export_diagnostics_path
    }

    #[inline]
    pub fn get_export_compiler_error_diagnostics(&self) -> bool {
        self.export_compiler_error_diagnostics
    }

    #[inline]
    pub fn get_export_compiler_warning_diagnostics(&self) -> bool {
        self.export_compiler_warning_diagnostics
    }

    #[inline]
    pub fn get_compiler_exported_diagnostics_clean(&self) -> bool {
        self.compiler_export_diagnostics_clean
    }

    #[inline]
    pub fn get_was_emited(&self) -> bool {
        !self.emit.is_empty()
    }

    #[inline]
    pub fn get_was_printed(&self) -> bool {
        !self.printable.is_empty()
    }

    #[inline]
    pub fn omit_default_optimizations(&self) -> bool {
        self.omit_default_optimizations
    }

    #[inline]
    pub fn contains_emitable(&self, emit: EmitableUnit) -> bool {
        self.emit.contains(&emit)
    }

    #[inline]
    pub fn contains_printable(&self, printable: PrintableUnit) -> bool {
        self.printable.contains(&printable)
    }

    #[inline]
    pub fn get_linking_compilers_configuration(&self) -> &LinkingCompilersConfiguration {
        &self.linking_compilers_config
    }

    #[inline]
    pub fn build_id(&self) -> &uuid::Uuid {
        &self.build_id
    }

    #[inline]
    pub fn disable_all_warnings(&self) -> bool {
        self.disable_all_warnings
    }

    #[inline]
    pub fn it_will_print(&self) -> bool {
        !self.printable.is_empty()
    }
}

impl CompilerOptions {
    #[inline]
    pub fn get_mut_llvm_backend(&mut self) -> &mut LLVMBackend {
        &mut self.llvm_backend
    }

    #[inline]
    pub fn get_mut_linking_compilers_configuration(
        &mut self,
    ) -> &mut LinkingCompilersConfiguration {
        &mut self.linking_compilers_config
    }
}

impl CompilationUnit {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_unit_content(&self) -> &str {
        &self.content
    }

    #[inline]
    pub fn get_unit_clone(&self) -> String {
        self.content.clone()
    }

    #[inline]
    pub fn get_path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn get_base_name(&self) -> String {
        self.base_name.clone()
    }
}
