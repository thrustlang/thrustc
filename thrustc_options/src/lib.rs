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
use thrustc_abi::ABIConfiguration;
use thrustc_backends::CompilerFeaturesMode;
use thrustc_backends::llvm::LLVMBackend;

use thrustc_ast::Ast;
use thrustc_errors::CompilationIssueCode;
use thrustc_logging::{self, LoggingType};
use thrustc_token::Token;

use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportCScope {
    MainOnly,
    TransitiveNoSystem,
    TransitiveAll,
}

impl ImportCScope {
    #[inline]
    pub fn to_str(scope: &str) -> Option<Self> {
        match scope {
            "main-only" => Some(Self::MainOnly),
            "transitive-no-system" => Some(Self::TransitiveNoSystem),
            "transitive-all" => Some(Self::TransitiveAll),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportCOptions {
    include_paths: Vec<PathBuf>,
    system_include_paths: Vec<PathBuf>,
    defines: Vec<String>,
    undefs: Vec<String>,
    import_scope: ImportCScope,
    import_scope_overridden: bool,
    target: Option<String>,
    sysroot: Option<PathBuf>,
    std: Option<String>,
    args: Vec<String>,
}

impl ImportCOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            include_paths: Vec::new(),
            system_include_paths: Vec::new(),
            defines: Vec::new(),
            undefs: Vec::new(),
            import_scope: ImportCScope::TransitiveNoSystem,
            import_scope_overridden: false,
            target: None,
            sysroot: None,
            std: None,
            args: Vec::new(),
        }
    }
}

impl ImportCOptions {
    #[inline]
    pub fn include_paths(&self) -> &[PathBuf] {
        self.include_paths.as_slice()
    }

    #[inline]
    pub fn system_include_paths(&self) -> &[PathBuf] {
        self.system_include_paths.as_slice()
    }

    #[inline]
    pub fn defines(&self) -> &[String] {
        self.defines.as_slice()
    }

    #[inline]
    pub fn undefs(&self) -> &[String] {
        self.undefs.as_slice()
    }

    #[inline]
    pub fn import_scope(&self) -> ImportCScope {
        self.import_scope
    }

    #[inline]
    pub fn import_scope_overridden(&self) -> bool {
        self.import_scope_overridden
    }

    #[inline]
    pub fn target(&self) -> Option<&str> {
        self.target.as_deref()
    }

    #[inline]
    pub fn sysroot(&self) -> Option<&Path> {
        self.sysroot.as_deref()
    }

    #[inline]
    pub fn std(&self) -> Option<&str> {
        self.std.as_deref()
    }

    #[inline]
    pub fn args(&self) -> &[String] {
        self.args.as_slice()
    }
}

impl ImportCOptions {
    #[inline]
    pub fn add_include_path(&mut self, path: PathBuf) {
        self.include_paths.push(path);
    }

    #[inline]
    pub fn add_system_include_path(&mut self, path: PathBuf) {
        self.system_include_paths.push(path);
    }

    #[inline]
    pub fn add_define(&mut self, def: String) {
        self.defines.push(def);
    }

    #[inline]
    pub fn add_undef(&mut self, name: String) {
        self.undefs.push(name);
    }

    #[inline]
    pub fn set_import_scope(&mut self, import_scope: ImportCScope) {
        self.import_scope = import_scope;
        self.import_scope_overridden = true;
    }

    #[inline]
    pub fn set_target(&mut self, target: String) {
        self.target = Some(target);
    }

    #[inline]
    pub fn set_sysroot(&mut self, sysroot: PathBuf) {
        self.sysroot = Some(sysroot);
    }

    #[inline]
    pub fn set_std(&mut self, std: String) {
        self.std = Some(std);
    }

    #[inline]
    pub fn add_arg(&mut self, arg: String) {
        self.args.push(arg);
    }
}

impl ImportCOptions {
    #[inline]
    pub fn include_paths_mut(&mut self) -> &mut Vec<PathBuf> {
        &mut self.include_paths
    }

    #[inline]
    pub fn system_include_paths_mut(&mut self) -> &mut Vec<PathBuf> {
        &mut self.system_include_paths
    }

    #[inline]
    pub fn defines_mut(&mut self) -> &mut Vec<String> {
        &mut self.defines
    }

    #[inline]
    pub fn undefs_mut(&mut self) -> &mut Vec<String> {
        &mut self.undefs
    }

    #[inline]
    pub fn args_mut(&mut self) -> &mut Vec<String> {
        &mut self.args
    }
}

#[derive(Debug, Clone)]
pub struct TranslateCOptions {
    include_paths: Vec<PathBuf>,
    system_include_paths: Vec<PathBuf>,
    defines: Vec<String>,
    undefs: Vec<String>,
    target: Option<String>,
    sysroot: Option<PathBuf>,
    std: Option<String>,
    args: Vec<String>,

    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct EmitCBindingsOptions {
    thrust: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
}

impl TranslateCOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            include_paths: Vec::new(),
            system_include_paths: Vec::new(),
            defines: Vec::new(),
            undefs: Vec::new(),
            target: None,
            sysroot: None,
            std: None,
            args: Vec::new(),

            out_dir: None,
            output: None,
        }
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            thrust: None,
            out_dir: None,
            output: None,
        }
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn thrust(&self) -> Option<&Path> {
        self.thrust.as_deref()
    }

    #[inline]
    pub fn out_dir(&self) -> Option<&Path> {
        self.out_dir.as_deref()
    }

    #[inline]
    pub fn output(&self) -> Option<&Path> {
        self.output.as_deref()
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn set_thrust(&mut self, thrust: PathBuf) {
        self.thrust = Some(thrust);
    }

    #[inline]
    pub fn set_out_dir(&mut self, out_dir: PathBuf) {
        self.out_dir = Some(out_dir);
    }

    #[inline]
    pub fn set_output(&mut self, output: PathBuf) {
        self.output = Some(output);
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn thrust_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.thrust
    }

    #[inline]
    pub fn out_dir_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.out_dir
    }

    #[inline]
    pub fn output_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.output
    }
}

impl TranslateCOptions {
    #[inline]
    pub fn include_paths(&self) -> &[PathBuf] {
        self.include_paths.as_slice()
    }

    #[inline]
    pub fn system_include_paths(&self) -> &[PathBuf] {
        self.system_include_paths.as_slice()
    }

    #[inline]
    pub fn defines(&self) -> &[String] {
        self.defines.as_slice()
    }

    #[inline]
    pub fn undefs(&self) -> &[String] {
        self.undefs.as_slice()
    }

    #[inline]
    pub fn target(&self) -> Option<&str> {
        self.target.as_deref()
    }

    #[inline]
    pub fn sysroot(&self) -> Option<&Path> {
        self.sysroot.as_deref()
    }

    #[inline]
    pub fn std(&self) -> Option<&str> {
        self.std.as_deref()
    }

    #[inline]
    pub fn args(&self) -> &[String] {
        self.args.as_slice()
    }

    #[inline]
    pub fn out_dir(&self) -> Option<&Path> {
        self.out_dir.as_deref()
    }

    #[inline]
    pub fn output(&self) -> Option<&Path> {
        self.output.as_deref()
    }
}

impl TranslateCOptions {
    #[inline]
    pub fn add_include_path(&mut self, path: PathBuf) {
        self.include_paths.push(path);
    }

    #[inline]
    pub fn add_system_include_path(&mut self, path: PathBuf) {
        self.system_include_paths.push(path);
    }

    #[inline]
    pub fn add_define(&mut self, def: String) {
        self.defines.push(def);
    }

    #[inline]
    pub fn add_undef(&mut self, name: String) {
        self.undefs.push(name);
    }

    #[inline]
    pub fn set_target(&mut self, target: String) {
        self.target = Some(target);
    }

    #[inline]
    pub fn set_sysroot(&mut self, sysroot: PathBuf) {
        self.sysroot = Some(sysroot);
    }

    #[inline]
    pub fn set_std(&mut self, std: String) {
        self.std = Some(std);
    }

    #[inline]
    pub fn add_arg(&mut self, arg: String) {
        self.args.push(arg);
    }

    #[inline]
    pub fn set_out_dir(&mut self, out_dir: PathBuf) {
        self.out_dir = Some(out_dir);
    }

    #[inline]
    pub fn set_output(&mut self, output: PathBuf) {
        self.output = Some(output);
    }
}

impl TranslateCOptions {
    #[inline]
    pub fn include_paths_mut(&mut self) -> &mut Vec<PathBuf> {
        &mut self.include_paths
    }

    #[inline]
    pub fn system_include_paths_mut(&mut self) -> &mut Vec<PathBuf> {
        &mut self.system_include_paths
    }

    #[inline]
    pub fn defines_mut(&mut self) -> &mut Vec<String> {
        &mut self.defines
    }

    #[inline]
    pub fn undefs_mut(&mut self) -> &mut Vec<String> {
        &mut self.undefs
    }

    #[inline]
    pub fn args_mut(&mut self) -> &mut Vec<String> {
        &mut self.args
    }

    #[inline]
    pub fn out_dir_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.out_dir
    }

    #[inline]
    pub fn output_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.output
    }
}

#[derive(Debug)]
pub struct CompilerOptions {
    compiler_tools_path: PathBuf,

    llvm: bool,
    llvm_backend: LLVMBackend,
    files: Vec<CompilationUnit>,
    build_dir: PathBuf,

    abi_configuration: ABIConfiguration,
    disable_all_warnings: bool,
    quiet: bool,

    stop_compilation_at: CompilationPhase,

    emit: Vec<EmitableUnit>,
    printable: Vec<PrintableUnit>,

    enable_ansi_colors: bool,
    omit_default_optimizations: bool,

    compiler_features: CompilerFeaturesMode,

    warnings_to_disable: Vec<CompilationIssueCode>,
    export_diagnostics_path: PathBuf,
    export_compiler_error_diagnostics: bool,
    export_compiler_warning_diagnostics: bool,
    clean_exported_compiler_diagnostics: bool,

    copy_output_to_clipboard: bool,
    clean_tokens: bool,
    clean_assembler: bool,
    clean_object: bool,
    clean_llvm_ir: bool,
    clean_llvm_bitcode: bool,
    clean_build: bool,
    obfuscate_archive_names: bool,
    obfuscate_ir: bool,

    std_root_path: Option<std::path::PathBuf>,
    std_version: Option<String>,

    linking_compilers_config: LinkingCompilersConfiguration,
    build_id: uuid::Uuid,

    import_c: ImportCOptions,
    emit_c_bindings: EmitCBindingsOptions,
    translate_c: TranslateCOptions,
    translate_c_to_thrust: Vec<PathBuf>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilationPhase {
    None,

    Lexer,
    Parser,
    Scoper,
    AstVerifier,
    TypeChecker,
    GeneralAnalyzer,
    AttributeChecker,
    Linter,

    LLVMIntrinsicChecker,
    LLVMCallConventionChecker,
    LLVMCodegen,
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

            abi_configuration: ABIConfiguration::new(false, thrustc_abi::SpecificABI::None),
            disable_all_warnings: false,
            quiet: false,
            stop_compilation_at: CompilationPhase::None,

            enable_ansi_colors: false,
            omit_default_optimizations: false,

            compiler_features: CompilerFeaturesMode::Stable,

            warnings_to_disable: Vec::with_capacity(u8::MAX as usize),
            export_diagnostics_path: "diagnostics".into(),
            export_compiler_error_diagnostics: false,
            export_compiler_warning_diagnostics: false,
            clean_exported_compiler_diagnostics: false,

            copy_output_to_clipboard: false,
            clean_tokens: false,
            clean_assembler: false,
            clean_object: false,
            clean_llvm_ir: false,
            clean_llvm_bitcode: false,
            clean_build: false,
            obfuscate_archive_names: true,
            obfuscate_ir: true,

            std_root_path: None,
            std_version: None,

            linking_compilers_config: LinkingCompilersConfiguration::new(),
            build_id: uuid::Uuid::new_v4(),

            import_c: ImportCOptions::new(),
            emit_c_bindings: EmitCBindingsOptions::new(),
            translate_c: TranslateCOptions::new(),
            translate_c_to_thrust: Vec::new(),
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
            thrustc_logging::print_warning(
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
    pub fn set_quiet(&mut self) {
        self.quiet = true;
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
    pub fn set_clean_exported_compiler_diagnostics(&mut self) {
        self.clean_exported_compiler_diagnostics = true;
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
    pub fn set_stop_compilation_at(&mut self, phase: CompilationPhase) {
        self.stop_compilation_at = phase;
    }

    #[inline]
    pub fn set_disable_abi_detection(&mut self, value: bool) {
        self.abi_configuration.set_disable(value);
    }

    #[inline]
    pub fn set_utilize_specific_abi(&mut self, specific: thrustc_abi::SpecificABI) {
        self.abi_configuration.set_specific(specific);
    }

    #[inline]
    pub fn set_warnings_to_disable(&mut self, warnings: Vec<CompilationIssueCode>) {
        self.warnings_to_disable = warnings;
    }

    #[inline]
    pub fn add_emit_option(&mut self, emit: EmitableUnit) {
        self.emit.push(emit);
    }

    #[inline]
    pub fn add_print_option(&mut self, printable: PrintableUnit) {
        self.printable.push(printable);
    }

    #[inline]
    pub fn set_compiler_feature_mode(&mut self, mode: CompilerFeaturesMode) {
        self.compiler_features = mode;
        thrustc_backends::set_compiler_features(mode);
    }

    #[inline]
    pub fn set_std_root_path(&mut self, std_root_path: std::path::PathBuf) {
        self.std_root_path = Some(std_root_path);
    }

    #[inline]
    pub fn set_std_version(&mut self, std_version: String) {
        self.std_version = Some(std_version);
    }

    #[inline]
    pub fn get_import_c_options(&self) -> &ImportCOptions {
        &self.import_c
    }

    #[inline]
    pub fn get_translate_c_options(&self) -> &TranslateCOptions {
        &self.translate_c
    }

    #[inline]
    pub fn get_emit_c_bindings_options(&self) -> &EmitCBindingsOptions {
        &self.emit_c_bindings
    }

    #[inline]
    pub fn get_translate_c_to_thrust_inputs(&self) -> &[PathBuf] {
        self.translate_c_to_thrust.as_slice()
    }
}

impl CompilerOptions {
    #[inline]
    pub fn llvm(&self) -> bool {
        self.llvm
    }

    #[inline]
    pub fn abi_configuration(&self) -> &ABIConfiguration {
        &self.abi_configuration
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
    pub fn clean_exported_compiler_diagnostics(&self) -> bool {
        self.clean_exported_compiler_diagnostics
    }

    #[inline]
    pub fn was_emited(&self) -> bool {
        !self.emit.is_empty()
    }

    #[inline]
    pub fn was_printed(&self) -> bool {
        !self.printable.is_empty()
    }

    #[inline]
    pub fn get_warnings_to_disable(&self) -> &[CompilationIssueCode] {
        self.warnings_to_disable.as_slice()
    }

    #[inline]
    pub fn stop_compilation_at(&self, phase: CompilationPhase) -> bool {
        self.stop_compilation_at == phase
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
    pub fn get_compiler_features(&self) -> CompilerFeaturesMode {
        self.compiler_features
    }

    #[inline]
    pub fn get_std_root_path(&self) -> Option<&Path> {
        self.std_root_path.as_deref()
    }

    #[inline]
    pub fn get_std_version(&self) -> Option<&str> {
        self.std_version.as_deref()
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
    pub fn quiet(&self) -> bool {
        self.quiet
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

    #[inline]
    pub fn get_mut_import_c_options(&mut self) -> &mut ImportCOptions {
        &mut self.import_c
    }

    #[inline]
    pub fn get_mut_translate_c_options(&mut self) -> &mut TranslateCOptions {
        &mut self.translate_c
    }

    #[inline]
    pub fn get_mut_emit_c_bindings_options(&mut self) -> &mut EmitCBindingsOptions {
        &mut self.emit_c_bindings
    }

    #[inline]
    pub fn add_translate_c_to_thrust(&mut self, path: PathBuf) {
        self.translate_c_to_thrust.push(path);
    }

    #[inline]
    pub fn get_mut_translate_c_to_thrust(&mut self) -> &mut Vec<PathBuf> {
        &mut self.translate_c_to_thrust
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
