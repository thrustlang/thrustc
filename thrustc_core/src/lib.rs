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

pub mod cleaner;
pub mod emit;
pub mod emitters;
pub mod finisher;
pub mod interrupt;
pub mod linkage;
pub mod print;
pub mod printers;
pub mod starter;
pub mod utils;
pub mod validate;

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::InitializationConfig;
use inkwell::targets::Target;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::targets::TargetTriple;

use thrustc_ast::Ast;
use thrustc_backends::ThrustOptimization;
use thrustc_backends::llvm::LLVMBackend;
use thrustc_backends::llvm::jit;
use thrustc_backends::llvm::jit::JITConfiguration;
use thrustc_backends::llvm::target::LLVMTarget;
use thrustc_diagnostician::Diagnostician;
use thrustc_lexer::Lexer;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_call_conventions_checker::LLVMCallConventionsChecker;
use thrustc_llvm_codegen::context::LLVMCodeGenContext;
use thrustc_llvm_codegen::jit::LLVMJITCompiler;
use thrustc_llvm_codegen::optimizer::LLVMOptimizationConfig;
use thrustc_llvm_codegen::optimizer::LLVMOptimizerFlags;
use thrustc_llvm_codegen::optimizer::LLVMOptimizerPasses;
use thrustc_llvm_compiler_intrinsic_checker::LLVMIntrinsicChecker;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::CompilationPhase;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;
use thrustc_options::Emited;
use thrustc_options::linkage::LinkingCompilersConfiguration;
use thrustc_parser::Parser;
use thrustc_parser::ParserContext;
use thrustc_preprocessor::Preprocessor;
use thrustc_semantic_analysis::SemanticAnalysis;
use thrustc_typesystem::type_layout::TargetInfo;

#[derive(Debug)]
pub struct ThrustCompiler<'thrustc> {
    ready: Vec<std::path::PathBuf>,
    unready: &'thrustc [CompilationUnit],

    options: &'thrustc CompilerOptions,

    linking_time: std::time::Duration,
    thrustc_frontend_time: std::time::Duration,
    thrustc_backend_time: std::time::Duration,
    thrustc_time: std::time::Duration,
}

pub type CompileTime = (
    bool,
    std::time::Duration,
    std::time::Duration,
    std::time::Duration,
    std::time::Duration,
);

impl<'thrustc> ThrustCompiler<'thrustc> {
    pub fn new(units: &'thrustc [CompilationUnit], options: &'thrustc CompilerOptions) -> Self {
        Self {
            ready: Vec::with_capacity(units.len()),
            unready: units,

            options,

            linking_time: std::time::Duration::default(),
            thrustc_frontend_time: std::time::Duration::default(),
            thrustc_backend_time: std::time::Duration::default(),
            thrustc_time: std::time::Duration::default(),
        }
    }
}

impl ThrustCompiler<'_> {
    pub fn compile(&mut self) -> CompileTime {
        if self.unready.is_empty() {
            thrustc_logging::print_critical_error(
                thrustc_logging::LoggingType::Error,
                "No input files",
            );
        }

        if self.get_compilation_options().llvm() {
            Target::initialize_all(&InitializationConfig::default());

            if self
                .get_compilation_options()
                .get_llvm_backend()
                .is_execution_in_jit()
            {
                return self.compile_jit_llvm();
            } else {
                return self.compile_aot_llvm();
            }
        } else {
            thrustc_logging::print_warning(
                thrustc_logging::LoggingType::Warning,
                "Unrecognizable code generator selection. You should select either LLVM or GCC.",
            );
        }

        (
            false,
            self.thrustc_time,
            self.thrustc_frontend_time,
            self.thrustc_backend_time,
            self.linking_time,
        )
    }
}

impl<'thrustc> ThrustCompiler<'thrustc> {
    fn compile_aot_llvm(&mut self) -> CompileTime {
        cleaner::auto_clean(self.get_compilation_options());

        self.discover_std_usage();

        if self.compile_imported_std().is_err() {
            return (
                true,
                self.thrustc_time,
                self.thrustc_frontend_time,
                self.thrustc_backend_time,
                self.linking_time,
            );
        }

        let mut it_failed: bool = false;

        for file in self.unready.iter() {
            it_failed = self.compile_file_with_llvm_aot(file).is_err();
        }

        it_failed = it_failed
            || self.get_compilation_options().was_printed()
            || self.get_compilation_options().was_emited()
            || self.get_compiled_files().is_empty();

        if it_failed {
            return (
                it_failed,
                self.thrustc_time,
                self.thrustc_frontend_time,
                self.thrustc_backend_time,
                self.linking_time,
            );
        }

        starter::linking_phase(self.get_compiled_files());

        let linking_compiler_config: &LinkingCompilersConfiguration =
            self.options.get_linking_compilers_configuration();

        if linking_compiler_config.get_use_clang() {
            linkage::link_with_clang(self);
        } else if linking_compiler_config.get_use_gcc() {
            linkage::link_with_gcc(self);
        } else {
            thrustc_logging::print_warning(
                thrustc_logging::LoggingType::Warning,
                "Unknown linking method, ignoring linking.",
            );
        }

        (
            it_failed,
            self.thrustc_time,
            self.thrustc_frontend_time,
            self.thrustc_backend_time,
            self.linking_time,
        )
    }

    fn discover_std_usage(&mut self) {
        for file in self.unready.iter() {
            let Ok(tokens) = Lexer::lex(file, self.options) else {
                continue;
            };

            let mut preprocessor: Preprocessor = Preprocessor::new();

            let _ = preprocessor.generate_modules(&tokens, self.options, file);
        }
    }

    fn compile_imported_std(&mut self) -> Result<(), ()> {
        if !thrustc_preprocessor::std_library::has_imported_std() {
            return Ok(());
        }

        let mut compiled_paths: std::collections::HashSet<std::path::PathBuf> =
            std::collections::HashSet::new();

        for module in thrustc_preprocessor::std_library::get_imported_std_modules() {
            let path: std::path::PathBuf = module.get_path().to_path_buf();

            if !path.is_file() {
                continue;
            }

            if !compiled_paths.insert(path.clone()) {
                continue;
            }

            let name: String = path
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().to_string());

            let base_name: String = path.file_stem().map_or_else(String::new, |base_name| {
                base_name.to_string_lossy().to_string()
            });

            let content: String = thrustc_reader::get_file_source_code(&path);
            let unit: CompilationUnit = CompilationUnit::new(name, path, content, base_name);

            self.compile_file_with_llvm_aot(&unit)?;
        }

        Ok(())
    }

    fn compile_imported_std_jit<'a>(
        &mut self,
        context: &'a Context,
    ) -> Result<Vec<Module<'a>>, ()> {
        if !thrustc_preprocessor::std_library::has_imported_std() {
            return Ok(Vec::with_capacity(0));
        }

        let mut modules: Vec<Module<'a>> = Vec::with_capacity(u8::MAX as usize);

        let mut compiled_paths: std::collections::HashSet<std::path::PathBuf> =
            std::collections::HashSet::new();

        for module in thrustc_preprocessor::std_library::get_imported_std_modules() {
            let path: std::path::PathBuf = module.get_path().to_path_buf();

            if !path.is_file() {
                continue;
            }

            if !compiled_paths.insert(path.clone()) {
                continue;
            }

            let name: String = path
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().to_string());

            let base_name: String = path.file_stem().map_or_else(String::new, |base_name| {
                base_name.to_string_lossy().to_string()
            });

            let content: String = thrustc_reader::get_file_source_code(&path);
            let unit: CompilationUnit = CompilationUnit::new(name, path, content, base_name);

            let compiled_file: either::Either<MemoryBuffer, ()> =
                self.compile_file_with_llvm_jit(&unit)?;

            if let Some(module) = compiled_file
                .left()
                .and_then(|memory_buffer| context.create_module_from_ir(memory_buffer).ok())
            {
                modules.push(module);
            }
        }

        Ok(modules)
    }

    fn compile_file_with_llvm_aot(&mut self, file: &CompilationUnit) -> Result<(), ()> {
        let file_time: std::time::Instant = std::time::Instant::now();
        let frontend_time: std::time::Instant = std::time::Instant::now();

        starter::archive_compilation_unit(file);

        let llvm_backend: &LLVMBackend = self.options.get_llvm_backend();
        let build_dir: &std::path::PathBuf = self.options.get_build_dir();

        let Ok(tokens) = Lexer::lex(file, self.options) else {
            return interrupt::archive_compilation_module(self, file, file_time);
        };

        self.update_thrustc_frontend_time(frontend_time.elapsed());

        if print::before_frontend(self, file, Emited::Tokens(&tokens)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        if emit::before_frontend(self, build_dir, file, Emited::Tokens(&tokens)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        if self.options.stop_compilation_at(CompilationPhase::Lexer) {
            return finisher::archive_compilation(self, file_time, file);
        }

        let mut preprocessor: Preprocessor = Preprocessor::new();

        let modules: Result<&[thrustc_preprocessor::module::Module], ()> =
            preprocessor.generate_modules(&tokens, self.options, file);

        self.update_thrustc_frontend_time(frontend_time.elapsed());

        if modules.is_err() {
            return interrupt::archive_compilation_module(self, file, file_time);
        }

        let modules: &[thrustc_preprocessor::module::Module] = modules.map_err(|_| {
            let _ = interrupt::archive_compilation_unit_with_message(
                self,
                thrustc_logging::LoggingType::Error,
                "Failed to get all modules from the preprocessor. Maybe this is a issue.",
                file,
                file_time,
            );
        })?;

        let parser: (ParserContext, bool) = Parser::parse(&tokens, modules, file, self.options);

        let parser_result: (ParserContext, bool) = parser;
        let parser_failed: bool = parser_result.1;

        let parser_context: ParserContext = parser_result.0;

        let ast: &[Ast] = parser_context.get_ast();

        if emit::before_frontend(self, build_dir, file, Emited::Ast(ast)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        if print::before_frontend(self, file, Emited::Ast(ast)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        if self.options.stop_compilation_at(CompilationPhase::Parser) {
            return finisher::archive_compilation(self, file_time, file);
        }

        {
            let mut semantic_analysis: SemanticAnalysis<'_> =
                SemanticAnalysis::new(ast, file, self.options);

            let semantic_analysis_failed: either::Either<bool, ()> =
                semantic_analysis.execute(parser_failed);

            self.update_thrustc_frontend_time(frontend_time.elapsed());

            match semantic_analysis_failed {
                either::Either::Left(semantic_analysis_failed) => {
                    if parser_failed || semantic_analysis_failed {
                        return interrupt::archive_compilation_module(self, file, file_time);
                    }
                }
                either::Either::Right(()) => {
                    return finisher::archive_compilation(self, file_time, file);
                }
            }
        }

        {
            let mut intrinsic_checker: LLVMIntrinsicChecker<'_> =
                LLVMIntrinsicChecker::new(ast, file, self.options);

            let mut call_conv_checker: LLVMCallConventionsChecker<'_> =
                LLVMCallConventionsChecker::new(ast, self.options, file);

            let intrinsic_checker_fail: bool = intrinsic_checker.analyze();

            self.update_thrustc_frontend_time(frontend_time.elapsed());

            if self
                .options
                .stop_compilation_at(CompilationPhase::LLVMIntrinsicChecker)
            {
                return finisher::archive_compilation(self, file_time, file);
            }

            let call_convention_checker_fail: bool = call_conv_checker.analyze();

            self.update_thrustc_frontend_time(frontend_time.elapsed());

            if self
                .options
                .stop_compilation_at(CompilationPhase::LLVMCallConventionChecker)
            {
                return finisher::archive_compilation(self, file_time, file);
            }

            if intrinsic_checker_fail || call_convention_checker_fail {
                return interrupt::archive_compilation_module(self, file, file_time);
            }
        }

        if print::after_frontend(self, file, Emited::Ast(ast)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        if emit::after_frontend(self, build_dir, file, Emited::Ast(ast)) {
            return finisher::archive_compilation(self, file_time, file);
        }

        let backend_time: std::time::Instant = std::time::Instant::now();

        let target: &LLVMTarget = llvm_backend.get_target();
        let llvm_triple: &TargetTriple = target.get_target_triple();

        let llvm_target_triple_formatted: String =
            llvm_triple.as_str().to_string_lossy().to_string();

        let llvm_target_triple: LLVMTargetTriple =
            LLVMTargetTriple::new(llvm_target_triple_formatted.clone());

        let llvm_cpu_name: &str = llvm_backend.get_target_cpu().get_cpu_name();
        let llvm_cpu_features: &str = llvm_backend.get_target_cpu().get_cpu_features();

        let compiler_optimization: ThrustOptimization = llvm_backend.get_optimization();
        let llvm_opt: OptimizationLevel = compiler_optimization.to_llvm_opt();

        let target: Target = Target::from_triple(llvm_triple).map_err(|_| {
            let _ = interrupt::archive_compilation_unit_with_message(
                self,
                thrustc_logging::LoggingType::Error,
                "The compiler couldn't be configured correctly. The target is possibly unrecognizable. Try again another target or try to fix it.",
                file,
                file_time,
            );
        })?;

        if !target.has_target_machine() {
            interrupt::archive_compilation_unit_with_message(
                self,
                thrustc_logging::LoggingType::Error,
                "The compiler couldn't be configured correctly. The specified target cannot be used for code generation. Try with another target.",
                file,
                file_time,
            )?;
        }

        let target_machine: TargetMachine = target
            .create_target_machine(
                llvm_triple,
                llvm_cpu_name,
                llvm_cpu_features,
                llvm_opt,
                llvm_backend.get_reloc_mode(),
                llvm_backend.get_code_model(),
            )
            .ok_or_else(|| {
                let _ = interrupt::archive_compilation_unit_with_message(
                    self,
                    thrustc_logging::LoggingType::Error,
                    "The compiler couldn't be configured correctly. Possibly the target is not supported for code generation.",
                    file,
                    file_time,
                );
            })?;

        let target_data: TargetData = target_machine.get_target_data();
        let target_triple: TargetTriple = target_machine.get_triple();

        let target_info: TargetInfo =
            TargetInfo::new(LLVMTargetTriple::new(llvm_target_triple_formatted));

        let mut target_abi: Option<LLVMABIRepresentation> = thrustc_llvm_abi::get_abi(
            self.options.abi_configuration().specific(),
            file,
            self.options,
            &llvm_target_triple,
            &target_info,
            &target_data,
        );

        if self.options.abi_configuration().disable() {
            target_abi = None;
        }

        let llvm_context: Context = Context::create();
        let llvm_builder: Builder = llvm_context.create_builder();
        let llvm_module: Module = llvm_context.create_module(file.get_name());

        llvm_module.set_triple(llvm_triple);
        llvm_module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        let mut llvm_codegen_context: LLVMCodeGenContext = LLVMCodeGenContext::new(
            &llvm_module,
            &llvm_context,
            &llvm_builder,
            &target_data,
            &target_triple,
            &target_machine,
            target_abi.as_ref(),
            Diagnostician::new(file, self.options),
            self.options,
            file,
        );

        thrustc_llvm_codegen::LLVMCompiler::compile(&mut llvm_codegen_context, ast);

        validate::llvm_codegen(&llvm_module, file)?;

        self.update_thrustc_backend_time(backend_time.elapsed());

        if print::llvm_before_optimization(self, &llvm_module, &target_machine, file, file_time)? {
            return finisher::archive_compilation(self, file_time, file);
        }

        if emit::llvm_before_optimization(
            self,
            &llvm_module,
            &target_machine,
            build_dir,
            file,
            file_time,
        )? {
            return finisher::archive_compilation(self, file_time, file);
        }

        if self
            .options
            .stop_compilation_at(CompilationPhase::LLVMCodegen)
        {
            return finisher::archive_compilation(self, file_time, file);
        }

        self.update_thrustc_backend_time(backend_time.elapsed());

        let llvm_optimizer_config: LLVMOptimizationConfig = LLVMOptimizationConfig::new(
            compiler_optimization,
            *llvm_backend.get_sanitizer(),
            *llvm_backend.get_symbol_linkage_strategy(),
            *llvm_backend.get_denormal_fp_behavior(),
            *llvm_backend.get_denormal_fp_32_bits_behavior(),
        );

        let llvm_optimizer_passes: LLVMOptimizerPasses<'_> = LLVMOptimizerPasses::new(
            llvm_backend.get_opt_passes(),
            llvm_backend.get_modificator_passes(),
        );

        let llvm_optimizer_flags: LLVMOptimizerFlags = LLVMOptimizerFlags::new(
            self.options.omit_default_optimizations(),
            llvm_backend.get_disable_all_sanitizers(),
        );

        thrustc_llvm_codegen::optimizer::LLVMOptimizer::new(
            &llvm_module,
            &llvm_context,
            &target_machine,
            llvm_optimizer_config,
            llvm_optimizer_flags,
            llvm_optimizer_passes,
        )
        .optimize();

        self.update_thrustc_backend_time(backend_time.elapsed());

        if print::llvm_after_optimization(self, &llvm_module, &target_machine, file, file_time)? {
            return finisher::archive_compilation(self, file_time, file);
        }

        if emit::llvm_after_optimization(
            self,
            &llvm_module,
            &target_machine,
            build_dir,
            file,
            file_time,
        )? {
            return finisher::archive_compilation(self, file_time, file);
        }

        let obj_file: std::path::PathBuf = finisher::llvm_obj_compilation(
            self.options,
            &llvm_module,
            &target_machine,
            build_dir,
            file.get_name(),
        );

        self.add_compiled_unit(obj_file);

        self.update_thrustc_backend_time(backend_time.elapsed());

        finisher::archive_compilation(self, file_time, file)?;

        Ok(())
    }
}

impl<'thrustc> ThrustCompiler<'thrustc> {
    fn compile_jit_llvm(&mut self) -> CompileTime {
        cleaner::auto_clean(self.get_compilation_options());

        self.discover_std_usage();

        let context: Context = Context::create();

        let mut it_failed: bool = false;
        let mut modules: Vec<Module> = Vec::with_capacity(u8::MAX as usize);

        let std_modules: Result<Vec<Module>, ()> = self.compile_imported_std_jit(&context);

        let std_modules: Vec<Module> = match std_modules {
            Ok(std_modules) => std_modules,
            Err(()) => {
                return (
                    true,
                    self.thrustc_time,
                    self.thrustc_frontend_time,
                    self.thrustc_backend_time,
                    self.linking_time,
                );
            }
        };

        modules.extend(std_modules);

        for file in self.unready.iter() {
            let compiled_file: Result<either::Either<MemoryBuffer, ()>, ()> =
                self.compile_file_with_llvm_jit(file);

            it_failed = compiled_file.is_err();

            if let Some(module) = compiled_file
                .ok()
                .and_then(|either| either.left())
                .and_then(|memory_buffer| context.create_module_from_ir(memory_buffer).ok())
            {
                modules.push(module)
            }
        }

        it_failed = it_failed
            || self.get_compilation_options().was_printed()
            || self.get_compilation_options().was_emited()
            || modules.is_empty();

        if it_failed {
            return (
                it_failed,
                self.thrustc_time,
                self.thrustc_frontend_time,
                self.thrustc_backend_time,
                self.linking_time,
            );
        }

        modules.reverse();

        let Some(module) = modules.first() else {
            thrustc_logging::print_warning(
                thrustc_logging::LoggingType::Warning,
                "There's nothing to compile for the JIT compiler. Skipping compilation.",
            );

            return (
                it_failed,
                self.thrustc_time,
                self.thrustc_frontend_time,
                self.thrustc_backend_time,
                self.linking_time,
            );
        };

        let llvm_backend: &LLVMBackend = self.options.get_llvm_backend();
        let config: &JITConfiguration = llvm_backend.get_jit_config();
        let opt_level: OptimizationLevel = llvm_backend.get_optimization().to_llvm_opt();

        let engine: ExecutionEngine = match module.create_jit_execution_engine(opt_level) {
            Ok(engine) => engine,
            Err(_) => {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    "The JIT compiler couldn't be created correctly. Unexpected issue.",
                );

                return (
                    it_failed,
                    self.thrustc_time,
                    self.thrustc_frontend_time,
                    self.thrustc_backend_time,
                    self.linking_time,
                );
            }
        };

        let llvm_jit: LLVMJITCompiler =
            thrustc_llvm_codegen::jit::LLVMJITCompiler::new(engine, config, modules);

        let llvm_jit_result: i32 = llvm_jit.compile_and_run().unwrap_or(1);

        std::process::exit(llvm_jit_result)
    }

    fn compile_file_with_llvm_jit(
        &mut self,
        file: &CompilationUnit,
    ) -> Result<either::Either<MemoryBuffer, ()>, ()> {
        let file_time: std::time::Instant = std::time::Instant::now();
        let frontend_time: std::time::Instant = std::time::Instant::now();

        starter::archive_compilation_unit(file);

        let llvm_backend: &LLVMBackend = self.options.get_llvm_backend();
        let build_dir: &std::path::PathBuf = self.options.get_build_dir();

        let Ok(tokens) = Lexer::lex(file, self.options) else {
            return interrupt::archive_compilation_module_jit(self, file, file_time);
        };

        self.update_thrustc_frontend_time(frontend_time.elapsed());

        if print::before_frontend(self, file, Emited::Tokens(&tokens)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if emit::before_frontend(self, build_dir, file, Emited::Tokens(&tokens)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if self.options.stop_compilation_at(CompilationPhase::Lexer) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        let mut preprocessor: Preprocessor = Preprocessor::new();
        let modules: Result<&[thrustc_preprocessor::module::Module], ()> =
            preprocessor.generate_modules(&tokens, self.options, file);

        self.update_thrustc_frontend_time(frontend_time.elapsed());

        if modules.is_err() {
            return interrupt::archive_compilation_module_jit(self, file, file_time);
        }

        let modules: &[thrustc_preprocessor::module::Module] = modules.map_err(|_| {
            let _ = interrupt::archive_compilation_unit_with_message(
                self,
                thrustc_logging::LoggingType::Error,
                "Failed to get all modules from the preprocessor. Maybe this is a issue.",
                file,
                file_time,
            );
        })?;

        let parser: (ParserContext, bool) = Parser::parse(&tokens, modules, file, self.options);

        let parser_result: (ParserContext, bool) = parser;
        let parser_failed: bool = parser_result.1;

        let parser_context: ParserContext = parser_result.0;

        let ast: &[Ast] = parser_context.get_ast();

        if print::before_frontend(self, file, Emited::Ast(ast)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if emit::before_frontend(self, build_dir, file, Emited::Ast(ast)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if self.options.stop_compilation_at(CompilationPhase::Parser) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        {
            let mut semantic_analysis: SemanticAnalysis<'_> =
                SemanticAnalysis::new(ast, file, self.options);

            let semantic_analysis_failed: either::Either<bool, ()> =
                semantic_analysis.execute(parser_failed);

            self.update_thrustc_frontend_time(frontend_time.elapsed());

            match semantic_analysis_failed {
                either::Either::Left(semantic_analysis_failed) => {
                    if parser_failed || semantic_analysis_failed {
                        return interrupt::archive_compilation_module_jit(self, file, file_time);
                    }
                }
                either::Either::Right(()) => {
                    return finisher::archive_compilation_module_jit(self, file_time, file);
                }
            }
        }

        {
            let mut intrinsic_checker: LLVMIntrinsicChecker<'_> =
                LLVMIntrinsicChecker::new(ast, file, self.options);

            let mut call_conv_checker: LLVMCallConventionsChecker<'_> =
                LLVMCallConventionsChecker::new(ast, self.options, file);

            let intrinsic_checker_fail: bool = intrinsic_checker.analyze();

            if self
                .options
                .stop_compilation_at(CompilationPhase::LLVMIntrinsicChecker)
            {
                return finisher::archive_compilation_module_jit(self, file_time, file);
            }

            let call_convention_fail: bool = call_conv_checker.analyze();

            if self
                .options
                .stop_compilation_at(CompilationPhase::LLVMCallConventionChecker)
            {
                return finisher::archive_compilation_module_jit(self, file_time, file);
            }

            self.update_thrustc_frontend_time(frontend_time.elapsed());

            if intrinsic_checker_fail || call_convention_fail {
                return interrupt::archive_compilation_module_jit(self, file, file_time);
            }
        }

        if print::after_frontend(self, file, Emited::Ast(ast)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if emit::after_frontend(self, build_dir, file, Emited::Ast(ast)) {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        let backend_time: std::time::Instant = std::time::Instant::now();

        let target: &LLVMTarget = llvm_backend.get_target();
        let llvm_triple: &TargetTriple = target.get_target_triple();

        let llvm_target_triple_formatted: String =
            llvm_triple.as_str().to_string_lossy().to_string();

        let llvm_target_triple: LLVMTargetTriple =
            LLVMTargetTriple::new(llvm_target_triple_formatted.clone());

        let llvm_cpu_name: &str = llvm_backend.get_target_cpu().get_cpu_name();
        let llvm_cpu_features: &str = llvm_backend.get_target_cpu().get_cpu_features();

        let compiler_optimization: ThrustOptimization = llvm_backend.get_optimization();
        let llvm_opt: OptimizationLevel = compiler_optimization.to_llvm_opt();

        let target: Target = Target::from_triple(llvm_triple).map_err(|_| {
            let _ = interrupt::archive_compilation_unit_with_message(
                self,
            thrustc_logging::LoggingType::Error,
                "Target-triple could not be built correctly. Maybe this target triple is invalid. Try another.",
                file,
                file_time,
            );
        })?;

        let target_machine: TargetMachine = target
            .create_target_machine(
                llvm_triple,
                llvm_cpu_name,
                llvm_cpu_features,
                llvm_opt,
                llvm_backend.get_reloc_mode(),
                llvm_backend.get_code_model(),
            )
            .ok_or_else(|| {
                let _ = interrupt::archive_compilation_unit_with_message(
                    self,
                    thrustc_logging::LoggingType::Error,
                    "Target machine could not be built correctly.",
                    file,
                    file_time,
                );
            })?;

        let target_data: TargetData = target_machine.get_target_data();
        let target_triple: TargetTriple = target_machine.get_triple();

        let target_info: TargetInfo =
            TargetInfo::new(LLVMTargetTriple::new(llvm_target_triple_formatted));

        let mut target_abi: Option<LLVMABIRepresentation> = thrustc_llvm_abi::get_abi(
            self.options.abi_configuration().specific(),
            file,
            self.options,
            &llvm_target_triple,
            &target_info,
            &target_data,
        );

        if self.options.abi_configuration().disable() {
            target_abi = None;
        }

        let llvm_context: Context = Context::create();
        let llvm_builder: Builder = llvm_context.create_builder();
        let llvm_module: Module = llvm_context.create_module(file.get_name());

        llvm_module.set_triple(llvm_triple);
        llvm_module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        jit::has_jit_available(&target)?;

        let mut llvm_codegen_context: LLVMCodeGenContext = LLVMCodeGenContext::new(
            &llvm_module,
            &llvm_context,
            &llvm_builder,
            &target_data,
            &target_triple,
            &target_machine,
            target_abi.as_ref(),
            Diagnostician::new(file, self.options),
            self.options,
            file,
        );

        thrustc_llvm_codegen::LLVMCompiler::compile(&mut llvm_codegen_context, ast);

        validate::llvm_codegen(&llvm_module, file)?;

        self.update_thrustc_backend_time(backend_time.elapsed());

        if print::llvm_before_optimization(self, &llvm_module, &target_machine, file, file_time)? {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if emit::llvm_before_optimization(
            self,
            &llvm_module,
            &target_machine,
            build_dir,
            file,
            file_time,
        )? {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if self
            .options
            .stop_compilation_at(CompilationPhase::LLVMCodegen)
        {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        let llvm_optimizer_config: LLVMOptimizationConfig = LLVMOptimizationConfig::new(
            compiler_optimization,
            *llvm_backend.get_sanitizer(),
            *llvm_backend.get_symbol_linkage_strategy(),
            *llvm_backend.get_denormal_fp_behavior(),
            *llvm_backend.get_denormal_fp_32_bits_behavior(),
        );

        let llvm_optimizer_passes: LLVMOptimizerPasses<'_> = LLVMOptimizerPasses::new(
            llvm_backend.get_opt_passes(),
            llvm_backend.get_modificator_passes(),
        );

        let llvm_optimizer_flags: LLVMOptimizerFlags = LLVMOptimizerFlags::new(
            self.options.omit_default_optimizations(),
            llvm_backend.get_disable_all_sanitizers(),
        );

        thrustc_llvm_codegen::optimizer::LLVMOptimizer::new(
            &llvm_module,
            &llvm_context,
            &target_machine,
            llvm_optimizer_config,
            llvm_optimizer_flags,
            llvm_optimizer_passes,
        )
        .optimize();

        self.update_thrustc_backend_time(backend_time.elapsed());

        if print::llvm_after_optimization(self, &llvm_module, &target_machine, file, file_time)? {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        if emit::llvm_after_optimization(
            self,
            &llvm_module,
            &target_machine,
            build_dir,
            file,
            file_time,
        )? {
            return finisher::archive_compilation_module_jit(self, file_time, file);
        }

        self.update_thrustc_backend_time(backend_time.elapsed());

        finisher::archive_compilation(self, file_time, file)?;

        Ok(either::Either::Left(llvm_module.write_bitcode_to_memory()))
    }
}

impl ThrustCompiler<'_> {
    #[inline]
    pub fn update_thrustc_time(&mut self, elapsed: std::time::Duration) {
        self.thrustc_time = elapsed;
    }

    #[inline]
    pub fn update_thrustc_frontend_time(&mut self, elapsed: std::time::Duration) {
        self.thrustc_frontend_time = elapsed;
    }

    #[inline]
    pub fn update_thrustc_backend_time(&mut self, elapsed: std::time::Duration) {
        self.thrustc_backend_time = elapsed;
    }
}

impl ThrustCompiler<'_> {
    #[inline]
    pub fn get_compiled_files(&self) -> &[std::path::PathBuf] {
        &self.ready
    }

    #[inline]
    pub fn get_compilation_options(&self) -> &CompilerOptions {
        self.options
    }
}

impl ThrustCompiler<'_> {
    #[inline]
    pub fn add_compiled_unit(&mut self, path: std::path::PathBuf) {
        self.ready.push(path);
    }
}
