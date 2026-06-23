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

#![allow(clippy::upper_case_acronyms)]

use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use colored::Colorize;
use thrustc_backends::ThrustCodeModel;
use thrustc_backends::ThrustOptimization;
use thrustc_core::CompileTime;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_logging::LoggingType;
use thrustc_logging::OutputIn;
use thrustc_options::CompilationPhase;
use thrustc_options::CompilerOptions;
use thrustc_options::EmitableUnit;
use thrustc_options::PrintableUnit;

use ahash::AHashMap as HashMap;

use thrustc_backends::ThrustRelocMode;
use thrustc_backends::llvm;
use thrustc_backends::llvm::DenormalFloatingPointBehavior;
use thrustc_backends::llvm::DenormalFloatingPointBehavior32BitFloatingPoint;
use thrustc_backends::llvm::Sanitizer;
use thrustc_backends::llvm::SanitizerConfiguration;
use thrustc_backends::llvm::SymbolLinkageMergeStrategy;
use thrustc_backends::llvm::debug::DwarfVersion;
use thrustc_backends::llvm::passes::LLVMModificatorPasses;
use thrustc_options::linkage::LinkingCompilersConfiguration;

mod help;

#[derive(Debug)]
pub struct CommandLine {
    options: CompilerOptions,
    args: Vec<String>,
    current: usize,
    position: CommandLinePosition,
    validation_cache: HashMap<String, bool>,
}

#[derive(Debug)]
pub struct ParsedArg {
    key: String,
    value: Option<String>,
}

impl ParsedArg {
    fn new(arg: &str) -> Self {
        if let Some(eq_pos) = arg.find('=') {
            let (key, value) = arg.split_at(eq_pos);

            return Self {
                key: key.to_string(),
                value: Some(value[1..].to_string()),
            };
        }

        if let Some(eq_pos) = arg.find(':') {
            let (key, value) = arg.split_at(eq_pos);

            return Self {
                key: key.to_string(),
                value: Some(value[1..].to_string()),
            };
        }

        Self {
            key: arg.to_string(),
            value: None,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum CommandLinePosition {
    #[default]
    ThrustCompiler,
    External,
}

impl CommandLinePosition {
    #[inline]
    pub fn at_external(&self) -> bool {
        matches!(self, CommandLinePosition::External)
    }
}

impl CommandLine {
    pub fn parse(mut args: Vec<String>) -> CommandLine {
        let processed_args: Vec<String> = Self::preprocess_args(&mut args);

        let mut command_line: CommandLine = Self {
            options: CompilerOptions::new(),
            args: processed_args,
            current: 0,
            position: CommandLinePosition::default(),
            validation_cache: HashMap::with_capacity(u8::MAX as usize),
        };

        command_line.build();
        command_line
    }

    fn preprocess_args(args: &mut Vec<String>) -> Vec<String> {
        let mut processed: Vec<String> = Vec::with_capacity(args.len().saturating_mul(2));

        if !args.is_empty() {
            args.remove(0);
        }

        for arg in args.iter() {
            let parsed: ParsedArg = ParsedArg::new(arg);

            processed.push(parsed.key);

            if let Some(value) = parsed.value {
                processed.push(value);
            }
        }

        processed
    }
}

impl CommandLine {
    fn build(&mut self) {
        if self.args.is_empty() {
            help::show_help();
        }

        while !self.is_eof() {
            let argument: String = self.args[self.current].clone();
            self.analyze(argument);
        }

        self.validate();
    }
}

impl CommandLine {
    fn validate(&mut self) {
        if !self.get_options().get_llvm_backend().is_full_jit() {
            self.get_mut_options()
                .get_mut_linking_compilers_configuration()
                .comprobate_status();
        }
    }
}

impl CommandLine {
    fn analyze(&mut self, argument: String) {
        let arg: &str = argument.as_str();

        match arg {
            "-h" | "--help" => {
                self.advance();

                match self.peek_optional() {
                    Some("opt") => {
                        self.advance();
                        help::show_optimization_help();
                    }
                    Some("emit") => {
                        self.advance();
                        help::show_emission_help();
                    }
                    Some("print") => {
                        self.advance();
                        help::show_printing_help();
                    }
                    Some("code-model") => {
                        self.advance();
                        help::show_code_model_help();
                    }
                    Some("reloc-model") => {
                        self.advance();
                        help::show_reloc_model_help();
                    }
                    Some("sanitizer") => {
                        self.advance();
                        help::show_sanitizer_help();
                    }
                    Some("symbol-linkage-strategy") => {
                        self.advance();
                        help::show_symbol_linkage_strategy_help();
                    }
                    Some("denormal-floating-point-behavior") => {
                        self.advance();
                        help::show_denormal_floating_point_behavior_help();
                    }
                    Some("denormal-floating-point-32-bits-behavior") => {
                        self.advance();
                        help::show_denormal_floating_point_32_bits_behavior_help();
                    }

                    _ => help::show_help(),
                }
            }

            "-v" | "--version" => {
                self.advance();
                thrustc_logging::write(OutputIn::Stdout, thrustc_constants::COMPILER_VERSION);
                std::process::exit(0);
            }

            "-build-dir" => {
                self.advance();

                let build_dir: PathBuf = self.peek().into();

                self.get_mut_options().set_build_dir(build_dir);

                self.advance();
            }

            "-tools-dir" => {
                self.advance();

                let compiler_home_path: PathBuf = self.peek().into();

                self.get_mut_options()
                    .set_compiler_tools_path(compiler_home_path);

                self.advance();
            }

            "-jit" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options().get_mut_llvm_backend().set_jit(true);
            }

            "-jit-libc" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_jit_required(arg);

                let libc: PathBuf = self.peek().into();

                if (libc.to_string_lossy().contains("/") || libc.to_string_lossy().contains("\\"))
                    && (!libc.exists() || !libc.is_file())
                {
                    self.report_error("A indicated C runtime doesn't exist.");
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_jit_config()
                    .set_libc_path(libc);

                self.advance();
            }

            "-jit-link" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_jit_required(arg);

                let library: PathBuf = self.peek().into();

                if (library.to_string_lossy().contains("/")
                    || library.to_string_lossy().contains("\\"))
                    && (!library.exists() || !library.is_file())
                {
                    self.report_error("A indicated dynamic library doesn't exist.");
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_jit_config()
                    .add_library(library);

                self.advance();
            }

            "-jit-entry" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_jit_required(arg);

                let entrypoint: Vec<u8> = self.peek().as_bytes().to_vec();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_jit_config()
                    .set_entry(entrypoint);

                self.advance();
            }

            "-start" => {
                self.advance();
                self.position = CommandLinePosition::External;
            }

            "-end" => {
                self.advance();
                self.position = CommandLinePosition::ThrustCompiler;
            }

            "-clang-link" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_not_gcc_active();

                let path: PathBuf = self.peek().into();

                if !self.validate_compiler_path(&path) {
                    self.report_error("Indicated external C & C++ compiler Clang doesn't exist.");
                }

                let compiler_config: &mut LinkingCompilersConfiguration = self
                    .get_mut_options()
                    .get_mut_linking_compilers_configuration();

                compiler_config.set_custom_clang(path);
                compiler_config.set_use_clang(true);

                self.advance();
            }

            "-gcc-link" => {
                self.advance();
                self.validate_not_clang_active();

                let path: PathBuf = self.peek().into();

                if !self.validate_compiler_path(&path) {
                    self.report_error(
                        "Indicated external GNU Compiler Collection (GCC) doesn't exist.",
                    );
                }

                let compiler_config: &mut LinkingCompilersConfiguration = self
                    .get_mut_options()
                    .get_mut_linking_compilers_configuration();

                compiler_config.set_custom_gcc(path);
                compiler_config.set_use_gcc(true);

                self.advance();
            }

            "-target" => {
                self.advance();
                self.validate_llvm_required(arg);

                let target: String = self.peek().to_string();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_arch(target);

                self.advance();
            }

            "-target-triple" => {
                self.advance();
                self.validate_llvm_required(arg);

                let target_triple: String = self.peek().to_string();

                if !LLVMTargetTriple::is_valid_llvm_target_triple_format(&target_triple) {
                    thrustc_logging::print_critical_error(
                        LoggingType::Error,
                        "Unknown target triple format.",
                    );
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_target_triple(target_triple);

                self.advance();
            }

            "-cpu" => {
                self.advance();
                self.validate_llvm_required(arg);

                let name: String = self.peek().to_string();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target_cpu()
                    .set_cpu_name(name);

                self.advance();
            }

            "-cpu-features" => {
                self.advance();
                self.validate_llvm_required(arg);

                let features: String = self.peek().to_string();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target_cpu()
                    .set_processador_features(features);

                self.advance();
            }

            "-cpu-enable-features" => {
                self.advance();
                self.validate_llvm_required(arg);

                let features: String = self.peek().to_string();
                let features_to_replace: Vec<&str> = features.split(";").collect();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target_cpu()
                    .add_cpu_features(features_to_replace);

                self.advance();
            }

            "-cpu-disable-features" => {
                self.advance();
                self.validate_llvm_required(arg);

                let features: String = self.peek().to_string();
                let features_to_replace: Vec<&str> = features.split(";").collect();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target_cpu()
                    .remove_cpu_features(features_to_replace);

                self.advance();
            }

            "-opt" => {
                self.advance();
                self.validate_llvm_required(arg);

                let opt: ThrustOptimization = self.parse_optimization_level(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_optimization(opt);

                self.advance();
            }

            "-emit" => {
                self.advance();
                self.validate_llvm_required(arg);

                let emitable: EmitableUnit = self.parse_emission_value(self.peek());

                self.get_mut_options().add_emit_option(emitable);

                self.advance();
            }

            "-print" => {
                self.advance();
                self.validate_llvm_required(arg);

                let pritable_unit: PrintableUnit = self.parse_printable_emission_value(self.peek());

                self.get_mut_options().add_print_option(pritable_unit);

                self.advance();
            }

            "-dbg" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_debug_config()
                    .set_debug_mode();
            }

            "-dbg-for-inlining" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_debug_config()
                    .set_split_debug_inlining();
            }

            "-dbg-for-profiling" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_debug_config()
                    .set_debug_for_profiling();
            }

            "-dbg-dwarf-version" => {
                self.advance();
                self.validate_llvm_required(arg);

                let dwarf_v: DwarfVersion = self.parse_dwarf_version(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_debug_config()
                    .set_dwarf_version(dwarf_v);

                self.advance();
            }

            "-stop-at" => {
                self.advance();
                self.validate_llvm_required(arg);

                let phase: CompilationPhase = self.parse_stop_compilation_phase_at(self.peek());

                self.get_mut_options().set_stop_compilation_at(phase);

                self.advance();
            }

            "-macos-version" => {
                self.advance();
                self.validate_llvm_required(arg);

                let version: String = self.peek().to_string();

                if !version.chars().all(|c| c.is_ascii_digit() || c == '.') {
                    self.report_error("MacOS version must contain only numbers and dots.");
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_macos_version(version);

                self.advance();
            }

            "-ios-version" => {
                self.advance();
                self.validate_llvm_required(arg);

                let version: String = self.peek().to_string();

                if !version.chars().all(|c| c.is_ascii_digit() || c == '.') {
                    self.report_error("iOS version must contain only numbers and dots.");
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_ios_version(version);

                self.advance();
            }

            "-cuda-version" => {
                self.advance();
                self.validate_llvm_required(arg);

                let version: String = self.peek().to_string();

                if !version.chars().all(|c| c.is_ascii_digit() || c == '.') {
                    self.report_error("Nvidia Cuda version must contain only numbers and dots.");
                }

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_nvidia_cuda_version(version);

                self.advance();
            }

            "-reloc-model" => {
                self.advance();
                self.validate_llvm_required(arg);

                let reloc_mode: ThrustRelocMode = self.parse_reloc_mode(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_reloc_mode(reloc_mode);

                self.advance();
            }

            "-code-model" => {
                self.advance();

                let code_model: ThrustCodeModel = self.parse_code_model(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_code_model(code_model);

                self.advance();
            }

            "-abi" => {
                self.advance();

                let specific: thrustc_abi::SpecificABI = self.parse_specific_abi(self.peek());

                self.get_mut_options().set_utilize_specific_abi(specific);

                self.advance();
            }

            "-L" => {
                self.advance();
                self.validate_llvm_required(arg);

                let library_path_dir: PathBuf = PathBuf::from(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_linker_config()
                    .add_library_path(library_path_dir);

                self.advance();
            }

            "-l" => {
                self.advance();
                self.validate_llvm_required(arg);

                let link_library: String = self.peek().into();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_linker_config()
                    .add_link_library(link_library);

                self.advance();
            }

            "-no-executable" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_linker_config()
                    .set_build_executable(false);
            }

            "-output" => {
                self.advance();
                self.validate_llvm_required(arg);

                let output: String = self.peek().into();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_linker_config()
                    .set_output(output);

                self.advance();
            }

            "-debug-linker-command" => {
                self.advance();
            }

            "--disable-abi" => {
                self.advance();
                self.get_mut_options().set_disable_abi_detection(true);
            }

            "--dump-compiler-version" => {
                self.advance();

                let version_file: std::fs::File = std::fs::File::options()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open("COMPILER_VERSION.txt")
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_critical_error(
                            LoggingType::Error,
                            "Fail to dump the compiler version into a flat .txt file!",
                        )
                    });

                let mut buff_writer: BufWriter<std::fs::File> = BufWriter::new(version_file);

                buff_writer
                    .write_all(thrustc_constants::COMPILER_VERSION.as_bytes())
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_critical_error(
                            LoggingType::Error,
                            "Fail to write the compiler version into a flat .txt file!",
                        )
                    });

                buff_writer.flush().unwrap_or_else(|_| {
                    thrustc_logging::print_critical_error(
                        LoggingType::Error,
                        "Fail to write the compiler version into a flat .txt file!",
                    )
                });

                std::process::exit(thrustc_constants::SUCCESFUL_CODE);
            }

            "--link-check" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_aot_is_enable(arg);
            }

            "--stack-protector" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_stack_protector();
            }

            "--symbol-linkage-strategy" => {
                self.advance();
                self.validate_llvm_required(arg);

                let strategy: SymbolLinkageMergeStrategy =
                    self.parse_symbol_linkage_strategy(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_symbol_linkage_strategy(strategy);

                self.advance();
            }

            "--denormal-floating-point-behavior" => {
                self.advance();

                let behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) =
                    self.parse_denormal_floating_point_behavior(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_denormal_fp_behavior(behavior);

                self.advance();
            }

            "--denormal-floating-point-32-bits-behavior" => {
                self.advance();

                let behavior: (
                    DenormalFloatingPointBehavior32BitFloatingPoint,
                    DenormalFloatingPointBehavior32BitFloatingPoint,
                ) = self.parse_denormal_floating_point_behavior_32_bit_floating_point(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_denormal_fp_32_bits_behavior(behavior);

                self.advance();
            }

            "--sanitizer" => {
                self.advance();
                self.validate_llvm_required(arg);

                let sanitizer: Sanitizer = self.parse_sanitizer(self.peek());

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_sanitizer(sanitizer);

                self.advance();
            }

            "--no-sanitize" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_sanitizer_required(arg);

                let (nosanitize_bounds, nosanitize_coverage) =
                    self.parse_sanitizer_config(self.peek());

                match self
                    .get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_sanitizer()
                {
                    Sanitizer::Address(config) => {
                        config.set_nosanitize_bounds(nosanitize_bounds);
                        config.set_nosanitize_coverage(nosanitize_coverage);
                    }
                    Sanitizer::Hwaddress(config) => {
                        config.set_nosanitize_bounds(nosanitize_bounds);
                        config.set_nosanitize_coverage(nosanitize_coverage);
                    }
                    Sanitizer::Memory(config) => {
                        config.set_nosanitize_bounds(nosanitize_bounds);
                        config.set_nosanitize_coverage(nosanitize_coverage);
                    }
                    Sanitizer::Memtag(config) => {
                        config.set_nosanitize_bounds(nosanitize_bounds);
                        config.set_nosanitize_coverage(nosanitize_coverage);
                    }
                    Sanitizer::Thread(config) => {
                        config.set_nosanitize_bounds(nosanitize_bounds);
                        config.set_nosanitize_coverage(nosanitize_coverage);
                    }

                    Sanitizer::None => {
                        self.report_error("Cannot modify a sanitizer settings without this option enabled. First, use \"--sanitize.\".");
                    }
                }

                self.advance();
            }

            "--target-triple-darwin-variant" => {
                self.advance();
                self.validate_llvm_required(arg);

                let target_triple: String = self.peek().to_string();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target()
                    .set_target_triple_darwin_variant(target_triple);

                self.advance();
            }

            "--disable-all-cpu-features" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_target_cpu()
                    .disable_cpu_all_features();
            }

            "--disable-all-sanitizers" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_disable_all_sanitizers();
            }

            "--disable-frame-pointer" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_omit_frame_pointer();
            }

            "--disable-uwtable" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_omit_uwtable();
            }

            "--disable-direct-access-external-data" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_omit_direct_access_external_data();
            }

            "--disable-rtlib-got" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_omit_rtlibusegot();
            }

            "--disable-safe-trapping-math" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_omit_trapping_math();
            }

            "--disable-safe-math" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_disable_safe_math();
            }

            "--disable-default-optimization" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options().set_omit_default_optimizations();
            }

            "--opt-passes" => {
                self.advance();
                self.validate_llvm_required(arg);

                let extra_opt_passes: String = self.peek().to_string();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_opt_passes(extra_opt_passes);

                self.advance();
            }

            "--modificator-opt-passes" => {
                self.advance();
                self.validate_llvm_required(arg);

                let raw_modificator_passes: &str = self.peek();
                let modificator_passes: Vec<LLVMModificatorPasses> =
                    LLVMModificatorPasses::into_llvm_modificator_passes(raw_modificator_passes);

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .set_modificator_passes(modificator_passes);

                self.advance();
            }

            "--disable-all-warnings" => {
                self.advance();

                self.get_mut_options().set_disable_all_warnings();
            }

            "--copy-output-to-clipboard" => {
                self.advance();
                self.validate_llvm_required(arg);
                self.validate_print_required(arg);

                self.get_mut_options().set_copy_output_to_clipboard();
            }

            "--debug-clang-command" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_linking_compilers_configuration()
                    .set_debug_clang_commands(true);
            }

            "--debug-gcc-command" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options()
                    .get_mut_linking_compilers_configuration()
                    .set_debug_gcc_commands(true);
            }

            "--export-compiler-errors" => {
                self.advance();

                self.get_mut_options()
                    .set_export_compiler_error_diagnostics();
            }

            "--export-compiler-warnings" => {
                self.advance();

                self.get_mut_options()
                    .set_export_compiler_warning_diagnostics();
            }

            "--export-diagnostics-path" => {
                self.advance();

                let path: PathBuf = PathBuf::from(self.peek());

                self.get_mut_options().set_export_diagnostic_path(path);

                self.advance();
            }

            "--clean-exported-diagnostics" => {
                self.advance();

                self.get_mut_options()
                    .set_compiler_exported_diagnostics_clean();
            }

            "--clean-build" => {
                self.advance();
                self.get_mut_options().set_clean_build();
            }

            "--clean-tokens" => {
                self.advance();
                self.get_mut_options().set_clean_tokens();
            }

            "--clean-assembler" => {
                self.advance();
                self.options.set_clean_assembler();
            }

            "--clean-llvm-ir" => {
                self.advance();
                self.options.set_clean_llvm_ir();
            }

            "--clean-llvm-bitcode" => {
                self.advance();
                self.get_mut_options().set_clean_llvm_bitcode();
            }

            "--clean-objects" => {
                self.advance();
                self.get_mut_options().set_clean_object();
            }

            "--no-obfuscate-archive-names" => {
                self.advance();
                self.get_mut_options().set_no_obfuscate_archive_names();
            }

            "--no-obfuscate-ir" => {
                self.advance();
                self.get_mut_options().set_no_obfuscate_ir();
            }

            "--enable-ansi-color" => {
                self.advance();
                self.validate_llvm_required(arg);

                self.get_mut_options().set_enable_ansi_colors();

                self.get_mut_options()
                    .get_mut_llvm_backend()
                    .get_mut_linker_config()
                    .set_use_ansi_colors();
            }

            "--print-targets" => {
                self.advance();
                llvm::info::print_all_targets();
            }

            "--print-host-target-triple" => {
                self.advance();
                llvm::info::print_host_target_triple();
            }

            "--print-supported-cpus" => {
                self.advance();
                llvm::info::print_specific_cpu_support(
                    self.get_options()
                        .get_llvm_backend()
                        .get_target()
                        .get_arch(),
                );
            }

            "--print-opt-passes" => {
                self.advance();
                llvm::info::print_all_available_opt_passes();
            }

            possible_file_path if self.is_thrust_file(possible_file_path) => {
                self.advance();
                self.handle_thrust_file(possible_file_path);
            }

            any => {
                self.advance();
                self.handle_unknown_argument(any);
            }
        }
    }
}

impl CommandLine {
    #[inline]
    fn peek_optional(&self) -> Option<&str> {
        if self.is_eof() {
            return None;
        }

        Some(&self.args[self.current])
    }

    #[inline]
    fn peek(&self) -> &str {
        if self.is_eof() {
            self.report_error("Expected value after flag or command.");
        }

        &self.args[self.current]
    }

    #[inline]
    fn advance(&mut self) {
        if self.is_eof() {
            self.report_error("Expected value after flag or command.");
        }

        self.current = self.current.saturating_add(1);
    }

    #[inline]
    fn report_error(&self, msg: &str) -> ! {
        thrustc_logging::print_critical_error(LoggingType::Error, msg);
    }
}

impl CommandLine {
    fn handle_thrust_file(&mut self, file_path: &str) {
        let mut path: PathBuf = PathBuf::from(file_path);

        let name: String = path.file_name().map_or_else(
            || {
                thrustc_logging::print_critical_error(
                    LoggingType::Error,
                    &format!("Unknown file name '{}'.", path.display()),
                );
            },
            |name| name.to_string_lossy().to_string(),
        );

        let base_name: String = path.file_stem().map_or_else(
            || {
                thrustc_logging::print_critical_error(
                    LoggingType::Error,
                    &format!("Unknown base file name '{}'.", path.display()),
                );
            },
            |name| name.to_string_lossy().to_string(),
        );

        if let Ok(canonicalized_path) = path.canonicalize() {
            path = canonicalized_path;
        }

        let content: String = thrustc_reader::get_file_source_code(&path);

        self.options
            .add_compilation_unit(name, path, content, base_name);
    }

    fn handle_unknown_argument(&mut self, arg: &str) {
        if self.position.at_external() {
            if self.options.get_llvm_backend().is_full_jit() {
                self.options
                    .get_mut_llvm_backend()
                    .get_mut_jit_config()
                    .add_argument(arg.to_string());

                return;
            } else {
                self.options
                    .get_mut_linking_compilers_configuration()
                    .add_argument(arg.to_string());

                return;
            }
        }

        thrustc_logging::print_critical_error(
            LoggingType::Error,
            &format!("Unknown argument: \"{}\".", arg),
        );
    }
}

impl CommandLine {
    fn parse_sanitizer_config(&self, spec: &str) -> (bool, bool) {
        let splitted: std::str::Split<'_, &str> = spec.split(";");

        let mut bounds: bool = false;
        let mut coverage: bool = false;

        for config in splitted {
            let (b, c) = match config {
                "bounds" => (true, false),
                "coverage" => (false, true),

                any => {
                    self.report_error(&format!("Invalid sanitizer modificator: '{}'.", any));
                }
            };

            bounds = bounds || b;
            coverage = coverage || c;
        }

        (bounds, coverage)
    }

    #[inline]
    fn parse_specific_abi(&self, abi: &str) -> thrustc_abi::SpecificABI {
        match abi.to_lowercase().as_str() {
            "system-v" => thrustc_abi::SpecificABI::SystemV,
            "cuda" => thrustc_abi::SpecificABI::NvidiaCuda,

            any => {
                self.report_error(&format!("Unknown specific ABI: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_sanitizer(&self, sanitizer: &str) -> Sanitizer {
        let config: SanitizerConfiguration = SanitizerConfiguration::new();

        match sanitizer {
            "address" => Sanitizer::Address(config),
            "hwaddress" => Sanitizer::Hwaddress(config),
            "memory" => Sanitizer::Memory(config),
            "thread" => Sanitizer::Thread(config),
            "memtag" => Sanitizer::Memtag(config),

            any => {
                self.report_error(&format!("Invalid sanitizer: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_denormal_floating_point_behavior(
        &self,
        approach: &str,
    ) -> (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) {
        let parts: Vec<&str> = approach.split(',').map(|s| s.trim()).collect();

        match parts.as_slice() {
            [out_str, in_str] => (
                self.map_single_strategy_denormal_floating_point_behavior(out_str),
                self.map_single_strategy_denormal_floating_point_behavior(in_str),
            ),
            [single_str] => {
                let mode: DenormalFloatingPointBehavior =
                    self.map_single_strategy_denormal_floating_point_behavior(single_str);
                (mode, mode)
            }
            _ => {
                self.report_error(&format!(
                    "Invalid denormal floating-point calculation approach: '{}'.",
                    approach
                ));
            }
        }
    }

    #[inline]
    fn parse_denormal_floating_point_behavior_32_bit_floating_point(
        &self,
        approach: &str,
    ) -> (
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ) {
        let parts: Vec<&str> = approach.split(',').map(|s| s.trim()).collect();

        match parts.as_slice() {
            [out_str, in_str] => (
                self.map_single_strategy_denormal_floating_point_32_bit_floating_point_behavior(
                    out_str,
                ),
                self.map_single_strategy_denormal_floating_point_32_bit_floating_point_behavior(
                    in_str,
                ),
            ),
            [single_str] => {
                let mode: DenormalFloatingPointBehavior32BitFloatingPoint = self
                    .map_single_strategy_denormal_floating_point_32_bit_floating_point_behavior(
                        single_str,
                    );
                (mode, mode)
            }
            _ => {
                self.report_error(&format!(
                    "Invalid denormal floating-point calculation approach: '{}'.",
                    approach
                ));
            }
        }
    }

    #[inline]
    fn parse_symbol_linkage_strategy(&self, strategy: &str) -> SymbolLinkageMergeStrategy {
        match strategy {
            "any" => SymbolLinkageMergeStrategy::Any,
            "exact" => SymbolLinkageMergeStrategy::Exact,
            "large" => SymbolLinkageMergeStrategy::Large,
            "samesize" => SymbolLinkageMergeStrategy::SameSize,
            "noduplicates" => SymbolLinkageMergeStrategy::NoDuplicates,

            any => {
                self.report_error(&format!(
                    "Unknown symbol linkage merge strategy: '{}'.",
                    any
                ));
            }
        }
    }

    #[inline]
    fn parse_stop_compilation_phase_at(&self, phase: &str) -> CompilationPhase {
        match phase.to_lowercase().as_str() {
            "lexing" => CompilationPhase::Lexer,
            "parsing" => CompilationPhase::Parser,
            "scope-analysis" => CompilationPhase::Scoper,
            "ast-verification" => CompilationPhase::AstVerifier,
            "type-checking" => CompilationPhase::TypeChecker,
            "general-analysis" => CompilationPhase::GeneralAnalyzer,
            "attribute-checking" => CompilationPhase::AttributeChecker,
            "linter" => CompilationPhase::Linter,
            "compiler-intrinsic-checking" => {
                if self.options.llvm() {
                    CompilationPhase::LLVMIntrinsicChecker
                } else {
                    CompilationPhase::None
                }
            }
            "compiler-callconventions-checking" => {
                if self.options.llvm() {
                    CompilationPhase::LLVMIntrinsicChecker
                } else {
                    CompilationPhase::None
                }
            }
            "codegen" => {
                if self.options.llvm() {
                    CompilationPhase::LLVMCodegen
                } else {
                    CompilationPhase::None
                }
            }

            any => {
                self.report_error(&format!("Unknown compilation phase: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_dwarf_version(&self, dwarf: &str) -> DwarfVersion {
        match dwarf.to_lowercase().as_str() {
            "v4" => DwarfVersion::V4,
            "v5" => DwarfVersion::V5,

            any => {
                self.report_error(&format!("Unknown Dwarf version: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_optimization_level(&self, opt: &str) -> ThrustOptimization {
        match opt {
            "O0" => ThrustOptimization::None,
            "O1" => ThrustOptimization::Low,
            "O2" => ThrustOptimization::Mid,
            "O3" => ThrustOptimization::High,
            "Os" => ThrustOptimization::Size,
            "Oz" => ThrustOptimization::Zize,

            any => {
                self.report_error(&format!("Unknown optimization level: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_printable_emission_value(&self, emit: &str) -> PrintableUnit {
        match emit {
            "llvm-ir" => PrintableUnit::LLVMIR,
            "unopt-llvm-ir" => PrintableUnit::UnOptLLVMIR,
            "asm" => PrintableUnit::Assembly,
            "unopt-asm" => PrintableUnit::UnOptAssembly,
            "pretty-tokens" => PrintableUnit::TokensPretty,
            "tokens" => PrintableUnit::Tokens,
            "unchecked-pretty-ast" => PrintableUnit::UnCheckedAstPretty,
            "pretty-ast" => PrintableUnit::AstPretty,
            "unchecked-ast" => PrintableUnit::UnCheckedAst,
            "ast" => PrintableUnit::Ast,

            any => {
                self.report_error(&format!("Unknown print option: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_emission_value(&self, emit: &str) -> EmitableUnit {
        match emit {
            "llvm-bc" => EmitableUnit::LLVMBitcode,
            "llvm-ir" => EmitableUnit::LLVMIR,
            "asm" => EmitableUnit::Assembly,
            "unopt-llvm-bc" => EmitableUnit::UnOptLLVMBitcode,
            "unopt-llvm-ir" => EmitableUnit::UnOptLLVMIR,
            "unopt-asm" => EmitableUnit::UnOptAssembly,
            "obj" => EmitableUnit::Object,
            "unchecked-pretty-ast" => EmitableUnit::UnCheckedAstPretty,
            "unchecked-ast" => EmitableUnit::UnCheckedAst,
            "pretty-ast" => EmitableUnit::AstPretty,
            "ast" => EmitableUnit::Ast,
            "pretty-tokens" => EmitableUnit::TokensPretty,
            "tokens" => EmitableUnit::Tokens,

            any => {
                self.report_error(&format!("Unknown emission option: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_reloc_mode(&self, reloc: &str) -> ThrustRelocMode {
        match reloc {
            "dynamic-no-pic" => ThrustRelocMode::DynamicNoPic,
            "pic" => ThrustRelocMode::PIC,
            "static" => ThrustRelocMode::Static,

            any => {
                self.report_error(&format!("Unknown reloc mode: '{}'.", any));
            }
        }
    }

    #[inline]
    fn parse_code_model(&self, model: &str) -> ThrustCodeModel {
        match model {
            "small" => ThrustCodeModel::Small,
            "medium" => ThrustCodeModel::Medium,
            "large" => ThrustCodeModel::Large,
            "kernel" => ThrustCodeModel::Kernel,

            any => {
                self.report_error(&format!("Unknown code model: '{}'.", any));
            }
        }
    }
}

impl CommandLine {
    fn map_single_strategy_denormal_floating_point_behavior(
        &self,
        strategy: &str,
    ) -> DenormalFloatingPointBehavior {
        match strategy {
            "IEEE" => DenormalFloatingPointBehavior::IEEE,
            "preserve-sign-signature" => DenormalFloatingPointBehavior::PreserveSignSignature,
            "transform-to-positive-zero" => DenormalFloatingPointBehavior::AsPositiveZero,
            "dynamic" => DenormalFloatingPointBehavior::Dynamic,
            any => {
                self.report_error(&format!(
                    "Unknown denormal floating-point calculation approach: '{}'.",
                    any
                ));
            }
        }
    }

    fn map_single_strategy_denormal_floating_point_32_bit_floating_point_behavior(
        &self,
        strategy: &str,
    ) -> DenormalFloatingPointBehavior32BitFloatingPoint {
        match strategy {
            "IEEE" => DenormalFloatingPointBehavior32BitFloatingPoint::IEEE,
            "preserve-sign-signature" => {
                DenormalFloatingPointBehavior32BitFloatingPoint::PreserveSignSignature
            }
            "transform-to-positive-zero" => {
                DenormalFloatingPointBehavior32BitFloatingPoint::AsPositiveZero
            }
            "dynamic" => DenormalFloatingPointBehavior32BitFloatingPoint::Dynamic,
            any => {
                self.report_error(&format!(
                    "Unknown denormal floating-point calculation approach: '{}'.",
                    any
                ));
            }
        }
    }
}

impl CommandLine {
    fn validate_llvm_required(&self, arg: &str) {
        if !self.options.llvm() {
            self.report_error(&format!(
                "Can't use '{}' without '-llvm-backend' flag previously.",
                arg
            ));
        }
    }

    fn validate_jit_required(&self, arg: &str) {
        if !self.options.get_llvm_backend().is_full_jit() {
            self.report_error(&format!(
                "Can't use '{}' without '-jit' flag previously.",
                arg
            ));
        }
    }

    fn validate_aot_is_enable(&self, arg: &str) {
        if self.options.get_llvm_backend().is_full_jit() {
            self.report_error(&format!(
                "Can't use '{}' if the '-jit' flag was enabled previously.",
                arg
            ));
        }
    }

    fn validate_print_required(&self, arg: &str) {
        if !self.options.it_will_print() {
            self.report_error(&format!(
                "Can't use '{}' without '-print' flag previously.",
                arg
            ));
        }
    }

    fn validate_not_gcc_active(&self) {
        if self
            .options
            .get_linking_compilers_configuration()
            .get_use_gcc()
        {
            self.report_error("Can't use '-clang-link' flag.");
        }
    }

    fn validate_not_clang_active(&self) {
        if self
            .options
            .get_linking_compilers_configuration()
            .get_use_clang()
        {
            self.report_error("Can't use '-gcc-link' flag.");
        }
    }

    fn validate_sanitizer_required(&self, arg: &str) {
        if !self.options.get_llvm_backend().get_sanitizer().is_none() {
            self.report_error(&format!(
                "Can't use '{}' without '--satinizer' flag previously.",
                arg
            ));
        }
    }

    fn validate_compiler_path(&mut self, path: &Path) -> bool {
        let path_str: String = path.to_string_lossy().to_string();

        if let Some(&result) = self.validation_cache.get(&path_str) {
            return result;
        }

        let exists: bool = path.exists() || std::process::Command::new(path).output().is_ok();

        self.validation_cache.insert(path_str, exists);

        exists
    }
}

impl CommandLine {
    fn is_thrust_file(&self, path: &str) -> bool {
        let path: PathBuf = PathBuf::from(path);

        if let Some(extension) = path.extension() {
            if path.exists()
                && path.is_file()
                && (thrustc_constants::COMPILER_OWN_FILE_EXTENSIONS
                    .contains(&extension.to_str().unwrap_or("unknown")))
            {
                return true;
            }
        }

        false
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.current >= self.args.len()
    }
}

impl CommandLine {
    #[inline]
    pub fn get_options(&self) -> &CompilerOptions {
        &self.options
    }

    #[inline]
    pub fn get_mut_options(&mut self) -> &mut CompilerOptions {
        &mut self.options
    }
}

#[inline]
pub fn set_up_basic() {
    colored::control::set_override(false);
}

#[inline]
pub fn set_up_ansi(options: &CompilerOptions) {
    if options.need_ansi_colors() {
        #[cfg(target_os = "windows")]
        {
            colored::control::set_virtual_terminal(true);
            colored::control::set_override(true);
        }

        #[cfg(target_os = "linux")]
        {
            colored::control::set_override(true);
        }
    }
}

#[inline]
pub fn report_compile_time(
    options: &CompilerOptions,
    start_time: std::time::Instant,
    compile_time: CompileTime,
) -> ! {
    let failed: bool = compile_time.0;

    let thrustc_time_ms: f64 = compile_time.1.as_secs_f64() * 1000.0;
    let frontend_time_ms: f64 = compile_time.2.as_secs_f64() * 1000.0;
    let backend_time_ms: f64 = compile_time.3.as_secs_f64() * 1000.0;
    let linking_time_ms: f64 = compile_time.4.as_secs_f64() * 1000.0;

    let backend_identifier: &str = if options.llvm() { "LLVM" } else { "GCC" };

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!(
            "\n{}\n{}\n\n{}\n{}\n{}\n{}\n{}\n",
            "─────────────────────────────────────────"
                .custom_color((141, 141, 142))
                .bold(),
            "Compile time report".custom_color((141, 141, 142)).bold(),
            format_args!("Thrust Compiler: {}ms", thrustc_time_ms),
            format_args!("Thrust Compiler - Frontend: {}ms", frontend_time_ms),
            format_args!(
                "Thrust Compiler - Backend ({}): {}ms",
                backend_identifier, backend_time_ms
            ),
            format_args!("Linking: {}ms", linking_time_ms),
            "─────────────────────────────────────────"
                .custom_color((141, 141, 142))
                .bold(),
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        &format!(
            "\r{} {}",
            "Finished".custom_color((141, 141, 142)).bold(),
            format!(
                "{}.{}s",
                start_time.elapsed().as_secs(),
                start_time.elapsed().as_millis()
            )
            .custom_color((141, 141, 142))
            .bold(),
        ),
    );

    if failed {
        std::process::exit(thrustc_constants::FAILURE_CODE);
    } else {
        std::process::exit(thrustc_constants::SUCCESFUL_CODE);
    }
}
