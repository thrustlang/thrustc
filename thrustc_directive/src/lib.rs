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

use std::borrow::Cow;

use thrustc_backends::llvm::debug::DwarfVersion;
use thrustc_backends::llvm::passes::LLVMModificatorPasses;
use thrustc_backends::llvm::{
    DenormalFloatingPointBehavior, DenormalFloatingPointBehavior32BitFloatingPoint, Sanitizer,
    SanitizerConfiguration, SymbolLinkageMergeStrategy,
};
use thrustc_backends::{ThrustCodeModel, ThrustOptimization, ThrustRelocMode};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationPhase, CompilerOptions, EmitableUnit, PrintableUnit};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

#[derive(Debug, Default)]
pub struct FileDirectives {
    pub optimization: Option<ThrustOptimization>,
    pub reloc_model: Option<ThrustRelocMode>,
    pub code_model: Option<ThrustCodeModel>,
    pub debug: bool,
    pub debug_for_inlining: bool,
    pub debug_for_profiling: bool,
    pub dwarf_version: Option<DwarfVersion>,
    pub stop_at: Option<CompilationPhase>,
    pub emit: Vec<EmitableUnit>,
    pub print: Vec<PrintableUnit>,
    pub stack_protector: bool,
    pub symbol_linkage_strategy: Option<SymbolLinkageMergeStrategy>,
    pub denormal_fp: Option<(DenormalFloatingPointBehavior, DenormalFloatingPointBehavior)>,
    pub denormal_fp_32: Option<(
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    )>,
    pub sanitizer: Option<Sanitizer>,
    pub no_sanitize_bounds: bool,
    pub no_sanitize_coverage: bool,
    pub disable_all_sanitizers: bool,
    pub disable_frame_pointer: bool,
    pub disable_uwtable: bool,
    pub disable_direct_access_external_data: bool,
    pub disable_rtlib_got: bool,
    pub disable_safe_trapping_math: bool,
    pub disable_safe_math: bool,
    pub disable_default_optimizations: bool,
    pub opt_passes: Option<String>,
    pub modificator_opt_passes: Option<Vec<LLVMModificatorPasses>>,
    pub warnings_to_disable: Vec<CompilationIssueCode>,
    pub disable_all_warnings: bool,
    pub no_obfuscate_archive_names: bool,
    pub no_obfuscate_ir: bool,
}

#[derive(Debug)]
pub struct FileOptions<'global, 'local> {
    global: &'global CompilerOptions,
    local: &'local FileDirectives,
}

impl<'global, 'local> FileOptions<'global, 'local> {
    #[inline]
    pub fn new(global: &'global CompilerOptions, local: &'local FileDirectives) -> Self {
        Self { global, local }
    }
}

impl<'global, 'local> FileOptions<'global, 'local> {
    #[inline]
    pub fn global(&self) -> &'global CompilerOptions {
        self.global
    }

    #[inline]
    pub fn directives(&self) -> &'local FileDirectives {
        self.local
    }

    #[inline]
    pub fn stop_compilation_at(&self, phase: CompilationPhase) -> bool {
        self.local.stop_at.map_or_else(
            || self.global.stop_compilation_at(phase),
            |local| local == phase,
        )
    }

    #[inline]
    pub fn contains_emitable(&self, emit: EmitableUnit) -> bool {
        self.local.emit.contains(&emit) || self.global.contains_emitable(emit)
    }

    #[inline]
    pub fn contains_printable(&self, printable: PrintableUnit) -> bool {
        self.local.print.contains(&printable) || self.global.contains_printable(printable)
    }

    #[inline]
    pub fn disable_all_warnings(&self) -> bool {
        self.local.disable_all_warnings || self.global.disable_all_warnings()
    }

    #[inline]
    pub fn optimization(&self) -> ThrustOptimization {
        self.local
            .optimization
            .unwrap_or_else(|| self.global.get_llvm_backend().get_optimization())
    }

    #[inline]
    pub fn reloc_model(&self) -> inkwell::targets::RelocMode {
        self.local.reloc_model.map_or_else(
            || self.global.get_llvm_backend().get_reloc_mode(),
            ThrustRelocMode::to_llvm,
        )
    }

    #[inline]
    pub fn code_model(&self) -> inkwell::targets::CodeModel {
        self.local.code_model.map_or_else(
            || self.global.get_llvm_backend().get_code_model(),
            ThrustCodeModel::to_llvm,
        )
    }

    #[inline]
    pub fn sanitizer(&self) -> Sanitizer {
        let mut sanitizer: Sanitizer = self
            .local
            .sanitizer
            .unwrap_or_else(|| *self.global.get_llvm_backend().get_sanitizer());

        match &mut sanitizer {
            Sanitizer::Address(config)
            | Sanitizer::Hwaddress(config)
            | Sanitizer::Memory(config)
            | Sanitizer::Thread(config)
            | Sanitizer::Memtag(config) => {
                if self.local.no_sanitize_bounds {
                    config.set_nosanitize_bounds(true);
                }

                if self.local.no_sanitize_coverage {
                    config.set_nosanitize_coverage(true);
                }
            }
            Sanitizer::None => {}
        }

        sanitizer
    }

    #[inline]
    pub fn symbol_linkage_strategy(&self) -> SymbolLinkageMergeStrategy {
        self.local
            .symbol_linkage_strategy
            .unwrap_or_else(|| *self.global.get_llvm_backend().get_symbol_linkage_strategy())
    }

    #[inline]
    pub fn denormal_fp(&self) -> (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) {
        self.local
            .denormal_fp
            .unwrap_or_else(|| *self.global.get_llvm_backend().get_denormal_fp_behavior())
    }

    #[inline]
    pub fn denormal_fp_32(
        &self,
    ) -> (
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ) {
        self.local.denormal_fp_32.unwrap_or_else(|| {
            *self
                .global
                .get_llvm_backend()
                .get_denormal_fp_32_bits_behavior()
        })
    }

    #[inline]
    pub fn opt_passes(&self) -> &str {
        self.local
            .opt_passes
            .as_deref()
            .unwrap_or_else(|| self.global.get_llvm_backend().get_opt_passes())
    }

    #[inline]
    pub fn modificator_opt_passes(&self) -> &[LLVMModificatorPasses] {
        self.local
            .modificator_opt_passes
            .as_deref()
            .unwrap_or_else(|| self.global.get_llvm_backend().get_modificator_passes())
    }

    #[inline]
    pub fn omit_default_optimizations(&self) -> bool {
        self.local.disable_default_optimizations || self.global.omit_default_optimizations()
    }

    #[inline]
    pub fn disable_all_sanitizers(&self) -> bool {
        self.local.disable_all_sanitizers
            || self.global.get_llvm_backend().get_disable_all_sanitizers()
    }

    #[inline]
    pub fn stack_protector(&self) -> bool {
        self.local.stack_protector || self.global.get_llvm_backend().needs_stack_protector()
    }

    #[inline]
    pub fn omit_frame_pointer(&self) -> bool {
        self.local.disable_frame_pointer || self.global.get_llvm_backend().omit_frame_pointer()
    }

    #[inline]
    pub fn omit_uwtable(&self) -> bool {
        self.local.disable_uwtable || self.global.get_llvm_backend().omit_uwtable()
    }

    #[inline]
    pub fn omit_direct_access_external_data(&self) -> bool {
        self.local.disable_direct_access_external_data
            || self
                .global
                .get_llvm_backend()
                .omit_direct_access_external_data()
    }

    #[inline]
    pub fn omit_rtlib_got(&self) -> bool {
        self.local.disable_rtlib_got || self.global.get_llvm_backend().omit_rtlibusegot()
    }

    #[inline]
    pub fn omit_trapping_math(&self) -> bool {
        self.local.disable_safe_trapping_math || self.global.get_llvm_backend().omit_trapping_math()
    }

    #[inline]
    pub fn disable_safe_math(&self) -> bool {
        self.local.disable_safe_math || self.global.get_llvm_backend().has_disable_safe_math()
    }

    #[inline]
    pub fn debug_mode(&self) -> bool {
        self.local.debug
            || self
                .global
                .get_llvm_backend()
                .get_debug_config()
                .is_debug_mode()
    }

    #[inline]
    pub fn debug_for_inlining(&self) -> bool {
        self.local.debug_for_inlining
            || self
                .global
                .get_llvm_backend()
                .get_debug_config()
                .need_split_debug_inlining()
    }

    #[inline]
    pub fn debug_for_profiling(&self) -> bool {
        self.local.debug_for_profiling
            || self
                .global
                .get_llvm_backend()
                .get_debug_config()
                .need_debug_info_for_profiling()
    }

    #[inline]
    pub fn dwarf_version(&self) -> u64 {
        self.local.dwarf_version.map_or_else(
            || {
                self.global
                    .get_llvm_backend()
                    .get_debug_config()
                    .get_dwarf_version()
            },
            |version| match version {
                DwarfVersion::V4 => 4,
                DwarfVersion::V5 => 5,
            },
        )
    }

    #[inline]
    pub fn obfuscate_archive_names(&self) -> bool {
        !self.local.no_obfuscate_archive_names && self.global.need_obfuscate_archive_names()
    }

    #[inline]
    pub fn obfuscate_ir(&self) -> bool {
        !self.local.no_obfuscate_ir && self.global.need_obfuscate_ir()
    }
}

pub fn parse_warning_codes(value: &str) -> Result<Vec<CompilationIssueCode>, String> {
    let mut warnings: Vec<CompilationIssueCode> = Vec::new();

    for warning in value.split(';') {
        let warning: &str = warning.trim();

        if warning.is_empty() {
            continue;
        }

        let code: CompilationIssueCode = CompilationIssueCode::parse(warning)
            .map_err(|_| format!("Invalid warning to disable: '{}'.", warning))?;

        if !code.is_warning() {
            return Err(format!("'{}' is not a warning code.", warning));
        }

        if !warnings.contains(&code) {
            warnings.push(code);
        }
    }

    if warnings.is_empty() {
        return Err("Expected at least one warning code.".into());
    }

    Ok(warnings)
}

pub fn apply_file_directives(tokens: &[Token]) -> Result<FileDirectives, CompilationIssue> {
    let mut directives: FileDirectives = FileDirectives::default();

    for (index, token) in tokens.iter().enumerate() {
        if token.get_type() != TokenType::Directive {
            continue;
        }

        let Some(spec_token) = tokens.get(index + 1) else {
            continue;
        };

        if spec_token.get_type() != TokenType::CString {
            continue;
        }

        if let Err(message) = self::apply_directive(spec_token.get_lexeme(), &mut directives) {
            return Err(self::invalid_directive(message, spec_token.get_span()));
        }
    }

    Ok(directives)
}

pub fn apply_directive(spec: &str, directives: &mut FileDirectives) -> Result<(), String> {
    let spec: &str = spec.trim();

    if !spec.starts_with('-') {
        return Err("The directive must contain a compiler flag beginning with '-'.".into());
    }

    let (flag, value): (&str, Option<&str>) = spec
        .split_once('=')
        .or_else(|| spec.split_once(':'))
        .map_or((spec, None), |(flag, value)| {
            (flag.trim(), Some(value.trim()))
        });

    if self::is_global_only(flag) {
        return Err(format!(
            "Flag '{}' is global and cannot be used as a file directive.",
            flag
        ));
    }

    match flag {
        "-opt" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.optimization = Some(self::parse_optimization(value)?);
        }

        "-reloc-model" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.reloc_model = Some(self::parse_reloc_model(value)?);
        }
        "-code-model" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.code_model = Some(self::parse_code_model(value)?);
        }
        "-dbg" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.debug = true;
        }
        "-dbg-for-inlining" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.debug_for_inlining = true;
        }
        "-dbg-for-profiling" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.debug_for_profiling = true;
        }
        "-dbg-dwarf-version" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.dwarf_version = Some(self::parse_dwarf(value)?);
        }
        "-stop-at" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.stop_at = Some(self::parse_phase(value)?);
        }
        "-emit" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.emit.push(self::parse_emit(value)?);
        }
        "-print" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.print.push(self::parse_print(value)?);
        }
        "--stack-protector" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.stack_protector = true;
        }
        "--symbol-linkage-strategy" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.symbol_linkage_strategy = Some(self::parse_linkage_strategy(value)?);
        }
        "--denormal-floating-point-behavior" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.denormal_fp = Some(self::parse_pair(value, self::parse_denormal_value)?);
        }
        "--denormal-floating-point-32-bits-behavior" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.denormal_fp_32 =
                Some(self::parse_pair(value, self::parse_denormal_32_value)?);
        }
        "--sanitizer" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.sanitizer = Some(self::parse_sanitizer(value)?);
        }
        "--no-sanitize" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            self::apply_no_sanitize(directives, value)?;
        }
        "--disable-all-sanitizers" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_all_sanitizers = true;
        }
        "--disable-frame-pointer" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_frame_pointer = true;
        }
        "--disable-uwtable" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_uwtable = true;
        }
        "--disable-direct-access-external-data" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_direct_access_external_data = true;
        }
        "--disable-rtlib-got" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_rtlib_got = true;
        }
        "--disable-safe-trapping-math" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_safe_trapping_math = true;
        }
        "--disable-safe-math" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_safe_math = true;
        }
        "--disable-default-optimizations" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_default_optimizations = true;
        }
        "--opt-passes" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.opt_passes = Some(value.to_string());
        }
        "--modificator-opt-passes" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            directives.modificator_opt_passes =
                Some(LLVMModificatorPasses::into_llvm_modificator_passes(value));
        }
        "--disable-warnings" => {
            let value: &str = value
                .filter(|value| !value.is_empty())
                .ok_or_else(|| format!("Directive flag '{}' expects a value using '='.", flag))?;

            for code in self::parse_warning_codes(value)? {
                if !directives.warnings_to_disable.contains(&code) {
                    directives.warnings_to_disable.push(code);
                }
            }
        }
        "--disable-all-warnings" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.disable_all_warnings = true;
        }
        "--no-obfuscate-archive-names" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.no_obfuscate_archive_names = true;
        }
        "--no-obfuscate-ir" => {
            if value.is_some() {
                return Err(format!(
                    "Directive flag '{}' does not accept a value.",
                    flag
                ));
            }

            directives.no_obfuscate_ir = true;
        }

        _ => return Err(format!("Unknown directive flag '{}'.", flag)),
    }

    Ok(())
}

pub fn combine_warnings_to_disable<'options>(
    options: &'options FileOptions<'_, '_>,
) -> Cow<'options, [CompilationIssueCode]> {
    if options.disable_all_warnings() {
        return Cow::Borrowed(CompilationIssueCode::ALL_WARNING_CODES);
    }

    if options.local.warnings_to_disable.is_empty() {
        return Cow::Borrowed(options.global.get_warnings_to_disable());
    }

    let mut warnings: Vec<CompilationIssueCode> = options.global.get_warnings_to_disable().to_vec();

    for code in &options.local.warnings_to_disable {
        if !warnings.contains(code) {
            warnings.push(*code);
        }
    }

    Cow::Owned(warnings)
}

fn invalid_directive(message: String, span: Span) -> CompilationIssue {
    CompilationIssue::Error(
        CompilationIssueCode::E0054,
        message,
        "Use a file-scoped compiler flag with the same spelling and value format as the CLI."
            .into(),
        None,
        span,
    )
}

fn is_global_only(flag: &str) -> bool {
    matches!(
        flag,
        "-h" | "--help"
            | "-v"
            | "--version"
            | "--explain"
            | "--dump-compiler-version"
            | "--print-targets"
            | "--print-host-target-triple"
            | "--print-supported-cpus"
            | "--print-opt-passes"
            | "-build-dir"
            | "-tools-dir"
            | "-mode"
            | "-std"
            | "-std-version"
            | "-target"
            | "-target-triple"
            | "--target-triple-darwin-variant"
            | "-cpu"
            | "-cpu-features"
            | "-cpu-enable-features"
            | "-cpu-disable-features"
            | "--disable-all-cpu-features"
            | "-abi"
            | "--disable-abi"
            | "-macos-version"
            | "-ios-version"
            | "-cuda-version"
            | "-jit"
            | "-jit-libc"
            | "-jit-link"
            | "-jit-entry"
            | "-jit-args"
            | "-link-with-clang"
            | "-link-with-gcc"
            | "-cc-args"
            | "--debug-clang-command"
            | "--debug-gcc-command"
            | "-L"
            | "-l"
            | "-no-executable"
            | "-o"
            | "-output"
            | "-debug-linker-command"
            | "--copy-output-to-clipboard"
            | "--export-compiler-errors"
            | "--export-compiler-warnings"
            | "--export-diagnostics-path"
            | "--clean-exported-diagnostics"
            | "--clean-build"
            | "--clean-tokens"
            | "--clean-assembler"
            | "--clean-llvm-ir"
            | "--clean-llvm-bitcode"
            | "--clean-objects"
            | "--enable-ansi-color"
    )
}

fn parse_optimization(value: &str) -> Result<ThrustOptimization, String> {
    match value {
        "O0" => Ok(ThrustOptimization::None),
        "O1" => Ok(ThrustOptimization::Low),
        "O2" => Ok(ThrustOptimization::Mid),
        "O3" => Ok(ThrustOptimization::High),
        "Os" => Ok(ThrustOptimization::Size),
        "Oz" => Ok(ThrustOptimization::Zize),
        _ => Err(format!("Unknown optimization level: '{}'.", value)),
    }
}

fn parse_reloc_model(value: &str) -> Result<ThrustRelocMode, String> {
    match value {
        "dynamic-no-pic" => Ok(ThrustRelocMode::DynamicNoPic),
        "pic" => Ok(ThrustRelocMode::PIC),
        "static" => Ok(ThrustRelocMode::Static),
        _ => Err(format!("Unknown relocation model: '{}'.", value)),
    }
}

fn parse_code_model(value: &str) -> Result<ThrustCodeModel, String> {
    match value {
        "small" => Ok(ThrustCodeModel::Small),
        "medium" => Ok(ThrustCodeModel::Medium),
        "large" => Ok(ThrustCodeModel::Large),
        "kernel" => Ok(ThrustCodeModel::Kernel),
        _ => Err(format!("Unknown code model: '{}'.", value)),
    }
}

fn parse_dwarf(value: &str) -> Result<DwarfVersion, String> {
    match value.to_lowercase().as_str() {
        "v4" => Ok(DwarfVersion::V4),
        "v5" => Ok(DwarfVersion::V5),
        _ => Err(format!("Unknown Dwarf version: '{}'.", value)),
    }
}

fn parse_phase(value: &str) -> Result<CompilationPhase, String> {
    match value.to_lowercase().as_str() {
        "lexing" => Ok(CompilationPhase::Lexer),
        "parsing" => Ok(CompilationPhase::Parser),
        "scope-analysis" => Ok(CompilationPhase::Scoper),
        "ast-verification" => Ok(CompilationPhase::AstVerifier),
        "type-checking" => Ok(CompilationPhase::TypeChecker),
        "general-analysis" => Ok(CompilationPhase::GeneralAnalyzer),
        "attribute-checking" => Ok(CompilationPhase::AttributeChecker),
        "linter" => Ok(CompilationPhase::Linter),
        "compiler-intrinsic-checking" => Ok(CompilationPhase::LLVMIntrinsicChecker),
        "compiler-callconventions-checking" => Ok(CompilationPhase::LLVMCallConventionChecker),
        "codegen" => Ok(CompilationPhase::LLVMCodegen),
        _ => Err(format!("Unknown compilation phase: '{}'.", value)),
    }
}

fn parse_emit(value: &str) -> Result<EmitableUnit, String> {
    match value {
        "llvm-bc" => Ok(EmitableUnit::LLVMBitcode),
        "llvm-ir" => Ok(EmitableUnit::LLVMIR),
        "asm" => Ok(EmitableUnit::Assembly),
        "unopt-llvm-bc" => Ok(EmitableUnit::UnOptLLVMBitcode),
        "unopt-llvm-ir" => Ok(EmitableUnit::UnOptLLVMIR),
        "unopt-asm" => Ok(EmitableUnit::UnOptAssembly),
        "obj" => Ok(EmitableUnit::Object),
        "unchecked-pretty-ast" => Ok(EmitableUnit::UnCheckedAstPretty),
        "unchecked-ast" => Ok(EmitableUnit::UnCheckedAst),
        "pretty-ast" => Ok(EmitableUnit::AstPretty),
        "ast" => Ok(EmitableUnit::Ast),
        "pretty-tokens" => Ok(EmitableUnit::TokensPretty),
        "tokens" => Ok(EmitableUnit::Tokens),
        _ => Err(format!("Unknown emission option: '{}'.", value)),
    }
}

fn parse_print(value: &str) -> Result<PrintableUnit, String> {
    match value {
        "llvm-ir" => Ok(PrintableUnit::LLVMIR),
        "unopt-llvm-ir" => Ok(PrintableUnit::UnOptLLVMIR),
        "asm" => Ok(PrintableUnit::Assembly),
        "unopt-asm" => Ok(PrintableUnit::UnOptAssembly),
        "pretty-tokens" => Ok(PrintableUnit::TokensPretty),
        "tokens" => Ok(PrintableUnit::Tokens),
        "unchecked-pretty-ast" => Ok(PrintableUnit::UnCheckedAstPretty),
        "pretty-ast" => Ok(PrintableUnit::AstPretty),
        "unchecked-ast" => Ok(PrintableUnit::UnCheckedAst),
        "ast" => Ok(PrintableUnit::Ast),
        _ => Err(format!("Unknown print option: '{}'.", value)),
    }
}

fn parse_linkage_strategy(value: &str) -> Result<SymbolLinkageMergeStrategy, String> {
    match value {
        "any" => Ok(SymbolLinkageMergeStrategy::Any),
        "exact" => Ok(SymbolLinkageMergeStrategy::Exact),
        "large" => Ok(SymbolLinkageMergeStrategy::Large),
        "samesize" => Ok(SymbolLinkageMergeStrategy::SameSize),
        "noduplicates" => Ok(SymbolLinkageMergeStrategy::NoDuplicates),
        _ => Err(format!(
            "Unknown symbol linkage merge strategy: '{}'.",
            value
        )),
    }
}

fn parse_pair<T: Copy>(
    value: &str,
    parser: fn(&str) -> Result<T, String>,
) -> Result<(T, T), String> {
    let parts: Vec<&str> = value.split(',').map(str::trim).collect();

    match parts.as_slice() {
        [single] => parser(single).map(|parsed| (parsed, parsed)),
        [output, input] => Ok((parser(output)?, parser(input)?)),
        _ => Err(format!(
            "Expected one or two comma-separated values, got '{}'.",
            value
        )),
    }
}

fn parse_denormal_value(value: &str) -> Result<DenormalFloatingPointBehavior, String> {
    match value {
        "IEEE" => Ok(DenormalFloatingPointBehavior::IEEE),
        "preserve-sign-signature" => Ok(DenormalFloatingPointBehavior::PreserveSignSignature),
        "transform-to-positive-zero" => Ok(DenormalFloatingPointBehavior::AsPositiveZero),
        "dynamic" => Ok(DenormalFloatingPointBehavior::Dynamic),
        _ => Err(format!(
            "Unknown denormal floating-point behavior: '{}'.",
            value
        )),
    }
}

fn parse_denormal_32_value(
    value: &str,
) -> Result<DenormalFloatingPointBehavior32BitFloatingPoint, String> {
    match value {
        "IEEE" => Ok(DenormalFloatingPointBehavior32BitFloatingPoint::IEEE),
        "preserve-sign-signature" => {
            Ok(DenormalFloatingPointBehavior32BitFloatingPoint::PreserveSignSignature)
        }
        "transform-to-positive-zero" => {
            Ok(DenormalFloatingPointBehavior32BitFloatingPoint::AsPositiveZero)
        }
        "dynamic" => Ok(DenormalFloatingPointBehavior32BitFloatingPoint::Dynamic),
        _ => Err(format!(
            "Unknown 32-bit denormal floating-point behavior: '{}'.",
            value
        )),
    }
}

fn parse_sanitizer(value: &str) -> Result<Sanitizer, String> {
    let config: SanitizerConfiguration = SanitizerConfiguration::new();
    
    match value {
        "address" => Ok(Sanitizer::Address(config)),
        "hwaddress" => Ok(Sanitizer::Hwaddress(config)),
        "memory" => Ok(Sanitizer::Memory(config)),
        "thread" => Ok(Sanitizer::Thread(config)),
        "memtag" => Ok(Sanitizer::Memtag(config)),
        _ => Err(format!("Invalid sanitizer: '{}'.", value)),
    }
}

fn apply_no_sanitize(directives: &mut FileDirectives, value: &str) -> Result<(), String> {
    for item in value.split(';').map(str::trim) {
        match item {
            "bounds" => directives.no_sanitize_bounds = true,
            "coverage" => directives.no_sanitize_coverage = true,
            _ => return Err(format!("Invalid sanitizer modifier: '{}'.", item)),
        }
    }

    Ok(())
}
