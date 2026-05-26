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

pub mod cpu;
pub mod debug;
pub mod info;
pub mod jit;
pub mod passes;
pub mod target;

use crate::{
    llvm::cpu::LLVMTargetCPU, llvm::debug::DebugConfiguration, llvm::jit::JITConfiguration,
    llvm::passes::LLVMModificatorPasses, llvm::target::LLVMTarget,
};

use super::{ThrustCodeModel, ThrustOptimization, ThrustRelocMode};

use inkwell::targets::{CodeModel, RelocMode, TargetMachine};
use thrustc_llvm_target_triple::LLVMTargetTriple;

#[derive(Debug)]
pub struct LLVMBackend {
    target: LLVMTarget,
    target_cpu: LLVMTargetCPU,

    optimization: ThrustOptimization,
    reloc_mode: RelocMode,
    code_model: CodeModel,

    symbol_linkage_extrategy: SymbolLinkageMergeStrategy,
    denormal_fp_behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior),
    denormal_fp_32_bits_behavior: (
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ),

    stack_protector: bool,
    sanitizer: Sanitizer,
    dbg_config: DebugConfiguration,

    modificator_passes: Vec<LLVMModificatorPasses>,
    opt_passes: String,

    omit_frame_pointer: bool,
    omit_uwtable: bool,
    omit_direct_access_external_data: bool,
    omit_rtlibusegot: bool,
    omit_trapping_math: bool,

    disable_all_sanitizers: bool,
    disable_safe_math: bool,

    needs_jit: bool,
    jit_config: JITConfiguration,
}

impl LLVMBackend {
    pub fn new() -> Self {
        let arch: String = TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .split("-")
            .collect::<Vec<_>>()
            .first()
            .map_or("generic", |v| v)
            .to_string();

        let target_triple: inkwell::targets::TargetTriple = TargetMachine::get_default_triple();

        let normalized_target_triple: LLVMTargetTriple = LLVMTargetTriple::new(
            TargetMachine::get_default_triple()
                .as_str()
                .to_string_lossy()
                .to_string(),
        );

        Self {
            target: LLVMTarget {
                arch,
                target_triple,
                normalized_target_triple,
                target_triple_darwin_variant: None,
                macos_version: None,
                ios_version: None,
                cuda_version: None,
            },
            target_cpu: LLVMTargetCPU {
                target_cpu: TargetMachine::get_host_cpu_name().to_string(),
                target_cpu_features: TargetMachine::get_host_cpu_features().to_string(),
            },
            optimization: ThrustOptimization::None,
            reloc_mode: RelocMode::Default,
            code_model: CodeModel::Default,

            symbol_linkage_extrategy: SymbolLinkageMergeStrategy::Any,
            denormal_fp_behavior: (
                DenormalFloatingPointBehavior::IEEE,
                DenormalFloatingPointBehavior::IEEE,
            ),
            denormal_fp_32_bits_behavior: (
                DenormalFloatingPointBehavior32BitFloatingPoint::IEEE,
                DenormalFloatingPointBehavior32BitFloatingPoint::IEEE,
            ),

            stack_protector: false,
            sanitizer: Sanitizer::None,
            dbg_config: DebugConfiguration::new(),

            modificator_passes: Vec::with_capacity(u8::MAX as usize),
            opt_passes: String::with_capacity(u8::MAX as usize),

            omit_frame_pointer: false,
            omit_uwtable: false,
            omit_direct_access_external_data: false,
            omit_rtlibusegot: false,
            omit_trapping_math: false,

            disable_all_sanitizers: false,
            disable_safe_math: false,

            needs_jit: false,
            jit_config: JITConfiguration::new(),
        }
    }
}

impl LLVMBackend {
    #[inline]
    pub fn get_reloc_mode(&self) -> RelocMode {
        self.reloc_mode
    }

    #[inline]
    pub fn get_code_model(&self) -> CodeModel {
        self.code_model
    }

    #[inline]
    pub fn get_optimization(&self) -> ThrustOptimization {
        self.optimization
    }

    #[inline]
    pub fn get_target(&self) -> &LLVMTarget {
        &self.target
    }

    #[inline]
    pub fn get_target_cpu(&self) -> &LLVMTargetCPU {
        &self.target_cpu
    }

    #[inline]
    pub fn get_opt_passes(&self) -> &str {
        self.opt_passes.as_str()
    }

    #[inline]
    pub fn get_modificator_passes(&self) -> &[LLVMModificatorPasses] {
        &self.modificator_passes
    }

    #[inline]
    pub fn get_jit_config(&self) -> &JITConfiguration {
        &self.jit_config
    }

    #[inline]
    pub fn get_debug_config(&self) -> &DebugConfiguration {
        &self.dbg_config
    }

    #[inline]
    pub fn get_sanitizer(&self) -> &Sanitizer {
        &self.sanitizer
    }

    #[inline]
    pub fn get_symbol_linkage_strategy(&self) -> &SymbolLinkageMergeStrategy {
        &self.symbol_linkage_extrategy
    }

    #[inline]
    pub fn get_denormal_fp_behavior(
        &self,
    ) -> &(DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) {
        &self.denormal_fp_behavior
    }

    #[inline]
    pub fn get_denormal_fp_32_bits_behavior(
        &self,
    ) -> &(
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ) {
        &self.denormal_fp_32_bits_behavior
    }

    #[inline]
    pub fn get_disable_all_sanitizers(&self) -> bool {
        self.disable_all_sanitizers
    }

    #[inline]
    pub fn omit_frame_pointer(&self) -> bool {
        self.omit_frame_pointer
    }

    #[inline]
    pub fn omit_uwtable(&self) -> bool {
        self.omit_uwtable
    }

    #[inline]
    pub fn omit_direct_access_external_data(&self) -> bool {
        self.omit_direct_access_external_data
    }

    #[inline]
    pub fn omit_rtlibusegot(&self) -> bool {
        self.omit_rtlibusegot
    }

    #[inline]
    pub fn omit_trapping_math(&self) -> bool {
        self.omit_trapping_math
    }

    #[inline]
    pub fn needs_stack_protector(&self) -> bool {
        self.stack_protector
    }

    #[inline]
    pub fn is_full_jit(&self) -> bool {
        self.needs_jit
    }

    #[inline]
    pub fn has_disable_safe_math(&self) -> bool {
        self.disable_safe_math
    }
}

impl LLVMBackend {
    #[inline]
    pub fn get_mut_target(&mut self) -> &mut LLVMTarget {
        &mut self.target
    }

    #[inline]
    pub fn get_mut_target_cpu(&mut self) -> &mut LLVMTargetCPU {
        &mut self.target_cpu
    }

    #[inline]
    pub fn get_mut_jit_config(&mut self) -> &mut JITConfiguration {
        &mut self.jit_config
    }

    #[inline]
    pub fn get_mut_debug_config(&mut self) -> &mut DebugConfiguration {
        &mut self.dbg_config
    }

    #[inline]
    pub fn get_mut_sanitizer(&mut self) -> &mut Sanitizer {
        &mut self.sanitizer
    }
}

impl LLVMBackend {
    #[inline]
    pub fn set_optimization(&mut self, opt: ThrustOptimization) {
        self.optimization = opt;
    }

    #[inline]
    pub fn set_reloc_mode(&mut self, reloc_mode: ThrustRelocMode) {
        self.reloc_mode = reloc_mode.to_llvm();
    }

    #[inline]
    pub fn set_code_model(&mut self, code_model: ThrustCodeModel) {
        self.code_model = code_model.to_llvm();
    }

    #[inline]
    pub fn set_sanitizer(&mut self, sanitizer: Sanitizer) {
        self.sanitizer = sanitizer;
    }

    #[inline]
    pub fn set_symbol_linkage_strategy(&mut self, strategy: SymbolLinkageMergeStrategy) {
        self.symbol_linkage_extrategy = strategy;
    }

    #[inline]
    pub fn set_denormal_fp_behavior(
        &mut self,
        behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior),
    ) {
        self.denormal_fp_behavior = behavior;
    }

    #[inline]
    pub fn set_denormal_fp_32_bits_behavior(
        &mut self,
        behavior: (
            DenormalFloatingPointBehavior32BitFloatingPoint,
            DenormalFloatingPointBehavior32BitFloatingPoint,
        ),
    ) {
        self.denormal_fp_32_bits_behavior = behavior;
    }

    #[inline]
    pub fn set_omit_frame_pointer(&mut self) {
        self.omit_frame_pointer = true;
    }

    #[inline]
    pub fn set_omit_uwtable(&mut self) {
        self.omit_uwtable = true;
    }

    #[inline]
    pub fn set_omit_direct_access_external_data(&mut self) {
        self.omit_direct_access_external_data = true;
    }

    #[inline]
    pub fn set_omit_rtlibusegot(&mut self) {
        self.omit_rtlibusegot = true;
    }

    #[inline]
    pub fn set_omit_trapping_math(&mut self) {
        self.omit_trapping_math = true;
    }

    #[inline]
    pub fn set_opt_passes(&mut self, opt_passes: String) {
        self.opt_passes = opt_passes;
    }

    #[inline]
    pub fn set_modificator_passes(&mut self, modificator_passes: Vec<LLVMModificatorPasses>) {
        self.modificator_passes = modificator_passes;
    }

    #[inline]
    pub fn set_disable_all_sanitizers(&mut self) {
        self.disable_all_sanitizers = true;
    }

    #[inline]
    pub fn set_jit(&mut self, value: bool) {
        self.needs_jit = value;
    }

    #[inline]
    pub fn set_stack_protector(&mut self) {
        self.stack_protector = true;
    }

    #[inline]
    pub fn set_disable_safe_math(&mut self) {
        self.disable_safe_math = true;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SanitizerConfiguration {
    nosanitize_bounds: bool,
    nosanitize_coverage: bool,
}

impl SanitizerConfiguration {
    #[inline]
    pub fn new() -> Self {
        Self {
            nosanitize_bounds: false,
            nosanitize_coverage: false,
        }
    }
}

impl SanitizerConfiguration {
    #[inline]
    pub fn has_nosanitize_bounds(&self) -> bool {
        self.nosanitize_bounds
    }

    #[inline]
    pub fn has_nosanitize_coverage(&self) -> bool {
        self.nosanitize_coverage
    }
}

impl SanitizerConfiguration {
    #[inline]
    pub fn set_nosanitize_bounds(&mut self, value: bool) {
        self.nosanitize_bounds = value;
    }

    #[inline]
    pub fn set_nosanitize_coverage(&mut self, value: bool) {
        self.nosanitize_coverage = value;
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolLinkageMergeStrategy {
    Any,
    Exact,
    Large,
    SameSize,
    NoDuplicates,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DenormalFloatingPointBehavior {
    IEEE,
    PreserveSignSignature,
    AsPositiveZero,
    Dynamic,
}

impl DenormalFloatingPointBehavior {
    pub fn as_llvm_repr(&self) -> &'static str {
        match self {
            Self::IEEE => "ieee",
            Self::PreserveSignSignature => "preserve-sign",
            Self::AsPositiveZero => "positive-zero",
            Self::Dynamic => "dynamic",
        }
    }

    pub fn is_default(
        behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior),
    ) -> bool {
        matches!(
            behavior,
            (
                DenormalFloatingPointBehavior::IEEE,
                DenormalFloatingPointBehavior::IEEE
            )
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DenormalFloatingPointBehavior32BitFloatingPoint {
    IEEE,
    PreserveSignSignature,
    AsPositiveZero,
    Dynamic,
}

impl DenormalFloatingPointBehavior32BitFloatingPoint {
    pub fn as_llvm_repr(&self) -> &'static str {
        match self {
            Self::IEEE => "ieee",
            Self::PreserveSignSignature => "preserve-sign",
            Self::AsPositiveZero => "positive-zero",
            Self::Dynamic => "dynamic",
        }
    }

    pub fn is_default(
        behavior: (
            DenormalFloatingPointBehavior32BitFloatingPoint,
            DenormalFloatingPointBehavior32BitFloatingPoint,
        ),
    ) -> bool {
        matches!(
            behavior,
            (
                DenormalFloatingPointBehavior32BitFloatingPoint::IEEE,
                DenormalFloatingPointBehavior32BitFloatingPoint::IEEE
            )
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Sanitizer {
    Address(SanitizerConfiguration),
    Memory(SanitizerConfiguration),
    Thread(SanitizerConfiguration),
    Hwaddress(SanitizerConfiguration),
    Memtag(SanitizerConfiguration),

    None,
}

impl Sanitizer {
    #[inline]
    pub fn is_address(&self) -> bool {
        matches!(self, Sanitizer::Address(..))
    }

    #[inline]
    pub fn is_memory(&self) -> bool {
        matches!(self, Sanitizer::Memory(..))
    }

    #[inline]
    pub fn is_thread(&self) -> bool {
        matches!(self, Sanitizer::Thread(..))
    }

    #[inline]
    pub fn is_hwaddress(&self) -> bool {
        matches!(self, Sanitizer::Hwaddress(..))
    }

    #[inline]
    pub fn is_memtag(&self) -> bool {
        matches!(self, Sanitizer::Memtag(..))
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, Sanitizer::None)
    }
}
