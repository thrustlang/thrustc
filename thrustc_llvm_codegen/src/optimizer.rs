use std::borrow::Cow;
use std::ffi::CStr;
use std::ffi::CString;

use inkwell::attributes::Attribute;
use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::comdat::Comdat;
use inkwell::comdat::ComdatSelectionKind;
use inkwell::context::Context;
use inkwell::llvm_sys::comdat::LLVMGetOrInsertComdat;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicTypeEnum;
use inkwell::values::AsValueRef;
use inkwell::values::BasicValueEnum;
use inkwell::values::CallSiteValue;
use inkwell::values::FunctionValue;
use inkwell::values::GlobalValue;
use inkwell::values::InstructionOpcode;
use inkwell::values::InstructionValue;

use thrustc_options::CompilerOptions;
use thrustc_options::ThrustOptimization;
use thrustc_options::backends::llvm::DenormalFloatingPointBehavior;
use thrustc_options::backends::llvm::DenormalFloatingPointBehavior32BitFloatingPoint;
use thrustc_options::backends::llvm::Sanitizer;
use thrustc_options::backends::llvm::SymbolLinkageMergeStrategy;
use thrustc_options::backends::llvm::passes::LLVMModificatorPasses;

use crate::targettriple::LLVMTargetTriple;
use crate::utils;

#[derive(Debug)]
pub struct LLVMOptimizer<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    machine: &'a TargetMachine,
    config: LLVMOptimizationConfig,
    flags: LLVMOptimizerFlags,
    passes: LLVMOptimizerPasses<'ctx>,
}

impl<'a, 'ctx> LLVMOptimizer<'a, 'ctx> {
    pub fn new(
        module: &'a Module<'ctx>,
        context: &'ctx Context,
        machine: &'a TargetMachine,
        config: LLVMOptimizationConfig,
        flags: LLVMOptimizerFlags,
        passes: LLVMOptimizerPasses<'ctx>,
    ) -> Self {
        Self {
            module,
            context,
            machine,
            config,
            flags,
            passes,
        }
    }
}

impl LLVMOptimizer<'_, '_> {
    #[inline]
    pub fn optimize(&self) {
        let custom_passes: &str = self.get_passes().get_llvm_custom_passes();
        let machine: &TargetMachine = self.get_machine();
        let target_data: TargetData = machine.get_target_data();
        let options: PassBuilderOptions = self.create_passes_builder();
        let config: LLVMOptimizationConfig = self.get_config();
        let flags: &LLVMOptimizerFlags = self.get_flags();

        let module: &Module = self.get_module();
        let context: &Context = self.get_context();

        LLVMSanitizer::new(module, context, config, *flags).run();
        LLVMComdatApplier::new(module, context, config).run();

        if !self.get_flags().get_disable_default_opt()
            && !config.get_compiler_optimization().is_high_opt()
        {
            LLVMParameterOptimizer::new(module, context, target_data).run();
            LLVMFunctionOptimizer::new(module, context).run();
        }

        LLVMMachineSpecificFunctionOptimizer::new(module, context, config).run();

        if !custom_passes.is_empty() {
            if let Err(error) = self
                .get_module()
                .run_passes(custom_passes, machine, options)
            {
                thrustc_logging::print_warn(
                    thrustc_logging::LoggingType::Warning,
                    &format!(
                        "Some optimizations passes couldn't be performed because: '{}'.",
                        error
                    ),
                );
            }
        } else {
            match config.get_compiler_optimization() {
                ThrustOptimization::None => {}

                ThrustOptimization::Low => {
                    if let Err(error) =
                        self.get_module()
                            .run_passes("default<O1>", machine, options)
                    {
                        thrustc_logging::print_warn(
                            thrustc_logging::LoggingType::Warning,
                            &format!(
                                "Some optimizations passes couldn't be performed because: '{}'.",
                                error
                            ),
                        );
                    }
                }

                ThrustOptimization::Mid => {
                    if let Err(error) =
                        self.get_module()
                            .run_passes("default<O2>", machine, options)
                    {
                        thrustc_logging::print_warn(
                            thrustc_logging::LoggingType::Warning,
                            &format!(
                                "Some optimizations passes couldn't be performed because: '{}'.",
                                error
                            ),
                        );
                    }
                }

                ThrustOptimization::High => {
                    if let Err(error) =
                        self.get_module()
                            .run_passes("default<O3>", machine, options)
                    {
                        thrustc_logging::print_warn(
                            thrustc_logging::LoggingType::Warning,
                            &format!(
                                "Some optimizations passes couldn't be performed because: '{:?}'.",
                                error
                            ),
                        );
                    }
                }

                ThrustOptimization::Size => {
                    if let Err(error) =
                        self.get_module()
                            .run_passes("default<Os>", machine, options)
                    {
                        thrustc_logging::print_warn(
                            thrustc_logging::LoggingType::Warning,
                            &format!(
                                "Some optimizations passes couldn't be performed because: '{:?}'.",
                                error
                            ),
                        );
                    }
                }

                ThrustOptimization::Zize => {
                    if let Err(error) =
                        self.get_module()
                            .run_passes("default<Oz>", machine, options)
                    {
                        thrustc_logging::print_warn(
                            thrustc_logging::LoggingType::Warning,
                            &format!(
                                "Some optimizations passes couldn't be performed because: '{:?}'.",
                                error
                            ),
                        );
                    }
                }
            }
        }
    }
}

impl LLVMOptimizer<'_, '_> {
    fn create_passes_builder(&self) -> PassBuilderOptions {
        let passes_builder: PassBuilderOptions = PassBuilderOptions::create();

        self.get_passes()
            .get_llvm_modificator_passes()
            .iter()
            .for_each(|pass| match pass {
                LLVMModificatorPasses::LoopVectorization => {
                    passes_builder.set_loop_vectorization(true);
                }
                LLVMModificatorPasses::LoopUnroll => {
                    passes_builder.set_loop_unrolling(true);
                }
                LLVMModificatorPasses::LoopInterleaving => {
                    passes_builder.set_loop_interleaving(true);
                }
                LLVMModificatorPasses::LoopSimplifyVectorization => {
                    passes_builder.set_loop_slp_vectorization(true);
                }
                LLVMModificatorPasses::MergeFunctions => {
                    passes_builder.set_merge_functions(true);
                }
                LLVMModificatorPasses::CallGraphProfile => {
                    passes_builder.set_call_graph_profile(true);
                }
                LLVMModificatorPasses::ForgetAllScevInLoopUnroll => {
                    passes_builder.set_forget_all_scev_in_loop_unroll(true);
                }
                LLVMModificatorPasses::LicmMssaNoAccForPromotionCap(value) => {
                    passes_builder.set_licm_mssa_no_acc_for_promotion_cap(*value);
                }
                LLVMModificatorPasses::LicmMssaOptCap(value) => {
                    passes_builder.set_licm_mssa_opt_cap(*value);
                }
            });

        passes_builder
    }
}

impl<'a, 'ctx> LLVMOptimizer<'a, 'ctx> {
    #[inline]
    pub fn get_module(&self) -> &Module<'ctx> {
        self.module
    }

    #[inline]
    pub fn get_context(&self) -> &'ctx Context {
        self.context
    }

    #[inline]
    pub fn get_flags(&self) -> &LLVMOptimizerFlags {
        &self.flags
    }

    #[inline]
    pub fn get_passes(&self) -> &LLVMOptimizerPasses<'_> {
        &self.passes
    }

    #[inline]
    pub fn get_machine(&self) -> &TargetMachine {
        self.machine
    }

    #[inline]
    pub fn get_config(&self) -> LLVMOptimizationConfig {
        self.config
    }
}

impl LLVMOptimizer<'_, '_> {
    #[inline]
    pub fn is_optimizable(options: &CompilerOptions) -> bool {
        (!options.omit_default_optimizations()
            && options
                .get_llvm_backend_options()
                .get_optimization()
                .is_none_opt())
            || options
                .get_llvm_backend_options()
                .get_optimization()
                .is_high_opt()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMOptimizerPasses<'ctx> {
    custom_passes: &'ctx str,
    modicator_passes: &'ctx [LLVMModificatorPasses],
}

impl<'ctx> LLVMOptimizerPasses<'ctx> {
    pub fn new(custom_passes: &'ctx str, modicator_passes: &'ctx [LLVMModificatorPasses]) -> Self {
        Self {
            custom_passes,
            modicator_passes,
        }
    }
}

impl<'ctx> LLVMOptimizerPasses<'ctx> {
    #[inline]
    pub fn get_llvm_custom_passes(&self) -> &str {
        self.custom_passes
    }

    #[inline]
    pub fn get_llvm_modificator_passes(&self) -> &[LLVMModificatorPasses] {
        self.modicator_passes
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMOptimizerFlags {
    disable_default_opt: bool,
    disable_all_sanitizers: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMOptimizationConfig {
    compiler_optimization: ThrustOptimization,
    sanitizer: Sanitizer,
    symbol_linkage_strategy: SymbolLinkageMergeStrategy,
    denormal_fp_behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior),
    denormal_fp_32_bits_behavior: (
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ),
}

impl LLVMOptimizationConfig {
    #[inline]
    pub fn new(
        compiler_optimization: ThrustOptimization,
        sanitizer: Sanitizer,
        symbol_linkage_strategy: SymbolLinkageMergeStrategy,
        denormal_fp_behavior: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior),
        denormal_fp_32_bits_behavior: (
            DenormalFloatingPointBehavior32BitFloatingPoint,
            DenormalFloatingPointBehavior32BitFloatingPoint,
        ),
    ) -> Self {
        Self {
            compiler_optimization,
            sanitizer,
            symbol_linkage_strategy,
            denormal_fp_behavior,
            denormal_fp_32_bits_behavior,
        }
    }
}

impl LLVMOptimizationConfig {
    #[inline]
    pub fn get_compiler_optimization(&self) -> ThrustOptimization {
        self.compiler_optimization
    }

    #[inline]
    pub fn get_sanitizer(&self) -> Sanitizer {
        self.sanitizer
    }

    #[inline]
    pub fn get_symbol_linkage_strategy(&self) -> SymbolLinkageMergeStrategy {
        self.symbol_linkage_strategy
    }

    #[inline]
    pub fn get_denormal_fp_behavior(
        &self,
    ) -> (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) {
        self.denormal_fp_behavior
    }

    #[inline]
    pub fn get_denormal_fp_32_bits_behavior(
        &self,
    ) -> (
        DenormalFloatingPointBehavior32BitFloatingPoint,
        DenormalFloatingPointBehavior32BitFloatingPoint,
    ) {
        self.denormal_fp_32_bits_behavior
    }
}

impl LLVMOptimizerFlags {
    #[inline]
    pub fn new(disable_default_opt: bool, disable_all_sanitizers: bool) -> Self {
        Self {
            disable_default_opt,
            disable_all_sanitizers,
        }
    }
}

impl LLVMOptimizerFlags {
    #[inline]
    pub fn get_disable_default_opt(&self) -> bool {
        self.disable_default_opt
    }

    #[inline]
    pub fn get_disable_all_sanitizers(&self) -> bool {
        self.disable_all_sanitizers
    }
}

#[derive(Debug)]
pub struct LLVMFunctionOptimizer<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    function: Option<FunctionValue<'ctx>>,
    optimizations: Option<LLVMFunctionOptimizations>,
}

impl<'a, 'ctx> LLVMFunctionOptimizer<'a, 'ctx> {
    #[inline]
    pub fn new(module: &'a Module<'ctx>, context: &'ctx Context) -> Self {
        Self {
            module,
            context,
            function: None,
            optimizations: None,
        }
    }
}

impl<'a, 'ctx> LLVMFunctionOptimizer<'a, 'ctx> {
    pub fn run(&mut self) {
        let ordered_functions: Vec<FunctionValue<'_>> =
            utils::get_functions_by_ordered_calls(self.module.get_functions().collect());

        {
            for function in ordered_functions.iter() {
                self.visit_function_once(*function);
            }
        }
    }
}

impl<'a, 'ctx> LLVMFunctionOptimizer<'a, 'ctx> {
    fn visit_function_once(&mut self, function: FunctionValue<'ctx>) {
        self.set_function(function);

        if function.get_first_basic_block().is_none() {
            let mut optimizations: LLVMFunctionOptimizations = LLVMFunctionOptimizations::new();

            optimizations.set_uwtable(false);
            optimizations.set_nounwind(false);

            const CONDISERABLE_USAGE_FOR_AGGRESIVE_OPT: u16 = 156;

            let usage: usize = self
                .module
                .get_functions()
                .filter(|other_function| *other_function != function)
                .flat_map(|function| function.get_basic_block_iter())
                .flat_map(|basic_block| basic_block.get_instructions())
                .filter(|instr| instr.get_opcode() == InstructionOpcode::Call)
                .filter(|instr| {
                    let callsite: CallSiteValue<'_> =
                        unsafe { CallSiteValue::new(instr.as_value_ref()) };

                    callsite.get_called_fn_value() == function
                })
                .count();

            if usage > CONDISERABLE_USAGE_FOR_AGGRESIVE_OPT as usize {
                optimizations.set_nolazybind(true);
            }

            self.set_optimizations(optimizations);
            self.optimize_function();

            self.reset_optimizations_state();
            self.reset_function();
        } else {
            let mut optimizations: LLVMFunctionOptimizations = LLVMFunctionOptimizations::new();

            const MAX_OPT_INSTRUCTIONS_LEN: usize = 5;
            const CONSIDERABLE_BASIC_BLOCKS_LEN: usize = 5;
            const CONSIDERABLE_INSTRUCTIONS_LEN: usize = 250;

            let blocks_count: usize = function.get_basic_block_iter().count();
            let instructions_count: usize = function
                .get_basic_block_iter()
                .map(|basic_block| basic_block.get_instructions().count())
                .sum();

            let applicable_norecurse: bool = function
                .get_basic_block_iter()
                .flat_map(|bb| bb.get_instructions())
                .filter(|instr| instr.get_opcode() == InstructionOpcode::Call)
                .any(|instr| {
                    let callsite: CallSiteValue =
                        unsafe { CallSiteValue::new(instr.as_value_ref()) };
                    let called: FunctionValue = callsite.get_called_fn_value();

                    self.function.is_some_and(|current| current == called)
                });

            if MAX_OPT_INSTRUCTIONS_LEN > instructions_count {
                optimizations.set_inlinehint(true);
            } else if blocks_count >= CONSIDERABLE_BASIC_BLOCKS_LEN
                && instructions_count >= CONSIDERABLE_INSTRUCTIONS_LEN
            {
                optimizations.set_optsize(true);
            }

            if !applicable_norecurse {
                optimizations.set_nocurse(true);
            }

            let is_valid_memorynone: bool = function
                .get_basic_block_iter()
                .flat_map(|basic_block| basic_block.get_instructions())
                .all(|inst| match inst.get_opcode() {
                    InstructionOpcode::Load
                    | InstructionOpcode::Store
                    | InstructionOpcode::AtomicCmpXchg
                    | InstructionOpcode::AtomicRMW => false,

                    InstructionOpcode::Call => {
                        let call_site: CallSiteValue<'_> =
                            unsafe { CallSiteValue::new(inst.as_value_ref()) };
                        let called_func: FunctionValue<'_> = call_site.get_called_fn_value();

                        for attribute in called_func.attributes(AttributeLoc::Function) {
                            let memory_id: u32 = Attribute::get_named_enum_kind_id("memory");
                            let memory_attr: Attribute =
                                self.context.create_enum_attribute(memory_id, 0);

                            if attribute == memory_attr {
                                return true;
                            }
                        }

                        false
                    }

                    _ => true,
                });

            let is_valid_memoryread: bool = function
                .get_basic_block_iter()
                .flat_map(|bb| bb.get_instructions())
                .all(|inst| match inst.get_opcode() {
                    InstructionOpcode::Store
                    | InstructionOpcode::AtomicRMW
                    | InstructionOpcode::AtomicCmpXchg => false,

                    InstructionOpcode::Call => {
                        let call_site: CallSiteValue<'_> =
                            unsafe { CallSiteValue::new(inst.as_value_ref()) };
                        let called_func: FunctionValue<'_> = call_site.get_called_fn_value();

                        for attribute in called_func.attributes(AttributeLoc::Function) {
                            let memory_id: u32 = Attribute::get_named_enum_kind_id("memory");
                            let memory_attr: Attribute =
                                self.context.create_enum_attribute(memory_id, 1);

                            if attribute == memory_attr {
                                return true;
                            }
                        }

                        false
                    }

                    _ => true,
                });

            let is_valid_memorywrite: bool = function
                .get_basic_block_iter()
                .flat_map(|bb| bb.get_instructions())
                .all(|inst| match inst.get_opcode() {
                    InstructionOpcode::Load
                    | InstructionOpcode::AtomicRMW
                    | InstructionOpcode::AtomicCmpXchg => false,

                    InstructionOpcode::Call => {
                        let call_site: CallSiteValue<'_> =
                            unsafe { CallSiteValue::new(inst.as_value_ref()) };
                        let called_func: FunctionValue<'_> = call_site.get_called_fn_value();

                        for attribute in called_func.attributes(AttributeLoc::Function) {
                            let memory_id: u32 = Attribute::get_named_enum_kind_id("memory");
                            let memory_attr: Attribute =
                                self.context.create_enum_attribute(memory_id, 2);

                            if attribute == memory_attr {
                                return true;
                            }
                        }

                        false
                    }

                    _ => true,
                });

            if is_valid_memorynone {
                optimizations.set_memorynone(true);
            } else if is_valid_memoryread {
                optimizations.set_memoryread(true);
            } else if is_valid_memorywrite {
                optimizations.set_memorywrite(true);
            }

            self.set_optimizations(optimizations);
            self.optimize_function();

            self.reset_optimizations_state();
            self.reset_function();
        }
    }
}

impl<'a, 'ctx> LLVMFunctionOptimizer<'a, 'ctx> {
    fn optimize_function(&mut self) {
        if let Some(optimizations) = self.get_optimizations() {
            if optimizations.has_nolazybind() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("nonlazybind");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_inlinehint() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("inlinehint");

                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_norecurse() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("norecurse");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_optsize() {
                let optsize_id: u32 = Attribute::get_named_enum_kind_id("optsize");
                let minsize_id: u32 = Attribute::get_named_enum_kind_id("minsize");

                let optsize: Attribute = self.context.create_enum_attribute(optsize_id, 0);
                let minsize: Attribute = self.context.create_enum_attribute(minsize_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, optsize);
                    function.add_attribute(AttributeLoc::Function, minsize);
                }
            }

            if optimizations.has_nounwind() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("nounwind");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_uwtable() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("uwtable");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 1);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_sspstrong() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("sspstrong");

                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_memorynone() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("memory");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_memoryread() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("memory");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 1);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }

            if optimizations.has_memorywrite() {
                let kind_id: u32 = Attribute::get_named_enum_kind_id("memory");
                let attribute: Attribute = self.context.create_enum_attribute(kind_id, 2);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, attribute);
                }
            }
        }
    }
}

impl<'a, 'ctx> LLVMFunctionOptimizer<'a, 'ctx> {
    #[inline]
    fn set_function(&mut self, function: FunctionValue<'ctx>) {
        self.function = Some(function);
    }

    #[inline]
    fn reset_function(&mut self) {
        self.function = None;
    }

    #[inline]
    fn set_optimizations(&mut self, optimizations: LLVMFunctionOptimizations) {
        self.optimizations = Some(optimizations);
    }

    #[inline]
    fn reset_optimizations_state(&mut self) {
        self.optimizations = None;
    }
}

impl LLVMFunctionOptimizer<'_, '_> {
    #[inline]
    fn get_optimizations(&self) -> Option<LLVMFunctionOptimizations> {
        self.optimizations
    }
}

#[derive(Debug, Clone, Copy)]
struct LLVMFunctionOptimizations {
    norecurse: bool,
    nounwind: bool,
    inlinehint: bool,
    optsize: bool,
    uwtable: bool,
    sspstrong: bool,
    memorynone: bool,
    memoryread: bool,
    memorywrite: bool,
    nolazybind: bool,
}

impl LLVMFunctionOptimizations {
    #[inline]
    fn new() -> Self {
        Self {
            norecurse: false,
            nounwind: true,
            inlinehint: false,
            optsize: false,
            uwtable: true,
            sspstrong: true,
            memorynone: false,
            memoryread: false,
            memorywrite: false,
            nolazybind: false,
        }
    }
}

impl LLVMFunctionOptimizations {
    #[inline]
    pub fn set_nocurse(&mut self, value: bool) {
        self.norecurse = value;
    }

    #[inline]
    pub fn set_inlinehint(&mut self, value: bool) {
        self.inlinehint = value;
    }

    #[inline]
    pub fn set_optsize(&mut self, value: bool) {
        self.optsize = value;
    }

    #[inline]
    pub fn set_memorynone(&mut self, value: bool) {
        self.memorynone = value;
    }

    #[inline]
    pub fn set_memoryread(&mut self, value: bool) {
        self.memoryread = value;
    }

    #[inline]
    pub fn set_memorywrite(&mut self, value: bool) {
        self.memorywrite = value;
    }

    #[inline]
    pub fn set_nounwind(&mut self, value: bool) {
        self.nounwind = value;
    }

    #[inline]
    pub fn set_uwtable(&mut self, value: bool) {
        self.uwtable = value;
    }

    #[inline]
    pub fn set_nolazybind(&mut self, value: bool) {
        self.nolazybind = value;
    }
}

impl LLVMFunctionOptimizations {
    #[inline]
    pub fn has_norecurse(&self) -> bool {
        self.norecurse
    }

    #[inline]
    pub fn has_nounwind(&self) -> bool {
        self.nounwind
    }

    #[inline]
    pub fn has_inlinehint(&self) -> bool {
        self.inlinehint
    }

    #[inline]
    pub fn has_optsize(&self) -> bool {
        self.optsize
    }

    #[inline]
    pub fn has_uwtable(&self) -> bool {
        self.uwtable
    }

    #[inline]
    pub fn has_sspstrong(&self) -> bool {
        self.sspstrong
    }

    #[inline]
    pub fn has_memorynone(&self) -> bool {
        self.memorynone
    }

    #[inline]
    pub fn has_memoryread(&self) -> bool {
        self.memoryread
    }

    #[inline]
    pub fn has_memorywrite(&self) -> bool {
        self.memorywrite
    }

    #[inline]
    pub fn has_nolazybind(&self) -> bool {
        self.nolazybind
    }
}

#[derive(Debug)]
pub struct LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    function: Option<FunctionValue<'ctx>>,
    optimizations: Option<LLVMMachineSpecificFunctionOptimizations>,
    config: LLVMOptimizationConfig,
}

impl<'a, 'ctx> LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    #[inline]
    pub fn new(
        module: &'a Module<'ctx>,
        context: &'ctx Context,
        config: LLVMOptimizationConfig,
    ) -> Self {
        Self {
            module,
            context,
            function: None,
            optimizations: None,
            config,
        }
    }
}

impl<'a, 'ctx> LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    pub fn run(&mut self) {
        let ordered_functions: Vec<FunctionValue<'_>> =
            utils::get_functions_by_ordered_calls(self.module.get_functions().collect());

        {
            for function in ordered_functions.iter() {
                self.visit_function_once(*function);
            }
        }
    }
}

impl<'a, 'ctx> LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    fn visit_function_once(&mut self, function: FunctionValue<'ctx>) {
        self.set_function(function);

        if function.get_first_basic_block().is_none() {
            self.reset_function();
        } else {
            let mut optimizations: LLVMMachineSpecificFunctionOptimizations =
                LLVMMachineSpecificFunctionOptimizations::new();
            let mut state_data: LLVMMachineSpecificFunctionStateData =
                LLVMMachineSpecificFunctionStateData::new();

            {
                self.previsit_function(function, &mut state_data);

                for basic_block in function.get_basic_block_iter() {
                    self.visit_block(basic_block, &mut state_data);
                }
            }

            self.optimize_function(&mut optimizations, &state_data);

            self.reset_function();
        }
    }

    fn previsit_function(
        &self,
        function: FunctionValue,
        state_data: &mut LLVMMachineSpecificFunctionStateData,
    ) {
        for parameter in function.get_param_iter() {
            if parameter.get_type().is_float_type() {
                state_data.set_has_floating_point(true);
            }
        }

        if let Some(return_type) = function.get_type().get_return_type() {
            if return_type.is_float_type() {
                state_data.set_has_floating_point(true);
            }
        }
    }

    fn visit_block(
        &mut self,
        basic_block: BasicBlock,
        state_data: &mut LLVMMachineSpecificFunctionStateData,
    ) {
        {
            for instruction in basic_block.get_instructions() {
                self.visit_instruction(instruction, state_data);
            }
        }
    }

    fn visit_instruction(
        &mut self,
        instruction: InstructionValue,
        state_data: &mut LLVMMachineSpecificFunctionStateData,
    ) {
        fn is_float_involved(instruction: &InstructionValue) -> bool {
            if instruction.get_type().is_float_type() {
                return true;
            }

            for i in 0..instruction.get_num_operands() {
                if let Some(op) = instruction.get_operand(i) {
                    if let Some(value) = op.left() {
                        let ty: BasicTypeEnum<'_> = value.get_type();

                        if ty.is_float_type() {
                            return true;
                        }
                    }
                }
            }

            false
        }

        match instruction.get_opcode() {
            InstructionOpcode::FAdd => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FSub => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FMul => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FDiv => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FRem => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FNeg => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FCmp => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::SIToFP => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::UIToFP => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FPToSI => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FPToUI => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FPTrunc => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::FPExt => {
                state_data.set_has_floating_point(true);
            }
            InstructionOpcode::Phi => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::Select => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::Load => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::Store => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::ExtractElement => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::InsertElement => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::ShuffleVector => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::ExtractValue => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }
            InstructionOpcode::InsertValue => {
                if is_float_involved(&instruction) {
                    state_data.set_has_floating_point(true);
                }
            }

            _ => (),
        }
    }
}

impl<'a, 'ctx> LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    fn optimize_function(
        &mut self,
        optimizations: &mut LLVMMachineSpecificFunctionOptimizations,
        state_data: &LLVMMachineSpecificFunctionStateData,
    ) {
        let has_floating_point: bool = state_data.has_floating_point();

        if !DenormalFloatingPointBehavior::is_default(self.config.get_denormal_fp_behavior())
            && has_floating_point
        {
            optimizations.set_denormal_fp_behavior(true);
        }

        if !DenormalFloatingPointBehavior32BitFloatingPoint::is_default(
            self.config.get_denormal_fp_32_bits_behavior(),
        ) && has_floating_point
        {
            optimizations.set_denormal_fp_32_bits_behavior(true);
        }

        if optimizations.has_denormal_fp_32_bits_behavior() {
            let behavior_tuple: (
                DenormalFloatingPointBehavior32BitFloatingPoint,
                DenormalFloatingPointBehavior32BitFloatingPoint,
            ) = self.config.get_denormal_fp_32_bits_behavior();

            let denormal_fp_math: String = match behavior_tuple {
                (out, in_) if out == in_ => in_.as_llvm_repr().to_string(),
                (out, in_) => format!("{},{}", out.as_llvm_repr(), in_.as_llvm_repr()),
            };

            let attribute: Attribute = self
                .context
                .create_string_attribute("denormal-fp-math-f32", &denormal_fp_math);

            if let Some(function) = self.function {
                function.add_attribute(AttributeLoc::Function, attribute);
            }
        }

        if optimizations.has_denormal_fp_behavior() {
            let behavior_tuple: (DenormalFloatingPointBehavior, DenormalFloatingPointBehavior) =
                self.config.get_denormal_fp_behavior();

            let denormal_fp_math: String = match behavior_tuple {
                (out, in_) if out == in_ => in_.as_llvm_repr().to_string(),
                (out, in_) => format!("{},{}", out.as_llvm_repr(), in_.as_llvm_repr()),
            };

            let attribute: Attribute = self
                .context
                .create_string_attribute("denormal-fp-math", &denormal_fp_math);

            if let Some(function) = self.function {
                function.add_attribute(AttributeLoc::Function, attribute);
            }
        }
    }
}

impl<'a, 'ctx> LLVMMachineSpecificFunctionOptimizer<'a, 'ctx> {
    #[inline]
    fn set_function(&mut self, function: FunctionValue<'ctx>) {
        self.function = Some(function);
    }

    #[inline]
    fn reset_function(&mut self) {
        self.function = None;
    }
}

impl LLVMMachineSpecificFunctionOptimizer<'_, '_> {
    #[inline]
    fn get_optimizations(&self) -> Option<LLVMMachineSpecificFunctionOptimizations> {
        self.optimizations
    }
}

#[derive(Debug, Clone, Copy)]
struct LLVMMachineSpecificFunctionOptimizations {
    denormal_fp_behavior: bool,
    denormal_fp_32_bits_behavior: bool,
}

impl LLVMMachineSpecificFunctionOptimizations {
    #[inline]
    pub fn new() -> Self {
        Self {
            denormal_fp_behavior: false,
            denormal_fp_32_bits_behavior: false,
        }
    }
}

impl LLVMMachineSpecificFunctionOptimizations {
    #[inline]
    pub fn set_denormal_fp_behavior(&mut self, value: bool) {
        self.denormal_fp_behavior = value;
    }

    #[inline]
    pub fn set_denormal_fp_32_bits_behavior(&mut self, value: bool) {
        self.denormal_fp_32_bits_behavior = value;
    }
}

impl LLVMMachineSpecificFunctionOptimizations {
    #[inline]
    pub fn has_denormal_fp_behavior(&self) -> bool {
        self.denormal_fp_behavior
    }

    #[inline]
    pub fn has_denormal_fp_32_bits_behavior(&self) -> bool {
        self.denormal_fp_32_bits_behavior
    }
}

#[derive(Debug, Clone, Copy)]
struct LLVMMachineSpecificFunctionStateData {
    has_floating_point_anywhere: bool,
}

impl LLVMMachineSpecificFunctionStateData {
    #[inline]
    pub fn new() -> Self {
        Self {
            has_floating_point_anywhere: false,
        }
    }
}

impl LLVMMachineSpecificFunctionStateData {
    #[inline]
    pub fn set_has_floating_point(&mut self, value: bool) {
        self.has_floating_point_anywhere = value
    }

    #[inline]
    pub fn has_floating_point(&self) -> bool {
        self.has_floating_point_anywhere
    }
}

#[derive(Debug)]
pub struct LLVMComdatApplier<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    config: LLVMOptimizationConfig,
}

impl<'a, 'ctx> LLVMComdatApplier<'a, 'ctx> {
    #[inline]
    pub fn new(
        module: &'a Module<'ctx>,
        context: &'ctx Context,
        config: LLVMOptimizationConfig,
    ) -> Self {
        Self {
            module,
            context,
            config,
        }
    }
}

impl<'a, 'ctx> LLVMComdatApplier<'a, 'ctx> {
    pub fn run(&self) {
        let target: LLVMTargetTriple = LLVMTargetTriple::new(&self.module.get_triple());

        if !target.is_xcoff_object_format() && !target.is_object_format_mach_o() {
            
            let same_name: Vec<(GlobalValue, FunctionValue)> = self
                .module
                .get_globals()
                .filter_map(|global| {
                    let global_name: Cow<'_, str> = utils::clean_llvm_name(global.get_name());
    
                    let global_is_linkage_external: bool = matches!(
                        global.get_linkage(),
                        Linkage::External
                            | Linkage::DLLExport
                            | Linkage::WeakAny
                            | Linkage::WeakODR
                            | Linkage::Common
                            | Linkage::ExternalWeak
                    );
    
                    self.module
                        .get_functions()
                        .find(|func| {
                            let function_name: Cow<'_, str> = utils::clean_llvm_name(func.get_name());
    
                            let function_is_linkage_external: bool = matches!(
                                global.get_linkage(),
                                Linkage::External
                                    | Linkage::DLLExport
                                    | Linkage::WeakAny
                                    | Linkage::WeakODR
                                    | Linkage::Common
                                    | Linkage::ExternalWeak
                            );
    
                            global_name == function_name
                                && global_is_linkage_external
                                && function_is_linkage_external
                        })
                        .map(|func| (global, func))
                })
                .collect();
    
            {
                let mut merge_strategy: ComdatSelectionKind =
                    match self.config.get_symbol_linkage_strategy() {
                        SymbolLinkageMergeStrategy::Any => ComdatSelectionKind::Any,
                        SymbolLinkageMergeStrategy::Exact => ComdatSelectionKind::ExactMatch,
                        SymbolLinkageMergeStrategy::Large => ComdatSelectionKind::Largest,
                        SymbolLinkageMergeStrategy::SameSize => ComdatSelectionKind::SameSize,
                        SymbolLinkageMergeStrategy::NoDuplicates => ComdatSelectionKind::NoDuplicates,
                    };
    
                if target.get_arch().contains("wasm") && merge_strategy != ComdatSelectionKind::Any {
                    thrustc_logging::print_warn(
                        thrustc_logging::LoggingType::Warning,
                        "WebAssembly target only support the any mode for the symbol linkage strategy!",
                    );
    
                    merge_strategy = ComdatSelectionKind::Any;
                }

                if target.is_object_format_elf() && merge_strategy != ComdatSelectionKind::Any && merge_strategy != ComdatSelectionKind::NoDuplicates {
                    thrustc_logging::print_warn(
                        thrustc_logging::LoggingType::Warning,
                        "ELF-based target only support the any and noduplicates modes for the symbol linkage strategy!",
                    );
                    
                    merge_strategy = ComdatSelectionKind::Any;
                }
    
                for (gl, func) in same_name.iter().rev() {
                    let cleaned: Cow<'_, str> = utils::clean_llvm_name(gl.get_name());
    
                    let c_string_str: CString =
                        CString::new(cleaned.as_ref()).unwrap_or_else(|error| {
                            thrustc_logging::print_warn(
                                thrustc_logging::LoggingType::Warning,
                                &format!(
                                    "Failed to prepare the object-matching linkage configurator for the upcoming binding phase due '{}'.",
                                    error
                                ),
                            );
    
                            CString::default()
                        });
    
                    let c_str: &CStr = c_string_str.as_c_str();
    
                    let comdat: Comdat = unsafe {
                        Comdat::new(LLVMGetOrInsertComdat(
                            self.module.as_mut_ptr(),
                            c_str.as_ptr(),
                        ))
                    };
    
                    comdat.set_selection_kind(merge_strategy);
    
                    gl.set_comdat(comdat);
                    func.as_global_value().set_comdat(comdat);
                }
            }
        }

    }
}

#[derive(Debug)]
pub struct LLVMSanitizer<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,

    function: Option<FunctionValue<'ctx>>,
    optimization: Option<LLVMSanitizerOptimization>,
    config: LLVMOptimizationConfig,
    flags: LLVMOptimizerFlags,
}

impl<'a, 'ctx> LLVMSanitizer<'a, 'ctx> {
    #[inline]
    pub fn new(
        module: &'a Module<'ctx>,
        context: &'ctx Context,
        config: LLVMOptimizationConfig,
        flags: LLVMOptimizerFlags,
    ) -> Self {
        Self {
            module,
            context,
            function: None,
            optimization: None,
            config,
            flags,
        }
    }
}

impl<'a, 'ctx> LLVMSanitizer<'a, 'ctx> {
    pub fn run(&mut self) {
        let config: LLVMOptimizationConfig = self.get_config();
        let flags: LLVMOptimizerFlags = self.get_flags();

        let test_optimization_instance: LLVMSanitizerOptimization =
            LLVMSanitizerOptimization::new(config, flags);

        if test_optimization_instance.is_neither() {
            return;
        }

        {
            for function in self.module.get_functions() {
                self.visit_function_once(function);
            }
        }
    }
}

impl<'a, 'ctx> LLVMSanitizer<'a, 'ctx> {
    fn visit_function_once(&mut self, function: FunctionValue<'ctx>) {
        self.set_function(function);

        let config: LLVMOptimizationConfig = self.get_config();
        let flags: LLVMOptimizerFlags = self.get_flags();

        let optimization: LLVMSanitizerOptimization = LLVMSanitizerOptimization::new(config, flags);

        self.set_optimizations(optimization);
        self.apply();
        self.reset_optimizations_state();

        self.reset_function();
    }
}

impl<'a, 'ctx> LLVMSanitizer<'a, 'ctx> {
    fn apply(&mut self) {
        if let Some(optimizations) = self.get_optimizations() {
            if optimizations.has_disable_all_sanitizers() {
                let disable_all_sanitizers_id: u32 =
                    Attribute::get_named_enum_kind_id("disable_sanitizer_instrumentation");

                let disable_all_sanitizers: Attribute = self
                    .context
                    .create_enum_attribute(disable_all_sanitizers_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, disable_all_sanitizers);
                }
            }

            if optimizations.has_sanitize_address() {
                let sanitize_address_id: u32 =
                    Attribute::get_named_enum_kind_id("sanitize_address");

                let sanitize_address: Attribute =
                    self.context.create_enum_attribute(sanitize_address_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, sanitize_address);
                }
            }

            if optimizations.has_sanitize_memory() {
                let sanitize_memory_id: u32 = Attribute::get_named_enum_kind_id("sanitize_memory");
                let sanitize_memory: Attribute =
                    self.context.create_enum_attribute(sanitize_memory_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, sanitize_memory);
                }
            }

            if optimizations.has_sanitize_thread() {
                let sanitize_thread_id: u32 = Attribute::get_named_enum_kind_id("sanitize_thread");
                let sanitize_thread: Attribute =
                    self.context.create_enum_attribute(sanitize_thread_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, sanitize_thread);
                }
            }

            if optimizations.has_sanitize_hwaddress() {
                let sanitize_hwaddress_id: u32 =
                    Attribute::get_named_enum_kind_id("sanitize_hwaddress");

                let sanitize_hwaddress: Attribute =
                    self.context.create_enum_attribute(sanitize_hwaddress_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, sanitize_hwaddress);
                }
            }

            if optimizations.has_sanitize_memtag() {
                let sanitize_memtag_id: u32 = Attribute::get_named_enum_kind_id("sanitize_memtag");
                let sanitize_memtag: Attribute =
                    self.context.create_enum_attribute(sanitize_memtag_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, sanitize_memtag);
                }
            }

            if optimizations.has_nosanitize_bounds() {
                let nosanitize_bounds_id: u32 =
                    Attribute::get_named_enum_kind_id("nosanitize_bounds");
                let nosanitize_bounds: Attribute =
                    self.context.create_enum_attribute(nosanitize_bounds_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, nosanitize_bounds);
                }
            }

            if optimizations.has_nosanitize_coverage() {
                let nosanitize_coverage_id: u32 =
                    Attribute::get_named_enum_kind_id("nosanitize_coverage");
                let nosanitize_coverage: Attribute = self
                    .context
                    .create_enum_attribute(nosanitize_coverage_id, 0);

                if let Some(function) = self.function {
                    function.add_attribute(AttributeLoc::Function, nosanitize_coverage);
                }
            }
        }
    }
}

impl<'a, 'ctx> LLVMSanitizer<'a, 'ctx> {
    #[inline]
    fn set_function(&mut self, function: FunctionValue<'ctx>) {
        self.function = Some(function);
    }

    #[inline]
    fn reset_function(&mut self) {
        self.function = None;
    }

    #[inline]
    fn set_optimizations(&mut self, optimization: LLVMSanitizerOptimization) {
        self.optimization = Some(optimization);
    }

    #[inline]
    fn reset_optimizations_state(&mut self) {
        self.optimization = None;
    }
}

impl LLVMSanitizer<'_, '_> {
    #[inline]
    fn get_optimizations(&self) -> Option<LLVMSanitizerOptimization> {
        self.optimization
    }

    #[inline]
    fn get_config(&self) -> LLVMOptimizationConfig {
        self.config
    }

    #[inline]
    fn get_flags(&self) -> LLVMOptimizerFlags {
        self.flags
    }
}

#[derive(Debug, Clone, Copy)]
struct LLVMSanitizerOptimization {
    sanitize_address: bool,
    sanitize_memory: bool,
    sanitize_thread: bool,
    sanitize_hwaddress: bool,
    sanitize_memtag: bool,
    disable_sanitizers: bool,
    nosanitize_bounds: bool,
    nosanitize_coverage: bool,
}

impl LLVMSanitizerOptimization {
    #[inline]
    fn new(config: LLVMOptimizationConfig, flags: LLVMOptimizerFlags) -> Self {
        let is_sanitize_address_enabled: bool = config.get_sanitizer().is_address();
        let is_sanitize_memory_enabled: bool = config.get_sanitizer().is_memory();
        let is_sanitize_thread_enabled: bool = config.get_sanitizer().is_thread();
        let is_sanitize_hwaddres_enabled: bool = config.get_sanitizer().is_hwaddress();
        let is_sanitize_memtag_enabled: bool = config.get_sanitizer().is_memtag();
        let is_disable_all_sanitizers: bool = flags.get_disable_all_sanitizers();

        let (nosanitize_bounds, nosanitize_coverage) = match config.get_sanitizer() {
            Sanitizer::Address(config) => (
                config.has_nosanitize_bounds(),
                config.has_nosanitize_coverage(),
            ),
            Sanitizer::Hwaddress(config) => (
                config.has_nosanitize_bounds(),
                config.has_nosanitize_coverage(),
            ),
            Sanitizer::Memory(config) => (
                config.has_nosanitize_bounds(),
                config.has_nosanitize_coverage(),
            ),
            Sanitizer::Memtag(config) => (
                config.has_nosanitize_bounds(),
                config.has_nosanitize_coverage(),
            ),
            Sanitizer::Thread(config) => (
                config.has_nosanitize_bounds(),
                config.has_nosanitize_coverage(),
            ),
            _ => (false, false),
        };

        Self {
            sanitize_address: is_sanitize_address_enabled,
            sanitize_memory: is_sanitize_memory_enabled,
            sanitize_thread: is_sanitize_thread_enabled,
            sanitize_hwaddress: is_sanitize_hwaddres_enabled,
            sanitize_memtag: is_sanitize_memtag_enabled,
            disable_sanitizers: is_disable_all_sanitizers,
            nosanitize_bounds,
            nosanitize_coverage,
        }
    }
}

impl LLVMSanitizerOptimization {
    #[inline]
    pub fn has_sanitize_address(&self) -> bool {
        self.sanitize_address
    }

    #[inline]
    pub fn has_sanitize_memory(&self) -> bool {
        self.sanitize_memory
    }

    #[inline]
    pub fn has_sanitize_thread(&self) -> bool {
        self.sanitize_thread
    }

    #[inline]
    pub fn has_sanitize_hwaddress(&self) -> bool {
        self.sanitize_hwaddress
    }

    #[inline]
    pub fn has_sanitize_memtag(&self) -> bool {
        self.sanitize_memtag
    }

    #[inline]
    pub fn has_nosanitize_bounds(&self) -> bool {
        self.nosanitize_bounds
    }

    #[inline]
    pub fn has_nosanitize_coverage(&self) -> bool {
        self.nosanitize_coverage
    }

    #[inline]
    pub fn has_disable_all_sanitizers(&self) -> bool {
        self.disable_sanitizers
    }

    #[inline]
    pub fn is_neither(&self) -> bool {
        !(self.sanitize_address
            || self.sanitize_memory
            || self.sanitize_thread
            || self.sanitize_hwaddress
            || self.sanitize_memtag
            || self.disable_sanitizers)
    }
}

#[derive(Debug)]
pub struct LLVMParameterOptimizer<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    target_data: TargetData,

    function: Option<FunctionValue<'ctx>>,
    target: Option<BasicValueEnum<'ctx>>,
    target_position: Option<u32>,
    optimizations: Option<LLVMParameterOptimizations>,

    has_returned: bool,
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    #[inline]
    pub fn new(module: &'a Module<'ctx>, context: &'ctx Context, target_data: TargetData) -> Self {
        Self {
            module,
            context,
            target_data,

            function: None,
            target: None,
            target_position: None,
            optimizations: None,

            has_returned: false,
        }
    }
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    pub fn run(&mut self) {
        let ordered_functions: Vec<FunctionValue<'_>> =
            utils::get_functions_by_ordered_calls(self.module.get_functions().collect());

        {
            for function in ordered_functions.iter() {
                self.visit_function_once(*function);
            }
        }
    }
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    fn visit_function_once(&mut self, function: FunctionValue<'ctx>) {
        if function.get_first_basic_block().is_none() {
            return;
        }

        self.set_function(function);

        {
            for (idx, parameter) in function.get_param_iter().enumerate() {
                self.set_target(parameter, idx as u32);

                let mut optimizations: LLVMParameterOptimizations =
                    LLVMParameterOptimizations::new(function, parameter);

                {
                    for basic_block in function.get_basic_blocks() {
                        self.visit_basic_block_once(basic_block, &mut optimizations);
                    }
                }

                self.forward_optimizations(&mut optimizations, function, parameter);

                self.optimize(&optimizations);

                self.reset_target();
            }
        }

        self.reset_returned();
        self.reset_function();
    }

    fn forward_optimizations(
        &mut self,
        optimizations: &mut LLVMParameterOptimizations,
        function: FunctionValue<'ctx>,
        parameter: BasicValueEnum<'ctx>,
    ) {
        let write_only_valid: bool = function.get_basic_blocks().iter().all(|bb| {
            bb.get_instructions().all(|inst| match inst.get_opcode() {
                InstructionOpcode::Load => {
                    let source_ptr: Option<BasicValueEnum<'_>> =
                        inst.get_operand(0).and_then(|res| res.left());

                    source_ptr != Some(parameter)
                }
                InstructionOpcode::Store => {
                    let value_to_store: Option<BasicValueEnum<'_>> =
                        inst.get_operand(0).and_then(|res| res.left());

                    if value_to_store == Some(parameter) {
                        return false;
                    }

                    true
                }
                InstructionOpcode::Call => {
                    {
                        for operand in inst.get_operands() {
                            if operand.and_then(|res| res.left()) == Some(parameter) {
                                return false;
                            }
                        }
                    }

                    true
                }

                _ => true,
            })
        });

        let read_only_valid: bool = function
            .get_basic_blocks()
            .iter()
            .flat_map(|bb| bb.get_instructions())
            .all(|inst| match inst.get_opcode() {
                InstructionOpcode::Store => {
                    if let Some(dest_ptr) = inst.get_operand(1).and_then(|res| res.left()) {
                        return dest_ptr != parameter;
                    }

                    true
                }
                InstructionOpcode::Call => {
                    {
                        for operand in inst.get_operands() {
                            if operand.and_then(|res| res.left()) == Some(parameter) {
                                return false;
                            }
                        }
                    }

                    true
                }

                _ => true,
            });

        let readnone_valid: bool = function
            .get_basic_blocks()
            .iter()
            .flat_map(|bb| bb.get_instructions())
            .all(|inst| {
                {
                    for operand in inst.get_operands() {
                        if let Some(operand) = operand.and_then(|res| res.left()) {
                            if operand == parameter {
                                match inst.get_opcode() {
                                    InstructionOpcode::Load
                                    | InstructionOpcode::Store
                                    | InstructionOpcode::AtomicRMW
                                    | InstructionOpcode::AtomicCmpXchg => return false,

                                    InstructionOpcode::GetElementPtr
                                    | InstructionOpcode::BitCast
                                    | InstructionOpcode::PtrToInt
                                    | InstructionOpcode::Call => return false,

                                    _ => (),
                                }
                            }
                        }
                    }
                }

                true
            });

        if read_only_valid && parameter.get_type().is_pointer_type() {
            optimizations.set_readonly_param_opt(true);
        } else if write_only_valid && parameter.get_type().is_pointer_type() {
            optimizations.set_writeonly_param_opt(true);
        } else if readnone_valid && parameter.get_type().is_pointer_type() {
            optimizations.set_readnone_opt(true);
        }
    }
    fn visit_basic_block_once(
        &mut self,
        basic_block: BasicBlock<'ctx>,
        optimizations: &mut LLVMParameterOptimizations,
    ) {
        {
            for instruction in basic_block.get_instructions() {
                self.visit_instruction_once(instruction, optimizations);
            }
        }
    }

    fn visit_instruction_once(
        &mut self,
        instruction: InstructionValue<'ctx>,
        optimizations: &mut LLVMParameterOptimizations,
    ) {
        if instruction.get_opcode() == InstructionOpcode::Call {
            let callsite: CallSiteValue = unsafe { CallSiteValue::new(instruction.as_value_ref()) };
            let called: FunctionValue = callsite.get_called_fn_value();

            if !callsite.is_tail_call() && self.function.is_some_and(|current| current == called) {
                callsite.set_tail_call(true);
            }
        }

        if instruction.get_opcode() == InstructionOpcode::Return
            && !self.has_returned_attr_in_this_function()
        {
            if let Some(operand_result) = instruction.get_operand(1) {
                if let Some(value) = operand_result.left() {
                    let is_param_eq: bool = if let Some(target) = self.target {
                        target == value
                    } else {
                        false
                    };

                    let is_bitcast_valid: bool = if let Some(target) = self.target {
                        self.target_data.get_bit_size(&value.get_type())
                            == self.target_data.get_bit_size(&target.get_type())
                    } else {
                        false
                    };

                    if is_param_eq && is_bitcast_valid {
                        optimizations.set_returned_param_opt(true);
                        self.set_has_returned();
                    }
                }
            }
        }
    }
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    #[inline]
    pub fn set_target(&mut self, target: BasicValueEnum<'ctx>, position: u32) {
        self.target = Some(target);
        self.target_position = Some(position);
    }

    #[inline]
    pub fn reset_target(&mut self) {
        self.target = None;
        self.target_position = None;
    }

    #[inline]
    pub fn set_function(&mut self, function: FunctionValue<'ctx>) {
        self.function = Some(function);
    }

    #[inline]
    pub fn reset_function(&mut self) {
        self.function = None;
    }
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    #[inline]
    pub fn set_has_returned(&mut self) {
        self.has_returned = true;
    }

    #[inline]
    pub fn reset_returned(&mut self) {
        self.has_returned = false;
    }

    #[inline]
    pub fn has_returned_attr_in_this_function(&self) -> bool {
        self.has_returned
    }
}

impl<'a, 'ctx> LLVMParameterOptimizer<'a, 'ctx> {
    fn optimize(&mut self, optimizations: &LLVMParameterOptimizations) {
        if optimizations.has_deferenceable() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("dereferenceable");

            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 1);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_noundef() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("noundef");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_align() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("align");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 1);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_returned() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("returned");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_readonly() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("readonly");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_writeonly() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("writeonly");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }

        if optimizations.has_readnone() {
            let kind_id: u32 = Attribute::get_named_enum_kind_id("readnone");
            let attribute: Attribute = self.context.create_enum_attribute(kind_id, 0);

            if let Some(function) = self.function {
                if let Some(target_pos) = self.target_position {
                    function.add_attribute(AttributeLoc::Param(target_pos), attribute);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMParameterOptimizations {
    deferenceable: bool,
    noundef: bool,
    align: bool,
    returned: bool,
    readonly: bool,
    writeonly: bool,
    readnone: bool,
}

impl LLVMParameterOptimizations {
    pub fn new(function: FunctionValue, parameter: BasicValueEnum) -> Self {
        Self {
            deferenceable: parameter.is_pointer_value(),
            noundef: !function.get_type().is_var_arg(),
            align: parameter.is_pointer_value() && !function.get_type().is_var_arg(),
            returned: false,
            readonly: false,
            writeonly: false,
            readnone: false,
        }
    }
}

impl LLVMParameterOptimizations {
    #[inline]
    pub fn set_returned_param_opt(&mut self, value: bool) {
        self.returned = value;
    }

    #[inline]
    pub fn set_readonly_param_opt(&mut self, value: bool) {
        self.readonly = value;
    }

    #[inline]
    pub fn set_writeonly_param_opt(&mut self, value: bool) {
        self.writeonly = value;
    }

    #[inline]
    pub fn set_readnone_opt(&mut self, value: bool) {
        self.readnone = value;
    }
}

impl LLVMParameterOptimizations {
    #[inline]
    pub fn has_deferenceable(&self) -> bool {
        self.deferenceable
    }

    #[inline]
    pub fn has_noundef(&self) -> bool {
        self.noundef
    }

    #[inline]
    pub fn has_align(&self) -> bool {
        self.align
    }

    #[inline]
    pub fn has_returned(&self) -> bool {
        self.returned
    }

    #[inline]
    pub fn has_readonly(&self) -> bool {
        self.readonly
    }

    #[inline]
    pub fn has_writeonly(&self) -> bool {
        self.writeonly
    }

    #[inline]
    pub fn has_readnone(&self) -> bool {
        self.readnone
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMExpressionOptimization {
    unnamed_addr: bool,
}

impl LLVMExpressionOptimization {
    #[inline]
    pub fn new() -> Self {
        Self {
            unnamed_addr: false,
        }
    }
}

impl LLVMExpressionOptimization {
    #[inline]
    pub fn setup_all_constant_optimizations(&mut self) {
        self.unnamed_addr = true;
    }
}

impl LLVMExpressionOptimization {
    #[inline]
    pub fn has_unnamed_addr(&self) -> bool {
        self.unnamed_addr
    }
}

impl LLVMExpressionOptimization {
    #[inline]
    pub fn denegate_all_expression_optimizations(&mut self) {
        self.unnamed_addr = false;
    }
}
