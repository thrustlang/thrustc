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

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::AsDIScope;
use inkwell::debug_info::DICompileUnit;
use inkwell::debug_info::DIFile;
use inkwell::debug_info::DIFlagsConstants;
use inkwell::debug_info::DILexicalBlock;
use inkwell::debug_info::DILocation;
use inkwell::debug_info::DIScope;
use inkwell::debug_info::DISubprogram;
use inkwell::debug_info::DISubroutineType;
use inkwell::debug_info::DIType;
use inkwell::debug_info::DWARFEmissionKind;
use inkwell::debug_info::DebugInfoBuilder;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicTypeEnum;
use inkwell::values::FunctionValue;

use thrustc_diagnostician::Diagnostician;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::context::LLVMCodeGenContext;
use crate::optimizer::LLVMOptimizer;
use crate::traits::LLVMDBGFunctionExtensions;
use crate::typegeneration;
use crate::types::LLVMDBGFunction;

#[derive(Debug)]
pub struct LLVMDebugContext<'a, 'ctx> {
    builder: DebugInfoBuilder<'ctx>,
    unit: DICompileUnit<'ctx>,
    target_machine: &'a TargetMachine,
    diagnostician: Diagnostician,
    subprograms: Vec<DISubprogram<'ctx>>,
    lexical_blocks: Vec<DILexicalBlock<'ctx>>,
    debug_locations: Vec<DILocation<'ctx>>,
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    pub fn new(
        llvm_module: &Module<'ctx>,
        target_machine: &'a TargetMachine,
        options: &CompilerOptions,
        unit: &CompilationUnit,
    ) -> Self {
        let is_optimized: bool = LLVMOptimizer::is_an_optimizable_module(options);

        let split_debug_inlining: bool = options
            .get_llvm_backend()
            .get_debug_config()
            .need_split_debug_inlining();

        let debug_info_for_profiling: bool = options
            .get_llvm_backend()
            .get_debug_config()
            .need_debug_info_for_profiling();

        let (builder, dicompileunit) = llvm_module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::C,
            unit.get_name(),
            &format!("{}", unit.get_path().display()),
            thrustc_constants::COMPILER_ID,
            is_optimized,
            "",
            0,
            "",
            DWARFEmissionKind::Full,
            0,
            split_debug_inlining,
            debug_info_for_profiling,
            "",
            "",
        );

        Self {
            builder,
            unit: dicompileunit,
            target_machine,
            diagnostician: Diagnostician::new(unit, options),
            subprograms: Vec::with_capacity(u8::MAX as usize),
            lexical_blocks: Vec::with_capacity(u8::MAX as usize),
            debug_locations: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    #[inline]
    pub fn finalize(&self) {
        self.builder.finalize();
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    pub fn dispatch_function_debug_data(
        &mut self,
        function: &LLVMDBGFunction<'ctx>,
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ) {
        let function_value: FunctionValue<'_> = function.get_value();
        let name: &str = function.get_name();
        let return_type: &Type = function.get_return_type();
        let parameter_types: Vec<Type> = function.get_parameters_types();
        let span: Span = function.get_span();
        let line: u32 = span.get_line();

        let llvm_return_type: Option<BasicTypeEnum<'_>> = if !return_type.is_void_type() {
            Some(typegeneration::generate_type(context, return_type))
        } else {
            None
        };

        let llvm_parameter_types: Vec<BasicTypeEnum<'_>> = parameter_types
            .iter()
            .map(|parameter_type| typegeneration::generate_type(context, parameter_type))
            .collect();

        let mut dbg_parameter_types: Vec<DIType<'_>> =
            Vec::with_capacity(llvm_parameter_types.len());

        for (parameter_type, llvm_parameter_type) in
            parameter_types.iter().zip(llvm_parameter_types.iter())
        {
            let ty: DIType<'_> =
                typegeneration::compile_as_dbg_type(self, parameter_type, *llvm_parameter_type);

            dbg_parameter_types.push(ty);
        }

        let dbg_return_type: Option<DIType> = llvm_return_type.map(|llvm_return_type| {
            typegeneration::compile_as_dbg_type(self, return_type, llvm_return_type)
        });

        let subroutine_type: DISubroutineType<'_> =
            self.get_debug_builder().create_subroutine_type(
                self.get_debug_unit().get_file(),
                dbg_return_type,
                &dbg_parameter_types,
                DIFlagsConstants::PUBLIC,
            );

        let is_optimized: bool =
            LLVMOptimizer::is_an_optimizable_module(context.get_compiler_options());

        let file: DIFile<'_> = self.get_debug_unit().get_file();

        let subprogram: DISubprogram<'_> = self.get_debug_builder().create_function(
            file.as_debug_info_scope(),
            name,
            None,
            file,
            line,
            subroutine_type,
            function.is_local(),
            function.is_definition(),
            0,
            inkwell::debug_info::DIFlagsConstants::PUBLIC,
            is_optimized,
        );

        self.add_subprogram(subprogram);
        function_value.set_subprogram(subprogram);
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    pub fn add_dbg_location(&mut self, context: &mut LLVMCodeGenContext<'_, 'ctx>, span: Span) {
        let llvm_context: &Context = context.get_llvm_context();
        let llvm_builder: &Builder = context.get_llvm_builder();

        llvm_builder.unset_current_debug_location();

        let line: u32 = span.get_line();
        let column: u32 = span.get_span_start();

        let debug_loc: DILocation<'_> = self.get_debug_builder().create_debug_location(
            llvm_context,
            line,
            column,
            self.get_scope(),
            None,
        );

        self.debug_locations.push(debug_loc);
        llvm_builder.set_current_debug_location(debug_loc);
    }

    pub fn add_dbg_block(&mut self, span: Span) {
        let line: u32 = span.get_line();
        let column: u32 = span.get_span_start();

        let parent_scope: DIScope = self.get_scope();

        let block: DILexicalBlock<'_> = self.get_debug_builder().create_lexical_block(
            parent_scope,
            self.get_debug_unit().get_file(),
            line,
            column,
        );

        self.lexical_blocks.push(block);
    }

    #[inline]
    pub fn reset_blocks(&mut self) {
        self.lexical_blocks.clear();
    }

    #[inline]
    pub fn reset_debug_locations(&mut self) {
        self.lexical_blocks.clear();
    }

    #[inline]
    pub fn add_subprogram(&mut self, subprogram: DISubprogram<'ctx>) {
        self.subprograms.push(subprogram);
    }

    #[inline]
    pub fn finish_subprogram(&mut self) {
        self.subprograms.pop();

        self.reset_debug_locations();
        self.reset_blocks();
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    #[inline]
    pub fn get_last_debug_lexical_block(&self) -> Option<DILexicalBlock<'ctx>> {
        self.lexical_blocks.last().copied()
    }

    #[inline]
    pub fn get_last_subprogram(&self) -> Option<DISubprogram<'ctx>> {
        self.subprograms.last().copied()
    }

    #[inline]
    pub fn get_scope(&self) -> DIScope<'ctx> {
        if let Some(lexical_block) = self.get_last_debug_lexical_block() {
            lexical_block.as_debug_info_scope()
        } else if let Some(subprogram) = self.get_last_subprogram() {
            subprogram.as_debug_info_scope()
        } else {
            self.get_debug_unit().as_debug_info_scope()
        }
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    #[inline]
    pub fn get_debug_builder(&self) -> &DebugInfoBuilder<'ctx> {
        &self.builder
    }

    #[inline]
    pub fn get_debug_unit(&self) -> &DICompileUnit<'ctx> {
        &self.unit
    }

    #[inline]
    pub fn get_target_data(&self) -> TargetData {
        self.target_machine.get_target_data()
    }
}

impl<'a, 'ctx> LLVMDebugContext<'a, 'ctx> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }
}
