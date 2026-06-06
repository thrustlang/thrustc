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

#![allow(clippy::too_many_arguments)]

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::targets::TargetTriple;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;

use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_abi::LLVMABICodeGenLocation;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;
use thrustc_span::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_layout::TargetInfo;

use crate::abort;
use crate::branch_context::LLVMLoopContext;
use crate::debug_context::LLVMDebugContext;
use crate::memory::SymbolAllocated;
use crate::memory::SymbolToAllocate;
use crate::optimizer::LLVMExpressionOptimization;
use crate::pointer_anchor::PointerAnchor;
use crate::table::LLVMSymbolsTable;
use crate::types::LLVMCtors;
use crate::types::LLVMDBGFunction;
use crate::types::LLVMDtors;
use crate::types::LLVMFunction;
use crate::types::LLVMStackProtectorPointer;

#[derive(Debug)]
pub struct LLVMCodeGenContext<'a, 'ctx> {
    module: &'a Module<'ctx>,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    target_info: TargetInfo,
    target_data: &'ctx TargetData,
    target_triple: &'ctx TargetTriple,
    target_machine: &'a TargetMachine,
    target_abi: Option<&'ctx LLVMABIRepresentation<'ctx>>,

    dbg_context: Option<LLVMDebugContext<'a, 'ctx>>,

    table: LLVMSymbolsTable<'ctx>,
    loop_ctx: LLVMLoopContext<'ctx>,
    ctors: LLVMCtors<'ctx>,
    dtors: LLVMDtors<'ctx>,

    codegen_location: Vec<CodeGenLocation>,

    ptr_anchor: Option<PointerAnchor<'ctx>>,

    current_function: Option<LLVMFunction<'ctx>>,
    function_stack_protector_ptr: Option<LLVMStackProtectorPointer<'ctx>>,

    expression_optimizations: LLVMExpressionOptimization,

    diagnostician: Diagnostician,
    options: &'ctx CompilerOptions,
}

impl<'a, 'ctx> LLVMCodeGenContext<'a, 'ctx> {
    pub fn new(
        module: &'a Module<'ctx>,
        context: &'ctx Context,
        builder: &'ctx Builder<'ctx>,
        target_data: &'ctx TargetData,
        target_triple: &'ctx TargetTriple,
        target_machine: &'a TargetMachine,
        target_abi: Option<&'ctx LLVMABIRepresentation<'ctx>>,
        diagnostician: Diagnostician,
        options: &'ctx CompilerOptions,
        file: &'ctx CompilationUnit,
    ) -> Self {
        let dbg_context: Option<LLVMDebugContext> = if options
            .get_llvm_backend()
            .get_debug_config()
            .is_debug_mode()
        {
            Some(LLVMDebugContext::new(module, target_machine, options, file))
        } else {
            None
        };

        let target_triple_formatted: String = target_triple.as_str().to_string_lossy().to_string();

        let target_info: TargetInfo =
            TargetInfo::new(LLVMTargetTriple::new(target_triple_formatted.clone()));

        Self {
            module,
            context,
            builder,
            target_info,
            target_data,
            target_triple,
            target_machine,
            target_abi,
            dbg_context,

            table: LLVMSymbolsTable::new(),
            loop_ctx: LLVMLoopContext::new(),

            ctors: LLVMCtors::new(),
            dtors: LLVMDtors::new(),

            codegen_location: Vec::new(),

            ptr_anchor: None,

            current_function: None,
            function_stack_protector_ptr: None,

            expression_optimizations: LLVMExpressionOptimization::new(),

            diagnostician,
            options,
        }
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    pub fn add_local_constant(&mut self, name: &'ctx str, symbol: SymbolAllocated<'ctx>) {
        if let Some(last_block) = self.table.get_mut_all_local_constants().last_mut() {
            last_block.insert(name, symbol);
        } else {
            abort::abort_codegen(
                self,
                "Failed to get the scope!",
                symbol.get_symbol_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }
    }

    #[inline]
    pub fn add_global_constant(&mut self, name: &'ctx str, symbol: SymbolAllocated<'ctx>) {
        self.table.add_global_constant(name, symbol);
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    pub fn add_local_static(&mut self, name: &'ctx str, symbol: SymbolAllocated<'ctx>) {
        if let Some(scope) = self.table.get_mut_all_local_statics().last_mut() {
            scope.insert(name, symbol);
        } else {
            abort::abort_codegen(
                self,
                "Failed to get the scope!",
                symbol.get_symbol_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    #[inline]
    pub fn add_global_static(&mut self, name: &'ctx str, static_: SymbolAllocated<'ctx>) {
        self.table.add_global_static(name, static_);
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn add_local_variable(&mut self, name: &'ctx str, symbol: SymbolAllocated<'ctx>) {
        if let Some(last_block) = self.table.get_mut_all_locals().last_mut() {
            last_block.insert(name, symbol);
        } else {
            abort::abort_codegen(
                self,
                "Failed to get the scope!",
                symbol.get_symbol_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    pub fn add_allocated_parameter(
        &mut self,
        name: &'ctx str,
        kind: &'ctx Type,
        ptr: PointerValue<'ctx>,
        span: Span,
    ) {
        let allocated_parameter: SymbolAllocated =
            SymbolAllocated::new(SymbolToAllocate::AllocatedParameter, kind, ptr.into(), span);

        self.table
            .add_allocated_parameter(name, allocated_parameter);
    }

    #[inline]
    pub fn add_parameter(
        &mut self,
        name: &'ctx str,
        ascii_name: &'ctx str,
        kind: &'ctx Type,
        value: BasicValueEnum<'ctx>,
        span: Span,
    ) {
        value.set_name(ascii_name);

        let parameter: SymbolAllocated =
            SymbolAllocated::new(SymbolToAllocate::Parameter, kind, value, span);

        self.table.add_parameter(name, parameter);
    }

    #[inline]
    pub fn add_function(&mut self, name: &'ctx str, function: LLVMFunction<'ctx>) {
        self.table.add_function(name, function);
    }
}

impl LLVMCodeGenContext<'_, '_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.table.begin_scope();
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.table.end_scope();
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn set_pointer_anchor(&mut self, anchor: PointerAnchor<'ctx>) {
        self.ptr_anchor = Some(anchor);
    }

    #[inline]
    pub fn mark_pointer_anchor(&mut self) {
        if let Some(anchor) = &mut self.ptr_anchor {
            anchor.triggered = true;
        }
    }

    #[inline]
    pub fn clear_pointer_anchor(&mut self) {
        self.ptr_anchor = None;
    }

    #[inline]
    pub fn set_current_function(&mut self, new_function: LLVMFunction<'ctx>) {
        self.current_function = Some(new_function);
    }

    #[inline]
    pub fn unset_current_function(&mut self) {
        self.current_function = None;
    }

    #[inline]
    pub fn set_function_stackguard_protector_pointer(&mut self, ptr: PointerValue<'ctx>) {
        self.function_stack_protector_ptr = Some(ptr);
    }

    #[inline]
    pub fn unset_function_stackguard_protector_pointer(&mut self) {
        self.function_stack_protector_ptr = None;
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn add_ctor(&mut self, ctor: PointerValue<'ctx>) {
        let last: Option<&(PointerValue, u32)> = self.ctors.iter().last();

        let order: u32 = if let Some((_, counter)) = last {
            counter + 1
        } else {
            1
        };

        self.ctors.insert((ctor, order));
    }

    #[inline]
    pub fn add_dtor(&mut self, dtor: PointerValue<'ctx>) {
        let last: Option<&(PointerValue, u32)> = self.ctors.iter().last();

        let order: u32 = if let Some((_, counter)) = last {
            counter + 1
        } else {
            1
        };

        self.dtors.insert((dtor, order));
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn add_codegen_location(&mut self, location: CodeGenLocation) {
        self.codegen_location.push(location);
    }

    #[inline]
    pub fn pop_current_codegen_location(&mut self) {
        self.codegen_location.pop();
    }
}

impl<'a, 'ctx> LLVMCodeGenContext<'a, 'ctx> {
    #[inline]
    pub fn get_llvm_module(&self) -> &'a Module<'ctx> {
        self.module
    }

    #[inline]
    pub fn get_llvm_context(&self) -> &'ctx Context {
        self.context
    }

    #[inline]
    pub fn get_llvm_builder(&self) -> &'ctx Builder<'ctx> {
        self.builder
    }

    #[inline]
    pub fn get_target_info(&self) -> &TargetInfo {
        &self.target_info
    }

    #[inline]
    pub fn get_target_data(&self) -> &TargetData {
        self.target_data
    }

    #[inline]
    pub fn get_target_triple(&self) -> &TargetTriple {
        self.target_triple
    }

    #[inline]
    pub fn get_target_machine(&self) -> &TargetMachine {
        self.target_machine
    }

    #[inline]
    pub fn get_debug_context(&self) -> Option<&LLVMDebugContext<'a, 'ctx>> {
        self.dbg_context.as_ref()
    }

    #[inline]
    pub fn get_compiler_options(&self) -> &CompilerOptions {
        self.options
    }

    #[inline]
    pub fn get_loop_ctx(&self) -> &LLVMLoopContext<'ctx> {
        &self.loop_ctx
    }

    #[inline]
    pub fn get_pointer_anchor(&self) -> Option<&PointerAnchor<'ctx>> {
        self.ptr_anchor.as_ref()
    }

    #[inline]
    pub fn get_function_stack_protector_pointer(&self) -> Option<&PointerValue<'ctx>> {
        self.function_stack_protector_ptr.as_ref()
    }

    #[inline]
    pub fn get_llvm_ctors(&self) -> &LLVMCtors<'ctx> {
        &self.ctors
    }

    #[inline]
    pub fn get_llvm_dtors(&self) -> &LLVMDtors<'ctx> {
        &self.dtors
    }

    #[inline]
    pub fn get_table(&self) -> &LLVMSymbolsTable<'ctx> {
        &self.table
    }

    #[inline]
    pub fn get_codegen_location(&self) -> CodeGenLocation {
        *(self
            .codegen_location
            .last()
            .unwrap_or(&CodeGenLocation::None))
    }

    #[inline]
    pub fn get_abi(&self) -> Option<&'ctx LLVMABIRepresentation<'ctx>> {
        self.target_abi
    }

    #[inline]
    pub fn has_abi(&self) -> bool {
        self.target_abi.is_some()
    }

    #[inline]
    pub fn get_current_function(&mut self, span: Span) -> LLVMFunction<'ctx> {
        self.current_function.clone().unwrap_or_else(|| {
            abort::abort_codegen(
                self,
                "Failed to compile a function internal reference!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }

    #[inline]
    pub fn get_last_builder_block(&mut self, span: Span) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap_or_else(|| {
            abort::abort_codegen(
                self,
                "Failed to get the last builder block!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }
}

impl<'a, 'ctx> LLVMCodeGenContext<'a, 'ctx> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        &mut self.target_info
    }

    #[inline]
    pub fn get_mut_loop_context(&mut self) -> &mut LLVMLoopContext<'ctx> {
        &mut self.loop_ctx
    }

    #[inline]
    pub fn get_mut_debug_context(&mut self) -> Option<&mut LLVMDebugContext<'a, 'ctx>> {
        self.dbg_context.as_mut()
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn start_function_debug_data(&mut self, dbg_proto: &LLVMDBGFunction<'ctx>) {
        let mut dbg_opt: Option<LLVMDebugContext<'_, '_>> = self.dbg_context.take();

        if let Some(ref mut dbg) = dbg_opt {
            dbg.dispatch_function_debug_data(dbg_proto, self);
        }

        self.dbg_context = dbg_opt;
    }

    #[inline]
    pub fn finish_function_debug_data(&mut self) {
        if let Some(dbg_context) = self.get_mut_debug_context() {
            dbg_context.finish_subprogram();
        }
    }

    #[inline]
    pub fn add_dbg_block_data(&mut self, span: Span) {
        let mut dbg_opt: Option<LLVMDebugContext<'_, '_>> = self.dbg_context.take();

        if let Some(ref mut dbg) = dbg_opt {
            dbg.add_dbg_block(span);
        }

        self.dbg_context = dbg_opt;
    }

    #[inline]
    pub fn mark_dbg_location(&mut self, span: Span) {
        let mut dbg_opt: Option<LLVMDebugContext<'_, '_>> = self.dbg_context.take();

        if let Some(ref mut dbg) = dbg_opt {
            dbg.add_dbg_location(self, span);
        }

        self.dbg_context = dbg_opt;
    }
}

impl<'ctx> LLVMCodeGenContext<'_, 'ctx> {
    #[inline]
    pub fn get_expressions_optimizations(&self) -> &LLVMExpressionOptimization {
        &self.expression_optimizations
    }

    #[inline]
    pub fn get_mut_expressions_optimizations(&mut self) -> &mut LLVMExpressionOptimization {
        &mut self.expression_optimizations
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

impl CodeGenLocation {
    #[inline]
    pub fn to_abi_representation(&self) -> LLVMABICodeGenLocation {
        match self {
            CodeGenLocation::CallArgExpr => LLVMABICodeGenLocation::CallArgExpr,
            CodeGenLocation::LValue => LLVMABICodeGenLocation::LValue,
            CodeGenLocation::RValue => LLVMABICodeGenLocation::RValue,
            CodeGenLocation::None => LLVMABICodeGenLocation::None,
        }
    }
}
