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

use ahash::AHashMap as HashMap;

use crate::{
    memory::SymbolAllocated,
    traits::LLVMFunctionExtensions,
    types::{
        LLVMAllocatedParameters, LLVMFunction, LLVMFunctions, LLVMFunctionsParameters,
        LLVMGlobalConstants, LLVMGlobalStatics, LLVMInstructions, LLVMLocalConstants,
        LLVMLocalStatics,
    },
};

#[derive(Debug)]
pub struct LLVMSymbolsTable<'ctx> {
    functions: LLVMFunctions<'ctx>,

    global_constants: LLVMGlobalConstants<'ctx>,
    global_statics: LLVMGlobalStatics<'ctx>,

    local_statics: LLVMLocalStatics<'ctx>,
    local_constants: LLVMLocalConstants<'ctx>,

    locals: LLVMInstructions<'ctx>,

    allocated_parameters: LLVMAllocatedParameters<'ctx>,
    parameters: LLVMFunctionsParameters<'ctx>,

    scope: usize,
}

impl LLVMSymbolsTable<'_> {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: HashMap::with_capacity(u8::MAX as usize),

            global_constants: HashMap::with_capacity(u8::MAX as usize),
            global_statics: HashMap::with_capacity(u8::MAX as usize),
            local_statics: Vec::with_capacity(u8::MAX as usize),
            local_constants: Vec::with_capacity(u8::MAX as usize),

            locals: Vec::with_capacity(u8::MAX as usize),
            allocated_parameters: HashMap::with_capacity(u8::MAX as usize),
            parameters: HashMap::with_capacity(u8::MAX as usize),

            scope: 0,
        }
    }
}

impl<'ctx> LLVMSymbolsTable<'ctx> {
    #[must_use]
    pub fn get_symbol(&self, name: &str) -> SymbolAllocated<'ctx> {
        for scope in self.locals.iter().rev() {
            if let Some(local) = scope.get(name) {
                return *local;
            }
        }

        for scope in self.local_constants.iter().rev() {
            if let Some(local_constant) = scope.get(name) {
                return *local_constant;
            }
        }

        for scope in self.local_statics.iter().rev() {
            if let Some(local_static) = scope.get(name) {
                return *local_static;
            }
        }

        if let Some(parameter) = self.parameters.get(name) {
            return *parameter;
        }

        if let Some(allocated_parameter) = self.allocated_parameters.get(name) {
            return *allocated_parameter;
        }

        if let Some(global_constant) = self.global_constants.get(name) {
            return *global_constant;
        }

        if let Some(global_static) = self.global_statics.get(name) {
            return *global_static;
        }

        if let Some(function) = self.functions.get(name) {
            return SymbolAllocated::new_function(
                function.get_value().as_global_value().as_pointer_value(),
                function.get_span(),
            );
        }

        self::codegen_abort(format!(
            "Unable to get '{}' allocated object at frame pointer number '#{}'.",
            name, self.scope
        ));
    }

    #[must_use]
    pub fn get_function(&self, name: &str) -> LLVMFunction<'ctx> {
        if let Some(function) = self.functions.get(name) {
            return function.clone();
        }

        self::codegen_abort(format!(
            "Unable to get '{}' function in global frame.",
            name
        ));
    }
}

impl<'ctx> LLVMSymbolsTable<'ctx> {
    #[inline]
    pub fn add_function(&mut self, name: &'ctx str, function: LLVMFunction<'ctx>) {
        self.functions.insert(name, function);
    }

    #[inline]
    pub fn add_parameter(&mut self, name: &'ctx str, parameter: SymbolAllocated<'ctx>) {
        self.parameters.insert(name, parameter);
    }

    #[inline]
    pub fn add_allocated_parameter(
        &mut self,
        name: &'ctx str,
        allocated_parameter: SymbolAllocated<'ctx>,
    ) {
        self.allocated_parameters.insert(name, allocated_parameter);
    }

    #[inline]
    pub fn add_global_constant(&mut self, name: &'ctx str, constant: SymbolAllocated<'ctx>) {
        self.global_constants.insert(name, constant);
    }

    #[inline]
    pub fn add_global_static(&mut self, name: &'ctx str, static_: SymbolAllocated<'ctx>) {
        self.global_statics.insert(name, static_);
    }
}

impl<'ctx> LLVMSymbolsTable<'ctx> {
    #[inline]
    pub fn get_mut_all_functions(&mut self) -> &mut LLVMFunctions<'ctx> {
        &mut self.functions
    }

    #[inline]
    pub fn get_mut_all_global_constants(&mut self) -> &mut LLVMGlobalConstants<'ctx> {
        &mut self.global_constants
    }

    #[inline]
    pub fn get_mut_all_local_constants(&mut self) -> &mut LLVMLocalConstants<'ctx> {
        &mut self.local_constants
    }

    #[inline]
    pub fn get_mut_all_global_statics(&mut self) -> &mut LLVMGlobalStatics<'ctx> {
        &mut self.global_statics
    }

    #[inline]
    pub fn get_mut_all_local_statics(&mut self) -> &mut LLVMLocalStatics<'ctx> {
        &mut self.local_statics
    }

    #[inline]
    pub fn get_mut_all_locals(&mut self) -> &mut LLVMInstructions<'ctx> {
        &mut self.locals
    }
}

impl LLVMSymbolsTable<'_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.local_statics
            .push(HashMap::with_capacity(u8::MAX as usize));
        self.local_constants
            .push(HashMap::with_capacity(u8::MAX as usize));
        self.locals.push(HashMap::with_capacity(u8::MAX as usize));

        self.scope = self.scope.saturating_add(1);

        debug_assert_eq!(
            self.locals.len(),
            self.scope,
            "LLVMSymbolsTable desync on begin_scope: locals.len()={} but scope={}. \
             A scope stack was pushed/popped without going through begin_scope/end_scope, \
             or begin_scope was called out of order relative to symbol registration.",
            self.locals.len(),
            self.scope
        );
        debug_assert_eq!(self.local_constants.len(), self.scope);
        debug_assert_eq!(self.local_statics.len(), self.scope);
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.local_statics.pop();
        self.local_constants.pop();
        self.locals.pop();

        self.scope = self.scope.saturating_sub(1);

        debug_assert_eq!(
            self.locals.len(),
            self.scope,
            "LLVMSymbolsTable desync on end_scope: locals.len()={} but scope={}.",
            self.locals.len(),
            self.scope
        );
        debug_assert_eq!(self.local_constants.len(), self.scope);
        debug_assert_eq!(self.local_statics.len(), self.scope);

        if self.scope == 0 {
            self.parameters.clear();
        }
    }
}

#[inline]
fn codegen_abort<T: std::fmt::Display>(message: T) -> ! {
    thrustc_logging::print_backend_bug(
        thrustc_logging::LoggingType::BackendBug,
        &format!("{}", message),
    );
}
