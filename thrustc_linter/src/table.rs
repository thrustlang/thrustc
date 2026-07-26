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

use thrustc_ast::{Ast, ast_metadata::FunctionParameterMetadata};
use thrustc_entities::{FunctionParameter, linter_entities::*};

use ahash::AHashMap as HashMap;
use thrustc_span::Span;

#[derive(Debug)]
pub struct LinterSymbolsTable<'linter> {
    functions: LinterFunctions<'linter>,
    asm_functions: LinterAssemblerFunctions<'linter>,
    intrinsics: LinterIntrinsics<'linter>,

    global_statics: LinterGlobalStatics<'linter>,
    global_constants: LinterGlobalConstants<'linter>,

    local_statics: LinterLocalStatics<'linter>,
    local_constants: LinterLocalConstants<'linter>,

    enums: LinterEnums<'linter>,
    structs: LinterStructs<'linter>,
    locals: LinterLocals<'linter>,
    parameters: LinterFunctionParameters<'linter>,
}

impl LinterSymbolsTable<'_> {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: HashMap::with_capacity(u8::MAX as usize),
            asm_functions: HashMap::with_capacity(u8::MAX as usize),
            intrinsics: HashMap::with_capacity(u8::MAX as usize),

            global_statics: HashMap::with_capacity(u8::MAX as usize),
            global_constants: HashMap::with_capacity(u8::MAX as usize),

            local_statics: Vec::with_capacity(u8::MAX as usize),
            local_constants: Vec::with_capacity(u8::MAX as usize),

            enums: HashMap::with_capacity(u8::MAX as usize),
            structs: HashMap::with_capacity(u8::MAX as usize),
            locals: Vec::with_capacity(u8::MAX as usize),
            parameters: HashMap::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'linter> LinterSymbolsTable<'linter> {
    #[must_use]
    pub fn get_asm_function_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterAssemblerFunctionInfo<'linter>> {
        self.asm_functions.get_mut(name)
    }

    #[must_use]
    pub fn get_function_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterFunctionInfo<'linter>> {
        self.functions.get_mut(name)
    }

    #[must_use]
    pub fn get_intrinsic_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterIntrinsicInfo<'linter>> {
        self.intrinsics.get_mut(name)
    }

    #[must_use]
    pub fn get_parameter_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterFunctionParameterInfo> {
        self.parameters.get_mut(name)
    }

    #[must_use]
    pub fn get_enum_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterEnumsFieldsInfo<'linter>> {
        self.enums.get_mut(name)
    }

    #[must_use]
    pub fn get_struct_info(
        &mut self,
        name: &'linter str,
    ) -> Option<&mut LinterStructFieldsInfo<'linter>> {
        self.structs.get_mut(name)
    }

    #[must_use]
    pub fn get_enum_field_info(
        &mut self,
        enum_name: &'linter str,
        field_name: &'linter str,
    ) -> Option<&mut LinterEnumFieldInfo> {
        if let Some(raw_enum_fields) = self.get_enum_info(enum_name) {
            let enum_fields: &mut HashMap<&'linter str, (Span, bool)> = &mut raw_enum_fields.0;

            if let Some(enum_field) = enum_fields.get_mut(field_name) {
                return Some(enum_field);
            }
        }

        None
    }

    #[must_use]
    pub fn get_local_info(&mut self, name: &'linter str) -> Option<&mut LinterLocalInfo> {
        for scope in self.locals.iter_mut().rev() {
            if let Some(local) = scope.get_mut(name) {
                return Some(local);
            }
        }

        None
    }

    #[must_use]
    pub fn get_constant_info(&mut self, name: &'linter str) -> Option<&mut LinterConstantInfo> {
        for scope in self.local_constants.iter_mut().rev() {
            if let Some(local) = scope.get_mut(name) {
                return Some(local);
            }
        }

        if let Some(global) = self.global_constants.get_mut(name) {
            return Some(global);
        }

        None
    }

    #[must_use]
    pub fn get_static_info(&mut self, name: &'linter str) -> Option<&mut LinterStaticInfo> {
        for scope in self.local_statics.iter_mut().rev() {
            if let Some(staticvar) = scope.get_mut(name) {
                return Some(staticvar);
            }
        }

        if let Some(glstatic) = self.global_statics.get_mut(name) {
            return Some(glstatic);
        }

        None
    }
}

impl<'linter> LinterSymbolsTable<'linter> {
    #[inline]
    pub fn get_all_function_parameters(&self) -> &LinterFunctionParameters<'_> {
        &self.parameters
    }

    #[inline]
    pub fn get_all_locals(&self) -> &LinterLocals<'_> {
        &self.locals
    }

    #[inline]
    pub fn get_all_enums(&self) -> &LinterEnums<'linter> {
        &self.enums
    }

    #[inline]
    pub fn get_all_global_constants(&self) -> &LinterGlobalConstants<'_> {
        &self.global_constants
    }

    #[inline]
    pub fn get_all_global_statics(&self) -> &LinterGlobalStatics<'_> {
        &self.global_statics
    }

    #[inline]
    pub fn get_all_local_constants(&self) -> &LinterLocalConstants<'_> {
        &self.local_constants
    }

    #[inline]
    pub fn get_all_locals_statics(&self) -> &LinterLocalStatics<'_> {
        &self.local_statics
    }

    #[inline]
    pub fn get_all_structs(&self) -> &LinterStructs<'_> {
        &self.structs
    }

    #[inline]
    pub fn get_all_functions(&self) -> &LinterFunctions<'_> {
        &self.functions
    }

    #[inline]
    pub fn get_all_asm_functions(&self) -> &LinterAssemblerFunctions<'_> {
        &self.asm_functions
    }

    #[inline]
    pub fn get_all_intrinsics(&self) -> &LinterIntrinsics<'_> {
        &self.intrinsics
    }
}

impl<'linter> LinterSymbolsTable<'linter> {
    #[inline]
    pub fn new_local(&mut self, name: &'linter str, info: LinterLocalInfo) {
        if let Some(scope) = self.locals.last_mut() {
            scope.insert(name, info);
        }
    }

    #[inline]
    pub fn new_asm_function(
        &mut self,
        name: &'linter str,
        info: LinterAssemblerFunctionInfo<'linter>,
    ) {
        self.asm_functions.insert(name, info);
    }

    #[inline]
    pub fn new_function(&mut self, name: &'linter str, info: LinterFunctionInfo<'linter>) {
        self.functions.insert(name, info);
    }

    #[inline]
    pub fn new_intrinsic(&mut self, name: &'linter str, info: LinterIntrinsicInfo<'linter>) {
        self.intrinsics.insert(name, info);
    }

    #[inline]
    pub fn new_global_constant(&mut self, name: &'linter str, info: LinterConstantInfo) {
        self.global_constants.insert(name, info);
    }

    #[inline]
    pub fn new_local_constant(&mut self, name: &'linter str, info: LinterConstantInfo) {
        if let Some(scope) = self.local_constants.last_mut() {
            scope.insert(name, info);
        }
    }

    #[inline]
    pub fn new_global_static(&mut self, name: &'linter str, info: LinterStaticInfo) {
        self.global_statics.insert(name, info);
    }

    #[inline]
    pub fn new_local_static(&mut self, name: &'linter str, info: LinterStaticInfo) {
        if let Some(scope) = self.local_statics.last_mut() {
            scope.insert(name, info);
        }
    }

    #[inline]
    pub fn new_parameter(&mut self, name: &'linter str, info: LinterFunctionParameterInfo) {
        self.parameters.insert(name, info);
    }

    #[inline]
    pub fn new_enum(&mut self, name: &'linter str, info: LinterEnumsFieldsInfo<'linter>) {
        self.enums.insert(name, info);
    }

    #[inline]
    pub fn new_struct(&mut self, name: &'linter str, info: LinterStructFieldsInfo<'linter>) {
        self.structs.insert(name, info);
    }
}

impl LinterSymbolsTable<'_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.local_constants
            .push(HashMap::with_capacity(u8::MAX as usize));
        self.locals.push(HashMap::with_capacity(u8::MAX as usize));
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.local_constants.pop();
        self.locals.pop();
    }
}

impl<'linter> LinterSymbolsTable<'linter> {
    #[inline]
    pub fn split_enum_field_name(
        &mut self,
        from: &'linter str,
    ) -> Option<(&'linter str, &'linter str)> {
        let splitted: Vec<&str> = from.split(".").collect();

        if let Some(enum_name) = splitted.first() {
            if let Some(field_name) = splitted.get(1) {
                return Some((enum_name, field_name));
            }
        }

        None
    }
}

impl<'linter> LinterSymbolsTable<'linter> {
    #[inline]
    pub fn declare_parameters(&mut self, parameters: &'linter [Ast]) {
        parameters.iter().for_each(|node| {
            let parameter: FunctionParameter = thrustc_entities::function_parameter_from_ast(node);

            let name: &str = parameter.0;
            let metadata: FunctionParameterMetadata = parameter.5;
            let span: Span = parameter.4;

            self.new_parameter(name, (span, false, !metadata.is_mutable()));
        });
    }

    #[inline]
    pub fn finish_parameters(&mut self) {
        self.parameters.clear();
    }
}
