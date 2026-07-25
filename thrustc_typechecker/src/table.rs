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

use thrustc_entities::typechecker_entities::*;

#[derive(Debug)]
pub struct TypeCheckerSymbolsTable<'symbol> {
    functions: TypeCheckerFunctions<'symbol>,
    assembler_functions: TypeCheckerAssemblerFunctions<'symbol>,
    compiler_intrinsics: TypeCheckerIntrinsics<'symbol>,

    locals: TypeCheckerLocals<'symbol>,
}

impl<'symbol> TypeCheckerSymbolsTable<'symbol> {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: HashMap::with_capacity(u8::MAX as usize),
            assembler_functions: HashMap::with_capacity(u8::MAX as usize),
            compiler_intrinsics: HashMap::with_capacity(u8::MAX as usize),
            locals: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'symbol> TypeCheckerSymbolsTable<'symbol> {
    #[inline]
    pub fn new_asm_function(
        &mut self,
        name: &'symbol str,
        function: TypeCheckerAssemblerFunction<'symbol>,
    ) {
        self.assembler_functions.insert(name, function);
    }

    #[inline]
    pub fn new_function(&mut self, name: &'symbol str, function: TypeCheckerFunction<'symbol>) {
        self.functions.insert(name, function);
    }

    #[inline]
    pub fn new_compiler_intrinsic(
        &mut self,
        name: &'symbol str,
        intrinsic: TypeCheckerIntrinsic<'symbol>,
    ) {
        self.compiler_intrinsics.insert(name, intrinsic);
    }
}

impl<'symbol> TypeCheckerSymbolsTable<'symbol> {
    #[inline]
    pub fn get_function(&self, name: &'symbol str) -> Option<&TypeCheckerFunction<'symbol>> {
        self.functions.get(name)
    }

    #[inline]
    pub fn get_asm_function(
        &self,
        name: &'symbol str,
    ) -> Option<&TypeCheckerAssemblerFunction<'symbol>> {
        self.assembler_functions.get(name)
    }

    #[inline]
    pub fn get_intrinsic(&self, name: &'symbol str) -> Option<&TypeCheckerIntrinsic<'symbol>> {
        self.compiler_intrinsics.get(name)
    }
}

impl<'symbol> TypeCheckerSymbolsTable<'symbol> {
    #[inline]
    pub fn contains_function(&self, name: &'symbol str) -> bool {
        self.functions.contains_key(name)
    }

    #[inline]
    pub fn contains_asm_function(&self, name: &'symbol str) -> bool {
        self.assembler_functions.contains_key(name)
    }

    #[inline]
    pub fn contains_compiler_intrinsic(&self, name: &'symbol str) -> bool {
        self.compiler_intrinsics.contains_key(name)
    }
}

impl TypeCheckerSymbolsTable<'_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.locals.push(HashMap::with_capacity(u8::MAX as usize));
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.locals.pop();
    }
}
