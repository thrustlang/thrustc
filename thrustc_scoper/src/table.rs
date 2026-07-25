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

use ahash::AHashSet as HashSet;

#[derive(Debug)]
pub struct ScoperSymbolTable<'symbol_table> {
    functions: HashSet<&'symbol_table str>,
    compiler_intrinsics: HashSet<&'symbol_table str>,
    assembler_functions: HashSet<&'symbol_table str>,
    statics: HashSet<&'symbol_table str>,
    constants: HashSet<&'symbol_table str>,

    locals: Vec<HashSet<&'symbol_table str>>,

    parameters: HashSet<&'symbol_table str>,
}

impl<'symbol_table> ScoperSymbolTable<'symbol_table> {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: HashSet::with_capacity(u8::MAX as usize),
            compiler_intrinsics: HashSet::with_capacity(u8::MAX as usize),
            assembler_functions: HashSet::with_capacity(u8::MAX as usize),
            statics: HashSet::with_capacity(u8::MAX as usize),
            constants: HashSet::with_capacity(u8::MAX as usize),
            locals: Vec::with_capacity(u8::MAX as usize),
            parameters: HashSet::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'symbol_table> ScoperSymbolTable<'symbol_table> {
    #[inline]
    pub fn add_function(&mut self, name: &'symbol_table str) {
        self.functions.insert(name);
    }

    #[inline]
    pub fn add_compiler_intrinsic(&mut self, name: &'symbol_table str) {
        self.compiler_intrinsics.insert(name);
    }

    #[inline]
    pub fn add_assembler_function(&mut self, name: &'symbol_table str) {
        self.assembler_functions.insert(name);
    }

    #[inline]
    pub fn add_static(&mut self, name: &'symbol_table str) {
        self.statics.insert(name);
    }

    #[inline]
    pub fn add_constant(&mut self, name: &'symbol_table str) {
        self.constants.insert(name);
    }

    #[inline]
    pub fn add_local(&mut self, name: &'symbol_table str) {
        let Some(last_scope) = self.locals.last_mut() else {
            return;
        };

        last_scope.insert(name);
    }

    #[inline]
    pub fn add_parameter(&mut self, name: &'symbol_table str) {
        self.parameters.insert(name);
    }
}

impl<'symbol_table> ScoperSymbolTable<'symbol_table> {
    #[inline]
    pub fn symbol_exists(&self, name: &'symbol_table str) -> bool {
        if self.parameters.get(name).is_some() {
            return true;
        }

        {
            for scope in self.locals.iter().rev() {
                if scope.get(name).is_some() {
                    return true;
                }
            }
        }

        if self.functions.get(name).is_some() {
            return true;
        }

        if self.assembler_functions.get(name).is_some() {
            return true;
        }

        if self.compiler_intrinsics.get(name).is_some() {
            return true;
        }

        if self.statics.get(name).is_some() {
            return true;
        }

        if self.constants.get(name).is_some() {
            return true;
        }

        false
    }
}

impl<'symbol_table> ScoperSymbolTable<'symbol_table> {
    #[inline]
    pub fn add_scope(&mut self) {
        self.locals.push(HashSet::with_capacity(u8::MAX as usize));
    }

    #[inline]
    pub fn pop_scope(&mut self) {
        self.locals.pop();
    }

    #[inline]
    pub fn drop_parameters(&mut self) {
        self.parameters.clear();
    }
}
