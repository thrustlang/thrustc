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
pub struct ScoperSymbolTable {
    functions: HashSet<String>,
    compiler_intrinsics: HashSet<String>,
    assembler_functions: HashSet<String>,
    statics: HashSet<String>,
    constants: HashSet<String>,

    locals: Vec<HashSet<String>>,

    parameters: HashSet<String>,
}

impl ScoperSymbolTable {
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

impl ScoperSymbolTable {
    #[inline]
    pub fn add_function(&mut self, name: &str) {
        self.functions.insert(name.to_string());
    }

    #[inline]
    pub fn add_compiler_intrinsic(&mut self, name: &str) {
        self.compiler_intrinsics.insert(name.to_string());
    }

    #[inline]
    pub fn add_assembler_function(&mut self, name: &str) {
        self.assembler_functions.insert(name.to_string());
    }

    #[inline]
    pub fn add_static(&mut self, name: &str) {
        self.statics.insert(name.to_string());
    }

    #[inline]
    pub fn add_constant(&mut self, name: &str) {
        self.constants.insert(name.to_string());
    }

    #[inline]
    pub fn add_local(&mut self, name: &str) {
        let Some(last_scope) = self.locals.last_mut() else {
            return;
        };

        last_scope.insert(name.to_string());
    }

    #[inline]
    pub fn add_parameter(&mut self, name: &str) {
        self.parameters.insert(name.to_string());
    }
}

impl ScoperSymbolTable {
    #[inline]
    pub fn symbol_exists(&self, name: &str) -> bool {
        if self.parameters.contains(name) {
            return true;
        }

        {
            for scope in self.locals.iter().rev() {
                if scope.contains(name) {
                    return true;
                }
            }
        }

        if self.functions.contains(name) {
            return true;
        }

        if self.assembler_functions.contains(name) {
            return true;
        }

        if self.compiler_intrinsics.contains(name) {
            return true;
        }

        if self.statics.contains(name) {
            return true;
        }

        if self.constants.contains(name) {
            return true;
        }

        false
    }
}

impl ScoperSymbolTable {
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
