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

use crate::signatures::Symbol;

#[derive(Debug)]
pub struct ModuleTable {
    types: HashMap<String, Symbol>,
    functions: HashMap<String, Symbol>,
    constants: HashMap<String, Symbol>,
    statics: HashMap<String, Symbol>,
    structs: HashMap<String, Symbol>,
    enums: HashMap<String, Symbol>,
}

impl ModuleTable {
    pub fn new() -> Self {
        Self {
            types: HashMap::with_capacity(u8::MAX as usize),
            functions: HashMap::with_capacity(u8::MAX as usize),
            constants: HashMap::with_capacity(u8::MAX as usize),
            statics: HashMap::with_capacity(u8::MAX as usize),
            structs: HashMap::with_capacity(u8::MAX as usize),
            enums: HashMap::with_capacity(u8::MAX as usize),
        }
    }
}

impl ModuleTable {
    #[inline]
    pub fn add_function(&mut self, name: String, symbol: Symbol) {
        self.functions.insert(name, symbol);
    }

    #[inline]
    pub fn add_constant(&mut self, name: String, symbol: Symbol) {
        self.constants.insert(name, symbol);
    }

    #[inline]
    pub fn add_static(&mut self, name: String, symbol: Symbol) {
        self.statics.insert(name, symbol);
    }

    #[inline]
    pub fn add_type(&mut self, name: String, symbol: Symbol) {
        self.types.insert(name, symbol);
    }

    #[inline]
    pub fn add_enum(&mut self, name: String, symbol: Symbol) {
        self.enums.insert(name, symbol);
    }

    #[inline]
    pub fn add_struct(&mut self, name: String, symbol: Symbol) {
        self.structs.insert(name, symbol);
    }
}

impl ModuleTable {
    #[inline]
    pub fn get_type(&self, name: String) -> Option<&Symbol> {
        self.types.get(name.as_str())
    }

    #[inline]
    pub fn get_enum(&self, name: String) -> Option<&Symbol> {
        self.enums.get(name.as_str())
    }

    #[inline]
    pub fn get_struct(&self, name: String) -> Option<&Symbol> {
        self.structs.get(name.as_str())
    }
}
