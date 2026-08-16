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

use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::signatures::{Symbol, Variant};

#[derive(Debug, Clone)]
pub struct Module {
    base_name: String,
    alias: Option<String>,
    symbols: Vec<Symbol>,
    submodules: Vec<Module>,
    path: PathBuf,
    unique_id: Uuid,
}

impl Module {
    pub fn new(base_name: String, path: PathBuf) -> Self {
        Module {
            base_name,
            alias: None,
            symbols: Vec::with_capacity(u8::MAX as usize),
            submodules: Vec::with_capacity(u8::MAX as usize),
            path,
            unique_id: Uuid::new_v4(),
        }
    }

    #[inline]
    pub fn set_alias(&mut self, alias: String) {
        self.alias = Some(alias);
    }
}

impl Module {
    #[inline]
    pub fn add_submodule(&mut self, module: Module) {
        self.submodules.push(module);
    }

    #[inline]
    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }
}

impl Module {
    #[inline]
    pub fn get_path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn get_symbols(&self) -> &[Symbol] {
        &self.symbols
    }

    #[inline]
    pub fn get_submodules(&self) -> &[Module] {
        &self.submodules
    }
}

impl Module {
    pub fn search_symbol(&self, hint: String, target_variant: Variant) -> Option<&Symbol> {
        {
            for symbol in self.symbols.iter() {
                let Symbol { name, variant, .. } = symbol;

                if hint == *name && *variant == target_variant {
                    return Some(symbol);
                }
            }
        }

        None
    }

    #[inline]
    pub fn find_submodule(&self, access: Vec<String>) -> Option<&Module> {
        let mut current_module: &Module = self;

        for name in access.iter() {
            let mut found: bool = false;

            for submodule in &current_module.submodules {
                if submodule.matches_name(name) {
                    current_module = submodule;
                    found = true;
                    break;
                }
            }

            if !found {
                return None;
            }
        }

        Some(current_module)
    }
}

impl Module {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.base_name
    }

    #[inline]
    pub fn get_alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }

    #[inline]
    pub fn matches_name(&self, name: &str) -> bool {
        self.base_name == name || self.alias.as_deref() == Some(name)
    }

    #[inline]
    pub fn get_unique_id(&self) -> &Uuid {
        &self.unique_id
    }
}
