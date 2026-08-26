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
    alias: Option<Vec<String>>,
    only: Option<Vec<String>>,
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
            only: None,
            symbols: Vec::with_capacity(u8::MAX as usize),
            submodules: Vec::with_capacity(u8::MAX as usize),
            path,
            unique_id: Uuid::new_v4(),
        }
    }
}

impl Module {
    #[inline]
    pub fn merge_import(&mut self, other: Module) {
        if other.only.is_none() {
            self.only = None;
        } else if let Some(names) = self.only.as_mut() {
            for name in other.only.unwrap_or_default() {
                if !names.contains(&name) {
                    names.push(name);
                }
            }
        }

        if self.alias.is_none() && other.alias.is_some() {
            self.alias = other.alias;
        }
    }
}

impl Module {
    #[inline]
    pub fn set_alias(&mut self, alias: Vec<String>) {
        self.alias = Some(alias);
    }

    #[inline]
    pub fn set_only(&mut self, only: Vec<String>) {
        self.only = Some(only);
    }

    #[inline]
    pub fn get_only(&self) -> Option<&[String]> {
        self.only.as_deref()
    }

    #[inline]
    pub fn is_only_restricted(&self) -> bool {
        self.only.is_some()
    }
}

impl Module {
    #[inline]
    pub fn add_submodule(&mut self, module: Module) {
        self.submodules.push(module);
    }

    #[inline]
    pub fn merge_submodule(&mut self, module: Module) {
        let path: PathBuf = module.get_path().to_path_buf();

        if let Some(existing) = self
            .submodules
            .iter_mut()
            .find(|submodule| submodule.get_path() == path)
        {
            existing.merge_import(module);
        } else {
            self.submodules.push(module);
        }
    }

    #[inline]
    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }

    #[inline]
    pub fn get_submodule_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.submodules
            .iter_mut()
            .find(|submodule| submodule.matches_name(name))
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
        if self.is_only_restricted() {
            return None;
        }

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
        let mut index: usize = 0;

        while index < access.len() {
            let mut matched: bool = false;

            if current_module.is_only_restricted() {
                return None;
            }

            for submodule in &current_module.submodules {
                if let Some(length) = submodule.alias_prefix_len(&access[index..]) {
                    current_module = submodule;
                    index += length;
                    matched = true;
                    break;
                }
            }

            if matched {
                continue;
            }

            for submodule in &current_module.submodules {
                if submodule.matches_name(&access[index]) {
                    current_module = submodule;
                    index += 1;
                    matched = true;
                    break;
                }
            }

            if !matched {
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
    pub fn get_alias(&self) -> Option<&[String]> {
        self.alias.as_deref()
    }

    #[inline]
    pub fn matches_name(&self, name: &str) -> bool {
        self.base_name == name
            || self
                .alias
                .as_ref()
                .is_some_and(|alias| alias.len() == 1 && alias[0] == name)
    }

    #[inline]
    pub fn alias_prefix_len(&self, access: &[String]) -> Option<usize> {
        let alias: &[String] = self.alias.as_ref()?;

        if access.len() < alias.len() {
            return None;
        }

        let (prefix, _) = access.split_at(alias.len());

        (prefix == alias).then_some(alias.len())
    }

    #[inline]
    pub fn get_unique_id(&self) -> &Uuid {
        &self.unique_id
    }
}
