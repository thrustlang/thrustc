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

use std::cell::RefCell;
use std::rc::Rc;

use ahash::AHashMap as HashMap;

use crate::module::Module;

pub type SharedModuleRegistry = Rc<RefCell<ModuleRegistry>>;

#[derive(Debug, Default)]
pub struct ModuleRegistry {
    modules: HashMap<String, Vec<Rc<Module>>>,
}

impl ModuleRegistry {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

impl ModuleRegistry {
    #[inline]
    pub fn register(&mut self, module: &Module) {
        let name: String = module.get_name().to_string();

        let modules: &mut Vec<Rc<Module>> = self.modules.entry(name).or_default();

        if let Some(index) = modules.iter().position(|candidate| {
            candidate.get_path() == module.get_path() && candidate.get_alias() == module.get_alias()
        }) {
            modules[index] = Rc::new(module.clone());
        } else {
            modules.push(Rc::new(module.clone()));
        }
    }

    #[inline]
    pub fn find(&self, name: &str) -> Option<Rc<Module>> {
        self.modules.get(name).and_then(|modules| {
            modules
                .iter()
                .min_by_key(|module| module.get_path().components().count())
                .cloned()
        })
    }
}

impl ModuleRegistry {
    pub fn resolve(&self, access: &[String]) -> Option<Rc<Module>> {
        for modules in self.modules.values() {
            for module in modules {
                if let Some(length) = module.alias_prefix_len(access) {
                    let rest: &[String] = &access[length..];

                    if rest.is_empty() {
                        return Some(module.clone());
                    }

                    if let Some(submodule) = module.find_submodule(rest.to_vec()) {
                        return Some(Rc::new(submodule.clone()));
                    }
                }
            }
        }

        let first: &String = access.first()?;

        let module: Rc<Module> = self.find(first)?;

        if access.len() == 1 {
            return Some(module);
        }

        module
            .find_submodule(access[1..].to_vec())
            .map(|submodule| Rc::new(submodule.clone()))
    }
}
