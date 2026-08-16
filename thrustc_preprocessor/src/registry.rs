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
    modules: HashMap<String, Rc<Module>>,
}

impl ModuleRegistry {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn register(&mut self, module: &Module) {
        let name: String = module.get_name().to_string();

        self.modules.insert(name, Rc::new(module.clone()));
    }

    #[inline]
    pub fn find(&self, name: &str) -> Option<Rc<Module>> {
        self.modules.get(name).cloned()
    }
}
