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

use thrustc_preprocessor::{
    module::Module,
    signatures::{Signature, Symbol, Variant},
};

#[derive(Debug)]
pub struct ExternalSymbolTable<'parser> {
    modules: &'parser [Module],
}

impl<'parser> ExternalSymbolTable<'parser> {
    #[inline]
    pub fn new(modules: &'parser [Module]) -> Self {
        Self { modules }
    }
}

impl<'parser> ExternalSymbolTable<'parser> {
    #[inline]
    pub fn find_module(&self, name: &str) -> Option<&'parser Module> {
        self.modules
            .iter()
            .find(|module| module.matches_name(name))
    }

    pub fn resolve(&self, access: &[String]) -> Option<&'parser Module> {
        for module in self.modules.iter() {
            if let Some(length) = module.alias_prefix_len(access) {
                let rest: &[String] = &access[length..];

                if rest.is_empty() {
                    return Some(module);
                }

                if let Some(submodule) = module.find_submodule(rest.to_vec()) {
                    return Some(submodule);
                }
            }
        }

        let first: &String = access.first()?;

        let module: &Module = self.find_module(first)?;

        if access.len() == 1 {
            return Some(module);
        }

        module.find_submodule(access[1..].to_vec())
    }

    pub fn find_symbol(
        &self,
        access: &[String],
        name: &str,
        variant: Variant,
    ) -> Option<&'parser Symbol> {
        let module: &Module = self.resolve(access)?;

        module.search_symbol(name.to_string(), variant)
    }

    pub fn search_signature(
        &self,
        access: &[String],
        name: &str,
        variant: Variant,
    ) -> Option<&'parser Signature> {
        self.find_symbol(access, name, variant)
            .map(|symbol| &symbol.signature)
    }
}
