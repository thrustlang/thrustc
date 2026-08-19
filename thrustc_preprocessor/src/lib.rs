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

#![allow(clippy::result_unit_err)]

use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::{context::PreprocessorContext, module::Module};

use ahash::AHashSet as HashSet;

mod abort;
mod context;
mod highmodule_parsing;
pub mod module;
mod moduletable;
mod parser;
pub mod registry;
pub mod signatures;
pub mod std_library;
mod submodule_parsing;

#[derive(Debug)]
pub struct Preprocessor {
    modules: Vec<Module>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Self {
            modules: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'preprocessor> Preprocessor {
    pub fn generate_modules(
        &mut self,
        tokens: &'preprocessor [Token],
        options: &'preprocessor CompilerOptions,
        file: &CompilationUnit,
    ) -> Result<&[Module], ()> {
        let file_path: std::path::PathBuf = file.get_path().to_path_buf();

        let mut visited: HashSet<std::path::PathBuf> = HashSet::with_capacity(u8::MAX as usize);
        visited.insert(file_path);

        let registry: crate::registry::SharedModuleRegistry =
            std::rc::Rc::new(std::cell::RefCell::new(crate::registry::ModuleRegistry::new()));

        let mut context: PreprocessorContext<'_> =
            PreprocessorContext::new(tokens, options, file, visited, registry);

        let mut merged: ahash::AHashMap<std::path::PathBuf, usize> =
            ahash::AHashMap::with_capacity(u8::MAX as usize);

        while !context.is_eof() {
            if context.check(TokenType::Import) {
                if let Ok(Some(module)) = highmodule_parsing::import::parse_import(&mut context) {
                    self.merge_module(&mut merged, module);
                }

                continue;
            }

            let _ = context.only_advance();
        }

        context.check_status()?;

        Ok(self.modules.as_slice())
    }

    fn merge_module(&mut self, merged: &mut ahash::AHashMap<std::path::PathBuf, usize>, module: Module) {
        let key: std::path::PathBuf = module.get_path().to_path_buf();

        if let Some(&index) = merged.get(&key) {
            self.modules[index].merge_import(module);
        } else {
            let index: usize = self.modules.len();
            self.modules.push(module);
            merged.insert(key, index);
        }
    }
}
