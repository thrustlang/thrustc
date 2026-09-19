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

use thrustc_builtins::BuiltinRegistry;
use thrustc_directive::FileOptions;
use thrustc_options::CompilationUnit;
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::{context::PreprocessorContext, module::Module};

use ahash::AHashSet as HashSet;

mod abort;
mod context;
mod highmodule_parsing;
pub mod module;
mod module_table;
mod parser;
pub mod registry;
mod shared;
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
        options: &'preprocessor FileOptions<'preprocessor, 'preprocessor>,
        file: &CompilationUnit,
        builtins: &'preprocessor BuiltinRegistry,
    ) -> Result<&[Module], ()> {
        let file_path: std::path::PathBuf = file.get_path().to_path_buf();

        let mut visited: HashSet<std::path::PathBuf> = HashSet::with_capacity(u8::MAX as usize);
        visited.insert(file_path);

        let registry: crate::registry::SharedModuleRegistry = std::rc::Rc::new(
            std::cell::RefCell::new(crate::registry::ModuleRegistry::new()),
        );

        let mut context: PreprocessorContext<'_> =
            PreprocessorContext::new(tokens, options, file, visited, registry, builtins);

        let mut merged: ahash::AHashMap<
            (std::path::PathBuf, Option<Vec<String>>),
            usize,
        > =
            ahash::AHashMap::with_capacity(u8::MAX as usize);
        let mut block_depth: usize = 0;

        while !context.is_eof() {
            if context.check(TokenType::LBrace) {
                block_depth = block_depth.saturating_add(1);
                let _ = context.only_advance();
                continue;
            }

            if context.check(TokenType::RBrace) {
                block_depth = block_depth.saturating_sub(1);
                let _ = context.only_advance();
                continue;
            }

            if block_depth == 0 && context.check(TokenType::Import) {
                match highmodule_parsing::import::parse_import(&mut context) {
                    Ok(Some(module)) => self.merge_module(&mut merged, module),
                    Ok(None) => (),
                    Err(()) => return Err(()),
                }

                continue;
            }

            if block_depth == 0 && context.check(TokenType::IfAttribute) {
                self.handle_conditional_imports(&mut context, &mut merged)?;

                continue;
            }

            let _ = context.only_advance();
        }

        context.check_status()?;

        Ok(self.modules.as_slice())
    }

    fn handle_conditional_imports(
        &mut self,
        context: &mut PreprocessorContext<'_>,
        merged: &mut ahash::AHashMap<(std::path::PathBuf, Option<Vec<String>>), usize>,
    ) -> Result<(), ()> {
        let first_condition: bool =
            highmodule_parsing::compiletime_conditional::evaluate_condition(context)?;

        let mut active: bool = false;

        if first_condition {
            self.merge_active_import(context, merged)?;
            active = true;
        } else {
            highmodule_parsing::compiletime_conditional::skip_import(context)?;
        }

        loop {
            if context.check(TokenType::ElifAttribute) {
                let condition: bool =
                    highmodule_parsing::compiletime_conditional::evaluate_condition(context)?;

                if !active && condition {
                    self.merge_active_import(context, merged)?;
                    active = true;
                } else {
                    highmodule_parsing::compiletime_conditional::skip_import(context)?;
                }

                continue;
            }

            if context.check(TokenType::ElseAttribute) && context.check_to(TokenType::If, 1) {
                context.consume(TokenType::ElseAttribute)?;

                let condition: bool =
                    highmodule_parsing::compiletime_conditional::evaluate_condition(context)?;

                if !active && condition {
                    self.merge_active_import(context, merged)?;
                    active = true;
                } else {
                    highmodule_parsing::compiletime_conditional::skip_import(context)?;
                }

                continue;
            }

            if context.check(TokenType::ElseAttribute) {
                context.consume(TokenType::ElseAttribute)?;

                if active {
                    highmodule_parsing::compiletime_conditional::skip_import(context)?;
                } else {
                    self.merge_active_import(context, merged)?;
                }

                break;
            }

            break;
        }

        Ok(())
    }

    fn merge_active_import(
        &mut self,
        context: &mut PreprocessorContext<'_>,
        merged: &mut ahash::AHashMap<(std::path::PathBuf, Option<Vec<String>>), usize>,
    ) -> Result<(), ()> {
        if !context.check(TokenType::Import) {
            return Ok(());
        }

        match highmodule_parsing::import::parse_import(context) {
            Ok(Some(module)) => self.merge_module(merged, module),
            Ok(None) => (),
            Err(()) => return Err(()),
        }

        Ok(())
    }

    fn merge_module(
        &mut self,
        merged: &mut ahash::AHashMap<(std::path::PathBuf, Option<Vec<String>>), usize>,
        module: Module,
    ) {
        let key: (std::path::PathBuf, Option<Vec<String>>) = (
            module.get_path().to_path_buf(),
            module.get_alias().map(|alias| alias.to_vec()),
        );

        if let Some(&index) = merged.get(&key) {
            self.modules[index].merge_import(module);
        } else {
            let index: usize = self.modules.len();

            self.modules.push(module);

            merged.insert(key, index);
        }
    }
}
