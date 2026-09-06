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

#![allow(clippy::too_many_arguments)]

use std::path::PathBuf;

use thrustc_ast::Ast;
use thrustc_attributes::ThrustAttributes;
use thrustc_builtins::BuiltinRegistry;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    abort,
    module::Module,
    registry::SharedModuleRegistry,
    shared::type_context::TypeParseContext,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing,
};

use thrustc_directive::FileOptions;
use thrustc_generics::GenericScope;

use ahash::AHashSet as HashSet;

#[derive(Debug)]
pub struct ModuleParser<'module_parser> {
    module: Module,
    tokens: Vec<Token>,
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
    visited: HashSet<PathBuf>,
    registry: SharedModuleRegistry,
    builtins: &'module_parser BuiltinRegistry,

    options: &'module_parser CompilerOptions,
    file_options: &'module_parser FileOptions<'module_parser, 'module_parser>,
    file: &'module_parser CompilationUnit,
    diagnostician: Diagnostician,

    current: usize,
    type_depth: u32,
    block_depth: u32,
    type_parameter_scope: GenericScope,
}

impl<'module_parser> ModuleParser<'module_parser> {
    pub fn new(
        name: String,
        tokens: Vec<Token>,
        options: &'module_parser CompilerOptions,
        file_options: &'module_parser FileOptions<'module_parser, 'module_parser>,
        file: &'module_parser CompilationUnit,
        visited: HashSet<PathBuf>,
        registry: SharedModuleRegistry,
        builtins: &'module_parser BuiltinRegistry,
    ) -> Self {
        Self {
            module: Module::new(name, file.get_path().to_path_buf()),
            tokens,

            errors: Vec::with_capacity(u8::MAX as usize),
            warnings: Vec::with_capacity(u8::MAX as usize),
            visited,
            registry,
            builtins,

            diagnostician: Diagnostician::new(file, options),
            options,
            file_options,
            file,

            current: 0,
            type_depth: 0,
            block_depth: 0,
            type_parameter_scope: GenericScope::new(),
        }
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    pub fn parse(mut self) -> Result<Module, ()> {
        while !self.is_eof() {
            let _ = self.forward_declare();
        }

        self.reset_position();

        while !self.is_eof() {
            self.reset_depths();
            let _ = self.start();
        }

        let warnings_to_disable =
            thrustc_directive::combine_warnings_to_disable(self.file_options);

        thrustc_errors::filter_warnings(&warnings_to_disable, &mut self.warnings);

        for warning in self.warnings.iter() {
            self.diagnostician
                .dispatch_diagnostic(warning, thrustc_logging::LoggingType::Warning);
        }

        if !self.errors.is_empty() {
            for error in self.errors.iter() {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            }

            return Err(());
        }

        Ok(self.module)
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    #[inline]
    pub fn enter_type(&mut self) -> Result<(), ()> {
        self.type_depth = self.type_depth.saturating_add(1);

        if self.type_depth > thrustc_constants::COMPILER_TOO_MANY_TYPE_DEPTH {
            return Err(());
        }

        Ok(())
    }

    #[inline]
    pub fn leave_type(&mut self) {
        self.type_depth = self.type_depth.saturating_sub(1);
    }

    #[inline]
    pub fn enter_block(&mut self) -> Result<(), ()> {
        self.block_depth = self.block_depth.saturating_add(1);

        if self.block_depth > thrustc_constants::COMPILER_TOO_MANY_BLOCK_DEPTH {
            return Err(());
        }

        Ok(())
    }

    #[inline]
    pub fn leave_block(&mut self) {
        self.block_depth = self.block_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_depths(&mut self) {
        self.type_depth = 0;
        self.block_depth = 0;
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    pub fn start(&mut self) -> Result<(), ()> {
        match self.peek().get_type() {
            TokenType::Fn => {
                let symbol: Symbol = submodule_parsing::function::parse_function(self)?;
                self.module.add_symbol(symbol);
            }
            TokenType::Static => {
                let symbol: Symbol = submodule_parsing::r#static::parse_static(self)?;
                self.module.add_symbol(symbol);
            }
            TokenType::Const => {
                self.advance_until(TokenType::SemiColon)?;
            }
            _ => {
                let _ = self.advance();
            }
        }

        Ok(())
    }

    fn forward_declare(&mut self) -> Result<(), ()> {
        match self.peek().get_type() {
            TokenType::Import => {
                submodule_parsing::import::parse_import(self)?;
            }
            TokenType::Type => {
                let symbol: Symbol = submodule_parsing::custom_type::parse_type(self)?;
                self.module.add_symbol(symbol);
            }
            TokenType::Struct => {
                let symbol: Symbol = submodule_parsing::structure::parse_structure(self)?;
                self.module.add_symbol(symbol);
            }
            TokenType::Fn => {
                self.skip_signature_or_body()?;
            }
            TokenType::Const => {
                let symbol: Symbol = submodule_parsing::constant::parse_constant(self)?;
                self.module.add_symbol(symbol);
            }
            TokenType::Static => {
                self.advance_until(TokenType::SemiColon)?;
            }
            _ => {
                let _ = self.advance();
            }
        }

        Ok(())
    }

    fn skip_signature_or_body(&mut self) -> Result<(), ()> {
        while !self.check(TokenType::LBrace) && !self.check(TokenType::SemiColon) {
            self.only_advance()?;
        }

        if self.check(TokenType::LBrace) {
            self.only_advance()?;

            let mut depth: usize = 1;

            while depth > 0 {
                if self.check(TokenType::RBrace) {
                    depth = depth.saturating_sub(1);
                } else if self.check(TokenType::LBrace) {
                    depth = depth.saturating_add(1);
                }

                self.only_advance()?;
            }
        } else {
            self.only_advance()?;
        }

        Ok(())
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline]
    pub fn add_warning(&mut self, warning: CompilationIssue) {
        self.warnings.push(warning);
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn consume(&mut self, kind: TokenType) -> Result<&Token, ()> {
        if self.peek().kind == kind {
            return self.advance();
        }

        Err(())
    }

    #[inline]
    pub fn consume_these(&mut self, these: &[TokenType]) -> Result<&Token, ()> {
        if these.contains(&self.peek().get_type()) {
            return self.advance();
        }

        Err(())
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    #[must_use]
    pub fn peek(&mut self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get current a lexical token!",
                Span::nothing(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }

    #[must_use]
    pub fn previous(&mut self) -> &Token {
        let index: (usize, bool) = self.current.overflowing_sub(1);

        let is_overflow: bool = index.1;
        let idx: usize = index.0;

        if is_overflow {
            let span: Span = self.peek().get_span();

            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to parse previous token position!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        } else {
            let span: Span = self.peek().get_span();

            self.tokens.get(idx).unwrap_or_else(|| {
                abort::abort_compilation(
                    &mut self.diagnostician,
                    CompilationPosition::Parser,
                    "Unable to get a lexical token!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
        }
    }
}

impl ModuleParser<'_> {
    #[must_use]
    pub fn check(&mut self, kind: TokenType) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().kind == kind
    }

    #[must_use]
    pub fn check_to(&mut self, kind: TokenType, modifier: usize) -> bool {
        if self.is_eof() {
            return false;
        }

        if self.current + modifier >= self.tokens.len() {
            return false;
        }

        self.tokens[self.current + modifier].kind == kind
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn match_token(&mut self, kind: TokenType) -> Result<bool, ()> {
        if self.peek().kind == kind {
            self.only_advance()?;
            return Ok(true);
        }

        Ok(false)
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn advance_until(&mut self, kind: TokenType) -> Result<(), ()> {
        while !self.match_token(kind)? {
            self.only_advance()?;
        }

        Ok(())
    }

    #[inline]
    pub fn advance_until_check(&mut self, kind: TokenType) -> Result<(), ()> {
        while !self.check(kind) {
            self.only_advance()?;
        }

        Ok(())
    }

    #[inline]
    pub fn advance_until_limits(&mut self, limits: &[TokenType]) -> Result<(), ()> {
        while !limits.iter().any(|limit| self.check(*limit)) {
            self.only_advance()?;
        }

        self.only_advance()?;

        Ok(())
    }

    #[inline]
    pub fn advance_until_check_limits(&mut self, limits: &[TokenType]) -> Result<(), ()> {
        while !limits.iter().any(|limit| self.check(*limit)) {
            self.only_advance()?;
        }

        Ok(())
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn reset_position(&mut self) {
        self.current = 0;
    }

    #[inline]
    pub fn only_advance(&mut self) -> Result<(), ()> {
        if !self.is_eof() {
            self.current = self.current.saturating_add(1);
            return Ok(());
        }

        Err(())
    }

    #[inline]
    pub fn advance(&mut self) -> Result<&Token, ()> {
        if !self.is_eof() {
            self.current = self.current.saturating_add(1);
            return Ok(self.previous());
        }

        Err(())
    }
}

impl ModuleParser<'_> {
    #[must_use]
    pub fn is_eof(&mut self) -> bool {
        self.peek().kind == TokenType::Eof
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    #[inline]
    pub fn get_mut_module(&mut self) -> &mut Module {
        &mut self.module
    }

    #[inline]
    pub fn get_module(&self) -> &Module {
        &self.module
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn has_visited(&self, path: &PathBuf) -> bool {
        self.visited.contains(path)
    }

    #[inline]
    pub fn mark_visited(&mut self, path: PathBuf) {
        self.visited.insert(path);
    }

    #[inline]
    pub fn unmark_visited(&mut self, path: &PathBuf) {
        self.visited.remove(path);
    }
}

impl<'module_parser> ModuleParser<'module_parser> {
    #[inline]
    pub fn get_options(&self) -> &'module_parser CompilerOptions {
        self.options
    }

    #[inline]
    pub fn get_file(&self) -> &'module_parser CompilationUnit {
        self.file
    }

    #[inline]
    pub fn get_global_visited_modules(&self) -> HashSet<PathBuf> {
        self.visited.clone()
    }

    #[inline]
    pub fn get_registry(&self) -> crate::registry::SharedModuleRegistry {
        self.registry.clone()
    }

    #[inline]
    pub fn get_builtins(&self) -> &BuiltinRegistry {
        self.builtins
    }
}

impl ModuleParser<'_> {
    #[inline]
    pub fn begin_generic_scope(&mut self) {
        self.type_parameter_scope.enter_scope();
    }

    #[inline]
    pub fn end_generic_scope(&mut self) {
        self.type_parameter_scope.exit_scope();
    }

    #[inline]
    pub fn push_type_parameter(&mut self, name: String, span: Span) {
        self.type_parameter_scope.push_parameter(name, span);
    }
}

impl TypeParseContext for ModuleParser<'_> {
    #[inline]
    fn peek(&mut self) -> &Token {
        self.peek()
    }

    #[inline]
    fn previous(&mut self) -> &Token {
        self.previous()
    }

    #[inline]
    fn advance(&mut self) -> Result<&Token, ()> {
        self.advance()
    }

    #[inline]
    fn only_advance(&mut self) -> Result<(), ()> {
        self.only_advance()
    }

    #[inline]
    fn consume(&mut self, kind: TokenType) -> Result<&Token, ()> {
        self.consume(kind)
    }

    #[inline]
    fn consume_these(&mut self, these: &[TokenType]) -> Result<&Token, ()> {
        self.consume_these(these)
    }

    #[inline]
    fn check(&mut self, kind: TokenType) -> bool {
        self.check(kind)
    }

    #[inline]
    fn match_token(&mut self, kind: TokenType) -> Result<bool, ()> {
        self.match_token(kind)
    }

    #[inline]
    fn get_builtins(&self) -> &BuiltinRegistry {
        self.get_builtins()
    }

    #[inline]
    fn get_registry(&self) -> crate::registry::SharedModuleRegistry {
        self.get_registry()
    }

    #[inline]
    fn get_options(&self) -> &CompilerOptions {
        self.get_options()
    }

    #[inline]
    fn get_file(&self) -> &CompilationUnit {
        self.get_file()
    }

    #[inline]
    fn enter_type(&mut self) -> Result<(), ()> {
        self.enter_type()
    }

    #[inline]
    fn leave_type(&mut self) {
        self.leave_type()
    }

    #[inline]
    fn add_error(&mut self, error: CompilationIssue) {
        self.add_error(error)
    }

    fn resolve_type_parameter(&self, name: &str) -> Option<Span> {
        self.type_parameter_scope.resolve(name)
    }

    fn resolve_named_type(&self, name: &str) -> Option<Type> {
        if let Some(symbol) = self
            .get_module()
            .search_symbol(name.to_string(), Variant::CustomType)
        {
            if let Signature::CustomType { kind, .. } = &symbol.signature {
                return Some(kind.clone());
            }
        }

        if let Some(symbol) = self
            .get_module()
            .search_symbol(name.to_string(), Variant::Struct)
        {
            if let Signature::Struct { kind, .. } = &symbol.signature {
                return Some(kind.clone());
            }
        }

        self.get_builtins().get_type(name).cloned()
    }

    fn resolve_named_generic(&self, name: &str) -> Option<(Vec<String>, Type)> {
        if let Some(symbol) = self
            .get_module()
            .search_symbol(name.to_string(), Variant::CustomType)
        {
            if let Signature::CustomType {
                kind,
                type_params: Some(type_params),
                ..
            } = &symbol.signature
            {
                return Some((type_params.clone(), kind.clone()));
            }
        }

        if let Some(symbol) = self
            .get_module()
            .search_symbol(name.to_string(), Variant::Struct)
        {
            if let Signature::Struct {
                kind,
                type_params: Some(type_params),
                ..
            } = &symbol.signature
            {
                return Some((type_params.clone(), kind.clone()));
            }
        }

        None
    }

    fn parse_constant_expr(&mut self) -> Result<Ast<'static>, ()> {
        submodule_parsing::expressions::parse_expr(self)
    }

    fn parse_attributes(&mut self, limits: &[TokenType]) -> Result<ThrustAttributes, ()> {
        submodule_parsing::attributes::build_attributes(self, limits)
    }
}
