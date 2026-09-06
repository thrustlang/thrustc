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

use std::path::PathBuf;

use thrustc_ast::Ast;
use thrustc_attributes::ThrustAttributes;
use thrustc_builtins::BuiltinRegistry;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_directive::FileOptions;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use ahash::AHashSet as HashSet;

use crate::{abort, highmodule_parsing, shared::type_context::TypeParseContext};

#[derive(Debug)]
pub struct PreprocessorContext<'preprocessor> {
    tokens: &'preprocessor [Token],
    options: &'preprocessor CompilerOptions,
    file_options: &'preprocessor FileOptions<'preprocessor, 'preprocessor>,
    visited: HashSet<PathBuf>,
    file: &'preprocessor CompilationUnit,
    diagnostician: Diagnostician,
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
    registry: crate::registry::SharedModuleRegistry,
    builtins: &'preprocessor BuiltinRegistry,
    current: usize,
    type_depth: u32,
}

impl<'preprocessor> PreprocessorContext<'preprocessor> {
    pub fn new(
        tokens: &'preprocessor [Token],
        file_options: &'preprocessor FileOptions<'preprocessor, 'preprocessor>,
        file: &'preprocessor CompilationUnit,
        visited: HashSet<PathBuf>,
        registry: crate::registry::SharedModuleRegistry,
        builtins: &'preprocessor BuiltinRegistry,
    ) -> Self {
        Self {
            tokens,
            options: file_options.global(),
            file_options,
            visited,
            file,
            diagnostician: Diagnostician::new(file, file_options.global()),
            errors: Vec::with_capacity(u8::MAX as usize),
            warnings: Vec::with_capacity(u8::MAX as usize),
            registry,
            builtins,
            current: 0,
            type_depth: 0,
        }
    }
}

impl PreprocessorContext<'_> {
    pub fn check_status(&mut self) -> Result<(), ()> {
        if !self.warnings.is_empty() {
            let warnings_to_disable =
                thrustc_directive::combine_warnings_to_disable(self.file_options);

            thrustc_errors::filter_warnings(&warnings_to_disable, &mut self.warnings);

            for warning in self.warnings.iter() {
                self.diagnostician
                    .dispatch_diagnostic(warning, thrustc_logging::LoggingType::Warning);
            }
        }

        if !self.errors.is_empty() {
            {
                for error in self.errors.iter() {
                    self.diagnostician
                        .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
                }
            }
        }

        Ok(())
    }
}

impl PreprocessorContext<'_> {
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

impl<'module_parser> PreprocessorContext<'module_parser> {
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

impl PreprocessorContext<'_> {
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

        let position: usize = self.current.saturating_add(modifier);

        if position >= self.tokens.len() {
            return false;
        }

        self.tokens[position].kind == kind
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn match_token(&mut self, kind: TokenType) -> Result<bool, ()> {
        if self.peek().kind == kind {
            self.only_advance()?;
            return Ok(true);
        }

        Ok(false)
    }
}

impl PreprocessorContext<'_> {
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

impl PreprocessorContext<'_> {
    #[inline]
    pub fn only_advance(&mut self) -> Result<(), ()> {
        if !self.is_eof() {
            self.current += 1;
            return Ok(());
        }

        Err(())
    }

    #[inline]
    pub fn advance(&mut self) -> Result<&Token, ()> {
        if !self.is_eof() {
            self.current += 1;
            return Ok(self.previous());
        }

        Err(())
    }
}

impl PreprocessorContext<'_> {
    #[must_use]
    pub fn is_eof(&mut self) -> bool {
        self.peek().kind == TokenType::Eof
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn merge_errors(&mut self, other: Vec<CompilationIssue>) {
        self.errors.extend(other);
    }

    #[inline]
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline]
    pub fn add_warning(&mut self, warning: CompilationIssue) {
        self.warnings.push(warning);
    }
}

impl PreprocessorContext<'_> {
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

impl<'module_parser> PreprocessorContext<'module_parser> {
    #[inline]
    pub fn get_options(&self) -> &'module_parser CompilerOptions {
        self.options
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

    #[inline]
    pub fn get_compilation_unit(&self) -> &CompilationUnit {
        self.file
    }
}

impl PreprocessorContext<'_> {
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
}

impl TypeParseContext for PreprocessorContext<'_> {
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
        self.get_compilation_unit()
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

    #[inline]
    fn resolve_named_type(&self, name: &str) -> Option<Type> {
        self.get_builtins().get_type(name).cloned()
    }

    #[inline]
    fn resolve_type_parameter(&self, _name: &str) -> Option<Span> {
        None
    }

    #[inline]
    fn resolve_named_generic(&self, _name: &str) -> Option<(Vec<String>, Type)> {
        None
    }

    #[inline]
    fn parse_constant_expr(&mut self) -> Result<Ast<'static>, ()> {
        highmodule_parsing::compiletime_conditional::parse_expression(self)
    }

    #[inline]
    fn parse_attributes(&mut self, _limits: &[TokenType]) -> Result<ThrustAttributes, ()> {
        Ok(Vec::new())
    }
}
