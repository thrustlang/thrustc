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

use thrustc_ast::Ast;
use thrustc_builtins::BuiltinRegistry;
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinArgument;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_parser_table::SymbolTable;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::abort::abort_compilation;

#[derive(Debug)]
pub struct GenericsContext<'parser, 'ctx> {
    tokens: &'parser [Token],
    position: &'ctx mut usize,
    ast: &'ctx mut Vec<Ast<'parser>>,
    symbols: &'ctx mut SymbolTable<'parser>,
    file: &'parser CompilationUnit,
    builtins: &'ctx mut BuiltinRegistry,
    options: &'parser CompilerOptions,
    current_function_name: Option<&'parser str>,
    errors: &'ctx mut Vec<CompilationIssue>,
    warnings: &'ctx mut Vec<CompilationIssue>,
    diagnostician: &'ctx mut Diagnostician,
}

impl<'parser, 'ctx> GenericsContext<'parser, 'ctx> {
    pub fn new(
        tokens: &'parser [Token],
        position: &'ctx mut usize,
        ast: &'ctx mut Vec<Ast<'parser>>,
        symbols: &'ctx mut SymbolTable<'parser>,
        file: &'parser CompilationUnit,
        builtins: &'ctx mut BuiltinRegistry,
        options: &'parser CompilerOptions,
        current_function_name: Option<&'parser str>,
        errors: &'ctx mut Vec<CompilationIssue>,
        warnings: &'ctx mut Vec<CompilationIssue>,
        diagnostician: &'ctx mut Diagnostician,
    ) -> GenericsContext<'parser, 'ctx> {
        GenericsContext {
            tokens,
            position,
            ast,
            symbols,
            file,
            builtins: &mut *builtins,
            options,
            current_function_name,
            errors,
            warnings,
            diagnostician,
        }
    }
}

impl<'parser, 'ctx> GenericsContext<'parser, 'ctx> {
    #[inline(always)]
    #[must_use]
    pub fn peek(&mut self) -> &'parser Token {
        self.tokens.get(*self.position).unwrap_or_else(|| {
            let span: Span = self.previous().get_span();

            abort_compilation(
                self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get a lexical token!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }

    #[inline(always)]
    #[must_use]
    pub fn previous(&mut self) -> &'parser Token {
        let index: (usize, bool) = self.position.overflowing_sub(1);

        let is_overflow: bool = index.1;
        let idx: usize = index.0;

        if is_overflow {
            let span: Span = self.peek().get_span();

            abort_compilation(
                self.diagnostician,
                CompilationPosition::Parser,
                "Unable to parse previous token position!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }

        self.tokens.get(idx).unwrap_or_else(|| {
            let span: Span = self.peek().get_span();

            abort_compilation(
                self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get a lexical token!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }

    #[inline(always)]
    #[must_use]
    pub fn check(&mut self, kind: TokenType) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().kind == kind
    }

    #[inline(always)]
    pub fn consume(
        &mut self,
        kind: TokenType,
        code: CompilationIssueCode,
        help: String,
    ) -> Result<&'parser Token, CompilationIssue> {
        if self.peek().get_type() == kind {
            return self.advance();
        }

        Err(CompilationIssue::Error(
            code,
            help,
            "You should make it match.".into(),
            None,
            self.previous().get_span(),
        ))
    }

    #[inline(always)]
    pub fn advance(&mut self) -> Result<&'parser Token, CompilationIssue> {
        if !self.is_eof() {
            *self.position = self.position.saturating_add(1);
            Ok(self.previous())
        } else {
            Err(CompilationIssue::Error(
                CompilationIssueCode::E0002,
                "EOF has been reached.".into(),
                "EOF".into(),
                None,
                self.peek().get_span(),
            ))
        }
    }

    #[must_use]
    #[inline(always)]
    pub fn is_eof(&mut self) -> bool {
        self.peek().kind == TokenType::Eof
    }

    #[inline(always)]
    pub fn get_symbols(&self) -> &SymbolTable<'parser> {
        self.symbols
    }

    #[inline(always)]
    pub fn get_mut_symbols(&mut self) -> &mut SymbolTable<'parser> {
        self.symbols
    }

    #[inline(always)]
    pub fn get_mut_ast(&mut self) -> &mut Vec<Ast<'parser>> {
        self.ast
    }

    #[inline(always)]
    pub fn get_file(&self) -> &'parser CompilationUnit {
        self.file
    }

    #[inline(always)]
    pub fn add_error_report(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline(always)]
    pub fn add_warning_report(&mut self, warning: CompilationIssue) {
        self.warnings.push(warning);
    }
}

impl<'parser, 'ctx> GenericsContext<'parser, 'ctx> {
    pub fn evaluate_builtin(
        &mut self,
        name: &str,
        args: &[BuiltinArgument],
        span: Span,
    ) -> Result<Ast<'parser>, CompilationIssue> {
        let result: Result<(Ast<'parser>, Vec<CompilationIssue>), CompilationIssue> =
            self.builtins.evaluate(
                name,
                args,
                span,
                self.current_function_name,
                self.options,
                self.file,
            );

        match result {
            Ok((ast, warnings)) => {
                self.warnings.extend(warnings);

                Ok(ast)
            }
            Err(error) => Err(error),
        }
    }
}
