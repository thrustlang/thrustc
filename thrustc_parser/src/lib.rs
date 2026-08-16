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

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_entities::parser_entities::{AssemblerFunctions, Functions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_logging::LoggingType;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_parser_context::{ControlContext, TypeContext};
use thrustc_parser_table::SymbolTable;
use thrustc_preprocessor::module::Module;

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

mod abort;
mod attributes;
mod builtins;
mod expressions;
mod modificators;
mod module_import;
mod reinterpret;
mod statements;
mod synchronize;
mod toplevel;
mod typegeneration;

#[derive(Debug)]
pub struct ParserContext<'parser> {
    tokens: &'parser [Token],
    ast: Vec<Ast<'parser>>,
    modules: &'parser [Module],

    errors: Vec<CompilationIssue>,
    bugs: Vec<CompilationIssue>,

    control_context: ControlContext,
    type_context: TypeContext,

    options: &'parser CompilerOptions,

    diagnostician: Diagnostician,
    table: SymbolTable<'parser>,

    position: usize,
    scope: usize,
}

#[derive(Debug)]
pub struct Parser<'parser> {
    tokens: &'parser [Token],
    file: &'parser CompilationUnit,
}

impl<'parser> Parser<'parser> {
    #[inline]
    pub fn parse(
        tokens: &'parser [Token],
        modules: &'parser [Module],
        file: &'parser CompilationUnit,
        options: &'parser CompilerOptions,
    ) -> (ParserContext<'parser>, bool) {
        Self { tokens, file }.start_parsing_nodes(modules, options)
    }
}

impl<'parser> Parser<'parser> {
    fn start_parsing_nodes(
        &mut self,
        modules: &'parser [Module],
        options: &'parser CompilerOptions,
    ) -> (ParserContext<'parser>, bool) {
        let mut ctx: ParserContext = ParserContext::new(self.tokens, modules, self.file, options);

        toplevel::parse_forward(&mut ctx);

        while !ctx.is_eof() {
            let top_node: Result<Ast<'_>, CompilationIssue> = toplevel::parse(&mut ctx);

            if let Ok(ast) = top_node {
                ctx.add_ast_node(ast);
                continue;
            }

            if let Err(error) = top_node {
                if error.is_bug() {
                    ctx.add_bug_report(error);
                } else {
                    ctx.add_error_report(error);
                }

                ctx.synchronize();
                continue;
            }
        }

        let throwed_errors: bool = ctx.verify();

        (ctx, throwed_errors)
    }
}

impl<'parser> ParserContext<'parser> {
    pub fn new(
        tokens: &'parser [Token],
        modules: &'parser [Module],
        file: &'parser CompilationUnit,
        options: &'parser CompilerOptions,
    ) -> Self {
        let functions: Functions = Functions::with_capacity(u8::MAX as usize);
        let asm_functions: AssemblerFunctions = AssemblerFunctions::with_capacity(u8::MAX as usize);

        let control_context: ControlContext = ControlContext::new();

        let table: SymbolTable =
            SymbolTable::with_functions(functions, asm_functions, options, file);

        let type_context: TypeContext = TypeContext::new();

        Self {
            tokens,
            ast: Vec::with_capacity(u8::MAX as usize),
            modules,

            errors: Vec::with_capacity(u8::MAX as usize),
            bugs: Vec::with_capacity(u8::MAX as usize),

            control_context,
            type_context,

            options,

            diagnostician: Diagnostician::new(file, options),
            table,

            position: 0,
            scope: 0,
        }
    }
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    pub fn get_modules(&self) -> &'parser [Module] {
        self.modules
    }
}

impl<'parser> ParserContext<'parser> {
    pub fn verify(&mut self) -> bool {
        if !self.bugs.is_empty() {
            for bug in self.bugs.iter() {
                self.diagnostician
                    .dispatch_diagnostic(bug, LoggingType::Bug);
            }

            return true;
        }

        if !self.errors.is_empty() {
            for error in self.errors.iter() {
                self.diagnostician
                    .dispatch_diagnostic(error, LoggingType::Error);
            }

            return true;
        }

        false
    }
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    #[must_use]
    pub fn peek(&mut self) -> &'parser Token {
        self.tokens.get(self.position).unwrap_or_else(|| {
            let span: Span = self.previous().get_span();

            abort::abort_compilation(
                self.get_mut_diagnostician(),
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

            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to parse previous token position!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }

        self.tokens.get(idx).unwrap_or_else(|| {
            let span: Span = self.peek().get_span();

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

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    #[must_use]
    pub fn check(&mut self, kind: TokenType) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().kind == kind
    }

    #[inline(always)]
    #[must_use]
    pub fn check_to(&mut self, kind: TokenType, modifier: usize) -> bool {
        if self.is_eof() {
            return false;
        }

        let next_index: usize = self.position.saturating_add(modifier);

        if next_index >= self.tokens.len() {
            return false;
        }

        self.tokens[next_index].kind == kind
    }

    #[inline(always)]
    #[must_use]
    pub fn check_ahead(&mut self, target: TokenType, breakers: &[TokenType]) -> bool {
        let mut last_position: usize = self.position;

        let has_ahead: bool = loop {
            if last_position >= self.tokens.len() {
                break false;
            }

            if breakers.contains(&self.tokens[last_position].kind) {
                break false;
            }

            if self.tokens[last_position].kind == target {
                break true;
            }

            last_position = last_position.saturating_add(1);
        };

        has_ahead
    }
}

impl<'parser> ParserContext<'parser> {
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
    pub fn consume_these(
        &mut self,
        these: &[TokenType],
        code: CompilationIssueCode,
        help: String,
    ) -> Result<&'parser Token, CompilationIssue> {
        if these.contains(&self.peek().get_type()) {
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
    pub fn go_back(&mut self) {
        self.position = self.position.saturating_sub(1);
    }

    #[inline(always)]
    pub fn match_token(&mut self, kind: TokenType) -> Result<bool, CompilationIssue> {
        if self.peek().kind == kind {
            self.only_advance()?;
            return Ok(true);
        }

        Ok(false)
    }

    #[inline(always)]
    pub fn only_advance(&mut self) -> Result<(), CompilationIssue> {
        if !self.is_eof() {
            self.position = self.position.saturating_add(1);
            Ok(())
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

    #[inline(always)]
    pub fn advance(&mut self) -> Result<&'parser Token, CompilationIssue> {
        if !self.is_eof() {
            self.position = self.position.saturating_add(1);
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
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    pub fn enter_expression(&mut self) -> Result<(), CompilationIssue> {
        let control: &mut ControlContext = self.get_mut_control_context();

        control.increase_expression_depth();

        if control.get_expression_depth() > thrustc_constants::COMPILER_TOO_MANY_EXPRESSION_DEPTH {
            let span: Span = self.peek().get_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0037,
                "Too many depth for a expression.".into(),
                "You should remove the expression nesting".into(),
                None,
                span,
            ));
        }

        Ok(())
    }

    #[inline(always)]
    pub fn leave_expression(&mut self) {
        self.get_mut_control_context().decrease_expression_depth();
    }

    #[inline(always)]
    pub fn enter_type(&mut self) -> Result<(), CompilationIssue> {
        let control: &mut ControlContext = self.get_mut_control_context();

        control.increase_type_depth();

        if control.get_type_depth() > thrustc_constants::COMPILER_TOO_MANY_TYPE_DEPTH {
            let span: Span = self.peek().get_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0037,
                "Too many depth for a type.".into(),
                "You should remove the type nesting".into(),
                None,
                span,
            ));
        }

        Ok(())
    }

    #[inline(always)]
    pub fn leave_type(&mut self) {
        self.get_mut_control_context().decrease_type_depth();
    }

    #[inline(always)]
    pub fn enter_block(&mut self) -> Result<(), CompilationIssue> {
        let control: &mut ControlContext = self.get_mut_control_context();

        control.increase_block_depth();

        if control.get_block_depth() > thrustc_constants::COMPILER_TOO_MANY_BLOCK_DEPTH {
            let span: Span = self.peek().get_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0037,
                "Too many depth for a code block.".into(),
                "You should remove the code block nesting".into(),
                None,
                span,
            ));
        }

        Ok(())
    }

    #[inline(always)]
    pub fn leave_block(&mut self) {
        self.get_mut_control_context().decrease_block_depth();
    }
}

impl ParserContext<'_> {
    #[inline(always)]
    pub fn reset_position(&mut self) {
        self.position = 0;
    }

    #[inline(always)]
    pub fn reset_scope(&mut self) {
        self.scope = 0;
    }

    #[inline(always)]
    pub fn begin_scope(&mut self) {
        self.scope = self.scope.saturating_add(1);
    }

    #[inline(always)]
    pub fn end_scope(&mut self) {
        self.scope = self.scope.saturating_sub(1);
    }
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    pub fn get_symbols(&self) -> &SymbolTable<'parser> {
        &self.table
    }

    #[inline(always)]
    pub fn get_control_context(&self) -> &ControlContext {
        &self.control_context
    }

    #[inline(always)]
    pub fn get_type_context(&self) -> &TypeContext {
        &self.type_context
    }

    #[inline(always)]
    pub fn get_options(&self) -> &CompilerOptions {
        self.options
    }

    #[inline(always)]
    pub fn get_ast(&self) -> &[Ast<'parser>] {
        &self.ast
    }
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    pub fn get_mut_symbols(&mut self) -> &mut SymbolTable<'parser> {
        &mut self.table
    }

    #[inline(always)]
    pub fn get_mut_control_context(&mut self) -> &mut ControlContext {
        &mut self.control_context
    }

    #[inline]
    pub fn get_mut_type_context(&mut self) -> &mut TypeContext {
        &mut self.type_context
    }

    #[inline(always)]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }
}

impl<'parser> ParserContext<'parser> {
    #[inline(always)]
    pub fn add_ast_node(&mut self, ast: Ast<'parser>) {
        self.ast.push(ast);
    }

    #[inline(always)]
    pub fn add_error_report(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline(always)]
    pub fn add_bug_report(&mut self, error: CompilationIssue) {
        self.bugs.push(error);
    }
}

impl ParserContext<'_> {
    #[must_use]
    #[inline(always)]
    pub fn is_main_scope(&self) -> bool {
        self.scope == 0
    }

    #[must_use]
    #[inline(always)]
    pub fn get_scope(&self) -> usize {
        self.scope
    }

    #[must_use]
    #[inline(always)]
    pub fn is_eof(&mut self) -> bool {
        self.peek().kind == TokenType::Eof
    }
}
