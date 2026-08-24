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
use thrustc_attributes::ThrustAttributes;
use thrustc_builtins::BuiltinRegistry;
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::Token;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::registry::SharedModuleRegistry;

pub trait TypeParseContext {
    fn peek(&mut self) -> &Token;
    fn previous(&mut self) -> &Token;
    fn advance(&mut self) -> Result<&Token, ()>;
    fn only_advance(&mut self) -> Result<(), ()>;
    fn consume(&mut self, kind: TokenType) -> Result<&Token, ()>;
    fn consume_these(&mut self, these: &[TokenType]) -> Result<&Token, ()>;
    fn check(&mut self, kind: TokenType) -> bool;
    fn match_token(&mut self, kind: TokenType) -> Result<bool, ()>;

    fn get_builtins(&self) -> &BuiltinRegistry;
    fn get_registry(&self) -> SharedModuleRegistry;
    fn get_options(&self) -> &CompilerOptions;
    fn get_file(&self) -> &CompilationUnit;

    fn enter_type(&mut self) -> Result<(), ()>;
    fn leave_type(&mut self);
    fn add_error(&mut self, error: CompilationIssue);
    fn resolve_named_type(&self, name: &str, span: Span) -> Option<Type>;
    fn parse_constant_expr(&mut self) -> Result<Ast<'static>, ()>;

    fn parse_attributes(&mut self, limits: &[TokenType]) -> Result<ThrustAttributes, ()>;
}
