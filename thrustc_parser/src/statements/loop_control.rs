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

use thrustc_ast::{Ast, NodeId};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::ParserContext;

pub fn parse_continue_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let continue_tk: &Token = ctx.consume(
        TokenType::Continue,
        CompilationIssueCode::E0001,
        "Expected 'continue' keyword.".into(),
    )?;

    let span: Span = continue_tk.get_span();

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::Continue {
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}

pub fn parse_break_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let break_tk: &Token = ctx.consume(
        TokenType::Break,
        CompilationIssueCode::E0001,
        "Expected 'break' keyword.".into(),
    )?;

    let span: Span = break_tk.get_span();

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::Break {
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}

pub fn parse_continueall_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let continueall_tk: &Token = ctx.consume(
        TokenType::ContinueAll,
        CompilationIssueCode::E0001,
        "Expected 'continueall' keyword.".into(),
    )?;

    let span: Span = continueall_tk.get_span();

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::ContinueAll {
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}

pub fn parse_breakall_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let breakall_tk: &Token = ctx.consume(
        TokenType::BreakAll,
        CompilationIssueCode::E0001,
        "Expected 'breakall' keyword.".into(),
    )?;

    let span: Span = breakall_tk.get_span();

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::BreakAll {
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}
