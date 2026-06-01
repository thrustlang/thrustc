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

use thrustc_ast::{Ast, NodeId, traits::AstStandardExtensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, statements};

pub fn parse_code_block_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let block_tk: &Token = ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let span: Span = block_tk.get_span();

    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let mut nodes: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut post: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.match_token(TokenType::RBrace)? {
        let statement: Ast<'_> = statements::parse(ctx)?;

        if statement.is_defer_keyword() {
            post.push(statement);
        } else {
            nodes.push(statement);
        }
    }

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    Ok(Ast::Block {
        nodes,
        post,
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}

pub fn parse_code_block_without_start_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let mut nodes: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut post: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.check(TokenType::RBrace) {
        let statement: Ast<'_> = statements::parse(ctx)?;

        if statement.is_defer_keyword() {
            post.push(statement);
        } else {
            nodes.push(statement);
        }
    }

    let block_tk: &Token = ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    let span: Span = block_tk.get_span();

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    Ok(Ast::Block {
        nodes,
        post,
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}
