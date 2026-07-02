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

use thrustc_ast::{Ast, NodeId, traits::AstCodeBlockEntensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    ParserContext, expressions,
    statements::{self, code_block},
};

pub fn build_conditional<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let if_tk: &Token = ctx.consume(
        TokenType::If,
        CompilationIssueCode::E0001,
        "Expected 'if' keyword.".into(),
    )?;

    let span: Span = if_tk.get_span();

    let condition: Ast = expressions::parse_expr(ctx)?;

    let body: Ast = if ctx.check(TokenType::LBrace) {
        code_block::parse_code_block_stmt(ctx)?
    } else {
        statements::parse(ctx)?
    };

    let mut elseif: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    while ctx.check(TokenType::Elif)
        || (ctx.check(TokenType::Else) && ctx.check_to(TokenType::If, 1))
    {
        if ctx.check(TokenType::Elif) {
            ctx.consume(
                TokenType::Elif,
                CompilationIssueCode::E0001,
                "Expected 'elif' keyword.".into(),
            )?;
        } else {
            ctx.consume(
                TokenType::Else,
                CompilationIssueCode::E0001,
                "Expected 'else' keyword.".into(),
            )?;

            ctx.consume(
                TokenType::If,
                CompilationIssueCode::E0001,
                "Expected 'if' keyword.".into(),
            )?;
        }

        let span: Span = ctx.previous().get_span();

        let condition: Ast = expressions::parse_expr(ctx)?;

        let body: Ast = if ctx.check(TokenType::LBrace) {
            code_block::parse_code_block_stmt(ctx)?
        } else {
            statements::parse(ctx)?
        };

        if !body.is_empty_code_block() {
            elseif.push(Ast::Elif {
                condition: condition.into(),
                block: body.into(),
                kind: Type::Void { span },
                span,
                id: NodeId::new(),
            });
        }
    }

    if ctx.match_token(TokenType::Else)? {
        let span: Span = ctx.previous().get_span();

        let else_body: Ast = if ctx.check(TokenType::LBrace) {
            code_block::parse_code_block_stmt(ctx)?
        } else {
            statements::parse(ctx)?
        };

        if !else_body.is_empty_code_block() {
            let else_node: Ast = Ast::Else {
                block: else_body.into(),
                kind: Type::Void { span },
                span,
                id: NodeId::new(),
            };

            return Ok(Ast::If {
                condition: condition.into(),
                then_branch: body.into(),
                else_if_branch: elseif,
                else_branch: Some(else_node.into()),
                kind: Type::Void { span },
                span,
                id: NodeId::new(),
            });
        }
    }

    let normal: Ast<'_> = Ast::If {
        condition: condition.into(),
        then_branch: body.into(),
        else_if_branch: elseif,
        else_branch: None,
        kind: Type::Void { span },
        span,
        id: NodeId::new(),
    };

    Ok(normal)
}
