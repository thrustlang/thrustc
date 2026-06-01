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

use crate::{
    ParserContext, expressions,
    statements::{self, code_block, variable},
};

pub fn parse_for_loop_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let for_tk: &Token = ctx.consume(
        TokenType::For,
        CompilationIssueCode::E0001,
        "Expected 'for' keyword.".into(),
    )?;

    let span: Span = for_tk.get_span();

    if ctx.check(TokenType::LBrace) {
        let body: Ast<'_> = code_block::parse_code_block_stmt(ctx)?;

        Ok(Ast::Loop {
            block: body.into(),
            kind: Type::Void(span),
            span,
            id: NodeId::new(),
        })
    } else if ctx.match_token(TokenType::SemiColon)? {
        while ctx.match_token(TokenType::SemiColon)? {}

        let body: Ast<'_> = if ctx.check(TokenType::LBrace) {
            code_block::parse_code_block_stmt(ctx)?
        } else {
            statements::parse(ctx)?
        };

        Ok(Ast::Loop {
            block: body.into(),
            kind: Type::Void(span),
            span,
            id: NodeId::new(),
        })
    } else {
        ctx.get_mut_symbols().begin_scope();
        ctx.begin_scope();

        let local: Ast = variable::build_variable_stmt(ctx)?;
        let condition: Ast = expressions::parse_expression(ctx)?;
        let actions: Ast = expressions::parse_expression(ctx)?;

        let body: Ast = if ctx.check(TokenType::LBrace) {
            code_block::parse_code_block_stmt(ctx)?
        } else {
            statements::parse(ctx)?
        };

        ctx.get_mut_symbols().end_scope();
        ctx.end_scope();

        Ok(Ast::For {
            local: local.into(),
            condition: condition.into(),
            actions: actions.into(),
            block: body.into(),
            kind: Type::Void(span),
            span,
            id: NodeId::new(),
        })
    }
}

pub fn parse_loop_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let loop_tk: &Token = ctx.consume(
        TokenType::Loop,
        CompilationIssueCode::E0001,
        "Expected 'loop' keyword.".into(),
    )?;

    let span: Span = loop_tk.get_span();
    let body: Ast = if ctx.check(TokenType::LBrace) {
        code_block::parse_code_block_stmt(ctx)?
    } else {
        statements::parse(ctx)?
    };

    Ok(Ast::Loop {
        block: body.into(),
        kind: Type::Void(span),
        span,
        id: NodeId::new(),
    })
}

pub fn parse_while_loop_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let while_tk: &Token = ctx.consume(
        TokenType::While,
        CompilationIssueCode::E0001,
        "Expected 'while' keyword.".into(),
    )?;

    let span: Span = while_tk.get_span();

    if ctx.check(TokenType::Var) {
        ctx.get_mut_symbols().begin_scope();
        ctx.begin_scope();

        let local: Ast<'_> = variable::build_variable_stmt(ctx)?;

        ctx.consume(
            TokenType::Colon,
            CompilationIssueCode::E0001,
            "Expected ':'.".into(),
        )?;

        let condition: Ast = expressions::parse_expr(ctx)?;

        let body: Ast = if ctx.check(TokenType::LBrace) {
            code_block::parse_code_block_stmt(ctx)?
        } else {
            statements::parse(ctx)?
        };

        ctx.get_mut_symbols().end_scope();
        ctx.end_scope();

        Ok(Ast::While {
            variable: Some(local.into()),
            condition: condition.into(),
            block: body.into(),
            kind: Type::Void(span),
            span,
            id: NodeId::new(),
        })
    } else {
        let condition: Ast = expressions::parse_expr(ctx)?;
        let block: Ast = code_block::parse_code_block_stmt(ctx)?;

        Ok(Ast::While {
            variable: None,
            condition: condition.into(),
            block: block.into(),
            kind: Type::Void(span),
            span,
            id: NodeId::new(),
        })
    }
}
