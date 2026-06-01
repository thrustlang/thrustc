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

use thrustc_ast::{Ast, NodeId, traits::AstGetType};
use thrustc_errors::CompilationIssue;
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::CastTypeExtensions};

use crate::{
    ParserContext,
    expressions::{self, precedences},
};

pub fn unary_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    if ctx.match_token(TokenType::Bang)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let expr: Ast = precedences::indirect_call::indirect_precedence(ctx)?;

        ctx.leave_expression();

        return Ok(Ast::UnaryOp {
            operator,
            node: expr.into(),
            kind: Type::Bool { span },
            before: false,
            span,
            id: NodeId::new(),
        });
    }

    if ctx.match_token(TokenType::Minus)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let expr: Ast = precedences::indirect_call::indirect_precedence(ctx)?;
        let expr_type: &Type = expr.get_value_type()?;

        let kind: Type = expr_type.narrowing();

        ctx.leave_expression();

        return Ok(Ast::UnaryOp {
            operator,
            node: expr.into(),
            kind,
            before: false,
            span,
            id: NodeId::new(),
        });
    }

    if ctx.match_token(TokenType::Not)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let expr: Ast = precedences::indirect_call::indirect_precedence(ctx)?;
        let expr_type: Type = expr.get_value_type()?.clone();

        ctx.leave_expression();

        return Ok(Ast::UnaryOp {
            operator,
            node: expr.into(),
            kind: expr_type,
            before: false,
            span,
            id: NodeId::new(),
        });
    }

    if ctx.match_token(TokenType::PlusPlus)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let expr: Ast = expressions::parse_expr(ctx)?;
        let expr_type: Type = expr.get_value_type()?.clone();

        ctx.leave_expression();

        return Ok(Ast::UnaryOp {
            operator,
            node: expr.into(),
            kind: expr_type,
            before: true,
            span,
            id: NodeId::new(),
        });
    }

    if ctx.match_token(TokenType::MinusMinus)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let expr: Ast = expressions::parse_expr(ctx)?;
        let expr_type: Type = expr.get_value_type()?.clone();

        ctx.leave_expression();

        return Ok(Ast::UnaryOp {
            operator,
            node: expr.into(),
            kind: expr_type,
            before: true,
            span,
            id: NodeId::new(),
        });
    }

    let instr: Ast = precedences::indirect_call::indirect_precedence(ctx)?;

    ctx.leave_expression();

    Ok(instr)
}
