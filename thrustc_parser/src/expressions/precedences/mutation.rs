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
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::PrecedenceTypeExtensions};

use crate::{
    ParserContext,
    expressions::{self, precedences},
};

pub fn equal_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let mut expression: Ast = precedences::cast::cast_precedence(ctx)?;

    if ctx.match_token(TokenType::Eq)?
        || ctx.match_token(TokenType::PlusEq)?
        || ctx.match_token(TokenType::MinusEq)?
        || ctx.match_token(TokenType::StarEq)?
        || ctx.match_token(TokenType::SlashEq)?
        || ctx.match_token(TokenType::ArithEq)?
        || ctx.match_token(TokenType::BAndEq)?
        || ctx.match_token(TokenType::BorEq)?
        || ctx.match_token(TokenType::XorEq)?
        || ctx.match_token(TokenType::LShiftEq)?
        || ctx.match_token(TokenType::RShiftEq)?
    {
        let operator_tk = ctx.previous();
        let span: Span = ctx.previous().get_span();

        let expr: Ast = expressions::parse_expr(ctx)?;

        if operator_tk.get_type() != TokenType::Eq {
            let left_type: &Type = expression.get_value_type()?;
            let right_type: &Type = expr.get_value_type()?;
            let kind: Type =
                left_type.get_term_precedence_type(right_type, operator_tk.get_type());

            expression = Ast::BinaryOp {
                left: expression.into(),
                operator: operator_tk.get_type(),
                right: expr.into(),
                kind,
                span,
                id: NodeId::new(),
            };

            ctx.leave_expression();

            return Ok(expression);
        }

        expression = Ast::Mutation {
            source: expression.into(),
            value: expr.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    ctx.leave_expression();

    Ok(expression)
}
