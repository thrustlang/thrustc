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
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::PrecedenceTypeExtensions};

use crate::{ParserContext, expressions::precedences};

pub fn factor<'parser>(ctx: &mut ParserContext<'parser>) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let mut left: Ast = precedences::mutation::equal_precedence(ctx)?;

    while ctx.match_token(TokenType::Slash)?
        || ctx.match_token(TokenType::Star)?
        || ctx.match_token(TokenType::SlashEq)?
        || ctx.match_token(TokenType::StarEq)?
    {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let right: Ast = precedences::mutation::equal_precedence(ctx)?;

        let left_type: &Type = left.get_value_type()?;
        let right_type: &Type = right.get_value_type()?;

        let kind: Type = left_type.get_factor_precedence_type(right_type);

        left = Ast::BinaryOp {
            left: left.clone().into(),
            operator,
            right: right.into(),
            kind,
            span,
            id: NodeId::new(),
        };
    }

    ctx.leave_expression();

    Ok(left)
}
