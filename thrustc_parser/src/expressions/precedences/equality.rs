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
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions::precedences};

pub fn equality_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let mut left: Ast = precedences::cmp::cmp_precedence(ctx)?;

    if ctx.match_token(TokenType::BangEq)? || ctx.match_token(TokenType::EqEq)? {
        let operator_tk: &Token = ctx.previous();
        let operator: TokenType = operator_tk.kind;
        let span: Span = operator_tk.get_span();

        let right: Ast = precedences::cmp::cmp_precedence(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator,
            right: right.into(),
            kind: Type::Bool { span },
            span,
            id: NodeId::new(),
        }
    }

    ctx.leave_expression();

    Ok(left)
}
