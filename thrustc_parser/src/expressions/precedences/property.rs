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

use crate::{
    ParserContext,
    expressions::{self, precedences},
};
use thrustc_ast::Ast;
use thrustc_errors::CompilationIssue;
use thrustc_token_type::TokenType;

pub fn property_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let mut expr: Ast = precedences::unary::unary_precedence(ctx)?;

    if ctx.match_token(TokenType::Dot)? {
        expr = expressions::property::build_property(ctx, expr, false)?;
    } else {
        while ctx.check(TokenType::Arrow) && ctx.check_to(TokenType::Identifier, 1) {
            ctx.advance()?;
            expr = expressions::property::build_property(ctx, expr, true)?;
        }
    }

    ctx.leave_expression();

    Ok(expr)
}
