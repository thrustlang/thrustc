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

pub mod array;
pub mod asm;
pub mod builtin_call;
pub mod call;
pub mod deref;
pub mod enum_value;
pub mod fixed_array;
pub mod index;
pub mod precedences;
pub mod property;
pub mod reference;
pub mod struct_constructor;

use thrustc_ast::Ast;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_context::SynchronizationPosition;
use thrustc_token_type::TokenType;

use crate::ParserContext;

pub fn parse_expression<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.get_mut_control_context()
        .add_sync_position(SynchronizationPosition::Expression);

    ctx.enter_expression()?;

    let expression: Ast = precedences::or::or_precedence(ctx)?;

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        String::from("Expected ';'."),
    )?;

    ctx.get_mut_control_context().pop_sync_position();
    ctx.leave_expression();

    Ok(expression)
}

pub fn parse_expr<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.get_mut_control_context()
        .add_sync_position(SynchronizationPosition::Expression);

    ctx.enter_expression()?;

    let expr: Ast = precedences::or::or_precedence(ctx)?;

    ctx.get_mut_control_context().pop_sync_position();
    ctx.leave_expression();

    Ok(expr)
}
