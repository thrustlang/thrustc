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

pub mod code_block;
pub mod compiletime_conditional;
pub mod conditional;
pub mod defer;
pub mod local_constant;
pub mod local_enum;
pub mod local_static;
pub mod local_struct;
pub mod local_type;
pub mod loop_control;
pub mod loops;
pub mod terminator;
pub mod variable;

use thrustc_ast::Ast;
use thrustc_errors::CompilationIssue;
use thrustc_parser_context::SynchronizationPosition;
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;

use crate::{ParserContext, expressions};

pub fn parse<'parser>(ctx: &mut ParserContext<'parser>) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.get_mut_control_context()
        .add_sync_position(SynchronizationPosition::Statement);

    let statement: Result<Ast<'parser>, CompilationIssue> = match &ctx.peek().get_type() {
        TokenType::LBrace => Ok(code_block::parse_code_block_stmt(ctx)?),
        TokenType::Return => Ok(terminator::parse_return_stmt(ctx)?),
        TokenType::Static => Ok(local_static::parse_static_stmt(ctx)?),
        TokenType::Const => Ok(local_constant::parse_constant_stmt(ctx)?),
        TokenType::Struct => Ok(local_struct::parse_structure_stmt(ctx)?),
        TokenType::Type => Ok(local_type::parse_custom_type_stmt(ctx)?),
        TokenType::Enum => Ok(local_enum::parse_enum_stmt(ctx)?),
        TokenType::Var => Ok(variable::build_variable_stmt(ctx)?),
        TokenType::If => Ok(conditional::build_conditional(ctx)?),
        TokenType::IfAttribute => Ok(compiletime_conditional::build_compiletime_conditional(ctx)?),
        TokenType::For => Ok(loops::parse_for_loop_stmt(ctx)?),
        TokenType::While => Ok(loops::parse_while_loop_stmt(ctx)?),
        TokenType::Loop => Ok(loops::parse_loop_stmt(ctx)?),
        TokenType::Continue => Ok(loop_control::parse_continue_stmt(ctx)?),
        TokenType::ContinueAll => Ok(loop_control::parse_continueall_stmt(ctx)?),
        TokenType::Break => Ok(loop_control::parse_break_stmt(ctx)?),
        TokenType::BreakAll => Ok(loop_control::parse_breakall_stmt(ctx)?),
        TokenType::Defer => Ok(defer::parse_post_executation_stmt(ctx)?),

        _ => Ok(expressions::parse_expression(ctx)?),
    };

    ctx.get_mut_control_context().pop_sync_position();

    statement
}
