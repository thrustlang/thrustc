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

use thrustc_ast::Ast;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_context::SynchronizationPosition;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::ParserContext;

pub mod compiler_intrinsic;
pub mod compiletime_conditional;
pub mod directive;
pub mod embedded_value;
pub mod global_assembler;
pub mod global_assembler_function;
pub mod global_constant;
pub mod global_enum;
pub mod global_function;
pub mod global_static;
pub mod global_struct;
pub mod global_type;
pub mod import;
pub mod importc;

pub fn parse<'parser>(ctx: &mut ParserContext<'parser>) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.get_mut_control_context()
        .add_sync_position(SynchronizationPosition::Declaration);

    let declaration: Result<Ast<'parser>, CompilationIssue> = match ctx.peek().get_type() {
        TokenType::Type => Ok(global_type::build_custom_type(ctx, false)?),
        TokenType::Struct => Ok(global_struct::build_structure(ctx, false)?),
        TokenType::Const => Ok(global_constant::build_global_const(ctx, false)?),
        TokenType::Static => Ok(global_static::build_global_static(ctx, false)?),
        TokenType::Enum => Ok(global_enum::build_enum(ctx, false)?),
        TokenType::Fn => Ok(global_function::build_function(ctx, false)?),
        TokenType::IfAttribute => Ok(compiletime_conditional::build_compiletime_declaration(ctx)?),
        TokenType::AsmFn => Ok(global_assembler_function::build_assembler_function(
            ctx, false,
        )?),
        TokenType::Intrinsic => Ok(compiler_intrinsic::build_compiler_intrinsic(ctx, false)?),
        TokenType::GlobalAsm => Ok(global_assembler::build_global_assembler(ctx)?),
        TokenType::Import => Ok(import::build_import(ctx)?),
        TokenType::Embedded => Ok(embedded_value::build_embedded(ctx)?),
        TokenType::Directive => Ok(directive::build_directive(ctx)?),

        _ => {
            let any: &Token = ctx.advance()?;

            Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected a top entity, not a statement and never an expression.".into(),
                "You should remove it from this scope.".into(),
                None,
                any.get_span(),
            ))
        }
    };

    ctx.get_mut_control_context().pop_sync_position();

    declaration
}

pub fn parse_forward(ctx: &mut ParserContext) {
    let mut at_block: bool = false;

    while !ctx.is_eof() {
        match ctx.peek().get_type() {
            TokenType::Type if !at_block => {
                let _ = global_type::build_custom_type(ctx, true);
            }
            TokenType::Struct if !at_block => {
                let _ = global_struct::build_structure(ctx, true);
            }
            TokenType::Static if !at_block => {
                let _ = global_static::build_global_static(ctx, true);
            }
            TokenType::Const if !at_block => {
                let _ = global_constant::build_global_const(ctx, true);
            }
            TokenType::Enum if !at_block => {
                let _ = global_enum::build_enum(ctx, true);
            }
            TokenType::Intrinsic if !at_block => {
                let _ = compiler_intrinsic::build_compiler_intrinsic(ctx, true);
            }
            TokenType::Fn if !at_block => {
                let _ = global_function::build_function(ctx, true);
            }
            TokenType::AsmFn if !at_block => {
                let _ = global_assembler_function::build_assembler_function(ctx, true);
            }
            TokenType::LBrace => {
                at_block = true;
                let _ = ctx.only_advance();
            }
            TokenType::RBrace => {
                at_block = false;
                let _ = ctx.only_advance();
            }

            _ => {
                let _ = ctx.only_advance();
            }
        }
    }

    ctx.reset_position();
}
