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
use thrustc_parser_context::SynchronizationPosition;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::{ParserContext, statements::code_block};

pub const SYNC_STATEMENTS: [TokenType; 19] = [
    TokenType::Return,
    TokenType::Static,
    TokenType::Const,
    TokenType::Struct,
    TokenType::Type,
    TokenType::Enum,
    TokenType::Var,
    TokenType::If,
    TokenType::IfAttribute,
    TokenType::ElifAttribute,
    TokenType::ElseAttribute,
    TokenType::For,
    TokenType::While,
    TokenType::Loop,
    TokenType::Continue,
    TokenType::ContinueAll,
    TokenType::Break,
    TokenType::BreakAll,
    TokenType::Defer,
];

pub const SYNC_DECLARATIONS: [TokenType; 14] = [
    TokenType::Type,
    TokenType::Struct,
    TokenType::Const,
    TokenType::Static,
    TokenType::Enum,
    TokenType::Fn,
    TokenType::IfAttribute,
    TokenType::ElifAttribute,
    TokenType::ElseAttribute,
    TokenType::AsmFn,
    TokenType::Intrinsic,
    TokenType::GlobalAsm,
    TokenType::Import,
    TokenType::Embedded,
];

impl<'parser> ParserContext<'parser> {
    pub fn synchronize(&mut self) {
        self.get_mut_control_context().reset_position();
        self.get_mut_control_context().reset_expression_depth();
        self.get_mut_control_context().reset_type_depth();
        self.get_mut_control_context().reset_block_depth();

        if let Some(position) = self.get_control_context().get_sync_position() {
            match position {
                SynchronizationPosition::Declaration => self::synchonize_top_level(self),
                SynchronizationPosition::Statement => self::synchronize_statement(self),
                SynchronizationPosition::Expression => self::synchonize_expression(self),
                SynchronizationPosition::NoRelevant => (),
            }
        }
    }
}

fn synchonize_top_level(ctx: &mut ParserContext) {
    ctx.get_mut_symbols().finish_parameters();
    ctx.get_mut_symbols().finish_scopes();
    ctx.get_mut_symbols().end_scope();
    ctx.reset_scope();
    ctx.clear_current_function_name();

    loop {
        if ctx.is_eof() {
            break;
        }

        let peeked: &Token = ctx.peek();

        if SYNC_DECLARATIONS.contains(&peeked.kind) {
            break;
        }

        let _ = ctx.only_advance();
    }
}

fn synchronize_statement<'parser>(ctx: &mut ParserContext<'parser>) {
    loop {
        if ctx.is_eof() {
            break;
        }

        if ctx.is_main_scope() {
            self::synchonize_top_level(ctx);
            break;
        }

        let peeked: &Token = ctx.peek();

        if SYNC_STATEMENTS.contains(&peeked.get_type()) {
            let first_: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                code_block::parse_code_block_without_start_stmt(ctx);

            if first_.is_ok() {
                let mut continue_first_loop: bool = false;

                while ctx.check_ahead(TokenType::RBrace, &SYNC_DECLARATIONS) {
                    if ctx.is_eof() {
                        break;
                    }

                    let second_: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                        code_block::parse_code_block_without_start_stmt(ctx);

                    if second_.is_err() {
                        continue_first_loop = true;
                        break;
                    }
                }

                if continue_first_loop {
                    let _ = ctx.only_advance();
                    continue;
                }

                break;
            } else {
                let _ = ctx.only_advance();
                continue;
            }
        }

        let _ = ctx.only_advance();
    }
}

fn synchonize_expression<'parser>(ctx: &mut ParserContext<'parser>) {
    loop {
        if ctx.is_eof() {
            break;
        }

        if ctx.is_main_scope() {
            self::synchonize_top_level(ctx);
            break;
        }

        let peeked: &Token = ctx.peek();

        if SYNC_STATEMENTS.contains(&peeked.get_type()) {
            let first_: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                code_block::parse_code_block_without_start_stmt(ctx);

            if first_.is_ok() {
                let mut continue_first_loop: bool = false;

                while ctx.check_ahead(TokenType::RBrace, &SYNC_DECLARATIONS) {
                    if ctx.is_eof() {
                        break;
                    }

                    let second_: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                        code_block::parse_code_block_without_start_stmt(ctx);

                    if second_.is_err() {
                        continue_first_loop = true;
                        break;
                    }
                }

                if continue_first_loop {
                    let _ = ctx.only_advance();
                    continue;
                }

                break;
            } else {
                let _ = ctx.only_advance();
                continue;
            }
        }

        let _ = ctx.only_advance();
    }
}
