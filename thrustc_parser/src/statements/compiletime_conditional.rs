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
use thrustc_builtins::BuiltinValue;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::{ParserContext, expressions, statements, statements::code_block};

pub fn build_compiletime_conditional<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let span: Span = ctx.peek().get_span();

    let first_condition: bool = self::evaluate_condition(ctx)?;

    let mut active: Option<Ast<'parser>> = None;

    if first_condition {
        active = Some(self::parse_branch(ctx)?);
    } else {
        self::parse_branch_discarded(ctx)?;
    }

    loop {
        if ctx.check(TokenType::ElifAttribute) {
            let condition: bool = self::evaluate_condition(ctx)?;

            if active.is_none() && condition {
                active = Some(self::parse_branch(ctx)?);
            } else {
                self::parse_branch_discarded(ctx)?;
            }

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) && ctx.check_to(TokenType::If, 1) {
            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            let condition: bool = self::evaluate_condition(ctx)?;

            if active.is_none() && condition {
                active = Some(self::parse_branch(ctx)?);
            } else {
                self::parse_branch_discarded(ctx)?;
            }

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) {
            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            if active.is_none() {
                active = Some(self::parse_branch(ctx)?);
            } else {
                self::parse_branch_discarded(ctx)?;
            }

            break;
        }

        break;
    }

    match active {
        Some(ast) => Ok(ast),
        None => {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                "The '@if' compile-time conditional has no active branch.".into(),
                "Every '@if'/'@elif' condition was false and there is no '@else' branch. Make a condition true or add an '@else' branch.".into(),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }
    }
}

fn parse_branch<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    if ctx.check(TokenType::LBrace) {
        code_block::parse_code_block_stmt(ctx)
    } else {
        statements::parse(ctx)
    }
}

fn parse_branch_discarded<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<(), CompilationIssue> {
    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let result: Result<(), CompilationIssue> = self::parse_branch(ctx).map(|_| ());

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    result
}

pub(crate) fn evaluate_condition<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<bool, CompilationIssue> {
    let if_tk: &Token = ctx.advance()?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let expression: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    match thrustc_builtins::value::fold(&expression) {
        Some(BuiltinValue::Bool(condition)) => Ok(condition),
        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "The compile-time condition must be a constant boolean.".into(),
            "You should use a constant expression, like @if(isLinux()) or @if(1 + 1 == 2).".into(),
            None,
            if_tk.get_span(),
        )),
    }
}