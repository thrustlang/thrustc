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
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;

use crate::statements::compiletime_conditional;
use crate::{ParserContext, synchronize};

pub fn build_compiletime_declaration<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let span: Span = ctx.peek().get_span();

    let first_condition: bool = compiletime_conditional::evaluate_condition(ctx)?;

    let mut active: Option<Ast<'parser>> = None;

    if first_condition {
        active = Some(super::parse(ctx)?);
    } else {
        self::skip_declaration(ctx);
    }

    loop {
        if ctx.check(TokenType::ElifAttribute) {
            let condition: bool = compiletime_conditional::evaluate_condition(ctx)?;

            if active.is_none() && condition {
                active = Some(super::parse(ctx)?);
            } else {
                self::skip_declaration(ctx);
            }

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) && ctx.check_to(TokenType::If, 1) {
            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            let condition: bool = compiletime_conditional::evaluate_condition(ctx)?;

            if active.is_none() && condition {
                active = Some(super::parse(ctx)?);
            } else {
                self::skip_declaration(ctx);
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
                active = Some(super::parse(ctx)?);
            } else {
                self::skip_declaration(ctx);
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

fn skip_declaration(ctx: &mut ParserContext) {
    let mut depth: usize = 0;
    let mut advanced: bool = false;

    loop {
        if ctx.is_eof() {
            break;
        }

        let token: TokenType = ctx.peek().get_type();

        if advanced
            && depth == 0
            && (synchronize::SYNC_DECLARATIONS.contains(&token)
                || matches!(
                    token,
                    TokenType::IfAttribute | TokenType::ElifAttribute | TokenType::ElseAttribute
                ))
        {
            break;
        }

        advanced = true;

        match token {
            TokenType::LBrace => depth = depth.saturating_add(1),
            TokenType::RBrace => {
                if depth == 0 {
                    break;
                }

                depth = depth.saturating_sub(1);
            }
            _ => (),
        }

        let _ = ctx.only_advance();
    }
}
