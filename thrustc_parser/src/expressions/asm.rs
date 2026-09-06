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

use thrustc_ast::{
    Ast, NodeId,
    traits::{AstCodeLocation, AstStandardExtensions},
};
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, attributes, expressions, typegeneration};

pub fn build_asm_code_block<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let asm_tk: &Token = ctx.consume(
        TokenType::Asm,
        CompilationIssueCode::E0001,
        "Expected 'asm' keyword.".into(),
    )?;

    let span: Span = asm_tk.get_span();

    let mut args: Vec<Ast> = Vec::with_capacity(10);

    let asm_type: Type = typegeneration::build_type(ctx, false)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::LParen, TokenType::LBrace])?;

    if ctx.match_token(TokenType::LParen)? {
        loop {
            if ctx.check(TokenType::RParen) {
                break;
            }

            args.push(expressions::parse_expr(ctx)?);

            if ctx.check(TokenType::RParen) {
                break;
            } else {
                ctx.consume(
                    TokenType::Colon,
                    CompilationIssueCode::E0001,
                    "Expected ','.".into(),
                )?;
            }
        }

        ctx.consume(
            TokenType::RParen,
            CompilationIssueCode::E0001,
            "Expected ')'.".into(),
        )?;
    }

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let mut assembler: String = String::with_capacity(u8::MAX as usize);
    let mut assembler_pos: usize = 0;

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        let raw_str: Ast = expressions::parse_expr(ctx)?;
        let raw_str_span: Span = raw_str.get_span();

        if !raw_str.is_cstring() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "It is not a null terminated.".into(),
                "You should write a literal string with null termination.".into(),
                None,
                raw_str_span,
            ));
        }

        let assembly: String = if let Ast::CString { bytes, .. } = raw_str {
            String::from_utf8_lossy(&bytes).to_string()
        } else {
            String::new()
        };

        if assembler_pos != 0 {
            assembler.push('\n');
        }

        assembler.push_str(&assembly);

        if ctx.check(TokenType::RBrace) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        assembler_pos += 1;
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let mut constraints: String = String::with_capacity(u8::MAX as usize);
    let mut constraint_pos: usize = 0;

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        let raw_str: Ast = expressions::parse_expr(ctx)?;
        let raw_str_span: Span = raw_str.get_span();

        if !raw_str.is_cnstring() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "It is not a null terminated.".into(),
                "You should write a literal string with null termination.".into(),
                None,
                raw_str_span,
            ));
        }

        let constraint: String = if let Ast::CString { bytes, .. } = raw_str {
            String::from_utf8_lossy(&bytes).to_string()
        } else {
            String::new()
        };

        if constraint_pos != 0 {
            constraints.push('\n');
        }

        constraints.push_str(&constraint);

        if ctx.check(TokenType::RBrace) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        constraint_pos += 1;
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    Ok(Ast::AsmValue {
        assembler,
        constraints,
        args,
        kind: asm_type,
        attributes,
        span,
        id: NodeId::new(),
    })
}
