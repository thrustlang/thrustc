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

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let mut parameters_types: Vec<Type> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        parameters_types.push(typegeneration::build_type(ctx, false)?);

        if ctx.check(TokenType::RParen) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
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

    let return_type: Type = typegeneration::build_type(ctx, false)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Assembler])?;

    let assembler_tk: &Token = ctx.consume(
        TokenType::Assembler,
        CompilationIssueCode::E0001,
        "Expected a '#asm{' block.".into(),
    )?;

    let raw_assembler: String = assembler_tk.get_lexeme().to_string();
    let mut assembler_lines: Vec<&str> = raw_assembler.lines().collect();

    while let Some(last) = assembler_lines.last() {
        let instruction: &str = last.trim();

        if instruction.is_empty() {
            assembler_lines.pop();

            continue;
        }

        if instruction.eq_ignore_ascii_case("ret") || instruction.eq_ignore_ascii_case("retq") {
            assembler_lines.pop();
        }

        break;
    }

    let assembler: String = assembler_lines.join("\n");
    let mut args: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

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
                    TokenType::Comma,
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

    Ok(Ast::AsmValue {
        assembler,
        parameters_types,
        args,
        kind: return_type,
        attributes,
        span,
        id: NodeId::new(),
    })
}
