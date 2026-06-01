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
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions};

pub fn build_global_assembler<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::GlobalAsm,
        CompilationIssueCode::E0001,
        "Expected 'global_asm' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let assembler: Ast = expressions::parse_expr(ctx)?;
    let asssembler_span: Span = assembler.get_span();

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    if !assembler.is_cstring() {
        ctx.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "It is not a null terminated.".into(),
            "You should write a literal string with null termination.".into(),
            None,
            asssembler_span,
        ));
    }

    let asm: String = if let Ast::CString { bytes, .. } = assembler {
        String::from_utf8_lossy(&bytes).to_string()
    } else {
        String::new()
    };

    Ok(Ast::GlobalAssembler {
        asm,
        span,
        kind: Type::Void(span),
        id: NodeId::new(),
    })
}
