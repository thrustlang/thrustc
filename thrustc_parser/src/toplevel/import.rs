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
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::ParserContext;

pub fn build_import<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Import,
        CompilationIssueCode::E0001,
        "Expected 'import' keyword.".into(),
    )?;

    let tk: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected string literal.".into(),
    )?;

    let span: Span = tk.get_span();

    if ctx.match_token(TokenType::As)? {
        ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected identifier for the module alias.".into(),
        )?;
    }

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::Import {
        span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}
