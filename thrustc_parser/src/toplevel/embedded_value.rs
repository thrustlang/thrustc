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
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, type_metadata::ArrayTypeMetadata};

use crate::ParserContext;

pub fn build_embedded<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Embedded,
        CompilationIssueCode::E0001,
        "Expected 'embedded' keyword.".into(),
    )?;

    let name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let lexeme: &str = name_tk.get_lexeme();
    let span: Span = name_tk.get_span();

    ctx.consume_these(
        &[TokenType::CNString, TokenType::CString],
        CompilationIssueCode::E0001,
        "Expected string literal.".into(),
    )?;

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    let array_ty: Type = Type::Array {
        base_type: Type::U8 { span }.into(),
        infered_type: None,
        metadata: ArrayTypeMetadata::new(None, None),
        span,
    };

    Ok(Ast::Embedded {
        name: lexeme,
        path: lexeme.into(),
        literal: lexeme,
        kind: array_ty,
        span,
        id: NodeId::new(),
    })
}
