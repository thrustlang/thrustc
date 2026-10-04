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
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::ParserContext;

pub fn build_import_c<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ImportC,
        CompilationIssueCode::E0001,
        "Expected 'importC' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    let path_tk: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected string literal.".into(),
    )?;

    let path_span: Span = path_tk.get_span();
    let path: String = path_tk.get_lexeme().to_string();

    if ctx.check(TokenType::Only) {
        let only_span: Span = ctx.peek().get_span();

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "'only' is not supported by 'importC'.".into(),
            "Remove the 'only { ... }' clause. 'importC' imports declarations from the header, it does not support selective import.".into(),
            None,
            only_span,
        ));
    }

    let mut alias: Option<Vec<String>> = None;

    if ctx.match_token(TokenType::As)? {
        let alias_tk: &Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected identifier for the C import alias.".into(),
        )?;

        let mut alias_parts: Vec<String> = vec![alias_tk.get_lexeme().to_string()];

        while ctx.match_token(TokenType::ColonColon)? {
            let part_tk: &Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected identifier after the path separator.".into(),
            )?;

            alias_parts.push(part_tk.get_lexeme().to_string());
        }

        alias = Some(alias_parts);
    }

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::ImportC {
        path,
        alias,
        span,
        path_span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}
