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

use std::path::Path;

use thrustc_ast::{Ast, NodeId};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::traits::TokenExtensions;
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

    let span: Span;
    let mut path_segments: Vec<String> = Vec::with_capacity(u8::MAX as usize);
    let mut string_path: Option<String> = None;

    if ctx.check(TokenType::Identifier) {
        let first_tk: &thrustc_token::Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected an identifier.".into(),
        )?;

        let mut identifier_span: Span = first_tk.get_span();
        path_segments.push(first_tk.get_lexeme().to_string());

        while ctx.check(TokenType::ColonColon) {
            ctx.only_advance()?;

            let part_tk: &thrustc_token::Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected an identifier after the path separator.".into(),
            )?;

            identifier_span = part_tk.get_span();
            path_segments.push(part_tk.get_lexeme().to_string());
        }

        span = identifier_span;
    } else {
        let tk: &thrustc_token::Token = ctx.consume_these(
            &[TokenType::CString, TokenType::CNString],
            CompilationIssueCode::E0001,
            "Expected string literal.".into(),
        )?;

        span = tk.get_span();
        string_path = Some(tk.get_lexeme().to_string());
    };

    let mut only_names: Vec<String> = Vec::with_capacity(u8::MAX as usize);

    if ctx.match_token(TokenType::Only)? {
        ctx.consume(
            TokenType::LBrace,
            CompilationIssueCode::E0001,
            "Expected '{' after 'only'.".into(),
        )?;

        while !ctx.check(TokenType::RBrace) {
            let name_tk: &thrustc_token::Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected an identifier in the 'only' list.".into(),
            )?;

            only_names.push(name_tk.get_lexeme().to_string());

            let _ = ctx.match_token(TokenType::Comma)?;
        }

        ctx.consume(
            TokenType::RBrace,
            CompilationIssueCode::E0001,
            "Expected '}' to close the 'only' list.".into(),
        )?;
    }

    let mut alias_parts: Vec<String> = Vec::with_capacity(u8::MAX as usize);

    if ctx.match_token(TokenType::As)? {
        let alias_tk: &thrustc_token::Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected identifier for the module alias.".into(),
        )?;

        alias_parts.push(alias_tk.get_lexeme().to_string());

        while ctx.match_token(TokenType::ColonColon)? {
            let part_tk: &thrustc_token::Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected identifier after the path separator.".into(),
            )?;

            alias_parts.push(part_tk.get_lexeme().to_string());
        }
    }

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    if !only_names.is_empty() {
        let access: Vec<String> = if !alias_parts.is_empty() {
            alias_parts
        } else if let Some(last) = path_segments.last() {
            vec![last.clone()]
        } else if let Some(path) = &string_path {
            vec![Path::new(path)
                .file_stem()
                .map_or_else(String::new, |stem| stem.to_string_lossy().to_string())]
        } else {
            Vec::new()
        };

        if !access.is_empty() {
            crate::module_import::synthesize_only_import(ctx, &access, &only_names, span)?;
        }
    }

    Ok(Ast::Import {
        span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}
