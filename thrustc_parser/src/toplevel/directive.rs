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
use thrustc_directive::FileDirectives;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::ParserContext;

pub fn build_directive<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let directive_tk: &Token = ctx.consume(
        TokenType::Directive,
        CompilationIssueCode::E0001,
        "Expected 'directive' keyword.".into(),
    )?;

    let span: Span = directive_tk.get_span();

    let spec_tk: &Token = ctx.consume(
        TokenType::CString,
        CompilationIssueCode::E0001,
        "Expected a string literal with the compiler flag, like \"--disable-warnings=W0001\"."
            .into(),
    )?;

    let spec: &str = spec_tk.get_lexeme();

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    let warnings_to_disable: Vec<thrustc_errors::CompilationIssueCode> =
        match thrustc_directive::parse_directive(spec) {
            Ok(codes) => codes,
            Err(message) => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    message,
                    "You should write a supported compiler flag, like '--disable-warnings=W0001'."
                        .into(),
                    None,
                    span,
                ));
            }
        };

    thrustc_directive::register_directives(
        ctx.get_file().get_path(),
        FileDirectives {
            warnings_to_disable,
        },
    );

    Ok(Ast::invalid_ast(span))
}