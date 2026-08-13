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

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::Lexer;

pub fn lex(lexer: &mut Lexer) -> Result<(), CompilationIssue> {
    let char: char = match lexer.advance() {
        '\\' => {
            lexer.end_span();
            let span: Span = Span::new(lexer.span());

            self::handle_char_scape_sequence(lexer, span)?
        }

        c => c,
    };

    lexer.end_span();

    let span: Span = Span::new(lexer.span());

    lexer.advance_only();

    if lexer.previous() != '\'' {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Unclosed character".into(),
            "You should close the literal character using '\'' at the final.".into(),
            None,
            span,
        ));
    }

    lexer.tokens.push(Token {
        lexeme: char.to_string(),
        ascii: String::default(),
        kind: TokenType::Char,
        span,
    });

    Ok(())
}

fn handle_char_scape_sequence(lexer: &mut Lexer, span: Span) -> Result<char, CompilationIssue> {
    match lexer.advance() {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        'r' => Ok('\r'),
        '\\' => Ok('\\'),
        '0' => Ok('\0'),
        '\'' => Ok('\''),
        '"' => Ok('"'),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Invalid escape sequence".into(),
            "You must utilize either '\\n', '\\t', '\\r', '\\0', '\\\\', '\\'', and '\\\"'.".into(),
            None,
            span,
        )),
    }
}
