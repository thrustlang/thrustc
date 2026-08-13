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
use thrustc_token_type::TokenType;

use crate::{Lexer, character, identifier, number, string};

pub fn analyze(lexer: &mut Lexer) -> Result<(), CompilationIssue> {
    match lexer.advance() {
        '[' => lexer.make(TokenType::LBracket),
        ']' => lexer.make(TokenType::RBracket),
        '(' => lexer.make(TokenType::LParen),
        ')' => lexer.make(TokenType::RParen),
        '{' => lexer.make(TokenType::LBrace),
        '}' => lexer.make(TokenType::RBrace),
        ',' => lexer.make(TokenType::Comma),
        '.' if lexer.char_match('.') && lexer.char_match('.') => lexer.make(TokenType::Pass),
        '.' if lexer.char_match('.') => lexer.make(TokenType::Range),
        '.' => lexer.make(TokenType::Dot),
        '%' => lexer.make(TokenType::Arith),
        '*' => lexer.make(TokenType::Star),
        '^' => lexer.make(TokenType::Xor),
        '~' => lexer.make(TokenType::Not),
        '/' if lexer.char_match('/') => loop {
            if lexer.peek() == '\n' || lexer.is_eof() {
                break;
            }

            lexer.advance_only();
        },
        '/' if lexer.char_match('*') => loop {
            if lexer.char_match('*') && lexer.char_match('/') {
                break;
            } else if lexer.is_eof() {
                lexer.end_span();

                let span: Span = Span::new(lexer.span());

                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected comment closer.".into(),
                    "You should write a comment closer '*/'.".into(),
                    None,
                    span,
                ));
            }

            if lexer.peek() == '\n' {
                lexer.line = lexer.line.saturating_add(1);
            }

            lexer.advance_only();
        },
        '/' => lexer.make(TokenType::Slash),
        ';' => lexer.make(TokenType::SemiColon),
        '-' if lexer.char_match('-') => lexer.make(TokenType::MinusMinus),
        '-' if lexer.char_match('=') => lexer.make(TokenType::MinusEq),
        '-' if lexer.char_match('>') => lexer.make(TokenType::Arrow),
        '-' => lexer.make(TokenType::Minus),
        '+' if lexer.char_match('+') => lexer.make(TokenType::PlusPlus),
        '+' if lexer.char_match('=') => lexer.make(TokenType::PlusEq),
        '+' => lexer.make(TokenType::Plus),
        ':' if lexer.char_match(':') => lexer.make(TokenType::ColonColon),
        ':' => lexer.make(TokenType::Colon),
        '!' if lexer.char_match('=') => lexer.make(TokenType::BangEq),
        '!' => lexer.make(TokenType::Bang),
        '=' if lexer.char_match('=') => lexer.make(TokenType::EqEq),
        '=' => lexer.make(TokenType::Eq),
        '<' if lexer.char_match('=') => lexer.make(TokenType::LessEq),
        '<' if lexer.char_match('<') => lexer.make(TokenType::LShift),
        '<' => lexer.make(TokenType::Less),
        '>' if lexer.char_match('=') => lexer.make(TokenType::GreaterEq),
        '>' if lexer.char_match('>') => lexer.make(TokenType::RShift),
        '>' => lexer.make(TokenType::Greater),

        '|' if lexer.char_match('|') => lexer.make(TokenType::Or),
        '|' => lexer.make(TokenType::Bor),
        '&' if lexer.char_match('&') => lexer.make(TokenType::And),
        '&' => lexer.make(TokenType::BAnd),

        '\r' | '\t' => {}
        ' ' => {}

        '\n' => {
            lexer.column = 0;
            lexer.line = lexer.line.saturating_add(1)
        }

        '\'' => character::lex(lexer)?,

        'n' if lexer.char_match('#') && lexer.char_match('"') => string::lex(lexer, false)?,
        '"' => string::lex(lexer, true)?,

        '0'..='9' => number::lex(lexer)?,

        identifier if lexer.is_identifier_boundary(identifier) => identifier::lex(lexer)?,

        _ => {
            lexer.end_span();

            let span: Span = Span::new(lexer.span());

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "It isn't recognized as a character.".into(),
                "You must use an available character in the language grammar.".into(),
                None,
                span,
            ));
        }
    }

    Ok(())
}
