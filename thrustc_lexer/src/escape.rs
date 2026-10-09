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

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use crate::Lexer;

pub fn read(lexer: &mut Lexer) -> Result<char, CompilationIssue> {
    lexer.advance_only();

    if lexer.is_eof() {
        lexer.end_span();

        let span: Span = Span::new(lexer.span());

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Unexpected EOF after escape character.".into(),
            "EOF".into(),
            None,
            span,
        ));
    }

    let escaped_char: char = lexer.advance();

    let value: u8 = match escaped_char {
        'a' => 0x07,
        'b' => 0x08,
        'f' => 0x0C,
        'n' => b'\n',
        'r' => b'\r',
        't' => b'\t',
        'v' => 0x0B,
        '\\' => b'\\',
        '\'' => b'\'',
        '"' => b'"',
        '?' => b'?',

        'x' | 'X' => {
            let mut accumulated: u32 = 0;
            let mut digits: usize = 0;

            while let Some(digit) = lexer.peek().to_digit(16) {
                accumulated = accumulated.wrapping_mul(16).wrapping_add(digit);

                lexer.advance_only();

                digits = digits.saturating_add(1);
            }

            if digits == 0 {
                lexer.end_span();

                let span: Span = Span::new(lexer.span());

                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Invalid hexadecimal escape sequence.".into(),
                    "You must write at least one hexadecimal digit after '\\x'.".into(),
                    None,
                    span,
                ));
            }

            accumulated as u8
        }

        '0'..='7' => {
            let mut accumulated: u32 = escaped_char.to_digit(8).unwrap_or(0);
            let mut digits: usize = 1;

            while digits < 3 {
                let Some(digit) = lexer.peek().to_digit(8) else {
                    break;
                };

                accumulated = accumulated.wrapping_mul(8).wrapping_add(digit);

                lexer.advance_only();

                digits = digits.saturating_add(1);
            }

            accumulated as u8
        }

        _ => {
            lexer.end_span();

            let span: Span = Span::new(lexer.span());

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Invalid escape sequence".into(),
                "You must utilize a valid escape sequence such as '\\n', '\\t', '\\r', '\\0', '\\\\', '\\'', '\\\"', '\\xHH' or '\\ooo'.".into(),
                None,
                span,
            ));
        }
    };

    Ok(value as char)
}
