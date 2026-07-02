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
use thrustc_span::Span;
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::Lexer;

const I8_MIN: isize = -128;
const I8_MAX: isize = 127;
const I16_MIN: isize = -32768;
const I16_MAX: isize = 32767;
const I32_MIN: isize = -2147483648;
const I32_MAX: isize = 2147483647;

const U8_MAX: usize = 255;
const U16_MAX: usize = 65535;
const U32_MAX: usize = 4294967295;

#[inline]
pub fn check_integer_format(lexer: &Lexer, lexeme: &str) -> Result<(), CompilationIssue> {
    let span: Span = Span::new(lexer.span());

    if let Some(rest) = lexeme.strip_prefix("0x") {
        return self::check_integer_radix_format(rest, 16, span);
    }

    if let Some(rest) = lexeme.strip_prefix("0b") {
        return self::check_integer_radix_format(rest, 2, span);
    }

    if let Some(rest) = lexeme.strip_prefix("0o") {
        return self::check_integer_radix_format(rest, 8, span);
    }

    let cleaned: String = lexeme.replace('_', "");

    match cleaned.parse::<usize>() {
        Ok(num) if num <= U8_MAX || num <= U16_MAX || num <= U32_MAX || num < usize::MAX => Ok(()),
        Ok(_) => Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Literal is too large to be represented in a integer type.".into(),
            "You can shorten it.".into(),
            None,
            span,
        )),
        Err(_) => match cleaned.parse::<isize>() {
            Ok(num)
                if (I8_MIN..=I8_MAX).contains(&num)
                    || (I16_MIN..=I16_MAX).contains(&num)
                    || (I32_MIN..=I32_MAX).contains(&num)
                    || (isize::MIN..=isize::MAX).contains(&num) =>
            {
                Ok(())
            }
            Ok(_) => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Literal is too large to be represented in a integer type.".into(),
                "You can shorten it.".into(),
                None,
                span,
            )),
            Err(_) => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Literal is too large to be represented in a integer type.".into(),
                "You can shorten it.".into(),
                None,
                span,
            )),
        },
    }
}

fn check_integer_radix_format(
    lexeme: &str,
    radix: u32,
    span: Span,
) -> Result<(), CompilationIssue> {
    let cleaned: String = lexeme.replace('_', "");

    let prefix_name: &str = if radix == 16 {
        "hexadecimal"
    } else if radix == 8 {
        "octal"
    } else {
        "binary"
    };

    match isize::from_str_radix(&cleaned, radix) {
        Ok(num)
            if (I8_MIN..=I8_MAX).contains(&num)
                || (I16_MIN..=I16_MAX).contains(&num)
                || (I32_MIN..=I32_MAX).contains(&num)
                || (isize::MIN..=isize::MAX).contains(&num) =>
        {
            Ok(())
        }
        Ok(_) => Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Integer out of bounds signed {} format.", prefix_name),
            "You can shorten it.".into(),
            None,
            span,
        )),
        Err(_) => match usize::from_str_radix(&cleaned, radix) {
            Ok(num) if num <= U8_MAX || num <= U16_MAX || num <= U32_MAX || num < usize::MAX => {
                Ok(())
            }
            Ok(_) => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                format!("Integer out of bounds unsigned {} format.", prefix_name),
                "You can shorten it.".into(),
                None,
                span,
            )),
            Err(_) => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                format!("Integer invalid {} format.", prefix_name),
                format!("It is not in {} format.", prefix_name),
                None,
                span,
            )),
        },
    }
}

pub fn lex(lexer: &mut Lexer) -> Result<(), CompilationIssue> {
    let mut is_hexadecimal: bool = false;
    let mut is_binary: bool = false;
    let mut is_octal: bool = false;

    while lexer.is_number_boundary(is_hexadecimal, is_binary, is_octal) {
        if is_hexadecimal && lexer.previous() == '0' && lexer.peek() == 'x' {
            lexer.end_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Integer hexadecimal prefix '0x' cannot be repeated.".into(),
                "Remove it.".into(),
                None,
                Span::new(lexer.span()),
            ));
        }

        if is_binary && lexer.previous() == '0' && lexer.peek() == 'b' {
            lexer.end_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Integer binary prefix '0b' cannot be repeated.".into(),
                "Remove it.".into(),
                None,
                Span::new(lexer.span()),
            ));
        }

        if is_octal && lexer.previous() == '0' && lexer.peek() == 'o' {
            lexer.end_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Integer octal prefix '0o' cannot be repeated.".into(),
                "Remove it.".into(),
                None,
                Span::new(lexer.span()),
            ));
        }

        if is_hexadecimal && (!lexer.peek().is_ascii_alphanumeric() && lexer.peek() != '_') {
            lexer.end_span();
            break;
        }

        if is_binary && (!lexer.peek().is_ascii_digit() && lexer.peek() != '_') {
            lexer.end_span();
            break;
        }

        if is_octal && (!lexer.peek().is_ascii_digit() && lexer.peek() != '_') {
            lexer.end_span();
            break;
        }

        if lexer.peek() == 'x' && lexer.peek_next().is_ascii_alphanumeric() {
            is_hexadecimal = true;
        }

        if lexer.peek() == 'b' && lexer.peek_next().is_ascii_digit() {
            is_binary = true;
        }

        if lexer.peek() == 'o' && lexer.peek_next().is_ascii_digit() {
            is_octal = true;
        }

        let _ = lexer.advance();
    }

    lexer.end_span();

    let span: Span = Span::new(lexer.span());

    let lexeme: String = lexer.lexeme();

    if lexeme.contains(".") {
        lexer.tokens.push(Token {
            lexeme,
            ascii: String::default(),
            kind: TokenType::Float,
            span,
        });

        return Ok(());
    } else {
        self::check_integer_format(lexer, &lexeme)?;

        lexer.tokens.push(Token {
            lexeme,
            ascii: String::default(),
            kind: TokenType::Integer,
            span,
        });
    }

    Ok(())
}
