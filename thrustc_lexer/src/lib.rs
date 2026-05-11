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

#![allow(clippy::result_unit_err)]

use thrustc_diagnostician::Diagnostician;
use thrustc_errors::CompilationIssue;
use thrustc_logging::LoggingType;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;
use thrustc_token::Token;
use thrustc_token_type::{TokenType, traits::TokenTypeExtensions};
use unicode_categories::UnicodeCategories;

const PREALLOCATED_TOKENS_CAPACITY: usize = 20 << 10;

mod character;
mod identifier;
mod lex;
mod number;
mod string;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    errors: Vec<CompilationIssue>,
    code: Vec<char>,

    column: usize,
    start: usize,
    current: usize,
    line: usize,
    span: (usize, usize),

    diagnostician: Diagnostician,
}

impl Lexer {
    pub fn lex(file: &CompilationUnit, options: &CompilerOptions) -> Result<Vec<Token>, ()> {
        let code: Vec<char> = file.get_unit_content().chars().collect();

        Self {
            tokens: Vec::with_capacity(PREALLOCATED_TOKENS_CAPACITY),
            errors: Vec::with_capacity(u8::MAX as usize),
            code,
            column: 0,
            start: 0,
            current: 0,
            line: 1,
            span: (0, 0),
            diagnostician: Diagnostician::new(file, options),
        }
        .start()
    }

    pub fn lex_for_preprocessor(
        file: &CompilationUnit,
        options: &CompilerOptions,
    ) -> Result<Vec<Token>, ()> {
        let code: Vec<char> = file.get_unit_content().chars().collect();

        Self {
            tokens: Vec::with_capacity(PREALLOCATED_TOKENS_CAPACITY),
            errors: Vec::with_capacity(u8::MAX as usize),
            code,
            column: 0,
            start: 0,
            current: 0,
            line: 1,
            span: (0, 0),
            diagnostician: Diagnostician::new(file, options),
        }
        .start_for_preprocessor()
    }
}

impl Lexer {
    fn start(&mut self) -> Result<Vec<Token>, ()> {
        while !self.is_eof() {
            self.start = self.current;
            self.start_span();

            if let Err(error) = lex::analyze(self) {
                self.add_error(error);
            }
        }

        if !self.errors.is_empty() {
            self.errors.iter().for_each(|error| {
                self.diagnostician
                    .dispatch_diagnostic(error, LoggingType::Error);
            });

            return Err(());
        }

        self.tokens.push(Token {
            lexeme: String::default(),
            ascii: String::default(),
            kind: TokenType::Eof,
            span: Span::new(self.span()),
        });

        Ok(std::mem::take(&mut self.tokens))
    }

    fn start_for_preprocessor(&mut self) -> Result<Vec<Token>, ()> {
        while !self.is_eof() {
            self.start = self.current;
            self.start_span();

            if let Err(error) = lex::analyze(self) {
                self.add_error(error);
            }
        }

        if !self.errors.is_empty() {
            return Err(());
        }

        self.tokens.push(Token {
            lexeme: String::default(),
            ascii: String::default(),
            kind: TokenType::Eof,
            span: Span::new(self.span()),
        });

        Ok(std::mem::take(&mut self.tokens))
    }
}

impl Lexer {
    pub fn make(&mut self, kind: TokenType) {
        self.end_span();

        let span: Span = Span::new(self.span());

        let lexeme: String = self.lexeme();

        let ascii: String = if kind.is_identifier() {
            string::convert_to_ascii(self, &lexeme)
        } else {
            String::default()
        };

        self.tokens.push(Token {
            lexeme,
            ascii,
            kind,
            span,
        });
    }
}

impl Lexer {
    #[must_use]
    #[inline(always)]
    pub fn char_match(&mut self, char: char) -> bool {
        if !self.is_eof() && self.code[self.current] == char {
            self.current = self.current.saturating_add(1);
            self.column = self.column.saturating_add(1);

            return true;
        }

        false
    }

    #[must_use]
    #[inline(always)]
    pub fn advance(&mut self) -> char {
        if self.current >= self.code.len() {
            '\0'
        } else {
            let ch: char = self.code[self.current];

            self.current = self.current.saturating_add(1);
            self.column = self.column.saturating_add(1);

            ch
        }
    }

    #[inline(always)]
    pub fn advance_only(&mut self) {
        self.current = self.current.saturating_add(1);
        self.column = self.column.saturating_add(1);
    }

    #[must_use]
    #[inline(always)]
    pub fn peek_at(&self, idx: usize) -> char {
        if idx >= self.code.len() {
            return '\0';
        }

        self.code[idx]
    }

    #[must_use]
    #[inline(always)]
    pub fn peek_next(&self) -> char {
        let idx: usize = self.current.saturating_add(1);

        if idx >= self.code.len() {
            return '\0';
        }

        self.code[idx]
    }

    #[must_use]
    #[inline(always)]
    pub fn previous(&self) -> char {
        if self.current == 0 || self.current > self.code.len() {
            '\0'
        } else {
            let idx: usize = self.current.saturating_sub(1);

            self.code[idx]
        }
    }

    #[must_use]
    #[inline(always)]
    pub fn peek(&self) -> char {
        if self.is_eof() {
            return '\0';
        }

        self.code[self.current]
    }

    #[inline]
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }
}

impl Lexer {
    #[inline(always)]
    pub fn lexeme(&self) -> String {
        if let Some(chars) = self.code.get(self.start..self.current) {
            return String::from_iter(chars);
        }

        thrustc_logging::print_warning(LoggingType::Warning, "Couldn't get some token lexeme.");
        String::default()
    }
}

impl Lexer {
    #[must_use]
    #[inline(always)]
    pub fn is_number_boundary(
        &self,
        is_hexadecimal: bool,
        is_binary: bool,
        is_octal: bool,
    ) -> bool {
        self.peek().is_ascii_digit()
            || self.peek() == '_'
            || self.peek() == '.'
            || self.peek() == 'x'
            || self.peek() == 'b'
            || self.peek() == 'o'
            || is_hexadecimal
            || is_binary
            || is_octal
    }

    #[must_use]
    #[inline(always)]
    pub fn is_identifier_boundary(&self, peeked: char) -> bool {
        peeked.is_alphanumeric() || peeked.is_symbol_other() || peeked == '_' || peeked == '@'
    }

    #[must_use]
    #[inline(always)]
    pub fn is_ascii_char(&self, peeked: char) -> bool {
        self.is_alpha_char(peeked) || peeked.is_ascii_digit() || peeked == '_' || peeked == '@'
    }

    #[must_use]
    #[inline(always)]
    pub fn is_alpha_char(&self, char: char) -> bool {
        char.is_ascii_lowercase() || char.is_ascii_uppercase()
    }

    #[must_use]
    #[inline(always)]
    pub fn is_eof(&self) -> bool {
        self.current >= self.code.len()
    }
}

impl Lexer {
    #[inline]
    pub fn span(&self) -> (u32, (u32, u32)) {
        (
            u32::try_from(self.line).unwrap_or(u32::MAX),
            (
                u32::try_from(self.span.0).unwrap_or(u32::MAX),
                u32::try_from(self.span.1).unwrap_or(u32::MAX),
            ),
        )
    }
}

impl Lexer {
    #[inline(always)]
    pub fn start_span(&mut self) {
        self.span.0 = self.column;
    }

    #[inline(always)]
    pub fn end_span(&mut self) {
        self.span.1 = self.column;
    }
}
