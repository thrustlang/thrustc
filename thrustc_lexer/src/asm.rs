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
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::Lexer;

pub fn lex(lexer: &mut Lexer) -> Result<(), CompilationIssue> {
    if !thrustc_backends::get_compiler_features().is_unstable_mode() {
        lexer.end_span();

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "The '#asm' feature is unstable.".into(),
            "You must pass '-mode unstable' to access it.".into(),
            None,
            Span::new(lexer.span()),
        ));
    }

    let start_line: u32 = u32::try_from(lexer.line).unwrap_or(u32::MAX);
    let start_column: u32 = u32::try_from(lexer.span.0).unwrap_or(u32::MAX);

    if lexer.peek() != 'a'
        || lexer.peek_at(lexer.current.saturating_add(1)) != 's'
        || lexer.peek_at(lexer.current.saturating_add(2)) != 'm'
    {
        lexer.end_span();

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Expected '#asm{' to start a raw assembler block.".into(),
            "You should write '#asm{' before the assembler instructions.".into(),
            None,
            Span::new(lexer.span()),
        ));
    }

    lexer.advance_only();
    lexer.advance_only();
    lexer.advance_only();

    loop {
        if lexer.is_eof() {
            lexer.end_span();

            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected '{' after '#asm'.".into(),
                "You should write '#asm{' before the assembler instructions.".into(),
                None,
                Span::new(lexer.span()),
            ));
        }

        match lexer.peek() {
            ' ' | '\t' | '\r' => {
                lexer.advance_only();
            }
            '\n' => {
                lexer.advance_only();
                lexer.line = lexer.line.saturating_add(1);
                lexer.column = 0;
            }
            '{' => {
                lexer.advance_only();

                break;
            }
            _ => {
                lexer.end_span();

                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected '{' after '#asm'.".into(),
                    "You should write '#asm{' before the assembler instructions.".into(),
                    None,
                    Span::new(lexer.span()),
                ));
            }
        }
    }

    let content_start: usize = lexer.current;
    let mut content_end: usize = lexer.current;
    let mut depth: usize = 1;

    while !lexer.is_eof() {
        let current: char = lexer.advance();

        if current == '{' {
            depth = depth.saturating_add(1);
        }

        if current == '}' {
            depth = depth.saturating_sub(1);

            if depth == 0 {
                content_end = lexer.current.saturating_sub(1);

                break;
            }
        }

        if current == '\n' {
            lexer.line = lexer.line.saturating_add(1);
            lexer.column = 0;
        }
    }

    if depth != 0 {
        lexer.end_span();

        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Expected '}' to close the raw assembler block.".into(),
            "You should close the block opened with '#asm{'.".into(),
            None,
            Span::new(lexer.span()),
        ));
    }

    let raw_content: String = String::from_iter(lexer.code[content_start..content_end].iter());
    let mut assembler: String = String::with_capacity(raw_content.len());
    let mut line_position: usize = 0;

    for line in raw_content.split('\n') {
        let trimmed: &str = line.trim();

        if trimmed.is_empty() {
            continue;
        }

        let instruction: &str = trimmed.strip_suffix(',').unwrap_or(trimmed);

        if line_position != 0 {
            assembler.push('\n');
        }

        assembler.push_str(instruction);

        line_position = line_position.saturating_add(1);
    }

    let end_column: u32 = u32::try_from(lexer.column).unwrap_or(u32::MAX);
    let span: Span = Span::new((start_line, (start_column, end_column)));

    lexer.tokens.push(Token {
        lexeme: assembler,
        ascii: String::default(),
        kind: TokenType::Assembler,
        span,
    });

    lexer.span.1 = lexer.column;

    Ok(())
}
