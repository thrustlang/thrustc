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

use std::path::PathBuf;

use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use ahash::AHashSet as HashSet;

use crate::abort;

#[derive(Debug)]
pub struct PreprocessorContext<'preprocessor> {
    tokens: &'preprocessor [Token],
    options: &'preprocessor CompilerOptions,
    visited: HashSet<PathBuf>,
    file: &'preprocessor CompilationUnit,
    diagnostician: Diagnostician,
    errors: Vec<CompilationIssue>,
    current: usize,
}

impl<'preprocessor> PreprocessorContext<'preprocessor> {
    pub fn new(
        tokens: &'preprocessor [Token],
        options: &'preprocessor CompilerOptions,
        file: &'preprocessor CompilationUnit,
        visited: HashSet<PathBuf>,
    ) -> Self {
        Self {
            tokens,
            options,
            visited,
            file,
            diagnostician: Diagnostician::new(file, options),
            errors: Vec::with_capacity(u8::MAX as usize),
            current: 0,
        }
    }
}

impl PreprocessorContext<'_> {
    pub fn check_status(&mut self) -> Result<(), ()> {
        if !self.errors.is_empty() {
            {
                for error in self.errors.iter() {
                    self.diagnostician
                        .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
                }
            }
        }

        Ok(())
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn consume(&mut self, kind: TokenType) -> Result<&Token, ()> {
        if self.peek().kind == kind {
            return self.advance();
        }

        Err(())
    }

    #[inline]
    pub fn consume_these(&mut self, these: &[TokenType]) -> Result<&Token, ()> {
        if these.contains(&self.peek().get_type()) {
            return self.advance();
        }

        Err(())
    }
}

impl<'module_parser> PreprocessorContext<'module_parser> {
    #[must_use]
    pub fn peek(&mut self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get current a lexical token!",
                Span::nothing(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }

    #[must_use]
    pub fn previous(&mut self) -> &Token {
        let index: (usize, bool) = self.current.overflowing_sub(1);

        let is_overflow: bool = index.1;
        let idx: usize = index.0;

        if is_overflow {
            let span: Span = self.peek().get_span();

            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to parse previous token position!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        } else {
            let span: Span = self.peek().get_span();

            self.tokens.get(idx).unwrap_or_else(|| {
                abort::abort_compilation(
                    &mut self.diagnostician,
                    CompilationPosition::Parser,
                    "Unable to get a lexical token!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
        }
    }
}

impl PreprocessorContext<'_> {
    #[must_use]
    pub fn check(&mut self, kind: TokenType) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().kind == kind
    }

    #[must_use]
    pub fn check_to(&mut self, kind: TokenType, modifier: usize) -> bool {
        if self.is_eof() {
            return false;
        }

        let position: usize = self.current.saturating_add(modifier);

        if position >= self.tokens.len() {
            return false;
        }

        self.tokens[position].kind == kind
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn match_token(&mut self, kind: TokenType) -> Result<bool, ()> {
        if self.peek().kind == kind {
            self.only_advance()?;
            return Ok(true);
        }

        Ok(false)
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn advance_until(&mut self, kind: TokenType) -> Result<(), ()> {
        while !self.match_token(kind)? {
            self.only_advance()?;
        }

        Ok(())
    }

    #[inline]
    pub fn advance_until_check(&mut self, kind: TokenType) -> Result<(), ()> {
        while !self.check(kind) {
            self.only_advance()?;
        }

        Ok(())
    }

    #[inline]
    pub fn advance_until_limits(&mut self, limits: &[TokenType]) -> Result<(), ()> {
        while !limits.iter().any(|limit| self.check(*limit)) {
            self.only_advance()?;
        }

        self.only_advance()?;

        Ok(())
    }

    #[inline]
    pub fn advance_until_check_limits(&mut self, limits: &[TokenType]) -> Result<(), ()> {
        while !limits.iter().any(|limit| self.check(*limit)) {
            self.only_advance()?;
        }

        Ok(())
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn only_advance(&mut self) -> Result<(), ()> {
        if !self.is_eof() {
            self.current += 1;
            return Ok(());
        }

        Err(())
    }

    #[inline]
    pub fn advance(&mut self) -> Result<&Token, ()> {
        if !self.is_eof() {
            self.current += 1;
            return Ok(self.previous());
        }

        Err(())
    }
}

impl PreprocessorContext<'_> {
    #[must_use]
    pub fn is_eof(&mut self) -> bool {
        self.peek().kind == TokenType::Eof
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn merge_errors(&mut self, other: Vec<CompilationIssue>) {
        self.errors.extend(other);
    }

    #[inline]
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }
}

impl PreprocessorContext<'_> {
    #[inline]
    pub fn has_visited(&self, path: &PathBuf) -> bool {
        self.visited.contains(path)
    }

    #[inline]
    pub fn mark_visited(&mut self, path: PathBuf) {
        self.visited.insert(path);
    }
}

impl<'module_parser> PreprocessorContext<'module_parser> {
    #[inline]
    pub fn get_options(&self) -> &'module_parser CompilerOptions {
        self.options
    }

    #[inline]
    pub fn get_global_visited_modules(&self) -> HashSet<PathBuf> {
        self.visited.clone()
    }

    #[inline]
    pub fn get_compilation_unit(&self) -> &CompilationUnit {
        self.file
    }
}
