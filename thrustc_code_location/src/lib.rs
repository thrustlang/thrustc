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

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct Span {
    pub line: u32,
    pub span: (u32, u32),
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.line, self.span.0, self.span.1)
    }
}

impl Span {
    #[inline]
    pub fn new(span: (u32, (u32, u32))) -> Self {
        let line: u32 = span.0;
        let start: u32 = span.1.0;
        let end: u32 = span.1.1;

        Self {
            line,
            span: (start, end),
        }
    }

    #[inline]
    pub fn nothing() -> Self {
        Self::new((1, (0, 0)))
    }
}

impl Span {
    #[inline]
    pub fn get_line(&self) -> u32 {
        self.line
    }

    #[inline]
    pub fn get_span_start(&self) -> u32 {
        self.span.0
    }

    #[inline]
    pub fn get_span_end(&self) -> u32 {
        self.span.1
    }
}
