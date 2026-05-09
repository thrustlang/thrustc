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

mod impls;
pub mod traits;

use serde::Serialize;
use thrustc_span::Span;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

use thrustc_token_type::TokenType;

use crate::traits::TokenExtensions;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Serialize)]
pub struct Token {
    pub lexeme: String,
    pub ascii: String,
    pub kind: TokenType,
    pub span: Span,
}

impl TokenExtensions for Token {
    #[inline]
    fn get_lexeme(&self) -> &str {
        &self.lexeme
    }

    #[inline]
    fn get_ascii_lexeme(&self) -> &str {
        &self.ascii
    }

    #[inline]
    fn get_span(&self) -> Span {
        self.span
    }

    #[inline]
    fn get_type(&self) -> TokenType {
        self.kind
    }

    #[inline]
    fn get_lexeme_first_byte(&self) -> u64 {
        *self.lexeme.as_bytes().first().unwrap_or(&b'\0') as u64
    }
}
