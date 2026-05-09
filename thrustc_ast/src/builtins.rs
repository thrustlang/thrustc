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

use serde::Serialize;
use thrustc_span::Span;
use thrustc_typesystem::Type;

use crate::Ast;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum AstBuiltin<'mir> {
    Halloc {
        of: Type,
        span: Span,
    },
    MemCpy {
        src: std::boxed::Box<Ast<'mir>>,
        dst: std::boxed::Box<Ast<'mir>>,
        size: std::boxed::Box<Ast<'mir>>,
        span: Span,
    },
    MemMove {
        src: std::boxed::Box<Ast<'mir>>,
        dst: std::boxed::Box<Ast<'mir>>,
        size: std::boxed::Box<Ast<'mir>>,
        span: Span,
    },
    MemSet {
        dst: std::boxed::Box<Ast<'mir>>,
        new_size: std::boxed::Box<Ast<'mir>>,
        size: std::boxed::Box<Ast<'mir>>,
        span: Span,
    },
    BitSizeOf {
        of: Type,
        span: Span,
    },
    AbiSizeOf {
        of: Type,
        span: Span,
    },
    AbiAlignOf {
        of: Type,
        span: Span,
    },
    AlignOf {
        of: Type,
        span: Span,
    },
    SizeOf {
        of: Type,
        span: Span,
    },
}
