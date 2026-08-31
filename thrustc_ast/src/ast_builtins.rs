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
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::Ast;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

#[derive(Debug, Clone, Serialize)]
pub enum DeferredBuiltinArgument<'compiler_builtin> {
    Type {
        ty: Type,
        span: Span,
    },
    Value {
        expression: std::boxed::Box<Ast<'compiler_builtin>>,
        span: Span,
    },
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum AstBuiltin<'compiler_builtin> {
    Halloc {
        of: Type,
        span: Span,
    },
    MemCpy {
        src: std::boxed::Box<Ast<'compiler_builtin>>,
        dst: std::boxed::Box<Ast<'compiler_builtin>>,
        size: std::boxed::Box<Ast<'compiler_builtin>>,
        span: Span,
    },
    MemMove {
        src: std::boxed::Box<Ast<'compiler_builtin>>,
        dst: std::boxed::Box<Ast<'compiler_builtin>>,
        size: std::boxed::Box<Ast<'compiler_builtin>>,
        span: Span,
    },
    MemSet {
        dst: std::boxed::Box<Ast<'compiler_builtin>>,
        new_size: std::boxed::Box<Ast<'compiler_builtin>>,
        size: std::boxed::Box<Ast<'compiler_builtin>>,
        span: Span,
    },
    BitSizeOf {
        ty: Type,
        span: Span,
    },
    AbiSizeOf {
        ty: Type,
        span: Span,
    },
    AbiAlignOf {
        ty: Type,
        span: Span,
    },
    ArbitraryArg {
        ty: Type,
        span: Span,
    },
    ArbitraryArgs {
        span: Span,
    },
    DeferredCompileTime {
        name: &'compiler_builtin str,
        arguments: Vec<DeferredBuiltinArgument<'compiler_builtin>>,
        span: Span,
    },
}
