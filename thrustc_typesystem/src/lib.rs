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

mod array_type;
mod constant_type;
mod fixed_array_type;
mod function_reference_type;
mod impls;
mod pointer_dereference;
mod pointer_type;
mod structure_type;
pub mod traits;
mod type_cast;
mod type_indexation;
mod type_inference;
pub mod type_layout;
mod type_location;
pub mod type_modificators;
mod type_precedence;
mod void_type;

use serde::Serialize;
use thrustc_span::Span;

use crate::type_modificators::FunctionReferenceTypeModificator;
use crate::type_modificators::StructureTypeModificator;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Eq, Serialize)]
pub enum Type {
    S8 {
        span: Span,
    },
    S16 {
        span: Span,
    },
    S32 {
        span: Span,
    },
    S64 {
        span: Span,
    },
    SSize {
        span: Span,
    },

    // Unsigned Integer Type
    U8 {
        span: Span,
    },
    U16 {
        span: Span,
    },
    U32 {
        span: Span,
    },
    U64 {
        span: Span,
    },
    U128 {
        span: Span,
    },
    USize {
        span: Span,
    },

    // Floating Point Type
    F32 {
        span: Span,
    },
    F64 {
        span: Span,
    },
    F128 {
        span: Span,
    },
    FX8680 {
        span: Span,
    },
    FPPC128 {
        span: Span,
    },

    // Boolean Type
    Bool {
        span: Span,
    },

    // Char Type
    Char(Span),

    // Constant Type
    Const(std::boxed::Box<Type>, Span),

    // Ptr Type
    Ptr {
        subtype: Option<std::boxed::Box<Type>>,
        address_space: Option<u16>,
        span: Span,
    },

    // Struct Type
    Struct {
        name: String,
        fields: std::vec::Vec<Type>,
        modifier: StructureTypeModificator,
        span: Span,
    },

    // Fixed FixedArray
    FixedArray(std::boxed::Box<Type>, u32, Span),

    // Array Type
    Array {
        base_type: std::boxed::Box<Type>,
        infered_type: Option<(std::boxed::Box<Type>, usize)>,
        span: Span,
    },

    // Memory Address
    Addr(Span),

    // Function Referece
    Fn(
        std::vec::Vec<Type>,
        std::boxed::Box<Type>,
        FunctionReferenceTypeModificator,
        Span,
    ),

    // Void Type
    Void(Span),

    // Unresolved Type
    Unresolved {
        hint: String,
        span: Span,
    },
}
