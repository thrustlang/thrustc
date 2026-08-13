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

use crate::{Type, traits::TypeCodeLocation};

impl TypeCodeLocation for Type {
    #[inline]
    fn get_span(&self) -> Span {
        match self {
            Type::Char { span }
            | Type::S8 { span }
            | Type::S16 { span }
            | Type::S32 { span }
            | Type::S64 { span }
            | Type::SSize { span }
            | Type::U8 { span }
            | Type::U16 { span }
            | Type::U32 { span }
            | Type::U64 { span }
            | Type::U128 { span }
            | Type::USize { span }
            | Type::F32 { span }
            | Type::F64 { span }
            | Type::F128 { span }
            | Type::FX8680 { span }
            | Type::FPPC128 { span }
            | Type::Bool { span }
            | Type::Void { span }
            | Type::Array { span, .. }
            | Type::FixedArray { span, .. }
            | Type::Const(_, span)
            | Type::Ptr { span, .. }
            | Type::Struct { span, .. }
            | Type::Fn { span, .. } => *span,
            Type::Unresolved { span, .. } => *span,
        }
    }
}
