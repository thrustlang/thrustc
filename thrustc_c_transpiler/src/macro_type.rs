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
use thrustc_typesystem::Type;

#[derive(Debug)]
pub struct MacroType;

impl MacroType {
    #[inline]
    pub fn from_thrust_text(text: &str, span: Span) -> Type {
        match text {
            "s8" => Type::S8 { span },
            "s16" => Type::S16 { span },
            "s32" => Type::S32 { span },
            "s64" => Type::S64 { span },
            "ssize" => Type::SSize { span },
            "u8" => Type::U8 { span },
            "u16" => Type::U16 { span },
            "u32" => Type::U32 { span },
            "u64" => Type::U64 { span },
            "u128" => Type::U128 { span },
            "usize" => Type::USize { span },
            "f32" => Type::F32 { span },
            "f64" => Type::F64 { span },
            "f128" => Type::F128 { span },
            "bool" => Type::Bool { span },
            "char" => Type::Char { span },
            "void" => Type::Void { span },
            _ => Type::Unresolved {
                hint: text.to_string(),
                span,
            },
        }
    }

    #[inline]
    pub fn to_thrust_type(ty: &Type) -> String {
        crate::type_format::format_type_thrust(ty)
    }
}
