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

use crate::{Type, traits::CastTypeExtensions};

impl CastTypeExtensions for Type {
    #[inline]
    fn narrowing_cast(&self) -> Type {
        match self {
            Type::U8 { span } => Type::S8 { span: *span },
            Type::U16 { span } => Type::S16 { span: *span },
            Type::U32 { span } => Type::S32 { span: *span },
            Type::U64 { span } => Type::S64 { span: *span },
            Type::USize { span } => Type::SSize { span: *span },

            Type::S8 { span } => Type::U8 { span: *span },
            Type::S16 { span } => Type::U16 { span: *span },
            Type::S32 { span } => Type::U32 { span: *span },
            Type::S64 { span } => Type::U64 { span: *span },
            Type::SSize { span } => Type::USize { span: *span },

            _ => self.clone(),
        }
    }
}
