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

use crate::{
    Type,
    traits::{ConstantTypeExtensions, TypeArrayEntensions},
};

impl TypeArrayEntensions for Type {
    #[inline]
    fn get_array_skipping_array_as_base_type(&self) -> Type {
        if let Type::Array {
            base_type: inner, ..
        } = self
        {
            return inner.get_array_skipping_array_as_base_type();
        }

        self.clone()
    }

    #[inline]
    fn get_array_base_type(&self) -> Type {
        let non_constant_type: Type = self.remove_all_constant_type();

        if let Type::Array {
            base_type: inner, ..
        } = non_constant_type
        {
            return (*inner).clone();
        }

        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = non_constant_type
        {
            return inner.get_array_base_type();
        }

        if let Type::Const(inner, ..) = non_constant_type {
            return inner.get_array_base_type();
        }

        self.clone()
    }

    #[inline]
    fn get_array_type_herarchy(&self) -> u8 {
        match self {
            Type::Bool { .. } => 1,
            Type::Char { .. } => 2,

            Type::U8 { .. } => 3,
            Type::U16 { .. } => 4,
            Type::U32 { .. } => 5,
            Type::U64 { .. } => 6,
            Type::U128 { .. } => 7,
            Type::USize { .. } => 8,

            Type::S8 { .. } => 9,
            Type::S16 { .. } => 10,
            Type::S32 { .. } => 11,
            Type::S64 { .. } => 12,
            Type::SSize { .. } => 13,

            Type::F32 { .. } => 15,
            Type::F64 { .. } => 16,
            Type::F128 { .. } => 17,
            Type::FX8680 { .. } => 18,
            Type::FPPC128 { .. } => 19,

            Type::Const(subtype, ..) => subtype.get_array_type_herarchy(),

            Type::Ptr {
                subtype: Some(subtype),
                ..
            } => subtype.get_array_type_herarchy(),
            Type::Ptr { subtype: None, .. } => 20,

            Type::Fn { .. } => 21,

            Type::Array { .. } => 22,
            Type::FixedArray { .. } => 23,
            Type::Struct { .. } => 24,

            Type::Void { .. } => 25,
            Type::Unresolved { .. } => 26,
        }
    }
}
