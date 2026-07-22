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
    traits::{ConstantTypeExtensions, VoidTypeExtensions},
};

impl VoidTypeExtensions for Type {
    fn contains_void_type(&self) -> bool {
        let ty: Type = self.remove_all_constant_type();

        fn contains_void_type_inner_type(inner_type: &Type) -> bool {
            match inner_type {
                Type::Ptr {
                    subtype: Some(inner_type),
                    ..
                } => contains_void_type_inner_type(inner_type),
                Type::Const(inner_type, ..) => contains_void_type_inner_type(inner_type),
                Type::Array {
                    infered_type: Some((inner_type, _)),
                    ..
                } => contains_void_type_inner_type(inner_type),
                Type::Array {
                    base_type: inner_type,
                    ..
                } => contains_void_type_inner_type(inner_type),
                Type::Struct { fields, .. } => fields.iter().any(contains_void_type_inner_type),
                Type::FixedArray { base_type, .. } => contains_void_type_inner_type(base_type),
                Type::Fn {
                    parameter_types,
                    return_type,
                    ..
                } => {
                    parameter_types.iter().any(contains_void_type_inner_type)
                        || contains_void_type_inner_type(return_type)
                }

                Type::Void { .. } | Type::Unresolved { .. } => true,

                _ => false,
            }
        }

        match &ty {
            Type::Ptr {
                subtype: Some(inner_type),
                ..
            } => contains_void_type_inner_type(inner_type),
            Type::Array {
                infered_type: Some((inner_type, _)),
                ..
            } => contains_void_type_inner_type(inner_type),
            Type::Array {
                base_type: inner_type,
                ..
            } => contains_void_type_inner_type(inner_type),
            Type::FixedArray { base_type, .. } => contains_void_type_inner_type(base_type),
            Type::Struct { fields, .. } => fields.iter().any(contains_void_type_inner_type),
            Type::Fn {
                parameter_types,
                return_type,
                ..
            } => {
                parameter_types.iter().any(contains_void_type_inner_type)
                    || contains_void_type_inner_type(return_type)
            }

            Type::Void { .. } | Type::Unresolved { .. } => true,

            _ => false,
        }
    }
}
