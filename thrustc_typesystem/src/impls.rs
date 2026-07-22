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

use std::hash::Hash;
use std::hash::Hasher;

use thrustc_span::Span;

use crate::traits::ConstantTypeExtensions;
use crate::traits::TypePointerExtensions;
use crate::type_metadata::StructTypeMetadata;
use crate::type_modificators::StructureTypeModificator;
use crate::{
    Type,
    traits::{TypeCodeLocation, TypeExtensions, TypeIsExtensions},
};

impl TypeIsExtensions for Type {
    #[inline(always)]
    fn is_char_type(&self) -> bool {
        matches!(self, Type::Char { .. })
    }

    #[inline(always)]
    fn is_void_type(&self) -> bool {
        let non_constant_ty: Type = self.remove_all_constant_type();

        if let Type::Ptr {
            subtype: Some(subtype),
            ..
        } = non_constant_ty
        {
            return subtype.is_void_type();
        }

        matches!(self, Type::Void { .. } | Type::Unresolved { .. })
    }

    #[inline(always)]
    fn is_bool_type(&self) -> bool {
        matches!(self, Type::Bool { .. })
    }

    #[inline(always)]
    fn is_struct_type(&self) -> bool {
        matches!(self, Type::Struct { .. })
    }

    #[inline(always)]
    fn is_fixed_array_type(&self) -> bool {
        matches!(self, Type::FixedArray { .. })
    }

    #[inline(always)]
    fn is_array_type(&self) -> bool {
        matches!(self, Type::Array { .. })
    }

    #[inline(always)]
    fn is_array_type_with_inference(&self) -> bool {
        matches!(
            self,
            Type::Array {
                infered_type: Some(..),
                ..
            }
        )
    }

    #[inline(always)]
    fn is_float_type(&self) -> bool {
        matches!(
            self,
            Type::F32 { .. }
                | Type::F64 { .. }
                | Type::F128 { .. }
                | Type::FX8680 { .. }
                | Type::FPPC128 { .. }
        )
    }

    #[inline(always)]
    fn is_ptr_type(&self) -> bool {
        matches!(self, Type::Ptr { .. })
    }

    #[inline(always)]
    fn is_const_type(&self) -> bool {
        matches!(self, Type::Const(..))
    }

    #[inline(always)]
    fn is_function_reference_type(&self) -> bool {
        matches!(self, Type::Fn { .. })
    }

    #[inline(always)]
    fn is_numeric_type(&self) -> bool {
        self.is_integer_type() || self.is_float_type() || self.is_char_type() || self.is_bool_type()
    }

    #[inline(always)]
    fn is_unsigned_integer_type(&self) -> bool {
        matches!(
            self,
            Type::U8 { .. }
                | Type::U16 { .. }
                | Type::U32 { .. }
                | Type::U64 { .. }
                | Type::U128 { .. }
                | Type::USize { .. }
        )
    }

    #[inline(always)]
    fn is_signed_integer_type(&self) -> bool {
        matches!(
            self,
            Type::S8 { .. }
                | Type::S16 { .. }
                | Type::S32 { .. }
                | Type::S64 { .. }
                | Type::SSize { .. }
        )
    }

    #[inline(always)]
    fn is_lesseq_unsigned32bit_integer(&self) -> bool {
        matches!(self, Type::U8 { .. } | Type::U16 { .. } | Type::U32 { .. })
    }

    #[inline(always)]
    fn is_integer_type(&self) -> bool {
        matches!(
            self,
            Type::S8 { .. }
                | Type::S16 { .. }
                | Type::S32 { .. }
                | Type::S64 { .. }
                | Type::SSize { .. }
                | Type::U8 { .. }
                | Type::U16 { .. }
                | Type::U32 { .. }
                | Type::U64 { .. }
                | Type::U128 { .. }
                | Type::USize { .. }
                | Type::Char { .. }
        )
    }

    #[inline(always)]
    fn is_unresolved_type(&self) -> bool {
        matches!(self, Type::Unresolved { .. })
    }

    #[inline(always)]
    fn get_type_herarchy(&self) -> u8 {
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

            Type::Const(subtype, ..) => subtype.get_type_herarchy(),

            Type::Ptr {
                subtype: Some(subtype),
                ..
            } => subtype.get_type_herarchy(),
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

impl TypeExtensions for Type {
    #[inline]
    fn get_address_space(&self) -> Option<u16> {
        let non_constant_ty: Type = self.remove_all_constant_type();

        if let Type::Ptr { address_space, .. } = non_constant_ty {
            return address_space;
        }

        if let Type::Array { metadata, .. } = non_constant_ty {
            return metadata.get_address_space();
        }

        if let Type::FixedArray { metadata, .. } = non_constant_ty {
            return metadata.get_address_space();
        }

        None
    }

    #[inline]
    fn is_value(&self) -> bool {
        self.is_numeric_type()
            || self.is_fixed_array_type()
            || self.is_struct_type()
            || self.is_const_value()
    }

    #[inline]
    fn is_const_value(&self) -> bool {
        if let Type::Const(inner, ..) = self {
            return inner.is_const_value();
        }

        self.is_numeric_type() || self.is_fixed_array_type() || self.is_struct_type()
    }

    #[inline]
    fn get_type_with_depth(&self, base_depth: u64) -> &Type {
        if base_depth == 0 {
            return self;
        }

        match self {
            Type::FixedArray {
                base_type: element_type,
                ..
            } => element_type.get_type_with_depth(base_depth - 1),
            Type::Array {
                infered_type: Some((infered_type, 0)),
                ..
            } => infered_type.get_type_with_depth(base_depth),
            Type::Array {
                base_type: element_type,
                ..
            } => element_type.get_type_with_depth(base_depth - 1),
            Type::Const(inner_type, ..) => inner_type.get_type_with_depth(base_depth - 1),
            Type::Ptr {
                subtype: Some(inner_type),
                ..
            } => inner_type.get_type_with_depth(base_depth - 1),
            Type::Struct { .. } => self,
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::SSize { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. }
            | Type::FX8680 { .. }
            | Type::FPPC128 { .. }
            | Type::Bool { .. }
            | Type::Char { .. }
            | Type::Void { .. }
            | Type::Ptr { subtype: None, .. }
            | Type::Fn { .. }
            | Type::Unresolved { .. } => self,
        }
    }

    #[inline]
    fn get_type_ref(&self) -> Type {
        if self.is_ptr_like_type() {
            self.clone()
        } else {
            Type::Ptr {
                subtype: Some(self.clone().into()),
                address_space: self.get_address_space(),
                span: self.get_span(),
            }
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::SSize { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. }
            | Type::FX8680 { .. }
            | Type::FPPC128 { .. }
            | Type::Bool { .. }
            | Type::Char { .. }
            | Type::Void { .. } => {}

            Type::Const(inner, _) => inner.hash(state),
            Type::Ptr { subtype: inner, .. } => inner.hash(state),
            Type::Struct {
                name,
                fields,
                metadata,
                ..
            } => {
                name.hash(state);
                fields.hash(state);
                metadata.hash(state);
            }
            Type::FixedArray {
                base_type, size, ..
            } => {
                base_type.hash(state);
                size.hash(state);
            }
            Type::Array {
                base_type,
                infered_type,
                ..
            } => {
                base_type.hash(state);
                infered_type.hash(state);
            }
            Type::Fn {
                parameter_types,
                return_type,
                modificator,
                ..
            } => {
                parameter_types.hash(state);
                return_type.hash(state);
                modificator.hash(state);
            }
            Type::Unresolved { hint, .. } => {
                hint.hash(state);
            }
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type::Fn {
                    return_type: return_type_1,
                    parameter_types: parameters_types_1,
                    modificator: modificator_1,
                    ..
                },
                Type::Fn {
                    return_type: return_type_2,
                    parameter_types: parameters_types_2,
                    modificator: modificator_2,
                    ..
                },
            ) => {
                parameters_types_1.len() == parameters_types_2.len()
                    && parameters_types_1
                        .iter()
                        .zip(parameters_types_2.iter())
                        .all(|(f1, f2)| f1 == f2)
                    && return_type_1 == return_type_2
                    && modificator_1 == modificator_2
            }

            (
                Type::Struct {
                    name: a,
                    fields: fields_1,
                    metadata: metadata_1,
                    ..
                },
                Type::Struct {
                    name: b,
                    fields: fields_2,
                    metadata: metadata_2,
                    ..
                },
            ) => {
                fields_1.len() == fields_2.len()
                    && a == b
                    && fields_1
                        .iter()
                        .zip(fields_2.iter())
                        .all(|(f1, f2)| f1 == f2)
                    && metadata_1 == metadata_2
            }

            (
                Type::FixedArray {
                    base_type: type_a,
                    size: size_a,
                    ..
                },
                Type::FixedArray {
                    base_type: type_b,
                    size: size_b,
                    ..
                },
            ) => type_a == type_b && size_a == size_b,

            (
                Type::Array {
                    base_type: target, ..
                },
                Type::Array {
                    base_type: from, ..
                },
            ) => target == from,
            (Type::Const(target, ..), Type::Const(from, ..)) => target == from,

            (Type::Char { .. }, Type::Char { .. }) => true,
            (Type::S8 { .. }, Type::S8 { .. }) => true,
            (Type::S16 { .. }, Type::S16 { .. }) => true,
            (Type::S32 { .. }, Type::S32 { .. }) => true,
            (Type::S64 { .. }, Type::S64 { .. }) => true,
            (Type::SSize { .. }, Type::SSize { .. }) => true,
            (Type::U8 { .. }, Type::U8 { .. }) => true,
            (Type::U16 { .. }, Type::U16 { .. }) => true,
            (Type::U32 { .. }, Type::U32 { .. }) => true,
            (Type::U64 { .. }, Type::U64 { .. }) => true,
            (Type::U128 { .. }, Type::U128 { .. }) => true,
            (Type::USize { .. }, Type::USize { .. }) => true,
            (Type::F32 { .. }, Type::F32 { .. }) => true,
            (Type::F64 { .. }, Type::F64 { .. }) => true,
            (Type::F128 { .. }, Type::F128 { .. }) => true,
            (Type::FX8680 { .. }, Type::FX8680 { .. }) => true,
            (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => true,
            (Type::Ptr { subtype: None, .. }, Type::Ptr { subtype: None, .. }) => true,
            (
                Type::Ptr {
                    subtype: Some(lhs), ..
                },
                Type::Ptr {
                    subtype: Some(rhs), ..
                },
            ) => lhs == rhs,
            (Type::Ptr { .. }, Type::Ptr { .. }) => true,
            (Type::Void { .. }, Type::Void { .. }) => true,
            (Type::Bool { .. }, Type::Bool { .. }) => true,

            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::S8 { .. } => write!(f, "s8"),
            Type::S16 { .. } => write!(f, "s16"),
            Type::S32 { .. } => write!(f, "s32"),
            Type::S64 { .. } => write!(f, "s64"),
            Type::SSize { .. } => write!(f, "ssize"),
            Type::U8 { .. } => write!(f, "u8"),
            Type::U16 { .. } => write!(f, "u16"),
            Type::U32 { .. } => write!(f, "u32"),
            Type::U64 { .. } => write!(f, "u64"),
            Type::U128 { .. } => write!(f, "u128"),
            Type::USize { .. } => write!(f, "usize"),
            Type::F32 { .. } => write!(f, "f32"),
            Type::F64 { .. } => write!(f, "f64"),
            Type::F128 { .. } => write!(f, "f128"),
            Type::FX8680 { .. } => write!(f, "fx86_80"),
            Type::FPPC128 { .. } => write!(f, "fppc_128"),
            Type::Bool { .. } => write!(f, "bool"),
            Type::Char { .. } => write!(f, "char"),
            Type::Unresolved { hint, .. } => write!(f, "unresolved[{}]", hint),
            Type::Fn {
                parameter_types,
                return_type,
                modificator,
                ..
            } => {
                let has_llvm_ignore: &str = if modificator.llvm().has_ignore() {
                    "<ignore>"
                } else {
                    ""
                };

                write!(
                    f,
                    "Fn{}[{}] -> {}",
                    has_llvm_ignore,
                    parameter_types
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )
            }
            Type::Const(inner_type, ..) => write!(f, "const {}", inner_type),
            Type::FixedArray {
                base_type, size, ..
            } => {
                write!(f, "array[{}; {}]", base_type, size)
            }
            Type::Array { base_type, .. } => {
                write!(f, "array[{}]", base_type)
            }
            Type::Struct {
                name,
                fields,
                metadata,
                ..
            } => {
                let struct_metadata: &StructTypeMetadata = metadata;

                let modifications: &StructureTypeModificator =
                    struct_metadata.get_struct_type_modificator();

                let has_llvm_packed_attribute: &str = if modifications.llvm().is_packed() {
                    "<packed>"
                } else {
                    ""
                };

                write!(f, "struct {}{} {{ ", name, has_llvm_packed_attribute)?;

                for field in fields.iter() {
                    write!(f, "{} ", field)?;
                }

                write!(f, "}}")
            }
            Type::Ptr {
                subtype: nested_type,
                ..
            } => {
                if let Some(nested_type) = nested_type {
                    write!(f, "ptr[")?;
                    write!(f, "{}", nested_type)?;

                    return write!(f, "]");
                }

                write!(f, "ptr")
            }
            Type::Void { .. } => write!(f, "void"),
        }
    }
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::Void {
            span: Span::nothing(),
        }
    }
}
