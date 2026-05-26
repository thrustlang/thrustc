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
    traits::{TypeIsExtensions, TypePointerExtensions},
};

impl TypePointerExtensions for Type {
    #[inline]
    fn is_ptr_like_type(&self) -> bool {
        if let Type::Const(subtype, ..) = self {
            return subtype.is_ptr_like_type();
        }

        matches!(
            self,
            Type::Ptr { .. } | Type::Addr(..) | Type::Array { .. } | Type::Fn(..)
        )
    }

    #[inline]
    fn is_flat_ptr_type(&self) -> bool {
        matches!(self, Type::Ptr { .. } | Type::Fn(..))
    }

    #[inline]
    fn is_ptr_composite_type(&self) -> bool {
        self.is_ptr_struct_type()
    }

    #[inline]
    fn is_ptr_indexable_like_type(&self) -> bool {
        self.is_ptr_struct_type() || self.is_fixed_array_type() || self.is_ptr_array_type()
    }

    #[inline]
    fn is_ptr_aggregate_value_like_type(&self) -> bool {
        self.is_ptr_fixed_array_type()
    }

    #[inline]
    fn is_ptr_aggregate_like_type(&self) -> bool {
        self.is_ptr_fixed_array_type() || self.is_ptr_array_type()
    }

    #[inline]
    fn is_ptr_value_like_type(&self) -> bool {
        self.is_ptr_struct_type()
            || self.is_ptr_fixed_array_type()
            || self.is_ptr_numeric_type()
            || self.is_ptr_array_type()
    }

    #[inline]
    fn is_typed_ptr_type(&self) -> bool {
        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = self
        {
            return inner.is_typed_ptr_type();
        }

        if let Type::Ptr { subtype: None, .. } = self {
            return false;
        }

        true
    }

    #[inline]
    fn is_ptr_struct_type(&self) -> bool {
        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = self
        {
            return inner.is_struct_type();
        }

        false
    }

    #[inline]
    fn is_ptr_fixed_array_type(&self) -> bool {
        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = self
        {
            return inner.is_fixed_array_type();
        }

        false
    }

    #[inline]
    fn is_ptr_numeric_type(&self) -> bool {
        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = self
        {
            return inner.is_numeric_type();
        }

        false
    }

    #[inline]
    fn is_ptr_array_type(&self) -> bool {
        if let Type::Ptr {
            subtype: Some(inner),
            ..
        } = self
        {
            return inner.is_array_type();
        }

        false
    }

    #[inline]
    fn get_address_space(&self) -> Option<u16> {
        if let Type::Ptr { address_space, .. } = self {
            return *address_space;
        }

        None
    }
}
