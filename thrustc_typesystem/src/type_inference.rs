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

use thrustc_span::Span;

use crate::{
    Type,
    metadata::FixedArrayTypeMetadata,
    traits::{
        ConstantTypeExtensions, InfererTypeExtensions, TypeCodeLocation, TypeExtensions,
        TypeIsExtensions,
    },
};

impl InfererTypeExtensions for Type {
    fn inferer_inner_type_from_type(&self, other: &Type) -> Option<Type> {
        let span: Span = self.get_span();

        let mut left: Type = self.remove_all_constant_type();
        let mut right: Type = other.remove_all_constant_type();

        match (&mut left, &mut right) {
            (
                Type::Array {
                    base_type,
                    infered_type: lhs_infered_type,
                    metadata: target_metadata,
                    ..
                },
                Type::Array {
                    infered_type: Some(rhs_infered_type),
                    metadata: from_metadata,
                    ..
                },
            ) => {
                let (Type::FixedArray { size, .. }, mut refcounter) =
                    (&*rhs_infered_type.0, rhs_infered_type.1)
                else {
                    return Some(left);
                };

                refcounter = refcounter.saturating_add(1);

                *lhs_infered_type = Some((
                    Type::FixedArray {
                        base_type: (*base_type).clone(),
                        size: *size,
                        metadata: FixedArrayTypeMetadata::new(base_type.get_address_space()),
                        span,
                    }
                    .into(),
                    refcounter,
                ));

                *target_metadata = from_metadata.clone();

                Some(left)
            }

            (
                Type::Array {
                    metadata: target_metadata,
                    ..
                },
                Type::Array {
                    metadata: from_metadata,
                    ..
                },
            ) => {
                *target_metadata = from_metadata.clone();
                Some(left)
            }

            _ => None,
        }
    }

    #[inline(always)]
    fn has_infered_inner_type(&self) -> bool {
        matches!(
            self,
            Type::Array {
                infered_type: Some(_),
                ..
            }
        )
    }

    #[inline(always)]
    fn is_inferer_inner_type_valid(&self) -> bool {
        let ty: Type = self.remove_all_constant_type();

        if let Type::Array {
            infered_type: Some((infered_type, 0 | 1)),
            ..
        } = ty
        {
            return infered_type.is_fixed_array_type();
        }

        false
    }

    #[inline(always)]
    fn is_inferer_inner_type_is_not_array_decay(&self) -> bool {
        let ty: Type = self.remove_all_constant_type();

        if let Type::Array {
            infered_type: Some((_, 0..=1)),
            ..
        } = ty
        {
            return true;
        }

        false
    }

    #[inline(always)]
    fn get_inferer_inner_type(&self) -> Type {
        let ty: Type = self.remove_all_constant_type();

        match ty {
            Type::Array {
                infered_type: Some((infered_type, 0 | 1)),
                ..
            } => (*infered_type).clone(),

            _ => self.clone(),
        }
    }
}
