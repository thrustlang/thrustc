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
use thrustc_token_type::TokenType;

use crate::Type;
use crate::type_modificators::StructureTypeModificator;

pub trait TypeIsExtensions {
    fn is_char_type(&self) -> bool;
    fn is_void_type(&self) -> bool;
    fn is_bool_type(&self) -> bool;
    fn is_struct_type(&self) -> bool;
    fn is_fixed_array_type(&self) -> bool;
    fn is_array_type(&self) -> bool;
    fn is_float_type(&self) -> bool;
    fn is_ptr_type(&self) -> bool;
    fn is_address_type(&self) -> bool;
    fn is_const_type(&self) -> bool;
    fn is_function_reference_type(&self) -> bool;
    fn is_numeric_type(&self) -> bool;
    fn is_unsigned_integer_type(&self) -> bool;
    fn is_signed_integer_type(&self) -> bool;
    fn is_lesseq_unsigned32bit_integer(&self) -> bool;
    fn is_integer_type(&self) -> bool;
    fn is_unresolved_type(&self) -> bool;

    fn get_type_herarchy(&self) -> u8;
}

pub trait FunctionReferenceExtensions {
    fn get_function_reference_return_type(&self) -> Type;
}

pub trait IndexExtensions {
    fn calculate_index_type(&self, depth: u64) -> &Type;
}

pub trait TypeExtensions {
    fn get_type_with_depth(&self, base_depth: u64) -> &Type;
    fn get_type_ref(&self) -> Type;

    fn is_value(&self) -> bool;
    fn is_const_value(&self) -> bool;
}

pub trait ConstantTypeExtensions {
    fn remove_all_constant_type(&self) -> Type;
}

pub trait TypeFixedArrayEntensions {
    fn get_fixed_array_base_type(&self) -> Type;
    fn get_fixed_array_type_herarchy(&self) -> u8;
}

pub trait TypeArrayEntensions {
    fn get_array_skipping_array_as_base_type(&self) -> Type;
    fn get_array_base_type(&self) -> Type;
    fn get_array_type_herarchy(&self) -> u8;
}

pub trait TypePointerExtensions {
    fn is_ptr_like_type(&self) -> bool;
    fn is_ptr_composite_type(&self) -> bool;
    fn is_ptr_aggregate_value_like_type(&self) -> bool;
    fn is_ptr_aggregate_like_type(&self) -> bool;
    fn is_ptr_indexable_like_type(&self) -> bool;
    fn is_ptr_value_like_type(&self) -> bool;
    fn is_typed_ptr_type(&self) -> bool;
    fn is_flat_ptr_type(&self) -> bool;

    fn is_ptr_struct_type(&self) -> bool;
    fn is_ptr_fixed_array_type(&self) -> bool;
    fn is_ptr_array_type(&self) -> bool;
    fn is_ptr_numeric_type(&self) -> bool;

    fn get_address_space(&self) -> Option<u16>;
}

pub trait TypeStructExtensions {
    fn get_struct_fields(&self) -> &[Type];
    fn create_struct_type(
        name: String,
        fields: &[Type],
        modificator: StructureTypeModificator,
        span: Span,
    ) -> Type;
}

pub trait CastTypeExtensions {
    fn narrowing(&self) -> Type;
}

pub trait TypeCodeLocation {
    fn get_span(&self) -> Span;
}

pub trait DereferenceExtensions {
    fn dereference(&self) -> Type;
    fn dereference_until_value(&self) -> Type;
}

pub trait InfererTypeExtensions {
    fn inferer_inner_type_from_type(&mut self, other: &Type);
    fn has_inferer_inner_type(&self) -> bool;
    fn is_inferer_inner_type_valid(&self) -> bool;
    fn is_inferer_inner_type_is_not_array_decay(&self) -> bool;
    fn get_inferer_inner_type(&self) -> Type;
}

pub trait VoidTypeExtensions {
    fn contains_void_type(&self) -> bool;
}

pub trait PrecedenceTypeExtensions {
    fn get_term_precedence_type(&self, other: &Type, operator: TokenType) -> Type;
    fn get_factor_precedence_type(&self, other: &Type) -> Type;
}
