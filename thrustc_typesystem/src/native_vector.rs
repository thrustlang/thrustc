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

use thrustc_token_type::TokenType;

use crate::{
    Type,
    traits::{ConstantTypeExtensions, TypeCodeLocation, TypeIsExtensions},
};

impl Type {
    pub fn get_native_vector_parts(&self) -> Option<(Type, u32)> {
        let ty: Type = self.remove_all_constant_type();

        let Type::NativeVector {
            element_type,
            element_count,
            ..
        } = ty
        else {
            return None;
        };

        Some((element_type.remove_all_constant_type(), element_count))
    }

    pub fn is_native_vector_scalar_expand_compatible(&self, scalar: &Type) -> bool {
        let Some((element, _)) = self.get_native_vector_parts() else {
            return false;
        };

        let scalar: Type = scalar.remove_all_constant_type();

        if element == scalar {
            return true;
        }

        if element.is_float_type() {
            return scalar.is_float_type() || scalar.is_integer_type();
        }

        if element.is_integer_type() {
            return scalar.is_integer_type() || scalar.is_bool_type();
        }

        if element.is_bool_type() {
            return scalar.is_bool_type();
        }

        false
    }

    pub fn get_native_vector_binary_result_type(
        &self,
        other: &Type,
        operator: TokenType,
    ) -> Option<Type> {
        let is_arithmetic: bool = matches!(
            operator,
            TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Arith
                | TokenType::PlusEq
                | TokenType::MinusEq
                | TokenType::StarEq
                | TokenType::SlashEq
                | TokenType::ArithEq
        );

        let is_bitwise: bool = matches!(
            operator,
            TokenType::BAnd
                | TokenType::Bor
                | TokenType::Xor
                | TokenType::BAndEq
                | TokenType::BorEq
                | TokenType::XorEq
        );

        let is_shift: bool = matches!(
            operator,
            TokenType::LShift
                | TokenType::RShift
                | TokenType::LShiftEq
                | TokenType::RShiftEq
        );

        let is_comparison: bool = matches!(
            operator,
            TokenType::EqEq
                | TokenType::BangEq
                | TokenType::Less
                | TokenType::LessEq
                | TokenType::Greater
                | TokenType::GreaterEq
        );

        let is_logical: bool = matches!(operator, TokenType::And | TokenType::Or);

        let left: Option<(Type, u32)> = self.get_native_vector_parts();
        let right: Option<(Type, u32)> = other.get_native_vector_parts();

        match (left, right) {
            (Some((left_element, left_count)), Some((right_element, right_count))) => {
                if left_count != right_count || left_element != right_element {
                    return None;
                }

                if is_arithmetic
                    && (left_element.is_integer_type() || left_element.is_float_type())
                {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(left_element),
                        element_count: left_count,
                        span: self.get_span(),
                    });
                }

                if is_bitwise && (left_element.is_integer_type() || left_element.is_bool_type()) {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(left_element),
                        element_count: left_count,
                        span: self.get_span(),
                    });
                }

                if is_shift && left_element.is_integer_type() {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(left_element),
                        element_count: left_count,
                        span: self.get_span(),
                    });
                }

                if is_comparison
                    && (left_element.is_integer_type()
                        || left_element.is_float_type()
                        || left_element.is_bool_type())
                {
                    return Some(Type::Bool {
                        span: self.get_span(),
                    });
                }

                if is_logical && left_element.is_bool_type() {
                    return Some(Type::Bool {
                        span: self.get_span(),
                    });
                }

                None
            }

            (Some((element, count)), None) => {
                if !self.is_native_vector_scalar_expand_compatible(other) {
                    return None;
                }

                if is_arithmetic && (element.is_integer_type() || element.is_float_type()) {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: self.get_span(),
                    });
                }

                if is_bitwise && (element.is_integer_type() || element.is_bool_type()) {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: self.get_span(),
                    });
                }

                if is_shift && element.is_integer_type() {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: self.get_span(),
                    });
                }

                if is_comparison
                    && (element.is_integer_type()
                        || element.is_float_type()
                        || element.is_bool_type())
                {
                    return Some(Type::Bool {
                        span: self.get_span(),
                    });
                }

                if is_logical && element.is_bool_type() {
                    return Some(Type::Bool {
                        span: self.get_span(),
                    });
                }

                None
            }

            (None, Some((element, count))) => {
                if !other.is_native_vector_scalar_expand_compatible(self) {
                    return None;
                }

                if is_arithmetic && (element.is_integer_type() || element.is_float_type()) {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: other.get_span(),
                    });
                }

                if is_bitwise && (element.is_integer_type() || element.is_bool_type()) {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: other.get_span(),
                    });
                }

                if is_shift && element.is_integer_type() {
                    return Some(Type::NativeVector {
                        element_type: std::boxed::Box::new(element),
                        element_count: count,
                        span: other.get_span(),
                    });
                }

                if is_comparison
                    && (element.is_integer_type()
                        || element.is_float_type()
                        || element.is_bool_type())
                {
                    return Some(Type::Bool {
                        span: other.get_span(),
                    });
                }

                if is_logical && element.is_bool_type() {
                    return Some(Type::Bool {
                        span: other.get_span(),
                    });
                }

                None
            }

            (None, None) => None,
        }
    }

    pub fn get_native_vector_unary_result_type(&self, operator: TokenType) -> Option<Type> {
        let (element, count) = self.get_native_vector_parts()?;

        match operator {
            TokenType::Minus if element.is_integer_type() || element.is_float_type() => {
                Some(Type::NativeVector {
                    element_type: std::boxed::Box::new(element),
                    element_count: count,
                    span: self.get_span(),
                })
            }

            TokenType::Not if element.is_integer_type() => Some(Type::NativeVector {
                element_type: std::boxed::Box::new(element),
                element_count: count,
                span: self.get_span(),
            }),

            TokenType::Bang if element.is_bool_type() => Some(Type::NativeVector {
                element_type: std::boxed::Box::new(element),
                element_count: count,
                span: self.get_span(),
            }),

            _ => None,
        }
    }
}
