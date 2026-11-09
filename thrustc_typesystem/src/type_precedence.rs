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
    traits::{PrecedenceTypeExtensions, TypeCodeLocation, TypeIsExtensions},
};

impl PrecedenceTypeExtensions for Type {
    fn get_term_precedence_type(&self, other: &Type, operator: TokenType) -> Type {
        if self.is_ptr_type() && other.is_ptr_type() && operator == TokenType::Minus {
            return Type::SSize {
                span: self.get_code_location(),
            };
        }

        if self.get_type_herarchy() >= other.get_type_herarchy() {
            return self.clone();
        }

        if other.get_type_herarchy() >= self.get_type_herarchy() {
            return other.clone();
        }

        self.clone()
    }

    fn get_factor_precedence_type(&self, other: &Type) -> Type {
        if self.get_type_herarchy() >= other.get_type_herarchy() {
            return self.clone();
        }

        if other.get_type_herarchy() >= self.get_type_herarchy() {
            return other.clone();
        }

        self.clone()
    }
}
