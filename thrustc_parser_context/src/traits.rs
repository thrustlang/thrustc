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

use thrustc_typesystem::Type;

pub trait TypeContextExtensions {
    fn get_infered_type(&self) -> Option<Type>;
    fn add_infered_type(&mut self, t: Type);
    fn pop_infered_type(&mut self);
    fn reset_infered_types(&mut self);
}

pub trait PositionExtensions {
    fn is_constant_position(&self) -> bool;
    fn is_static_position(&self) -> bool;
    fn is_variable_position(&self) -> bool;
    fn is_expression_position(&self) -> bool;
    fn is_irrelevant_position(&self) -> bool;
}
