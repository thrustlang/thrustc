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


pub trait TokenTypeBuiltinExtensions {
    fn is_builtin(&self) -> bool;
}

pub trait TokenTypeExtensions {
    fn is_logical_operator(&self) -> bool;
    fn is_logical_gate(&self) -> bool;
    fn is_compound_assignment_operator(&self) -> bool;
    fn is_minus_minus_operator(&self) -> bool;
    fn is_plus_plus_operator(&self) -> bool;
    fn is_address(&self) -> bool;
    fn is_void(&self) -> bool;
    fn is_bool(&self) -> bool;
    fn is_array(&self) -> bool;
    fn is_ptr(&self) -> bool;
    fn is_float(&self) -> bool;
    fn is_const(&self) -> bool;
    fn is_fn_ref(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_type(&self) -> bool;
    fn is_identifier(&self) -> bool;
    fn is_declaration(&self) -> bool;
    fn is_stmt(&self) -> bool;
}

pub trait TokenTypeAttributesExtensions {
    fn is_attribute(&self) -> bool;
}
