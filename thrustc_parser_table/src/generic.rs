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

use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};

#[derive(Debug, Clone)]
pub struct GenericFunctionEntry {
    pub name: String,
    pub type_params: Vec<String>,
    pub parameter_types: Vec<Type>,
    pub parameter_names: Vec<String>,
    pub return_type: Type,
    pub attributes: ThrustAttributes,
    pub has_local_template: bool,
    pub has_varargs: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericStructEntry<'parser> {
    pub type_params: Vec<String>,
    pub field_names: Vec<&'parser str>,
    pub field_types: Vec<Type>,
    pub metadata: StructTypeMetadata,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericCustomTypeEntry {
    pub type_params: Vec<String>,
    pub kind: Type,
}