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
    name: String,
    type_params: Vec<String>,
    parameter_types: Vec<Type>,
    parameter_names: Vec<String>,
    return_type: Type,
    attributes: ThrustAttributes,
    has_local_template: bool,
    has_varargs: bool,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericStructEntry {
    type_params: Vec<String>,
    field_names: Vec<String>,
    field_types: Vec<Type>,
    metadata: StructTypeMetadata,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericCustomTypeEntry {
    type_params: Vec<String>,
    kind: Type,
}

impl GenericFunctionEntry {
    #[inline]
    pub fn new(
        name: String,
        type_params: Vec<String>,
        function_signature: (Vec<Type>, Vec<String>, Type),
        attributes: ThrustAttributes,
        has_local_template: bool,
        has_varargs: bool,
        span: Span,
    ) -> Self {
        let parameter_types: Vec<Type> = function_signature.0;
        let parameter_names: Vec<String> = function_signature.1;
        let return_type: Type = function_signature.2;

        Self {
            name,
            type_params,
            parameter_types,
            parameter_names,
            return_type,
            attributes,
            has_local_template,
            has_varargs,
            span,
        }
    }
}

impl GenericFunctionEntry {
    #[inline]
    pub fn get_name(&self) -> &String {
        &self.name
    }

    #[inline]
    pub fn get_type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    #[inline]
    pub fn get_parameter_types(&self) -> &Vec<Type> {
        &self.parameter_types
    }

    #[inline]
    pub fn get_parameter_names(&self) -> &Vec<String> {
        &self.parameter_names
    }

    #[inline]
    pub fn get_return_type(&self) -> &Type {
        &self.return_type
    }

    #[inline]
    pub fn get_attributes(&self) -> &ThrustAttributes {
        &self.attributes
    }

    #[inline]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl GenericFunctionEntry {
    #[inline]
    pub fn has_local_template(&self) -> bool {
        self.has_local_template
    }

    #[inline]
    pub fn has_varargs(&self) -> bool {
        self.has_varargs
    }
}

impl GenericFunctionEntry {
    #[inline]
    pub fn get_mut_name(&mut self) -> &mut String {
        &mut self.name
    }

    #[inline]
    pub fn get_mut_type_params(&mut self) -> &mut Vec<String> {
        &mut self.type_params
    }

    #[inline]
    pub fn get_mut_parameter_types(&mut self) -> &mut Vec<Type> {
        &mut self.parameter_types
    }

    #[inline]
    pub fn get_mut_parameter_names(&mut self) -> &mut Vec<String> {
        &mut self.parameter_names
    }

    #[inline]
    pub fn get_mut_return_type(&mut self) -> &mut Type {
        &mut self.return_type
    }

    #[inline]
    pub fn get_mut_attributes(&mut self) -> &mut ThrustAttributes {
        &mut self.attributes
    }

    #[inline]
    pub fn get_mut_has_local_template(&mut self) -> &mut bool {
        &mut self.has_local_template
    }

    #[inline]
    pub fn get_mut_has_varargs(&mut self) -> &mut bool {
        &mut self.has_varargs
    }

    #[inline]
    pub fn get_mut_span(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl GenericFunctionEntry {
    #[inline]
    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    #[inline]
    pub fn set_type_params(&mut self, type_params: Vec<String>) {
        self.type_params = type_params;
    }

    #[inline]
    pub fn set_parameter_types(&mut self, parameter_types: Vec<Type>) {
        self.parameter_types = parameter_types;
    }

    #[inline]
    pub fn set_parameter_names(&mut self, parameter_names: Vec<String>) {
        self.parameter_names = parameter_names;
    }

    #[inline]
    pub fn set_return_type(&mut self, return_type: Type) {
        self.return_type = return_type;
    }

    #[inline]
    pub fn set_attributes(&mut self, attributes: ThrustAttributes) {
        self.attributes = attributes;
    }

    #[inline]
    pub fn set_has_local_template(&mut self, has_local_template: bool) {
        self.has_local_template = has_local_template;
    }

    #[inline]
    pub fn set_has_varargs(&mut self, has_varargs: bool) {
        self.has_varargs = has_varargs;
    }

    #[inline]
    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl GenericStructEntry {
    #[inline]
    pub fn new(
        type_params: Vec<String>,
        field_names: Vec<String>,
        field_types: Vec<Type>,
        metadata: StructTypeMetadata,
        span: Span,
    ) -> Self {
        Self {
            type_params,
            field_names,
            field_types,
            metadata,
            span,
        }
    }
}

impl GenericStructEntry {
    #[inline]
    pub fn get_type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    #[inline]
    pub fn get_field_names(&self) -> &Vec<String> {
        &self.field_names
    }

    #[inline]
    pub fn get_field_types(&self) -> &Vec<Type> {
        &self.field_types
    }

    #[inline]
    pub fn get_metadata(&self) -> StructTypeMetadata {
        self.metadata
    }

    #[inline]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl GenericStructEntry {
    #[inline]
    pub fn get_mut_type_params(&mut self) -> &mut Vec<String> {
        &mut self.type_params
    }

    #[inline]
    pub fn get_mut_field_names(&mut self) -> &mut Vec<String> {
        &mut self.field_names
    }

    #[inline]
    pub fn get_mut_field_types(&mut self) -> &mut Vec<Type> {
        &mut self.field_types
    }

    #[inline]
    pub fn get_mut_metadata(&mut self) -> &mut StructTypeMetadata {
        &mut self.metadata
    }

    #[inline]
    pub fn get_mut_span(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl GenericStructEntry {
    #[inline]
    pub fn set_type_params(&mut self, type_params: Vec<String>) {
        self.type_params = type_params;
    }

    #[inline]
    pub fn set_field_names(&mut self, field_names: Vec<String>) {
        self.field_names = field_names;
    }

    #[inline]
    pub fn set_field_types(&mut self, field_types: Vec<Type>) {
        self.field_types = field_types;
    }

    #[inline]
    pub fn set_metadata(&mut self, metadata: StructTypeMetadata) {
        self.metadata = metadata;
    }

    #[inline]
    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl GenericCustomTypeEntry {
    #[inline]
    pub fn new(type_params: Vec<String>, kind: Type) -> Self {
        Self { type_params, kind }
    }
}

impl GenericCustomTypeEntry {
    #[inline]
    pub fn get_type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    #[inline]
    pub fn get_kind(&self) -> &Type {
        &self.kind
    }
}

impl GenericCustomTypeEntry {
    #[inline]
    pub fn get_mut_type_params(&mut self) -> &mut Vec<String> {
        &mut self.type_params
    }

    #[inline]
    pub fn get_mut_kind(&mut self) -> &mut Type {
        &mut self.kind
    }
}

impl GenericCustomTypeEntry {
    #[inline]
    pub fn set_type_params(&mut self, type_params: Vec<String>) {
        self.type_params = type_params;
    }

    #[inline]
    pub fn set_kind(&mut self, kind: Type) {
        self.kind = kind;
    }
}
