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

use thrustc_ast_modificators::Modificators;
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub signature: Signature,
    pub variant: Variant,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Variant {
    Function,
    Constant,
    Static,
    Struct,
    CustomType,
}

#[derive(Debug, Clone)]
pub enum Signature {
    Function {
        kind: Type,
        invalid_kind: Type,
        type_params: Option<Vec<String>>,
        parameters: Vec<(String, Type, Span)>,
        attributes: ThrustAttributes,
        span: Span,
    },
    Constant {
        kind: Type,
        invalid_kind: Type,
        value: Option<BuiltinValue>,
        attributes: ThrustAttributes,
        modificators: Modificators,
        span: Span,
    },
    Static {
        kind: Type,
        invalid_kind: Type,
        is_mutable: bool,
        attributes: ThrustAttributes,
        modificators: Modificators,
        span: Span,
    },
    Struct {
        kind: Type,
        invalid_kind: Type,
        type_params: Option<Vec<String>>,
        fields: Vec<(String, Type, Span)>,
        span: Span,
    },
    CustomType {
        kind: Type,
        invalid_kind: Type,
        type_params: Option<Vec<String>>,
        attributes: ThrustAttributes,
        span: Span,
    },
}

impl Signature {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            Signature::Function { span, .. } => *span,
            Signature::Constant { span, .. } => *span,
            Signature::Static { span, .. } => *span,
            Signature::Struct { span, .. } => *span,
            Signature::CustomType { span, .. } => *span,
        }
    }
}
