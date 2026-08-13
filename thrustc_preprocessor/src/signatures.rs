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
use thrustc_typesystem::Type;

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub signature: Signature,
    pub variant: Variant,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Variant {
    Function,
    Constant,
    Static,
    Struct,
    CustomType,
}

#[derive(Debug)]
pub enum Signature {
    Function {
        kind: Type,
        invalid_kind: Type,
        parameters: Vec<(Type, Span)>,
        attributes: ThrustAttributes,
        span: Span,
    },
    Constant {
        kind: Type,
        invalid_kind: Type,
        attributes: ThrustAttributes,
        span: Span,
    },
    Static {
        kind: Type,
        invalid_kind: Type,
        attributes: ThrustAttributes,
        span: Span,
    },
    Struct {
        kind: Type,
        invalid_kind: Type,
        span: Span,
    },
    CustomType {
        kind: Type,
        invalid_kind: Type,
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
