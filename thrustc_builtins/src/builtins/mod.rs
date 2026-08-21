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

mod alignof;
mod sizeof;

use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;

use crate::builtin_type::BuiltinTypeInfo;
use crate::registry::BuiltinRegistry;

pub fn register_default_builtins(registry: &mut BuiltinRegistry) {
    registry.register_function(alignof::AlignOf);
    registry.register_function(sizeof::SizeOf);

    registry.register_type(BuiltinTypeInfo::new(
        "CString",
        Type::Array {
            base_type: Type::Char {
                span: Span::nothing(),
            }
            .into(),
            infered_type: None,
            metadata: ArrayTypeMetadata::new(None, None),
            span: Span::nothing(),
        },
    ));
}