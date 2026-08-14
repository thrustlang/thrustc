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

use crate::{Ast, traits::AstLiteralExtensions};

impl AstLiteralExtensions for Ast<'_> {
    fn is_totaly_literal_value(&self) -> bool {
        match self {
            Ast::Integer { .. }
            | Ast::Float { .. }
            | Ast::Boolean { .. }
            | Ast::Char { .. }
            | Ast::CString { .. }
            | Ast::CNString { .. }
            | Ast::NullPtr { .. } => true,

            Ast::FixedArray { items, .. } => {
                items.iter().all(|item| item.is_totaly_literal_value())
            }
            Ast::Array { items, .. } => items.iter().all(|item| item.is_totaly_literal_value()),

            Ast::EnumValue { value, .. } => value.is_totaly_literal_value(),

            Ast::Group { node, .. } => node.is_totaly_literal_value(),
            Ast::BinaryOp { left, right, .. } => {
                left.is_totaly_literal_value() && right.is_totaly_literal_value()
            }
            Ast::UnaryOp { node, .. } => node.is_totaly_literal_value(),

            _ => false,
        }
    }

    #[inline]
    fn is_literal_value(&self) -> bool {
        matches!(
            self,
            Ast::Integer { .. }
                | Ast::Float { .. }
                | Ast::Boolean { .. }
                | Ast::Char { .. }
                | Ast::CString { .. }
                | Ast::CNString { .. }
                | Ast::NullPtr { .. }
                | Ast::Array { .. }
                | Ast::FixedArray { .. }
        )
    }

    #[inline]
    fn is_literal_ptr_value(&self) -> bool {
        matches!(self, |Ast::CString { .. }| Ast::CNString { .. })
    }
}
