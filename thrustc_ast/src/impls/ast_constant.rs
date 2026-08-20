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

use crate::{
    Ast,
    traits::AstConstantExtensions,
};

impl AstConstantExtensions for Ast<'_> {
    fn is_constant_value(&self) -> bool {
        match self {
            Ast::Integer { .. }
            | Ast::Float { .. }
            | Ast::Boolean { .. }
            | Ast::Char { .. }
            | Ast::CNString { .. }
            | Ast::CString { .. }
            | Ast::NullPtr { .. } => true,

            Ast::EnumValue { value, .. } => value.is_constant_value(),
            Ast::GetLocation { expr, .. } => expr.is_constant_value(),
            Ast::Group { node, .. } => node.is_constant_value(),
            Ast::BinaryOp { left, right, .. } => {
                left.is_constant_value() && right.is_constant_value()
            }
            Ast::UnaryOp { node, .. } => node.is_constant_value(),
            Ast::Reference { metadata, .. } => metadata.is_constant_ref(),
            Ast::As { metadata, .. } => metadata.is_constant(),
            Ast::FixedArray { items, .. } => items.iter().all(|item| item.is_constant_value()),
            Ast::Array { items, .. } => items.iter().all(|item| item.is_constant_value()),
            Ast::Constructor { data, .. } => {
                data.iter().all(|(_, node, ..)| node.is_constant_value())
            }

            // No es una constante evaluable en tiempo de compilación.
            Ast::GlobalAssembler { .. }
            | Ast::Index { .. }
            | Ast::Embedded { .. }
            | Ast::Struct { .. }
            | Ast::Property { .. }
            | Ast::If { .. }
            | Ast::Elif { .. }
            | Ast::Else { .. }
            | Ast::For { .. }
            | Ast::While { .. }
            | Ast::Loop { .. }
            | Ast::Continue { .. }
            | Ast::Break { .. }
            | Ast::ContinueAll { .. }
            | Ast::BreakAll { .. }
            | Ast::Block { .. }
            | Ast::Defer { .. }
            | Ast::CustomType { .. }
            | Ast::Enum { .. }
            | Ast::CompilerIntrinsic { .. }
            | Ast::CompilerIntrinsicParameter { .. }
            | Ast::AssemblerFunction { .. }
            | Ast::AssemblerFunctionParameter { .. }
            | Ast::Function { .. }
            | Ast::FunctionParameter { .. }
            | Ast::Return { .. }
            | Ast::Static { .. }
            | Ast::Const { .. }
            | Ast::Var { .. }
            | Ast::Mutation { .. }
            | Ast::Address { .. }
            | Ast::Write { .. }
            | Ast::Load { .. }
            | Ast::Deref { .. }
            | Ast::ModuleExpression { .. }
            | Ast::Call { .. }
            | Ast::IndirectCall { .. }
            | Ast::AsmValue { .. }
            | Ast::Builtin { .. }
            | Ast::Import { .. }
            | Ast::ImportC { .. }
            | Ast::Unreachable { .. }
            | Ast::Invalid { .. } => false,
        }
    }
}
