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

use thrustc_ast::ModuleExpressionValues;
use thrustc_span::Span;
use thrustc_typesystem::Type;

use crate::Ast;

pub fn visit_all_types<'ast>(ast: &Ast<'ast>, on_type: &mut impl FnMut(&Type, Span)) {
    match ast {
        Ast::CString { kind, span, .. }
        | Ast::CNString { kind, span, .. }
        | Ast::Char { kind, span, .. }
        | Ast::Boolean { kind, span, .. }
        | Ast::Integer { kind, span, .. }
        | Ast::Float { kind, span, .. }
        | Ast::NullPtr { kind, span }
        | Ast::GlobalAssembler { kind, span, .. }
        | Ast::Embedded { kind, span, .. }
        | Ast::Continue { kind, span, .. }
        | Ast::Break { kind, span, .. }
        | Ast::ContinueAll { kind, span, .. }
        | Ast::BreakAll { kind, span, .. }
        | Ast::CustomType { kind, span, .. }
        | Ast::IntrinsicParameter { kind, span, .. }
        | Ast::AssemblerFunctionParameter { kind, span, .. }
        | Ast::FunctionParameter { kind, span, .. }
        | Ast::Reference { kind, span, .. }
        | Ast::Import { kind, span, .. }
        | Ast::ImportC { kind, span, .. }
        | Ast::Unreachable { kind, span, .. }
        | Ast::Invalid { kind, span, .. } => {
            on_type(kind, *span);
        }

        Ast::FixedArray {
            items, kind, span, ..
        }
        | Ast::Array {
            items, kind, span, ..
        } => {
            on_type(kind, *span);
            for item in items {
                visit_all_types(item, on_type);
            }
        }

        Ast::Index {
            source,
            index,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(source, on_type);
            visit_all_types(index, on_type);
        }

        Ast::Struct { kind, span, .. } => {
            on_type(kind, *span);
        }
        Ast::Constructor { kind, span, .. } => {
            on_type(kind, *span);
        }
        Ast::Enum { kind, span, .. } => {
            on_type(kind, *span);
        }
        Ast::Property {
            source, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(source, on_type);
        }
        Ast::Builtin { kind, span, .. } => {
            on_type(kind, *span);
        }

        Ast::If {
            condition,
            then_branch,
            else_if_branch,
            else_branch,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(condition, on_type);
            visit_all_types(then_branch, on_type);
            for elif in else_if_branch {
                visit_all_types(elif, on_type);
            }
            if let Some(else_branch) = else_branch {
                visit_all_types(else_branch, on_type);
            }
        }
        Ast::Elif {
            condition,
            block,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(condition, on_type);
            visit_all_types(block, on_type);
        }
        Ast::Else {
            block, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(block, on_type);
        }

        // ---------------------------------------------------
        // Loops
        // ---------------------------------------------------
        Ast::For {
            local,
            condition,
            actions,
            block,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(local, on_type);
            visit_all_types(condition, on_type);
            visit_all_types(actions, on_type);
            visit_all_types(block, on_type);
        }
        Ast::While {
            variable,
            condition,
            block,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            if let Some(variable) = variable {
                visit_all_types(variable, on_type);
            }
            visit_all_types(condition, on_type);
            visit_all_types(block, on_type);
        }
        Ast::Loop {
            block, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(block, on_type);
        }

        Ast::Block {
            nodes,
            post,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            for node in nodes {
                visit_all_types(node, on_type);
            }
            for node in post {
                visit_all_types(node, on_type);
            }
        }
        Ast::Defer {
            node, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(node, on_type);
        }

        Ast::EnumValue {
            value, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(value, on_type);
        }

        Ast::Intrinsic {
            parameters,
            parameters_types,
            return_type,
            span,
            ..
        } => {
            on_type(return_type, *span);

            for t in parameters_types {
                on_type(t, *span);
            }
            for p in parameters {
                visit_all_types(p, on_type);
            }
        }
        Ast::AssemblerFunction {
            parameters,
            parameters_types,
            return_type,
            span,
            ..
        } => {
            on_type(return_type, *span);

            for t in parameters_types {
                on_type(t, *span);
            }

            for p in parameters {
                visit_all_types(p, on_type);
            }
        }

        Ast::Function {
            parameters,
            parameter_types,
            body,
            return_type,
            span,
            ..
        } => {
            on_type(return_type, *span);

            for t in parameter_types {
                on_type(t, *span);
            }

            for p in parameters {
                visit_all_types(p, on_type);
            }

            if let Some(body) = body {
                visit_all_types(body, on_type);
            }
        }
        Ast::Return {
            expression,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);

            if let Some(expression) = expression {
                visit_all_types(expression, on_type);
            }
        }

        Ast::Static {
            kind, value, span, ..
        } => {
            on_type(kind, *span);

            if let Some(value) = value {
                visit_all_types(value, on_type);
            }
        }
        Ast::Const {
            kind, value, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(value, on_type);
        }
        Ast::Var {
            kind, value, span, ..
        } => {
            on_type(kind, *span);

            if let Some(value) = value {
                visit_all_types(value, on_type);
            }
        }

        Ast::Mutation {
            source,
            value,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(source, on_type);
            visit_all_types(value, on_type);
        }
        Ast::Address {
            source,
            indexes,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(source, on_type);
            for idx in indexes {
                visit_all_types(idx, on_type);
            }
        }
        Ast::Write {
            source,
            write_value,
            write_type,
            span,
            ..
        } => {
            on_type(write_type, *span);
            visit_all_types(source, on_type);
            visit_all_types(write_value, on_type);
        }
        Ast::Load {
            source, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(source, on_type);
        }
        Ast::Deref {
            value, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(value, on_type);
        }

        Ast::As {
            from, cast, span, ..
        } => {
            on_type(cast, *span);
            visit_all_types(from, on_type);
        }

        Ast::GetLocation {
            expr, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(expr, on_type);
        }
        Ast::ModuleExpression { values, .. } => match values {
            ModuleExpressionValues::Call { arguments, .. } => {
                for arg in arguments {
                    visit_all_types(arg, on_type);
                }
            }
            ModuleExpressionValues::Reference { .. } => {}
        },
        Ast::Call {
            args, kind, span, ..
        } => {
            on_type(kind, *span);
            for arg in args {
                visit_all_types(arg, on_type);
            }
        }
        Ast::IndirectCall {
            function,
            function_type,
            args,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            on_type(function_type, *span);
            visit_all_types(function, on_type);
            for arg in args {
                visit_all_types(arg, on_type);
            }
        }
        Ast::AsmValue {
            args, kind, span, ..
        } => {
            on_type(kind, *span);
            for arg in args {
                visit_all_types(arg, on_type);
            }
        }
        Ast::BinaryOp {
            left,
            right,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            visit_all_types(left, on_type);
            visit_all_types(right, on_type);
        }
        Ast::UnaryOp {
            kind, node, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(node, on_type);
        }
        Ast::Group {
            node, kind, span, ..
        } => {
            on_type(kind, *span);
            visit_all_types(node, on_type);
        }
    }
}
