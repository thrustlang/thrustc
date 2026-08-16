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

use thrustc_ast::{
    ModuleExpressionValues,
    traits::AstCodeLocation,
};
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::Ast;

pub fn visit_all_types<'ast>(ast: &Ast<'ast>, on_type: &mut impl FnMut(&Type, Span)) -> Option<Span> {
    self::visit_all_types_inner(ast, on_type, 0)
}

fn visit_all_types_inner<'ast>(
    ast: &Ast<'ast>,
    on_type: &mut impl FnMut(&Type, Span),
    depth: u32,
) -> Option<Span> {
    if depth > thrustc_constants::COMPILER_TOO_MANY_EXPRESSION_DEPTH {
        return Some(ast.get_span());
    }

    let depth: u32 = depth.saturating_add(1);

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
        | Ast::CompilerIntrinsicParameter { kind, span, .. }
        | Ast::AssemblerFunctionParameter { kind, span, .. }
        | Ast::FunctionParameter { kind, span, .. }
        | Ast::Reference { kind, span, .. }
        | Ast::Import { kind, span, .. }
        | Ast::ImportC { kind, span, .. }
        | Ast::Unreachable { kind, span, .. }
        | Ast::Invalid { kind, span, .. } => {
            on_type(kind, *span);
            None
        }

        Ast::FixedArray {
            items, kind, span, ..
        }
        | Ast::Array {
            items, kind, span, ..
        } => {
            on_type(kind, *span);
            for item in items {
                if let Some(span) = visit_all_types_inner(item, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }

        Ast::Index {
            source,
            index,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(index, on_type, depth) {
                return Some(span);
            }
            None
        }

        Ast::Struct { kind, span, .. } => {
            on_type(kind, *span);
            None
        }
        Ast::Constructor { kind, span, .. } => {
            on_type(kind, *span);
            None
        }
        Ast::Enum { kind, span, .. } => {
            on_type(kind, *span);
            None
        }
        Ast::Property {
            source, kind, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Builtin { kind, span, .. } => {
            on_type(kind, *span);
            None
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
            if let Some(span) = visit_all_types_inner(condition, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(then_branch, on_type, depth) {
                return Some(span);
            }
            for elif in else_if_branch {
                if let Some(span) = visit_all_types_inner(elif, on_type, depth) {
                    return Some(span);
                }
            }
            if let Some(else_branch) = else_branch {
                if let Some(span) = visit_all_types_inner(else_branch, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::Elif {
            condition,
            block,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(condition, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(block, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Else {
            block, kind, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(block, on_type, depth) {
                return Some(span);
            }
            None
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
            if let Some(span) = visit_all_types_inner(local, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(condition, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(actions, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(block, on_type, depth) {
                return Some(span);
            }
            None
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
                if let Some(span) = visit_all_types_inner(variable, on_type, depth) {
                    return Some(span);
                }
            }

            if let Some(span) = visit_all_types_inner(condition, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(block, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Loop {
            block, kind, span, ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(block, on_type, depth) {
                return Some(span);
            }
            None
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
                if let Some(span) = visit_all_types_inner(node, on_type, depth) {
                    return Some(span);
                }
            }

            for node in post {
                if let Some(span) = visit_all_types_inner(node, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::Defer {
            node, kind, span, ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(node, on_type, depth) {
                return Some(span);
            }
            None
        }

        Ast::EnumValue {
            value, kind, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                return Some(span);
            }
            None
        }

        Ast::CompilerIntrinsic {
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
                if let Some(span) = visit_all_types_inner(p, on_type, depth) {
                    return Some(span);
                }
            }
            None
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
                if let Some(span) = visit_all_types_inner(p, on_type, depth) {
                    return Some(span);
                }
            }
            None
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
                if let Some(span) = visit_all_types_inner(p, on_type, depth) {
                    return Some(span);
                }
            }

            if let Some(body) = body {
                if let Some(span) = visit_all_types_inner(body, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::Return {
            expression,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);

            if let Some(expression) = expression {
                if let Some(span) = visit_all_types_inner(expression, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }

        Ast::Static {
            kind, value, span, ..
        } => {
            on_type(kind, *span);

            if let Some(value) = value {
                if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::Const {
            kind, value, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Var {
            kind, value, span, ..
        } => {
            on_type(kind, *span);

            if let Some(value) = value {
                if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }

        Ast::Mutation {
            source,
            value,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Address {
            source,
            indexes,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }

            for idx in indexes {
                if let Some(span) = visit_all_types_inner(idx, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::Write {
            source,
            write_value,
            write_type,
            span,
            ..
        } => {
            on_type(write_type, *span);

            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(write_value, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Load {
            source, kind, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(source, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Deref {
            value, kind, span, ..
        } => {
            on_type(kind, *span);
            if let Some(span) = visit_all_types_inner(value, on_type, depth) {
                return Some(span);
            }
            None
        }

        Ast::As {
            from, cast, span, ..
        } => {
            on_type(cast, *span);
            if let Some(span) = visit_all_types_inner(from, on_type, depth) {
                return Some(span);
            }
            None
        }

        Ast::GetLocation {
            expr, kind, span, ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(expr, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::ModuleExpression { values, .. } => match values {
            ModuleExpressionValues::Call { arguments, .. } => {
                for arg in arguments {
                    if let Some(span) = visit_all_types_inner(arg, on_type, depth) {
                        return Some(span);
                    }
                }
                None
            }

            ModuleExpressionValues::Reference { .. } => None,
        },
        Ast::Call {
            args, kind, span, ..
        } => {
            on_type(kind, *span);

            for arg in args {
                if let Some(span) = visit_all_types_inner(arg, on_type, depth) {
                    return Some(span);
                }
            }
            None
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

            if let Some(span) = visit_all_types_inner(function, on_type, depth) {
                return Some(span);
            }

            for arg in args {
                if let Some(span) = visit_all_types_inner(arg, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::AsmValue {
            args, kind, span, ..
        } => {
            on_type(kind, *span);
            for arg in args {
                if let Some(span) = visit_all_types_inner(arg, on_type, depth) {
                    return Some(span);
                }
            }
            None
        }
        Ast::BinaryOp {
            left,
            right,
            kind,
            span,
            ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(left, on_type, depth) {
                return Some(span);
            }
            if let Some(span) = visit_all_types_inner(right, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::UnaryOp {
            kind, node, span, ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(node, on_type, depth) {
                return Some(span);
            }
            None
        }
        Ast::Group {
            node, kind, span, ..
        } => {
            on_type(kind, *span);

            if let Some(span) = visit_all_types_inner(node, on_type, depth) {
                return Some(span);
            }
            None
        }
    }
}
