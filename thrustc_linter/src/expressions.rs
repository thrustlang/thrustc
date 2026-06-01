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

use thrustc_ast::{Ast, ast_builtins::AstBuiltin, traits::AstCodeLocation};
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_span::Span;
use thrustc_token_type::traits::TokenTypeExtensions;

use crate::Linter;

pub fn analyze<'linter>(linter: &mut Linter<'linter>, expr: &'linter Ast) {
    match expr {
        Ast::Group { node, .. } => {
            linter.analyze_expr(node);
        }

        Ast::BinaryOp { left, right, .. } => {
            linter.analyze_expr(left);
            linter.analyze_expr(right);
        }

        Ast::UnaryOp { operator, node, .. } => {
            if let Ast::Reference { name, .. } = &**node {
                crate::mark_as_used(linter, name);

                if operator.is_minus_minus_operator() || operator.is_plus_plus_operator() {
                    crate::mark_as_mutated(linter, name);
                }
            }

            linter.analyze_expr(node);
        }

        Ast::AsmValue { args, .. } => {
            for node in args.iter() {
                linter.analyze_expr(node);
            }
        }

        Ast::Index { source, index, .. } => {
            linter.analyze_expr(source);
            linter.analyze_expr(index);
        }

        Ast::Property { source, .. } => {
            linter.analyze_expr(source);
        }

        Ast::Constructor {
            name, data, span, ..
        } => {
            for (_, expr, ..) in data.iter() {
                linter.analyze_expr(expr);
            }

            if let Some(structure) = linter.symbols.get_struct_info(name) {
                structure.2 = true;
            } else {
                linter.add_bug(CompilationIssue::FrontEndBug(
                    String::from("Structure not caught"),
                    format!("Could not get named struct with name '{}'.", name),
                    *span,
                    CompilationPosition::Linter,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ));
            }
        }

        Ast::IndirectCall { function, args, .. } => {
            linter.analyze_expr(function);

            {
                for argument in args.iter() {
                    linter.analyze_expr(argument);
                }
            }
        }

        Ast::Call {
            name, span, args, ..
        } => {
            if let Some(function) = linter.get_mut_symbols().get_function_info(name) {
                function.1 = true;

                {
                    for argument in args.iter() {
                        linter.analyze_expr(argument);
                    }
                }

                return;
            }

            if let Some(asm_function) = linter.get_mut_symbols().get_asm_function_info(name) {
                asm_function.1 = true;

                {
                    for argument in args.iter() {
                        linter.analyze_expr(argument);
                    }
                }

                return;
            }

            if let Some(intrinsic) = linter.get_mut_symbols().get_intrinsic_info(name) {
                intrinsic.1 = true;

                {
                    for argument in args.iter() {
                        linter.analyze_expr(argument);
                    }
                }

                return;
            }

            linter.add_bug(CompilationIssue::FrontEndBug(
                String::from("Call not caught"),
                format!("Could not get named function '{}'.", name),
                *span,
                CompilationPosition::Linter,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Ast::Reference { name, .. } => {
            crate::mark_as_used(linter, name);
        }

        Ast::FixedArray { items, .. } | Ast::Array { items, .. } => {
            items.iter().for_each(|item| {
                linter.analyze_expr(item);
            });
        }

        Ast::Mutation { source, .. } => {
            linter.analyze_expr(source);
        }

        Ast::EnumValue {
            name, value, span, ..
        } => {
            if let Some((enum_name, field_name)) =
                linter.get_mut_symbols().split_enum_field_name(name)
            {
                if let Some(union) = linter.get_mut_symbols().get_enum_info(enum_name) {
                    union.2 = true;
                }

                if let Some(enum_field) = linter
                    .get_mut_symbols()
                    .get_enum_field_info(enum_name, field_name)
                {
                    enum_field.1 = true;
                }

                linter.analyze_expr(value);

                return;
            }

            linter.add_bug(CompilationIssue::FrontEndBug(
                String::from("Enum value not caught"),
                format!("Could not get correct name of the enum field '{}'.", name),
                *span,
                CompilationPosition::Linter,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Ast::Builtin { builtin, .. } => match builtin {
            AstBuiltin::MemCpy { src, dst, size, .. } => {
                linter.analyze_expr(src);
                linter.analyze_expr(dst);
                linter.analyze_expr(size);
            }
            AstBuiltin::MemMove { src, dst, size, .. } => {
                linter.analyze_expr(src);
                linter.analyze_expr(dst);
                linter.analyze_expr(size);
            }
            AstBuiltin::MemSet {
                dst,
                new_size,
                size,
                ..
            } => {
                linter.analyze_expr(dst);
                linter.analyze_expr(new_size);
                linter.analyze_expr(size);
            }
            AstBuiltin::Halloc { .. }
            | AstBuiltin::AlignOf { .. }
            | AstBuiltin::SizeOf { .. }
            | AstBuiltin::AbiSizeOf { .. }
            | AstBuiltin::BitSizeOf { .. }
            | AstBuiltin::AbiAlignOf { .. } => (),
        },

        Ast::As { from, .. } => {
            linter.analyze_expr(from);
        }

        Ast::Deref { value, .. } => {
            linter.analyze_expr(value);
        }

        Ast::GetLocation { expr, .. } => {
            linter.analyze_expr(expr);
        }

        Ast::Integer { .. }
        | Ast::Boolean { .. }
        | Ast::CString { .. }
        | Ast::CNString { .. }
        | Ast::Float { .. }
        | Ast::NullPtr { .. }
        | Ast::Char { .. } => (),

        _ => {
            let span: Span = expr.get_span();

            linter.add_bug(CompilationIssue::FrontEndBug(
                "Expression not caught".into(),
                "Expression could not be caught for processing.".into(),
                span,
                CompilationPosition::Linter,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }
    }
}
