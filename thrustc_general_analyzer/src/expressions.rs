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
    Ast,
    ast_builtins::AstBuiltin,
    traits::{AstCodeLocation, AstGetType, AstMemoryExtensions, AstStandardExtensions},
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type,
    traits::{TypeExtensions, TypePointerExtensions},
};

use crate::GeneralAnalyzer;

pub fn validate_node<'analyzer>(
    analyzer: &mut GeneralAnalyzer<'analyzer>,
    node: &'analyzer Ast,
) -> Result<(), CompilationIssue> {
    match node {
        Ast::BinaryOp { left, right, .. } => {
            analyzer.analyze_expr(left)?;
            analyzer.analyze_expr(right)?;

            Ok(())
        }

        Ast::UnaryOp { node, .. } => {
            analyzer.analyze_expr(node)?;

            Ok(())
        }

        Ast::Group { node, .. } => {
            analyzer.analyze_expr(node)?;

            Ok(())
        }

        Ast::FixedArray { items, .. } => {
            {
                for node in items.iter() {
                    analyzer.analyze_expr(node)?;
                }
            }

            Ok(())
        }

        Ast::Array { items, .. } => {
            {
                for node in items.iter() {
                    analyzer.analyze_expr(node)?;
                }
            }

            Ok(())
        }

        Ast::Index { source, index, .. } => {
            let source_type: &Type = source.get_any_type()?;

            if source.is_reference() && !source.is_memory_assigned_value()? {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0007,
                    "An reference with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            if (!source.is_memory_assigned_value()? || !source.is_reference())
                && source_type.is_value()
            {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            analyzer.analyze_expr(index)?;

            Ok(())
        }

        Ast::Property { source, .. } => {
            analyzer.analyze_expr(source)?;
            Ok(())
        }

        Ast::Constructor { data, .. } => {
            {
                for (_, node, ..) in data.iter() {
                    analyzer.analyze_expr(node)?;
                }
            }

            Ok(())
        }

        Ast::Call { args, .. } => args.iter().try_for_each(|arg| analyzer.analyze_expr(arg)),

        Ast::IndirectCall { function, args, .. } => {
            analyzer.analyze_expr(function)?;

            {
                for argument in args.iter() {
                    analyzer.analyze_expr(argument)?;
                }
            }

            Ok(())
        }

        Ast::GetLocation { expr, span, .. } => {
            let expr_type: &Type = expr.get_value_type()?;

            if expr.is_reference() && !expr.is_memory_assigned_value()? {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0007,
                    "An reference with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    *span,
                ));
            } else if !expr.is_reference() && !expr_type.is_ptr_like_type() {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    *span,
                ));
            }

            analyzer.analyze_expr(expr)?;

            Ok(())
        }
        Ast::Deref { value, .. } => {
            analyzer.analyze_expr(value)?;
            Ok(())
        }
        Ast::As { from, .. } => {
            analyzer.analyze_expr(from)?;

            Ok(())
        }
        Ast::Builtin { builtin, .. } => match builtin {
            AstBuiltin::MemSet {
                dst,
                new_size,
                size,
                ..
            } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(new_size)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::MemMove { dst, src, size, .. } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(src)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::MemCpy { dst, src, size, .. } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(src)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::Halloc { .. }
            | AstBuiltin::AlignOf { .. }
            | AstBuiltin::SizeOf { .. }
            | AstBuiltin::AbiSizeOf { .. }
            | AstBuiltin::BitSizeOf { .. }
            | AstBuiltin::AbiAlignOf { .. } => Ok(()),
        },

        Ast::AsmValue { .. }
        | Ast::EnumValue { .. }
        | Ast::Reference { .. }
        | Ast::Integer { .. }
        | Ast::Boolean { .. }
        | Ast::CString { .. }
        | Ast::CNString { .. }
        | Ast::Float { .. }
        | Ast::NullPtr { .. }
        | Ast::Char { .. } => Ok(()),

        _ => {
            let span: Span = node.get_span();

            analyzer.add_bug(CompilationIssue::FrontendBug(
                "Expression not caught".into(),
                "Expression could not be caught for processing.".into(),
                span,
                CompilationPosition::Analyzer,
                std::path::PathBuf::from(file!()),
                line!(),
            ));

            Ok(())
        }
    }
}
