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
    traits::{AstCodeLocation, AstGetType, AstLiteralExtensions},
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};

use thrustc_code_location::Span;
use thrustc_typesystem::{
    Type,
    traits::{
        ConstantTypeExtensions, TypeArrayEntensions, TypeCodeLocation, TypeFixedArrayEntensions,
        TypeIsExtensions, TypePointerExtensions, VoidTypeExtensions,
    },
};

use crate::{
    TypeChecker, context::TypeCheckerControlContext, operations, type_checking,
    type_metadata::TypeCheckerNodeMetadata,
};

mod call_expr;
mod compiler_builtins;

pub fn validate_node<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    node: &'type_checker Ast,
) -> Result<(), CompilationIssue> {
    match node {
        Ast::BinaryOp {
            left,
            operator,
            right,
            kind,
            span,
            ..
        } => {
            let left_type: &Type = left.get_value_type()?;
            let right_type: &Type = right.get_value_type()?;

            operations::binary_operation::validate_binary_node(
                operator, left_type, right_type, *span,
            )?;

            typechecker.analyze_expr(left)?;
            typechecker.analyze_expr(right)?;

            if left_type.contains_void_type()
                || left_type.is_void_type()
                || right_type.contains_void_type()
                || right_type.is_void_type()
            {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    left_type.get_span(),
                ));
            }

            if kind.contains_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }

        Ast::UnaryOp {
            operator,
            node,
            kind,
            span,
            ..
        } => {
            operations::unary_operation::validate_unary_node(
                operator,
                node.get_value_type()?,
                *span,
            )?;

            typechecker.analyze_expr(node)?;

            let expr_type: &Type = node.get_value_type()?;

            if expr_type.contains_void_type() || expr_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    expr_type.get_span(),
                ));
            }

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }

        Ast::Group { node, kind, .. } => {
            typechecker.analyze_expr(node)?;

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }

        Ast::FixedArray {
            items, kind, span, ..
        } => {
            if kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "An element is expected for type inference.".into(),
                    "It must have at least one element.".into(),
                    None,
                    *span,
                ));
            } else if kind.contains_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            for node in items.iter() {
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(node.is_totaly_literal_value());
                let item_type: &Type = node.get_value_type()?;
                let base_type: Type = kind.get_fixed_array_base_type();

                let span: Span = node.get_span();

                {
                    let control_context: &mut TypeCheckerControlContext =
                        typechecker.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = type_checking::check_type_together(
                        &base_type,
                        item_type,
                        Some(node),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        typechecker.add_error_report(error);
                    }
                }

                typechecker.analyze_expr(node)?;
            }

            Ok(())
        }

        Ast::Array {
            items, kind, span, ..
        } => {
            if kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "An element is expected for type inference.".into(),
                    "It must have at least one element.".into(),
                    None,
                    *span,
                ));
            } else if kind.contains_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            for node in items.iter() {
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(node.is_totaly_literal_value());
                let item_type: &Type = node.get_value_type()?;
                let base_type: Type = kind.get_array_base_type();
                let span: Span = node.get_span();

                if item_type.contains_void_type() || item_type.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        item_type.get_span(),
                    ));
                }

                {
                    let control_context: &mut TypeCheckerControlContext =
                        typechecker.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = type_checking::check_type_together(
                        &base_type,
                        item_type,
                        Some(node),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        typechecker.add_error_report(error);
                    }
                }

                typechecker.analyze_expr(node)?;
            }

            Ok(())
        }

        Ast::Index { source, index, .. } => {
            let index_type: &Type = index.get_value_type()?;
            let source_type: &Type = source.get_value_type()?;
            let span: Span = index.get_span();

            if !index_type.is_integer_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    format!("Expected integer value, got '{}'.", index_type),
                    "You should make it match in an integer value.".into(),
                    None,
                    span,
                ));
            }

            typechecker.analyze_expr(index)?;
            typechecker.analyze_expr(source)?;

            if index_type.contains_void_type()
                || index_type.is_void_type()
                || source_type.contains_void_type()
                || source_type.is_void_type()
            {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    index_type.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Property { source, data, .. } => {
            let source_type: &Type = source.get_value_type()?;
            let source_span: Span = source.get_span();

            if !source_type.is_struct_type() && !source_type.is_ptr_struct_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    format!("A structure type was expected within a structure 'struct T' type, or structure pointer 'ptr[struct T]', got '{}' type.", source_type),
                    "It should be a structure or structure pointer reference; make it match.".into(),
                    None,
                    source_span,
                ));
            }

            typechecker.analyze_expr(source)?;

            if source_type.contains_void_type() || source_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    source_type.get_span(),
                ));
            }

            for (ty, (subtype, ..)) in data.iter() {
                if ty.contains_void_type() || ty.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        ty.get_span(),
                    ));
                }

                if subtype.contains_void_type() || subtype.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        subtype.get_span(),
                    ));
                }

                if !ty.is_struct_type() && !ty.is_ptr_struct_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        format!("A structure type was expected within a structure 'struct T' type, or structure pointer 'ptr[struct T]', got '{}' type.", source_type),
                        "It should be a structure or structure pointer reference; make it match.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }

            Ok(())
        }

        Ast::Constructor { data, .. } => {
            for (_, expr, target_type, _) in data.iter() {
                let span: Span = expr.get_span();
                let from_type: &Type = expr.get_value_type()?;

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(expr.is_totaly_literal_value());

                {
                    let control_context: &mut TypeCheckerControlContext =
                        typechecker.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = type_checking::check_type_together(
                        target_type,
                        from_type,
                        Some(expr),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        typechecker.add_error_report(error);
                    }
                }

                typechecker.analyze_expr(expr)?;

                if target_type.contains_void_type() || target_type.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        target_type.get_span(),
                    ));
                }

                if from_type.contains_void_type() || from_type.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        from_type.get_span(),
                    ));
                }
            }

            Ok(())
        }

        Ast::Call {
            name, args, span, ..
        } => {
            if let Some(metadata) = typechecker.get_table().get_function(name) {
                return call_expr::validate_node(typechecker, *metadata, args, span);
            }

            if let Some(metadata) = typechecker.get_table().get_intrinsic(name) {
                return call_expr::validate_node(typechecker, *metadata, args, span);
            }

            if let Some(metadata) = typechecker.get_table().get_asm_function(name) {
                return call_expr::validate_node(typechecker, *metadata, args, span);
            }

            typechecker.add_error_report(CompilationIssue::FrontendBug(
                "Function not found".into(),
                "Function could not be found for processing.".into(),
                *span,
                CompilationPosition::TypeChecker,
                std::path::PathBuf::from(file!()),
                line!(),
            ));

            Ok(())
        }

        Ast::IndirectCall {
            function,
            function_type,
            args,
            span,
            ..
        } => {
            typechecker.analyze_expr(function)?;

            let fn_type: Type = function_type.remove_all_constant_type();

            if !fn_type.is_function_reference_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Expected function reference type for call anonymously.".into(),
                    "It should be a function type refernce.".into(),
                    None,
                    function.get_span(),
                ));

                {
                    for argument in args.iter() {
                        typechecker.analyze_expr(argument)?;
                    }
                }

                return Ok(());
            }

            let Type::Fn {
                return_type,
                parameter_types,
                modificator,
                ..
            } = fn_type
            else {
                unreachable!()
            };

            let required_count: usize = parameter_types.len();
            let provided_count: usize = args.len();

            let var_args: bool = modificator.llvm().has_ignore();

            if return_type.contains_inner_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    *span,
                ));
            }

            if parameter_types
                .iter()
                .any(|ty| ty.contains_void_type() || ty.is_void_type())
            {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    *span,
                ));
            }

            if required_count != provided_count && !var_args {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0022,
                    format!(
                        "Expected arguments total '{}', not '{}'.",
                        required_count, provided_count
                    ),
                    "You should try to filling it out using the equals type.".into(),
                    None,
                    *span,
                ));

                let expected_types: String = parameter_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0023,
                    format!("Arguments were expected in the order '{}'.", expected_types),
                    "You should reorder it equals to its type.".into(),
                    None,
                    *span,
                ));

                return Ok(());
            }

            {
                for (target_type, expr) in parameter_types.iter().zip(args.iter()) {
                    let from_type: &Type = expr.get_value_type()?;
                    let expr_metadata: TypeCheckerNodeMetadata =
                        TypeCheckerNodeMetadata::new(expr.is_totaly_literal_value());

                    {
                        let control_context: &mut TypeCheckerControlContext =
                            typechecker.get_mut_control_context();

                        control_context.reset_checking_depth();

                        if let Err(error) = type_checking::check_type_together(
                            target_type,
                            from_type,
                            Some(expr),
                            None,
                            expr_metadata,
                            expr.get_span(),
                            control_context,
                        ) {
                            typechecker.add_error_report(error);
                        }
                    }
                }
            }

            {
                for argument in args.iter() {
                    typechecker.analyze_expr(argument)?;
                }
            }

            Ok(())
        }

        Ast::Deref { value, kind, .. } => {
            let value_type: &Type = value.get_value_type()?;

            if !value_type.is_ptr_like_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    value.get_span(),
                ));
            }

            typechecker.analyze_expr(value)?;

            if value_type.contains_void_type() || value_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    value_type.get_span(),
                ));
            }

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Load { source, kind, .. } => {
            let source_type: &Type = source.get_value_type()?;

            typechecker.analyze_expr(source)?;

            if source_type.contains_void_type() || source_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    source_type.get_span(),
                ));
            }

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::As {
            from,
            cast: cast_type,
            metadata,
            span,
            ..
        } => {
            let from_type: &Type = from.get_value_type()?;

            let control_context: &mut TypeCheckerControlContext =
                typechecker.get_mut_control_context();

            type_checking::check_type_cast(cast_type, from_type, metadata, span, control_context)?;

            control_context.reset_type_cast_depth();

            typechecker.analyze_expr(from)?;

            if cast_type.contains_void_type() || cast_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    cast_type.get_span(),
                ));
            }

            if from_type.contains_void_type() || from_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    from_type.get_span(),
                ));
            }

            Ok(())
        }

        Ast::Builtin { builtin, .. } => compiler_builtins::validate_node(typechecker, builtin),

        Ast::AsmValue { args, kind, .. } => {
            for node in args.iter() {
                let node_type: &Type = node.get_value_type()?;

                if node_type.contains_void_type() || node_type.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        node_type.get_span(),
                    ));
                }
            }

            if kind.contains_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }

        Ast::EnumValue { value, kind, .. } => {
            let node_type: &Type = value.get_value_type()?;

            if node_type.contains_void_type() || node_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    node_type.get_span(),
                ));
            }

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Reference { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Integer { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Boolean { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::CString { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::CNString { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Float { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::NullPtr { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::Char { kind, .. } => {
            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }
        Ast::GetLocation { expr, kind, .. } => {
            let expr_type: &Type = expr.get_value_type()?;

            if expr_type.contains_void_type() || expr_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    expr_type.get_span(),
                ));
            }

            if kind.contains_void_type() || kind.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    kind.get_span(),
                ));
            }

            Ok(())
        }

        _ => {
            let span: Span = node.get_span();

            typechecker.add_bug(CompilationIssue::FrontendBug(
                "Expression not caught".into(),
                "Expression could not be caught for processing.".into(),
                span,
                CompilationPosition::TypeChecker,
                std::path::PathBuf::from(file!()),
                line!(),
            ));

            Ok(())
        }
    }
}
