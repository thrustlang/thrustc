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
    traits::{AstCodeBlockEntensions, AstCodeLocation, AstGetType},
};

use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeIsExtensions, VoidTypeExtensions},
};

use crate::{TypeChecker, type_checking, type_support, visit_type};

pub fn validate_node<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    node: &'type_checker Ast,
) -> Result<(), CompilationIssue> {
    match node {
        Ast::AssemblerFunction {
            name,
            parameters,
            parameters_types,
            return_type,
            attributes,
            ..
        } => {
            visit_type::visit_all_types(node, &mut |ty, _| {
                type_support::check_target_type_support(typechecker, ty);
                type_checking::check_if_a_type_is_unresolved(typechecker, ty);
            });

            typechecker
                .get_mut_table()
                .new_asm_function(name, (return_type, parameters_types, attributes));

            if return_type.contains_inner_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            {
                for node in parameters.iter() {
                    let type_: &Type = node.get_any_type();
                    let span: Span = node.get_span();

                    if type_.contains_void_type() || type_.is_void_type() {
                        typechecker.add_error_report(CompilationIssue::Error(
                            CompilationIssueCode::E0019,
                            "Cannot use 'void' as a value.".into(),
                            "You should remove whatever type or value where void type belongs."
                                .into(),
                            None,
                            span,
                        ));
                    }
                }
            }

            Ok(())
        }
        Ast::CompilerIntrinsic {
            name,
            parameters,
            parameters_types,
            return_type,
            attributes,
            ..
        } => {
            visit_type::visit_all_types(node, &mut |ty, _| {
                type_support::check_target_type_support(typechecker, ty);
                type_checking::check_if_a_type_is_unresolved(typechecker, ty);
            });

            typechecker
                .get_mut_table()
                .new_compiler_intrinsic(name, (return_type, parameters_types, attributes));

            if return_type.contains_inner_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            if attributes.has_noreturn_attribute() && !return_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "@noReturn intrinsic must have a void return type.".into(),
                    "A function that never returns cannot produce a value.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            {
                for node in parameters.iter() {
                    let type_: &Type = node.get_any_type();
                    let span: Span = node.get_span();

                    if type_.contains_void_type() || type_.is_void_type() {
                        typechecker.add_error_report(CompilationIssue::Error(
                            CompilationIssueCode::E0019,
                            "Cannot use 'void' as a value.".into(),
                            "You should remove whatever type or value where void type belongs."
                                .into(),
                            None,
                            span,
                        ));
                    }
                }
            }

            Ok(())
        }

        Ast::Function {
            name,
            parameters,
            parameter_types,
            body,
            return_type,
            attributes,
            span,
            ..
        } => {
            visit_type::visit_all_types(node, &mut |ty, _| {
                type_support::check_target_type_support(typechecker, ty);
                type_checking::check_if_a_type_is_unresolved(typechecker, ty);
            });

            typechecker
                .get_mut_type_context()
                .set_current_function_type((return_type, *span));

            typechecker
                .get_mut_table()
                .new_function(name, (return_type, parameter_types, attributes));

            {
                for node in parameters.iter() {
                    let type_: &Type = node.get_any_type();
                    let span: Span = node.get_span();

                    if type_.contains_void_type() || type_.is_void_type() {
                        typechecker.add_error_report(CompilationIssue::Error(
                            CompilationIssueCode::E0019,
                            "Cannot use 'void' as a value.".into(),
                            "You should remove whatever type or value where void type belongs."
                                .into(),
                            None,
                            span,
                        ));
                    }
                }
            }

            if return_type.contains_inner_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            let has_noreturn_attribute: bool = attributes.has_noreturn_attribute();

            if has_noreturn_attribute && !return_type.is_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "@noReturn function must have a void return type.".into(),
                    "A function that never returns cannot produce a value.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            if let Some(body) = body {
                typechecker.analyze_stmt(body)?;

                if has_noreturn_attribute && !body.has_terminator() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "@noReturn function must always terminate and never fall off the end.".into(),
                        "A noreturn function must end with a return, an unreachable, or a terminator."
                            .into(),
                        None,
                        *span,
                    ));
                }

                if !body.has_terminator() && !return_type.is_void_type() {
                    typechecker.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        format!("Expected return with type '{}'.", return_type),
                        format!("It should be return '{}'.", return_type),
                        None,
                        *span,
                    ));
                }
            }

            typechecker
                .get_mut_type_context()
                .unset_current_function_type();

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
