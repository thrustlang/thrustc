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

use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeIsExtensions, VoidTypeExtensions},
};

use crate::TypeChecker;

pub fn validate<'type_checker>(
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
            if !typechecker.get_table().constains_asm_function(name) {
                typechecker
                    .get_mut_table()
                    .new_asm_function(name, (return_type, parameters_types, attributes));
            }

            if return_type.contains_void_type() {
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
                    let type_: &Type = node.get_any_type()?;
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

        Ast::Intrinsic {
            name,
            parameters,
            parameters_types,
            return_type,
            attributes,
            ..
        } => {
            if !typechecker.get_table().constains_intrinsic(name) {
                typechecker
                    .get_mut_table()
                    .new_intrinsic(name, (return_type, parameters_types, attributes));
            }

            if return_type.contains_void_type() {
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
                    let type_: &Type = node.get_any_type()?;
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
            typechecker
                .get_mut_type_context()
                .set_current_function_type((return_type, *span));

            if !typechecker.get_table().constains_function(name) {
                typechecker
                    .get_mut_table()
                    .new_function(name, (return_type, parameter_types, attributes));
            }

            {
                for node in parameters.iter() {
                    let type_: &Type = node.get_any_type()?;
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

            if return_type.contains_void_type() {
                typechecker.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "Cannot use 'void' as a value.".into(),
                    "You should remove whatever type or value where void type belongs.".into(),
                    None,
                    return_type.get_span(),
                ));
            }

            if let Some(body) = body {
                typechecker.analyze_stmt(body)?;

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

            typechecker.add_bug(CompilationIssue::FrontEndBug(
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
