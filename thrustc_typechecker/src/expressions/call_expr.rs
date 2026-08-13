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
use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_entities::typechecker_entities::TypeCheckerFunction;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_typesystem::{Type, traits::VoidTypeExtensions};

use crate::{
    TypeChecker, context::TypeCheckerControlContext, type_checking,
    type_metadata::TypeCheckerNodeMetadata,
};

pub fn validate_node<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    metadata: TypeCheckerFunction<'type_checker>,
    args: &'type_checker [Ast],
    span: &Span,
) -> Result<(), CompilationIssue> {
    let (return_type, parameter_types, attributes) = metadata;

    let required_count: usize = parameter_types.len();
    let provided_count: usize = args.len();

    let var_args: bool = attributes.has_ignore_attribute();

    if return_type.contains_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            *span,
        ));
    }

    if parameter_types.iter().any(|ty| ty.contains_void_type()) {
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
        for arg in args.iter() {
            typechecker.analyze_expr(arg)?;
        }
    }

    Ok(())
}
