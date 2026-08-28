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

use std::collections::HashMap;

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

use crate::substitution::substitute;

pub type TypeEnv = HashMap<String, Type>;

#[derive(Debug, Clone)]
pub struct SolveResult {
    pub env: TypeEnv,
    pub return_type: Type,
}

pub fn solve(
    type_params: &[String],
    explicit_args: &[Type],
    parameter_types: &[Type],
    argument_types: &[Type],
    return_type: &Type,
    has_varargs: bool,
    span: Span,
) -> Result<SolveResult, CompilationIssue> {
    if explicit_args.len() > type_params.len() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0049,
            "Too many generic type arguments.".into(),
            "You should provide at most one type per generic parameter.".into(),
            None,
            span,
        ));
    }

    if !has_varargs && parameter_types.len() != argument_types.len() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0050,
            "The call does not match the generic signature.".into(),
            "The number of arguments must match the number of parameters.".into(),
            None,
            span,
        ));
    }

    let mut env: TypeEnv = TypeEnv::with_capacity(type_params.len());

    for (index, explicit) in explicit_args.iter().enumerate() {
        env.insert(type_params[index].clone(), explicit.clone());
    }

    for (parameter_type, argument_type) in parameter_types.iter().zip(argument_types.iter()) {
        self::unify(parameter_type, argument_type, &mut env);
    }

    for parameter in type_params.iter() {
        if !env.contains_key(parameter) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0051,
                format!("Could not infer the generic type parameter '{}'.", parameter),
                "Provide it explicitly between brackets or make it appear in the arguments.".into(),
                None,
                span,
            ));
        }
    }

    let return_type: Type = substitute(return_type, &env);

    Ok(SolveResult { env, return_type })
}

fn unify(declared: &Type, provided: &Type, env: &mut TypeEnv) {
    if let Type::Unresolved { hint, .. } = declared {
        if !env.contains_key(hint) {
            env.insert(hint.clone(), provided.clone());
        }

        return;
    }

    match declared {
        Type::Const(inner, _) => {
            if let Type::Const(provided_inner, _) = provided {
                self::unify(inner, provided_inner, env);
            }
        }
        Type::Ptr {
            subtype: declared_subtype,
            ..
        } => {
            if let Type::Ptr {
                subtype: provided_subtype,
                ..
            } = provided
            {
                if let (Some(declared_inner), Some(provided_inner)) =
                    (declared_subtype.as_ref(), provided_subtype.as_ref())
                {
                    self::unify(declared_inner, provided_inner, env);
                }
            }
        }
        Type::FixedArray {
            base_type: declared_base,
            ..
        } => {
            if let Type::FixedArray {
                base_type: provided_base,
                ..
            } = provided
            {
                self::unify(declared_base, provided_base, env);
            }
        }
        Type::Array {
            base_type: declared_base,
            ..
        } => {
            if let Type::Array {
                base_type: provided_base,
                ..
            } = provided
            {
                self::unify(declared_base, provided_base, env);
            }
        }
        Type::Struct {
            fields: declared_fields,
            ..
        } => {
            if let Type::Struct {
                fields: provided_fields,
                ..
            } = provided
            {
                for (declared_field, provided_field) in
                    declared_fields.iter().zip(provided_fields.iter())
                {
                    self::unify(declared_field, provided_field, env);
                }
            }
        }
        Type::Fn {
            return_type: declared_return,
            parameter_types: declared_parameters,
            ..
        } => {
            if let Type::Fn {
                return_type: provided_return,
                parameter_types: provided_parameters,
                ..
            } = provided
            {
                self::unify(declared_return, provided_return, env);

                for (declared_parameter, provided_parameter) in
                    declared_parameters.iter().zip(provided_parameters.iter())
                {
                    self::unify(declared_parameter, provided_parameter, env);
                }
            }
        }
        _ => (),
    }
}