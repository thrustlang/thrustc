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

use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_errors::CompilationIssueCode;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::ConstantTypeExtensions;
use thrustc_typesystem::traits::TypeCodeLocation;

use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use thrustc_compile_time::{BuiltinArgument, BuiltinValue};

#[derive(Debug)]
pub struct TypeWidth;

impl CompileTimeBuiltinFunction for TypeWidth {
    #[inline]
    fn name(&self) -> &'static str {
        "typeWidth"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::USize {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Type],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let ty: &Type = match &args[0] {
            BuiltinArgument::Type { ty, .. } => ty,
            BuiltinArgument::Value { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'typeWidth' compiler builtin expects a type argument.".into(),
                    "You should pass a type to 'typeWidth', like typeWidth(u32).".into(),
                    None,
                    *span,
                ));
            }
        };

        let width: u32 = match context.target_info.get_type_layout(ty) {
            either::Either::Left(layout) => layout.width,
            either::Either::Right(layout) => layout.width,
        };

        Ok(BuiltinValue::Integer(width as u64))
    }
}

#[derive(Debug)]
pub struct FieldCount;

impl CompileTimeBuiltinFunction for FieldCount {
    #[inline]
    fn name(&self) -> &'static str {
        "fieldCount"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::USize {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Type],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let ty: &Type = match &args[0] {
            BuiltinArgument::Type { ty, .. } => ty,
            BuiltinArgument::Value { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'fieldCount' compiler builtin expects a type argument.".into(),
                    "You should pass a type to 'fieldCount', like fieldCount(MyStruct).".into(),
                    None,
                    *span,
                ));
            }
        };

        let stripped: Type = ty.remove_all_constant_type();

        match &stripped {
            Type::Struct { fields, .. } => Ok(BuiltinValue::Integer(fields.len() as u64)),
            _ => Err(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                "The 'fieldCount' compiler builtin expects a struct type argument.".into(),
                "You should pass a struct type to 'fieldCount', like fieldCount(MyStruct).".into(),
                None,
                stripped.get_span(),
            )),
        }
    }
}
