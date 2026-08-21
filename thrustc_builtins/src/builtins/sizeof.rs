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

use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct SizeOf;

impl CompileTimeBuiltinFunction for SizeOf {
    fn name(&self) -> &'static str {
        "sizeOf"
    }

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
                    "The 'sizeOf' compiler builtin expects a type argument.".into(),
                    "You should pass a type to 'sizeOf', like sizeOf(u32).".into(),
                    None,
                    *span,
                ));
            }
        };

        let size_of: u32 = match context.target_info.get_type_layout(ty) {
            either::Either::Left(layout) => layout.into_layout().sizeof,
            either::Either::Right(layout) => layout.into_layout().sizeof,
        };

        Ok(BuiltinValue::Integer(size_of as u64))
    }
}