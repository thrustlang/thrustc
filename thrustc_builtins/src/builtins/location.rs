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
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;

use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct File;

impl CompileTimeBuiltinFunction for File {
    #[inline]
    fn name(&self) -> &'static str {
        "file"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: self::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[crate::value::BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let path: std::path::PathBuf = context.file.get_path().to_path_buf();

        Ok(BuiltinValue::CString(
            path.to_string_lossy().into_owned().into_bytes(),
        ))
    }
}

#[derive(Debug)]
pub struct FileLine;

impl CompileTimeBuiltinFunction for FileLine {
    #[inline]
    fn name(&self) -> &'static str {
        "fileLine"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::U32 {
                span: Span::nothing(),
            },
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[crate::value::BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        Ok(BuiltinValue::Integer(context.call_span.get_line() as u64))
    }
}

#[derive(Debug)]
pub struct CurrentFuncName;

impl CompileTimeBuiltinFunction for CurrentFuncName {
    #[inline]
    fn name(&self) -> &'static str {
        "currentFuncName"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: self::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[crate::value::BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let name: &str = context.current_function.unwrap_or("");

        Ok(BuiltinValue::CString(name.as_bytes().to_vec()))
    }
}

pub fn cstring_type() -> Type {
    Type::Const(
        Type::Array {
            base_type: Type::Char {
                span: Span::nothing(),
            }
            .into(),
            infered_type: None,
            metadata: ArrayTypeMetadata::new(None, None),
            span: Span::nothing(),
        }
        .into(),
        Span::nothing(),
    )
}
