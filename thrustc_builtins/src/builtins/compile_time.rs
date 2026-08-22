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

use crate::builtins::location::cstring_type;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct StaticAssert;

impl CompileTimeBuiltinFunction for StaticAssert {
    #[inline]
    fn name(&self) -> &'static str {
        "staticAssert"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Void {
                span: Span::nothing(),
            },
            parameters: vec![
                BuiltinParameter::Value(Type::Bool {
                    span: Span::nothing(),
                }),
                BuiltinParameter::Value(self::cstring_type()),
            ],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let condition: bool = match &args[0] {
            BuiltinArgument::Value {
                value: BuiltinValue::Bool(condition),
                ..
            } => *condition,
            BuiltinArgument::Value { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'staticAssert' compiler builtin expects a boolean condition.".into(),
                    "You should pass a constant boolean to 'staticAssert'.".into(),
                    None,
                    *span,
                ));
            }
            BuiltinArgument::Type { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'staticAssert' compiler builtin expects a boolean condition.".into(),
                    "You should pass a constant boolean to 'staticAssert'.".into(),
                    None,
                    *span,
                ));
            }
        };

        let message: String = match &args[1] {
            BuiltinArgument::Value {
                value: BuiltinValue::CString(bytes),
                ..
            } => String::from_utf8_lossy(bytes).into_owned(),
            _ => "Static assertion failed.".to_string(),
        };

        if !condition {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                format!("Static assertion failed: {}.", message),
                "The condition of the 'staticAssert' builtin was not satisfied.".into(),
                None,
                context.call_span,
            ));
        }

        Ok(BuiltinValue::Void)
    }
}

#[derive(Debug)]
pub struct CompileError;

impl CompileTimeBuiltinFunction for CompileError {
    #[inline]
    fn name(&self) -> &'static str {
        "compileError"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Void {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Value(self::cstring_type())],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let message: String = match &args[0] {
            BuiltinArgument::Value {
                value: BuiltinValue::CString(bytes),
                ..
            } => String::from_utf8_lossy(bytes).into_owned(),
            _ => "Compilation error.".to_string(),
        };

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            message,
            "The 'compileError' builtin was invoked.".into(),
            None,
            context.call_span,
        ))
    }
}

#[derive(Debug)]
pub struct CompileWarning;

impl CompileTimeBuiltinFunction for CompileWarning {
    #[inline]
    fn name(&self) -> &'static str {
        "compileWarning"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Void {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Value(self::cstring_type())],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let message: String = match &args[0] {
            BuiltinArgument::Value {
                value: BuiltinValue::CString(bytes),
                ..
            } => String::from_utf8_lossy(bytes).into_owned(),
            _ => "Compilation warning.".to_string(),
        };

        context.warnings.push(CompilationIssue::Warning(
            CompilationIssueCode::W0031,
            message,
            context.call_span,
        ));

        Ok(BuiltinValue::Void)
    }
}