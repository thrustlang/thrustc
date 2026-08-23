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
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::{ConstantTypeExtensions, TypeCodeLocation, TypePointerExtensions};

use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct FixedArraySize;

impl CompileTimeBuiltinFunction for FixedArraySize {
    #[inline]
    fn name(&self) -> &'static str {
        "fixedArraySize"
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
                    "The 'fixedArraySize' compiler builtin expects a type argument.".into(),
                    "You should pass a fixed array type, like fixedArraySize(array[u8; 4])."
                        .into(),
                    None,
                    *span,
                ));
            }
        };

        let stripped: Type = ty.remove_all_constant_type();

        match stripped {
            Type::FixedArray { size, .. } => Ok(BuiltinValue::Integer(size as u64)),
            _ => Err(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                "The 'fixedArraySize' compiler builtin expects a fixed array type.".into(),
                "You should pass a fixed array type, like fixedArraySize(array[u8; 4]).".into(),
                None,
                stripped.get_span(),
            )),
        }
    }
}

#[derive(Debug)]
pub struct IsSameType;

impl CompileTimeBuiltinFunction for IsSameType {
    #[inline]
    fn name(&self) -> &'static str {
        "isSameType"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Bool {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Type, BuiltinParameter::Type],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let left: &Type = match &args[0] {
            BuiltinArgument::Type { ty, .. } => ty,
            BuiltinArgument::Value { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'isSameType' compiler builtin expects type arguments.".into(),
                    "You should pass two types, like isSameType(u32, s32).".into(),
                    None,
                    *span,
                ));
            }
        };

        let right: &Type = match &args[1] {
            BuiltinArgument::Type { ty, .. } => ty,
            BuiltinArgument::Value { span, .. } => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'isSameType' compiler builtin expects type arguments.".into(),
                    "You should pass two types, like isSameType(u32, s32).".into(),
                    None,
                    *span,
                ));
            }
        };

        Ok(BuiltinValue::Bool(
            left.remove_all_constant_type() == right.remove_all_constant_type(),
        ))
    }
}

#[derive(Debug)]
pub struct IsPtrLike;

impl CompileTimeBuiltinFunction for IsPtrLike {
    #[inline]
    fn name(&self) -> &'static str {
        "isPtrLike"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Bool {
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
                    "The 'isPtrLike' compiler builtin expects a type argument.".into(),
                    "You should pass a type, like isPtrLike(ptr[u32]).".into(),
                    None,
                    *span,
                ));
            }
        };

        Ok(BuiltinValue::Bool(ty.remove_all_constant_type().is_ptr_like_type()))
    }
}

#[derive(Debug)]
pub struct IsFixedArrayOfSize;

impl CompileTimeBuiltinFunction for IsFixedArrayOfSize {
    #[inline]
    fn name(&self) -> &'static str {
        "isFixedArrayOfSize"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Bool {
                span: Span::nothing(),
            },
            parameters: vec![
                BuiltinParameter::Type,
                BuiltinParameter::Value(Type::USize {
                    span: Span::nothing(),
                }),
            ],
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
                    "The 'isFixedArrayOfSize' compiler builtin expects a type argument.".into(),
                    "You should pass a type, like isFixedArrayOfSize(array[u8; 4], 4).".into(),
                    None,
                    *span,
                ));
            }
        };

        let size: u64 = match &args[1] {
            BuiltinArgument::Value {
                value: BuiltinValue::Integer(size),
                ..
            } => *size,
            _ => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'isFixedArrayOfSize' compiler builtin expects a constant size value."
                        .into(),
                    "You should pass a constant integer size, like isFixedArrayOfSize(array[u8; 4], 4)."
                        .into(),
                    None,
                    Span::nothing(),
                ));
            }
        };

        match ty.remove_all_constant_type() {
            Type::FixedArray { size: fixed, .. } => Ok(BuiltinValue::Bool(fixed as u64 == size)),
            _ => Ok(BuiltinValue::Bool(false)),
        }
    }
}