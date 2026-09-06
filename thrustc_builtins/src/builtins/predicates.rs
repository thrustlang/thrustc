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
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use thrustc_compile_time::{BuiltinArgument, BuiltinValue};

#[derive(Debug)]
pub struct IsConst;

impl CompileTimeBuiltinFunction for IsConst {
    #[inline]
    fn name(&self) -> &'static str {
        "isConst"
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
                    "The 'isConst' compiler builtin expects a type argument.".into(),
                    "You should pass a type to 'isConst', like isConst(const u32).".into(),
                    None,
                    *span,
                ));
            }
        };

        Ok(BuiltinValue::Bool(ty.is_const_type()))
    }
}

macro_rules! define_predicate_builtin {
    ($struct_name:ident, $builtin_name:literal, $check:expr) => {
        #[derive(Debug)]
        pub struct $struct_name;

        impl CompileTimeBuiltinFunction for $struct_name {
            #[inline]
            fn name(&self) -> &'static str {
                $builtin_name
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
                            format!(
                                "The '{}' compiler builtin expects a type argument.",
                                $builtin_name
                            ),
                            format!(
                                "You should pass a type to '{}', like {}(u32).",
                                $builtin_name, $builtin_name
                            ),
                            None,
                            *span,
                        ));
                    }
                };

                let stripped: Type = ty.remove_all_constant_type();

                Ok(BuiltinValue::Bool($check(&stripped)))
            }
        }
    };
}

define_predicate_builtin!(IsSigned, "isSigned", |ty: &Type| {
    ty.is_signed_integer_type()
});
define_predicate_builtin!(IsUnsigned, "isUnsigned", |ty: &Type| {
    ty.is_unsigned_integer_type()
});
define_predicate_builtin!(IsInteger, "isInteger", |ty: &Type| { ty.is_integer_type() });
define_predicate_builtin!(IsFloat, "isFloat", |ty: &Type| ty.is_float_type());
define_predicate_builtin!(IsBool, "isBool", |ty: &Type| ty.is_bool_type());
define_predicate_builtin!(IsChar, "isChar", |ty: &Type| ty.is_char_type());
define_predicate_builtin!(IsPointer, "isPointer", |ty: &Type| ty.is_ptr_type());
define_predicate_builtin!(IsArray, "isArray", |ty: &Type| ty.is_array_type());
define_predicate_builtin!(IsFixedArray, "isFixedArray", |ty: &Type| {
    ty.is_fixed_array_type()
});
define_predicate_builtin!(IsStruct, "isStruct", |ty: &Type| ty.is_struct_type());
define_predicate_builtin!(IsVoid, "isVoid", |ty: &Type| ty.is_void_type());
define_predicate_builtin!(IsNumeric, "isNumeric", |ty: &Type| { ty.is_numeric_type() });
define_predicate_builtin!(IsFunction, "isFunction", |ty: &Type| {
    ty.is_function_reference_type()
});
