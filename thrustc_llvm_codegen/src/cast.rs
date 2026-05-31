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

#![allow(unnecessary_transmutes)]

use inkwell::builder::Builder;
use inkwell::targets::TargetData;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FloatType;
use inkwell::types::IntType;
use inkwell::types::PointerType;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_ast::Ast;
use thrustc_ast::traits::AstCodeLocation;
use thrustc_typesystem::traits::ConstantTypeExtensions;
use thrustc_typesystem::traits::TypeCodeLocation;
use thrustc_typesystem::traits::TypePointerExtensions;

use crate::abort;
use crate::codegen;
use crate::context::LLVMCodeGenContext;
use crate::traits::AstLLVMGetType;
use crate::typegeneration;

use thrustc_typesystem::traits::TypeIsExtensions;

use thrustc_span::Span;
use thrustc_typesystem::Type;

/* ######################################################################


    INTEGER CAST


########################################################################*/

pub fn compiler_int_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    target_type: &Type,
    from_type: &Type,
    from_value: BasicValueEnum<'ctx>,
    span: Span,
) -> Option<BasicValueEnum<'ctx>> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let from_type: Type = from_type.remove_all_constant_type();
    let target_type: Type = target_type.remove_all_constant_type();

    if !from_type.is_integer_type() || !target_type.is_integer_type() {
        return None;
    }

    if from_type == target_type {
        return None;
    }

    let is_signed: bool =
        target_type.is_signed_integer_type() || from_type.is_signed_integer_type();

    if is_signed {
        let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);

        let int_value: IntValue<'_> = from_value.into_int_value();
        let int_type: IntType<'_> = cast_type.into_int_type();

        let casted_value: IntValue<'_> = llvm_builder
            .build_int_cast_sign_flag(int_value, int_type, true, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to cast integer!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        Some(casted_value.into())
    } else {
        let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);

        let int_value: IntValue<'_> = from_value.into_int_value();
        let int_type: IntType<'_> = cast_type.into_int_type();

        let casted_value: IntValue<'_> = llvm_builder
            .build_int_cast(int_value, int_type, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to cast integer!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        Some(casted_value.into())
    }
}

/* ######################################################################


    FLOAT CAST


########################################################################*/

pub fn compile_float_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    target_type: &Type,
    from_type: &Type,
    from_value: BasicValueEnum<'ctx>,
    span: Span,
) -> Option<BasicValueEnum<'ctx>> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let from_type: Type = from_type.remove_all_constant_type();
    let target_type: Type = target_type.remove_all_constant_type();

    if !from_type.is_float_type() || !target_type.is_float_type() {
        return None;
    }

    if from_type == target_type {
        return None;
    }

    let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);

    let float_value: FloatValue<'_> = from_value.into_float_value();
    let float_type: FloatType<'_> = cast_type.into_float_type();

    let casted_value: FloatValue<'_> = llvm_builder
        .build_float_cast(float_value, float_type, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to cast float!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    Some(casted_value.into())
}

/* ######################################################################


    INTELLIGENT CAST (TRY CAST)


########################################################################*/

#[inline]
pub fn try_smart_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    target_type: Option<&Type>,
    from_type: &Type,
    from_value: BasicValueEnum<'ctx>,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let Some(target_type) = target_type else {
        return from_value;
    };

    let from_type: Type = from_type.remove_all_constant_type();
    let target_type: Type = target_type.remove_all_constant_type();

    if from_value.is_float_value() && target_type.is_float_type() {
        return self::compile_float_cast(context, &target_type, &from_type, from_value, span)
            .unwrap_or(from_value);
    }

    if from_value.is_int_value() && target_type.is_integer_type() {
        return self::compiler_int_cast(context, &target_type, &from_type, from_value, span)
            .unwrap_or(from_value);
    }

    from_value
}

#[inline]
pub fn try_smart_constant_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    target_type: &Type,
    from_type: &Type,
    from_value: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let from_type: Type = from_type.remove_all_constant_type();
    let target_type: Type = target_type.remove_all_constant_type();

    if from_type.is_numeric_type() && target_type.is_numeric_type() {
        return self::compile_constant_numeric_cast(
            context,
            from_value,
            &target_type,
            from_type.is_signed_integer_type(),
        );
    }

    from_value
}

/* ######################################################################

    UNIVERSAL CAST (COMPILE CAST)

########################################################################*/

pub fn compile_type_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let from_type: Type = expr.get_type_for_llvm().remove_all_constant_type();
    let target_type: Type = cast_type.remove_all_constant_type();

    if from_type.is_numeric_type() && target_type.is_numeric_type() {
        let value: BasicValueEnum = codegen::compile_as_value(context, expr, None);
        let cast: BasicTypeEnum = typegeneration::generate_type(context, &target_type);

        if value.is_int_value() && cast.is_int_type() {
            let int_value: IntValue<'_> = value.into_int_value();
            let cast_type: IntType<'_> = cast.into_int_type();

            let casted_value: IntValue<'_> = llvm_builder
                .build_int_cast(int_value, cast_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                });

            return casted_value.into();
        }

        if value.is_float_value() && cast.is_float_type() {
            let float_value: FloatValue<'_> = value.into_float_value();
            let cast_type: FloatType<'_> = cast.into_float_type();

            let casted_value: FloatValue<'_> = llvm_builder
                .build_float_cast(float_value, cast_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                });

            return casted_value.into();
        }

        if value.is_int_value() && cast.is_float_type() {
            let is_signed_int: bool = from_type.is_signed_integer_type();
            let int_value: IntValue<'_> = value.into_int_value();
            let cast_type: FloatType<'_> = cast.into_float_type();

            let casted_value: FloatValue<'_> = if is_signed_int {
                llvm_builder.build_signed_int_to_float(int_value, cast_type, "")
            } else {
                llvm_builder.build_unsigned_int_to_float(int_value, cast_type, "")
            }
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    &format!(
                        "Failed to cast '{}' type to '{}' type.",
                        from_type, target_type
                    ),
                    expr.get_span(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            });

            return casted_value.into();
        }

        if self::is_same_bit_size(context, &from_type, &target_type) {
            let casted_value: BasicValueEnum<'_> = llvm_builder
                .build_bit_cast(value, cast, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                });

            return casted_value;
        }
    }

    if from_type.is_ptr_like_type() && target_type.is_integer_type() {
        let from_value: BasicValueEnum = codegen::compile_as_ptr_value(context, expr, None);

        if from_value.is_pointer_value() {
            let cast_type: BasicTypeEnum = typegeneration::generate_type(context, &target_type);
            let ptr_value: PointerValue<'_> = from_value.into_pointer_value();
            let int_type: IntType<'_> = cast_type.into_int_type();

            let casted: IntValue<'_> = llvm_builder
                .build_ptr_to_int(ptr_value, int_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                });

            return casted.into();
        };
    }

    if from_type.is_integer_type() && target_type.is_ptr_like_type() {
        let value: BasicValueEnum = codegen::compile_as_value(context, expr, None);

        if value.is_int_value() {
            let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);
            let int_value: IntValue<'_> = value.into_int_value();
            let ptr_type: PointerType<'_> = cast_type.into_pointer_type();

            return llvm_builder
                .build_int_to_ptr(int_value, ptr_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                })
                .into();
        }
    }

    if target_type.is_ptr_like_type() {
        let value: BasicValueEnum = codegen::compile_as_ptr_value(context, expr, None);

        if value.is_pointer_value() {
            let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);
            let ptr_value: PointerValue<'_> = value.into_pointer_value();
            let ptr_type: PointerType<'_> = cast_type.into_pointer_type();

            return llvm_builder
                .build_pointer_cast(ptr_value, ptr_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        &format!(
                            "Failed to cast '{}' type to '{}' type.",
                            from_type, target_type
                        ),
                        expr.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                })
                .into();
        };
    }

    abort::abort_codegen(
        context,
        &format!(
            "Failed to cast '{}' type to '{}' type.",
            from_type, target_type
        ),
        expr.get_span(),
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

#[inline]
pub fn compile_constant_type_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let from_type: Type = expr.get_type_for_llvm().remove_all_constant_type();
    let target_type: Type = cast_type.remove_all_constant_type();

    if from_type.is_numeric_type() && target_type.is_numeric_type() {
        let value: BasicValueEnum = codegen::compile_constant_as_value(context, expr, cast_type);

        return self::compile_constant_numeric_cast(
            context,
            value,
            &target_type,
            from_type.is_signed_integer_type(),
        );
    }

    if from_type.is_ptr_like_type() && target_type.is_integer_type() {
        let value: BasicValueEnum =
            codegen::compile_constant_as_ptr_value(context, expr, cast_type);

        if value.is_pointer_value() {
            let cast_type: BasicTypeEnum = typegeneration::generate_type(context, &target_type);

            let ptr_value: PointerValue<'_> = value.into_pointer_value();
            let int_type: IntType<'_> = cast_type.into_int_type();

            let casted_value: IntValue<'_> = ptr_value.const_to_int(int_type);

            return casted_value.into();
        };
    }

    if from_type.is_integer_type() && target_type.is_ptr_like_type() {
        let value: BasicValueEnum = codegen::compile_constant_as_value(context, expr, cast_type);

        if value.is_int_value() {
            let cast_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, &target_type);

            let int_value: IntValue<'_> = value.into_int_value();
            let ptr_type: PointerType<'_> = cast_type.into_pointer_type();

            let casted_value: PointerValue<'_> = int_value.const_to_pointer(ptr_type);

            return casted_value.into();
        }
    }

    if target_type.is_ptr_like_type() {
        let value: BasicValueEnum =
            codegen::compile_constant_as_ptr_value(context, expr, cast_type);

        if value.is_pointer_value() {
            return value;
        };
    }

    abort::abort_codegen(
        context,
        &format!(
            "Failed to cast '{}' type to '{}' type.",
            from_type, target_type
        ),
        expr.get_span(),
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

/* ######################################################################

    NUMERIC CAST (FLOAT & INT)

########################################################################*/

pub fn compile_constant_numeric_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    value: BasicValueEnum<'ctx>,
    target: &Type,
    is_signed: bool,
) -> BasicValueEnum<'ctx> {
    let cast_type: BasicTypeEnum = typegeneration::generate_type(context, target);

    if value.is_int_value() && target.is_integer_type() {
        let int_value: IntValue = value.into_int_value();

        if is_signed {
            if let Some(value) = int_value.get_sign_extended_constant() {
                let new_value: u64 = unsafe { std::mem::transmute::<i64, u64>(value) };
                return cast_type.into_int_type().const_int(new_value, true).into();
            }
        }

        if let Some(value) = int_value.get_zero_extended_constant() {
            return cast_type.into_int_type().const_int(value, false).into();
        }

        abort::abort_codegen(
            context,
            "Failed to extract constant value from a supposed constant value!",
            target.get_span(),
            std::path::PathBuf::from(file!()),
            line!(),
        );
    }

    if value.is_float_value() && target.is_float_type() {
        let float_value: FloatValue = value.into_float_value();

        let (constant_value, ..) = float_value.get_constant().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to extract constant value from a supposed constant value!",
                target.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            );
        });

        return cast_type
            .into_float_type()
            .const_float(constant_value)
            .into();
    }

    if value.is_int_value() && target.is_float_type() {
        let int_value: IntValue<'_> = value.into_int_value();
        let float_type: FloatType<'_> = cast_type.into_float_type();

        if is_signed {
            if let Some(value) = int_value.get_sign_extended_constant() {
                let new_value: f64 = unsafe { std::mem::transmute::<i64, f64>(value) };
                return float_type.const_float(new_value).into();
            }
        }

        if let Some(value) = int_value.get_zero_extended_constant() {
            let new_value: f64 = unsafe { std::mem::transmute::<u64, f64>(value) };
            return float_type.const_float(new_value).into();
        }

        abort::abort_codegen(
            context,
            "Failed to extract constant value from a supposed constant value!",
            target.get_span(),
            std::path::PathBuf::from(file!()),
            line!(),
        );
    }

    if value.is_float_value() && target.is_integer_type() {
        let float_value: FloatValue<'_> = value.into_float_value();
        let int_type: IntType<'_> = cast_type.into_int_type();

        let (constant_value, ..) = float_value.get_constant().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to extract constant value from a supposed constant value!",
                target.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            );
        });

        let new_value: u64 = unsafe { std::mem::transmute::<f64, u64>(constant_value) };

        return int_type.const_int(new_value, is_signed).into();
    }

    abort::abort_codegen(
        context,
        "Failed to cast a constant numeric value!",
        target.get_span(),
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

#[inline]
fn is_same_bit_size(context: &mut LLVMCodeGenContext<'_, '_>, lhs: &Type, rhs: &Type) -> bool {
    let lhs: BasicTypeEnum = typegeneration::generate_type(context, lhs);
    let rhs: BasicTypeEnum = typegeneration::generate_type(context, rhs);

    let target_data: &TargetData = context.get_target_data();

    target_data.get_bit_size(&lhs) == target_data.get_bit_size(&rhs)
}

/* ######################################################################


    INTEGER CAST (TOGETHER)


########################################################################*/

pub fn compile_constant_int_together_cast<'ctx>(
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    signatures: (bool, bool),
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    match lhs
        .get_type()
        .get_bit_width()
        .cmp(&rhs.get_type().get_bit_width())
    {
        std::cmp::Ordering::Greater => {
            if signatures.0 || signatures.1 {
                if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                    let lhs_new_value: u64 = unsafe { std::mem::transmute::<i64, u64>(lhs_number) };

                    return (rhs.get_type().const_int(lhs_new_value, true), rhs);
                } else if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                    let rhs_new_value: u64 = unsafe { std::mem::transmute::<i64, u64>(rhs_number) };

                    return (lhs, lhs.get_type().const_int(rhs_new_value, true));
                }
            }

            if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                return (rhs.get_type().const_int(lhs_number, true), rhs);
            }

            if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                return (lhs, lhs.get_type().const_int(rhs_number, true));
            }

            (rhs.get_type().const_zero(), rhs.get_type().const_zero())
        }
        std::cmp::Ordering::Less => {
            if signatures.0 || signatures.1 {
                if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                    let rhs_new_value: u64 = unsafe { std::mem::transmute::<i64, u64>(rhs_number) };

                    return (lhs, lhs.get_type().const_int(rhs_new_value, true));
                } else if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                    let lhs_new_value: u64 = unsafe { std::mem::transmute::<i64, u64>(lhs_number) };

                    return (rhs.get_type().const_int(lhs_new_value, true), rhs);
                }
            }

            if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                return (lhs, lhs.get_type().const_int(rhs_number, false));
            }

            if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                return (rhs.get_type().const_int(lhs_number, false), rhs);
            }

            (rhs.get_type().const_zero(), rhs.get_type().const_zero())
        }

        _ => (lhs, rhs),
    }
}

pub fn compile_int_together_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    left: IntValue<'ctx>,
    right: IntValue<'ctx>,
    signatures: (bool, bool),
    span: Span,
) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let llvm_builder: &Builder = context.get_llvm_builder();

    match left
        .get_type()
        .get_bit_width()
        .cmp(&right.get_type().get_bit_width())
    {
        std::cmp::Ordering::Greater => {
            let new_right_value: IntValue = if signatures.0 || signatures.1 {
                llvm_builder
                    .build_int_cast_sign_flag(right, left.get_type(), true, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to cast the integer together!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            } else {
                llvm_builder
                    .build_int_cast(right, left.get_type(), "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to cast the integer together!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            };

            (left, new_right_value)
        }
        std::cmp::Ordering::Less => {
            let new_left_value: IntValue = if signatures.0 || signatures.1 {
                llvm_builder
                    .build_int_cast_sign_flag(left, right.get_type(), true, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to cast the integer together!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            } else {
                llvm_builder
                    .build_int_cast(left, right.get_type(), "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to cast the integer together!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            };

            (new_left_value, right)
        }
        _ => (left, right),
    }
}

/* ######################################################################


    FLOAT CAST (TOGETHER)


########################################################################*/

pub fn compile_constant_float_together_cast<'ctx>(
    left: FloatValue<'ctx>,
    right: FloatValue<'ctx>,
) -> (FloatValue<'ctx>, FloatValue<'ctx>) {
    let left_type: FloatType = left.get_type();
    let right_type: FloatType = right.get_type();

    if left_type == right_type {
        return (left, right);
    }

    let new_left_value: FloatValue = if left_type != right_type {
        if let Some((value, ..)) = left.get_constant() {
            right.get_type().const_float(value)
        } else {
            left
        }
    } else {
        left
    };

    let new_right_value: FloatValue = if right_type != left_type {
        if let Some((value, ..)) = right.get_constant() {
            left.get_type().const_float(value)
        } else {
            right
        }
    } else {
        right
    };

    (new_left_value, new_right_value)
}

pub fn compile_float_together_cast<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    left: FloatValue<'ctx>,
    right: FloatValue<'ctx>,
    span: Span,
) -> (FloatValue<'ctx>, FloatValue<'ctx>) {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let left_type: FloatType = left.get_type();
    let right_type: FloatType = right.get_type();

    if left_type == right_type {
        return (left, right);
    }

    let new_left_value: FloatValue = if left_type != right_type {
        llvm_builder
            .build_float_cast(left, right_type, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to cast floats together!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
    } else {
        left
    };

    let new_right_value: FloatValue = if right_type != left_type {
        llvm_builder
            .build_float_cast(right, left_type, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to cast floats together!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
    } else {
        right
    };

    (new_left_value, new_right_value)
}

pub fn select_ssa_integer_type<'a>(cast_type: Option<&'a Type>, integer_ty: &'a Type) -> &'a Type {
    let ssa_integer_type: &Type = integer_ty;

    let Some(cast_type) = cast_type else {
        return ssa_integer_type;
    };

    if cast_type.is_integer_type() {
        return cast_type;
    }

    ssa_integer_type
}

pub fn select_ssa_float_type<'a>(cast_type: Option<&'a Type>, float_ty: &'a Type) -> &'a Type {
    let ssa_float_type: &Type = float_ty;

    let Some(cast_type) = cast_type else {
        return ssa_float_type;
    };

    if cast_type.is_float_type() {
        return cast_type;
    }

    ssa_float_type
}
