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

use inkwell::values::{BasicValueEnum, IntValue, VectorValue};

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_entities::BinaryOperation;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{ConstantTypeExtensions, TypeIsExtensions},
};

use crate::{
    abort, codegen,
    context::{CodeGenLocation, LLVMCodeGenContext},
    expressions::native_vector,
    memory, predicates,
    traits::AstLLVMGetType,
    type_cast,
};

#[derive(Debug)]
struct NativeVectorBinaryOperation;

impl NativeVectorBinaryOperation {
    fn compile_operand<'ctx>(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        expression: &'ctx Ast<'_>,
        vector_type: &Type,
        element_type: &Type,
        span: Span,
    ) -> VectorValue<'ctx> {
        let operand_type: Type = expression.get_type_for_llvm().remove_all_constant_type();

        if operand_type.is_native_vector_type() {
            return codegen::compile_as_value(context, expression, None).into_vector_value();
        }

        let scalar_type: &Type = expression.get_type_for_llvm();
        let scalar: BasicValueEnum =
            codegen::compile_as_value(context, expression, Some(element_type));

        let scalar: BasicValueEnum =
            type_cast::try_smart_cast(context, Some(element_type), scalar_type, scalar, span);

        native_vector::expand_scalar_to_vector(context, scalar, vector_type, span)
    }

    fn compile_value_operation<'ctx>(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        left: VectorValue<'ctx>,
        right: VectorValue<'ctx>,
        operator: TokenType,
        element_type: &Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let llvm_builder: &inkwell::builder::Builder<'_> = context.get_llvm_builder();
        let is_float: bool = element_type.is_float_type();
        let is_signed: bool = element_type.is_signed_integer_type();

        if Self::is_comparison(operator) {
            let mask: VectorValue = if is_float {
                llvm_builder
                    .build_float_compare(
                        predicates::get_float_predicate(context, &operator, span),
                        left,
                        right,
                        "",
                    )
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compare native vector elements.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            } else {
                llvm_builder
                    .build_int_compare(
                        predicates::get_integer_predicate(
                            context, &operator, is_signed, is_signed, span,
                        ),
                        left,
                        right,
                        "",
                    )
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compare native vector elements.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
            };

            let all: bool = operator != TokenType::BangEq;

            return native_vector::reduce_bool_vector(context, mask, all, span);
        }

        if Self::is_logical(operator) {
            let mask: VectorValue = if operator == TokenType::And {
                llvm_builder.build_and(left, right, "").unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to combine native bool vector with AND.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
            } else {
                llvm_builder.build_or(left, right, "").unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to combine native bool vector with OR.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
            };

            let all: bool = operator == TokenType::And;

            return native_vector::reduce_bool_vector(context, mask, all, span);
        }

        let result: VectorValue = match operator {
            TokenType::Plus => {
                if is_float {
                    llvm_builder.build_float_add(left, right, "")
                } else {
                    llvm_builder.build_int_add(left, right, "")
                }
            }
            TokenType::Minus => {
                if is_float {
                    llvm_builder.build_float_sub(left, right, "")
                } else {
                    llvm_builder.build_int_sub(left, right, "")
                }
            }
            TokenType::Star => {
                if is_float {
                    llvm_builder.build_float_mul(left, right, "")
                } else {
                    llvm_builder.build_int_mul(left, right, "")
                }
            }
            TokenType::Slash => {
                if is_float {
                    llvm_builder.build_float_div(left, right, "")
                } else if is_signed {
                    llvm_builder.build_int_signed_div(left, right, "")
                } else {
                    llvm_builder.build_int_unsigned_div(left, right, "")
                }
            }
            TokenType::Arith => {
                if is_float {
                    llvm_builder.build_float_rem(left, right, "")
                } else if is_signed {
                    llvm_builder.build_int_signed_rem(left, right, "")
                } else {
                    llvm_builder.build_int_unsigned_rem(left, right, "")
                }
            }
            TokenType::BAnd => llvm_builder.build_and(left, right, ""),
            TokenType::Bor => llvm_builder.build_or(left, right, ""),
            TokenType::Xor => llvm_builder.build_xor(left, right, ""),
            TokenType::LShift => llvm_builder.build_left_shift(left, right, ""),
            TokenType::RShift => llvm_builder.build_right_shift(left, right, is_signed, ""),

            _ => abort::abort_codegen(
                context,
                "Failed to compile a native vector binary operation.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        }
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile a native vector binary instruction.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        result.into()
    }

    fn compile_compound<'ctx>(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        binary: BinaryOperation<'ctx>,
        cast_type: Option<&Type>,
    ) -> BasicValueEnum<'ctx> {
        let source: &Ast = binary.0;
        let operator: TokenType = Self::base_operator(*binary.1);
        let right_expression: &Ast = binary.2;
        let span: Span = binary.3;

        let source_type: Type = source.get_type_for_llvm().remove_all_constant_type();

        let Some((element_type, _)) = source_type.get_native_vector_parts() else {
            abort::abort_codegen(
                context,
                "Expected native vector type for compound assignment.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        };

        context.add_codegen_location(CodeGenLocation::LValue);

        let pointer: BasicValueEnum = codegen::compile_as_ptr_value(context, source, None);

        context.pop_current_codegen_location();

        let pointer = pointer.into_pointer_value();
        let left: BasicValueEnum = memory::load(context, pointer, &source_type, span);
        let left: VectorValue = left.into_vector_value();

        let right: VectorValue =
            Self::compile_operand(context, right_expression, &source_type, &element_type, span);

        let result: BasicValueEnum =
            Self::compile_value_operation(context, left, right, operator, &element_type, span);

        memory::store(context, pointer, result, span);

        type_cast::try_smart_cast(context, cast_type, &source_type, result, span)
    }

    fn compile_constant_operand<'ctx>(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        expression: &'ctx Ast<'_>,
        vector_type: &Type,
        element_type: &Type,
        span: Span,
    ) -> VectorValue<'ctx> {
        let operand_type: Type = expression.get_type_for_llvm().remove_all_constant_type();

        if operand_type.is_native_vector_type() {
            return codegen::compile_constant_as_value(context, expression, vector_type)
                .into_vector_value();
        }

        let scalar: BasicValueEnum =
            codegen::compile_constant_as_value(context, expression, element_type);

        native_vector::expand_scalar_to_vector_constant(context, scalar, vector_type, span)
    }

    fn compile_constant_value_operation<'ctx>(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        left: VectorValue<'ctx>,
        right: VectorValue<'ctx>,
        operator: TokenType,
        element_type: &Type,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let element_count: u32 = left.get_type().get_size();
        let element_llvm_type = left.get_type().get_element_type();
        let bool_type = context.get_llvm_context().bool_type();
        let is_float: bool = element_type.is_float_type();
        let is_bool: bool = element_type.is_bool_type();
        let is_signed: bool = element_type.is_signed_integer_type();

        if Self::is_comparison(operator) {
            let mut result: IntValue =
                bool_type.const_int(u64::from(operator != TokenType::BangEq), false);

            for index in 0..element_count {
                let index: IntValue = context
                    .get_llvm_context()
                    .i32_type()
                    .const_int(u64::from(index), false);

                let left_lane: BasicValueEnum = left.const_extract_element(index);
                let right_lane: BasicValueEnum = right.const_extract_element(index);

                let lane: bool = if is_float {
                    let left_value: f64 = left_lane
                        .into_float_value()
                        .get_constant()
                        .unwrap_or_else(|| {
                            abort::abort_codegen(
                                context,
                                "Expected a constant native vector float lane.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .0;

                    let right_value: f64 = right_lane
                        .into_float_value()
                        .get_constant()
                        .unwrap_or_else(|| {
                            abort::abort_codegen(
                                context,
                                "Expected a constant native vector float lane.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .0;

                    match operator {
                        TokenType::EqEq => left_value == right_value,
                        TokenType::BangEq => left_value != right_value,
                        TokenType::Less => left_value < right_value,
                        TokenType::LessEq => left_value <= right_value,
                        TokenType::Greater => left_value > right_value,
                        TokenType::GreaterEq => left_value >= right_value,
                        _ => abort::abort_codegen(
                            context,
                            "Failed to compare constant native vector float lanes.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    }
                } else if is_bool {
                    let left_value: bool = left_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default()
                        != 0;

                    let right_value: bool = right_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default()
                        != 0;

                    match operator {
                        TokenType::EqEq => left_value == right_value,
                        TokenType::BangEq => left_value != right_value,
                        TokenType::Less => !left_value & right_value,
                        TokenType::LessEq => !left_value || right_value,
                        TokenType::Greater => left_value & !right_value,
                        TokenType::GreaterEq => left_value || !right_value,
                        _ => abort::abort_codegen(
                            context,
                            "Failed to compare constant native vector bool lanes.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    }
                } else if is_signed {
                    let left_value: i64 = left_lane
                        .into_int_value()
                        .get_sign_extended_constant()
                        .unwrap_or_default();

                    let right_value: i64 = right_lane
                        .into_int_value()
                        .get_sign_extended_constant()
                        .unwrap_or_default();

                    match operator {
                        TokenType::EqEq => left_value == right_value,
                        TokenType::BangEq => left_value != right_value,
                        TokenType::Less => left_value < right_value,
                        TokenType::LessEq => left_value <= right_value,
                        TokenType::Greater => left_value > right_value,
                        TokenType::GreaterEq => left_value >= right_value,
                        _ => abort::abort_codegen(
                            context,
                            "Failed to compare constant native vector signed lanes.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    }
                } else {
                    let left_value: u64 = left_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default();

                    let right_value: u64 = right_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default();

                    match operator {
                        TokenType::EqEq => left_value == right_value,
                        TokenType::BangEq => left_value != right_value,
                        TokenType::Less => left_value < right_value,
                        TokenType::LessEq => left_value <= right_value,
                        TokenType::Greater => left_value > right_value,
                        TokenType::GreaterEq => left_value >= right_value,
                        _ => abort::abort_codegen(
                            context,
                            "Failed to compare constant native vector unsigned lanes.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    }
                };

                let lane: IntValue = bool_type.const_int(u64::from(lane), false);

                result = if operator == TokenType::BangEq {
                    result.const_or(lane)
                } else {
                    result.const_and(lane)
                };
            }

            return result.into();
        }

        if Self::is_logical(operator) {
            let mut result: IntValue =
                bool_type.const_int(u64::from(operator == TokenType::And), false);

            for index in 0..element_count {
                let index: IntValue = context
                    .get_llvm_context()
                    .i32_type()
                    .const_int(u64::from(index), false);

                let left_value: bool = left
                    .const_extract_element(index)
                    .into_int_value()
                    .get_zero_extended_constant()
                    .unwrap_or_default()
                    != 0;

                let right_value: bool = right
                    .const_extract_element(index)
                    .into_int_value()
                    .get_zero_extended_constant()
                    .unwrap_or_default()
                    != 0;

                let lane: bool = if operator == TokenType::And {
                    left_value && right_value
                } else {
                    left_value || right_value
                };

                let lane: IntValue = bool_type.const_int(u64::from(lane), false);

                result = if operator == TokenType::And {
                    result.const_and(lane)
                } else {
                    result.const_or(lane)
                };
            }

            return result.into();
        }

        let mut values: Vec<BasicValueEnum> = Vec::with_capacity(element_count as usize);

        for index in 0..element_count {
            let index: IntValue = context
                .get_llvm_context()
                .i32_type()
                .const_int(u64::from(index), false);

            let left_lane: BasicValueEnum = left.const_extract_element(index);
            let right_lane: BasicValueEnum = right.const_extract_element(index);

            let result: BasicValueEnum =
                if is_float {
                    let left_value: f64 = left_lane
                        .into_float_value()
                        .get_constant()
                        .unwrap_or_else(|| {
                            abort::abort_codegen(
                                context,
                                "Expected a constant native vector float lane.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .0;

                    let right_value: f64 = right_lane
                        .into_float_value()
                        .get_constant()
                        .unwrap_or_else(|| {
                            abort::abort_codegen(
                                context,
                                "Expected a constant native vector float lane.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .0;

                    let value: f64 = match operator {
                        TokenType::Plus => left_value + right_value,
                        TokenType::Minus => left_value - right_value,
                        TokenType::Star => left_value * right_value,
                        TokenType::Slash => {
                            if right_value == 0.0 {
                                abort::abort_codegen(
                                    context,
                                    "Native vector constant division by zero.",
                                    span,
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            }

                            left_value / right_value
                        }
                        TokenType::Arith => {
                            if right_value == 0.0 {
                                abort::abort_codegen(
                                    context,
                                    "Native vector constant division by zero.",
                                    span,
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            }

                            left_value % right_value
                        }

                        _ => abort::abort_codegen(
                            context,
                            "Failed to compile a constant native vector float operation.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    };

                    element_llvm_type
                        .into_float_type()
                        .const_float(value)
                        .into()
                } else if is_bool {
                    let left_value: bool = left_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default()
                        != 0;

                    let right_value: bool = right_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default()
                        != 0;

                    let value: bool = match operator {
                        TokenType::BAnd => left_value && right_value,
                        TokenType::Bor => left_value || right_value,
                        TokenType::Xor => left_value != right_value,

                        _ => abort::abort_codegen(
                            context,
                            "Failed to compile a constant native vector bool operation.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        ),
                    };

                    element_llvm_type
                        .into_int_type()
                        .const_int(u64::from(value), false)
                        .into()
                } else if is_signed {
                    let left_value: i64 = left_lane
                        .into_int_value()
                        .get_sign_extended_constant()
                        .unwrap_or_default();

                    let right_value: i64 = right_lane
                        .into_int_value()
                        .get_sign_extended_constant()
                        .unwrap_or_default();

                    let value: i64 =
                        match operator {
                            TokenType::Plus => left_value.wrapping_add(right_value),
                            TokenType::Minus => left_value.wrapping_sub(right_value),
                            TokenType::Star => left_value.wrapping_mul(right_value),
                            TokenType::Slash => {
                                if right_value == 0 {
                                    abort::abort_codegen(
                                        context,
                                        "Native vector constant division by zero.",
                                        span,
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    );
                                }

                                left_value.overflowing_div(right_value).0
                            }
                            TokenType::Arith => {
                                if right_value == 0 {
                                    abort::abort_codegen(
                                        context,
                                        "Native vector constant division by zero.",
                                        span,
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    );
                                }

                                left_value.overflowing_rem(right_value).0
                            }
                            TokenType::BAnd => left_value & right_value,
                            TokenType::Bor => left_value | right_value,
                            TokenType::Xor => left_value ^ right_value,
                            TokenType::LShift => left_value
                                .wrapping_shl(u32::try_from(right_value).unwrap_or_default()),
                            TokenType::RShift => left_value
                                .wrapping_shr(u32::try_from(right_value).unwrap_or_default()),

                            _ => abort::abort_codegen(
                                context,
                                "Failed to compile a constant native vector signed operation.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            ),
                        };

                    element_llvm_type
                        .into_int_type()
                        .const_int(value as u64, true)
                        .into()
                } else {
                    let left_value: u64 = left_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default();

                    let right_value: u64 = right_lane
                        .into_int_value()
                        .get_zero_extended_constant()
                        .unwrap_or_default();

                    let value: u64 =
                        match operator {
                            TokenType::Plus => left_value.wrapping_add(right_value),
                            TokenType::Minus => left_value.wrapping_sub(right_value),
                            TokenType::Star => left_value.wrapping_mul(right_value),
                            TokenType::Slash => {
                                if right_value == 0 {
                                    abort::abort_codegen(
                                        context,
                                        "Native vector constant division by zero.",
                                        span,
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    );
                                }

                                left_value / right_value
                            }
                            TokenType::Arith => {
                                if right_value == 0 {
                                    abort::abort_codegen(
                                        context,
                                        "Native vector constant division by zero.",
                                        span,
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    );
                                }

                                left_value % right_value
                            }
                            TokenType::BAnd => left_value & right_value,
                            TokenType::Bor => left_value | right_value,
                            TokenType::Xor => left_value ^ right_value,
                            TokenType::LShift => left_value
                                .wrapping_shl(u32::try_from(right_value).unwrap_or_default()),
                            TokenType::RShift => left_value
                                .wrapping_shr(u32::try_from(right_value).unwrap_or_default()),

                            _ => abort::abort_codegen(
                                context,
                                "Failed to compile a constant native vector unsigned operation.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            ),
                        };

                    element_llvm_type
                        .into_int_type()
                        .const_int(value, false)
                        .into()
                };

            values.push(result);
        }

        inkwell::types::VectorType::const_vector(&values).into()
    }
}

impl NativeVectorBinaryOperation {
    #[inline]
    fn is_compound(operator: TokenType) -> bool {
        matches!(
            operator,
            TokenType::PlusEq
                | TokenType::MinusEq
                | TokenType::StarEq
                | TokenType::SlashEq
                | TokenType::ArithEq
                | TokenType::BAndEq
                | TokenType::BorEq
                | TokenType::XorEq
                | TokenType::LShiftEq
                | TokenType::RShiftEq
        )
    }

    #[inline]
    fn base_operator(operator: TokenType) -> TokenType {
        match operator {
            TokenType::PlusEq => TokenType::Plus,
            TokenType::MinusEq => TokenType::Minus,
            TokenType::StarEq => TokenType::Star,
            TokenType::SlashEq => TokenType::Slash,
            TokenType::ArithEq => TokenType::Arith,
            TokenType::BAndEq => TokenType::BAnd,
            TokenType::BorEq => TokenType::Bor,
            TokenType::XorEq => TokenType::Xor,
            TokenType::LShiftEq => TokenType::LShift,
            TokenType::RShiftEq => TokenType::RShift,

            _ => operator,
        }
    }

    #[inline]
    fn is_comparison(operator: TokenType) -> bool {
        matches!(
            operator,
            TokenType::EqEq
                | TokenType::BangEq
                | TokenType::Less
                | TokenType::LessEq
                | TokenType::Greater
                | TokenType::GreaterEq
        )
    }

    #[inline]
    fn is_logical(operator: TokenType) -> bool {
        matches!(operator, TokenType::And | TokenType::Or)
    }
}

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let left_type: Type = binary.0.get_type_for_llvm().remove_all_constant_type();
    let right_type: Type = binary.2.get_type_for_llvm().remove_all_constant_type();
    let operator: TokenType = *binary.1;
    let span: Span = binary.3;

    if NativeVectorBinaryOperation::is_compound(operator) {
        return NativeVectorBinaryOperation::compile_compound(context, binary, cast_type);
    }

    let Some(result_type) = left_type.get_native_vector_binary_result_type(&right_type, operator)
    else {
        abort::abort_codegen(
            context,
            "Failed to determine the native vector binary result type.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let vector_type: Type = if left_type.is_native_vector_type() {
        left_type
    } else {
        right_type
    };

    let Some((element_type, _)) = vector_type.get_native_vector_parts() else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector binary operation.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let left: VectorValue = NativeVectorBinaryOperation::compile_operand(
        context,
        binary.0,
        &vector_type,
        &element_type,
        span,
    );

    let right: VectorValue = NativeVectorBinaryOperation::compile_operand(
        context,
        binary.2,
        &vector_type,
        &element_type,
        span,
    );

    let result: BasicValueEnum = NativeVectorBinaryOperation::compile_value_operation(
        context,
        left,
        right,
        operator,
        &element_type,
        span,
    );

    type_cast::try_smart_cast(context, cast_type, &result_type, result, span)
}

pub fn compile_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let left_type: Type = binary.0.get_type_for_llvm().remove_all_constant_type();
    let right_type: Type = binary.2.get_type_for_llvm().remove_all_constant_type();
    let operator: TokenType = *binary.1;
    let span: Span = binary.3;

    let Some(result_type) = left_type.get_native_vector_binary_result_type(&right_type, operator)
    else {
        abort::abort_codegen(
            context,
            "Failed to determine the constant native vector binary result type.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let vector_type: Type = if left_type.is_native_vector_type() {
        left_type
    } else {
        right_type
    };

    let Some((element_type, _)) = vector_type.get_native_vector_parts() else {
        abort::abort_codegen(
            context,
            "Expected native vector type for constant native vector binary operation.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let left: VectorValue = NativeVectorBinaryOperation::compile_constant_operand(
        context,
        binary.0,
        &vector_type,
        &element_type,
        span,
    );

    let right: VectorValue = NativeVectorBinaryOperation::compile_constant_operand(
        context,
        binary.2,
        &vector_type,
        &element_type,
        span,
    );

    let result: BasicValueEnum = NativeVectorBinaryOperation::compile_constant_value_operation(
        context,
        left,
        right,
        operator,
        &element_type,
        span,
    );

    type_cast::try_smart_cast(context, Some(cast_type), &result_type, result, span)
}
