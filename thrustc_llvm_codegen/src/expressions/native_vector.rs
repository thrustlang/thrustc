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

use inkwell::types::{BasicTypeEnum, IntType, VectorType};
use inkwell::values::{BasicValueEnum, IntValue, VectorValue};

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_typesystem::{Type, traits::ConstantTypeExtensions};

use crate::context::LLVMCodeGenContext;
use crate::traits::AstLLVMGetType;
use crate::{abort, codegen, type_cast, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    vector_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let vector_type: &Type = cast_type.unwrap_or(vector_type);

    let Type::NativeVector { element_type, .. } = vector_type else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector literal.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, vector_type);
    let mut vector: VectorValue = llvm_type.const_zero().into_vector_value();

    for (index, item) in items.iter().enumerate() {
        let value: BasicValueEnum = codegen::compile_as_value(context, item, Some(element_type));

        let value_type: &Type = item.get_type_for_llvm();
        let value: BasicValueEnum =
            type_cast::try_smart_cast(context, Some(element_type), value_type, value, span);

        let index: u64 = u64::try_from(index).unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to parse the native vector index.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let index: IntValue = context
            .get_llvm_context()
            .i32_type()
            .const_int(index, false);

        vector = context
            .get_llvm_builder()
            .build_insert_element(vector, value, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to insert native vector element.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    vector.into()
}

pub fn compile_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    vector_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let Type::NativeVector {
        element_type,
        element_count,
        ..
    } = vector_type.remove_all_constant_type()
    else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector constant.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let mut values: Vec<BasicValueEnum> = Vec::with_capacity(element_count as usize);

    for item in items.iter() {
        let value: BasicValueEnum =
            codegen::compile_constant_as_value(context, item, &element_type);

        values.push(value);
    }

    VectorType::const_vector(&values).into()
}

/*
 * LLVM calls this operation a splat: the scalar value is replicated into
 * every lane of the vector. LLVM has no dedicated splat instruction, so it
 * is commonly built by inserting the scalar into an undef vector or by
 * using shufflevector. For example, splatting 7 into <4 x i32> produces
 * <7, 7, 7, 7>.
 * Lane = item
 */
pub fn expand_scalar_to_vector<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    value: BasicValueEnum<'ctx>,
    vector_type: &Type,
    span: Span,
) -> VectorValue<'ctx> {
    let Some((_, element_count)) = vector_type.get_native_vector_parts() else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector scalar expansion.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, vector_type);
    let mut vector: VectorValue = llvm_type.into_vector_type().get_undef();

    for index in 0..element_count {
        let index: IntValue = context
            .get_llvm_context()
            .i32_type()
            .const_int(u64::from(index), false);

        vector = context
            .get_llvm_builder()
            .build_insert_element(vector, value, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to expand native vector scalar.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    vector
}

pub fn expand_scalar_to_vector_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    value: BasicValueEnum<'ctx>,
    vector_type: &Type,
    span: Span,
) -> VectorValue<'ctx> {
    let Some((_, element_count)) = vector_type.get_native_vector_parts() else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector constant scalar expansion.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let mut values: Vec<BasicValueEnum> = Vec::with_capacity(element_count as usize);

    for _ in 0..element_count {
        values.push(value);
    }

    VectorType::const_vector(&values)
}

/*
 * Reduces a boolean vector mask to a scalar bool. With `all == true` every
 * lane must be true (AND reduction); with `all == false` at least one lane
 * must be true (OR reduction). A lane is one element of the vector: for
 * <4 x i1>, lane 0 is the first boolean element.
 */
pub fn reduce_bool_vector<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    vector: VectorValue<'ctx>,
    all: bool,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let bool_type: IntType<'_> = context.get_llvm_context().bool_type();
    let mut result: IntValue = bool_type.const_int(u64::from(all), false);

    for index in 0..vector.get_type().get_size() {
        let index: IntValue = context
            .get_llvm_context()
            .i32_type()
            .const_int(u64::from(index), false);

        let lane: BasicValueEnum = context
            .get_llvm_builder()
            .build_extract_element(vector, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to extract native vector comparison lane.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let lane: IntValue = lane.into_int_value();

        result = if all {
            context
                .get_llvm_builder()
                .build_and(result, lane, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to reduce native vector comparison with AND.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
        } else {
            context
                .get_llvm_builder()
                .build_or(result, lane, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to reduce native vector comparison with OR.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
        };
    }

    result.into()
}

/*
 * Constant counterpart of `reduce_bool_vector`. It extracts each lane (each
 * vector element) as a constant, combines it with AND when `all == true` or
 * with OR when `all == false`, and returns the resulting scalar bool.
 */
pub fn reduce_bool_vector_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    vector: VectorValue<'ctx>,
    all: bool,
) -> BasicValueEnum<'ctx> {
    let bool_type: IntType<'_> = context.get_llvm_context().bool_type();
    let mut result: IntValue = bool_type.const_int(u64::from(all), false);

    for index in 0..vector.get_type().get_size() {
        let index: IntValue = context
            .get_llvm_context()
            .i32_type()
            .const_int(u64::from(index), false);

        let lane: IntValue = vector.const_extract_element(index).into_int_value();

        result = if all {
            result.const_and(lane)
        } else {
            result.const_or(lane)
        };
    }

    result.into()
}
