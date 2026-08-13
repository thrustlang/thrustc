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

use crate::abort;
use crate::codegen;
use crate::context::CodeGenLocation;
use crate::context::LLVMCodeGenContext;
use crate::memory;
use crate::predicates;
use crate::type_cast;

use inkwell::values::PointerValue;
use thrustc_ast::Ast;
use thrustc_ast::traits::AstMemoryExtensions;
use thrustc_entities::BinaryOperation;
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::Type;

use inkwell::builder::Builder;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;

pub fn compile_float_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: &'ctx Ast,
    rhs: &'ctx Ast,
    cast: Option<&Type>,
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    match operator {
        TokenType::PlusEq => {
            if lhs.is_memory_assigned_reference() {
                context.add_codegen_location(CodeGenLocation::LValue);
                let reference: BasicValueEnum<'_> =
                    codegen::compile_as_ptr_value(context, lhs, cast);
                context.pop_current_codegen_location();

                if reference.is_pointer_value() {
                    let ptr: PointerValue<'_> = reference.into_pointer_value();

                    let old_value: FloatValue<'_> =
                        codegen::compile_as_value(context, lhs, cast).into_float_value();
                    let value: FloatValue<'_> =
                        codegen::compile_as_value(context, rhs, cast).into_float_value();

                    let new_value: BasicValueEnum<'_> = llvm_builder
                        .build_float_add(old_value, value, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile '+' operation!",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        })
                        .into();

                    memory::store(context, ptr, new_value, span);

                    new_value
                } else {
                    abort::abort_codegen(
                        context,
                        "Failed to compile '+=' operation!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            } else {
                let lhs: BasicValueEnum = codegen::compile_as_value(context, lhs, cast);
                let rhs: BasicValueEnum = codegen::compile_as_value(context, rhs, cast);

                let old_value: FloatValue<'_> = lhs.into_float_value();
                let value: FloatValue<'_> = rhs.into_float_value();

                llvm_builder
                    .build_float_add(old_value, value, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile '+' operation!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .into()
            }
        }

        TokenType::MinusEq => {
            if lhs.is_memory_assigned_reference() {
                context.add_codegen_location(CodeGenLocation::LValue);
                let reference: BasicValueEnum<'_> =
                    codegen::compile_as_ptr_value(context, lhs, cast);
                context.pop_current_codegen_location();

                if reference.is_pointer_value() {
                    let ptr: PointerValue<'_> = reference.into_pointer_value();

                    let old_value: FloatValue<'_> =
                        codegen::compile_as_value(context, lhs, cast).into_float_value();
                    let value: FloatValue<'_> =
                        codegen::compile_as_value(context, rhs, cast).into_float_value();

                    let new_value: BasicValueEnum<'_> = llvm_builder
                        .build_float_sub(old_value, value, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile '-' operation!",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        })
                        .into();

                    memory::store(context, ptr, new_value, span);

                    new_value
                } else {
                    abort::abort_codegen(
                        context,
                        "Failed to compile '-=' operation!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            } else {
                let lhs: BasicValueEnum = codegen::compile_as_value(context, lhs, cast);
                let rhs: BasicValueEnum = codegen::compile_as_value(context, rhs, cast);

                let old_value: FloatValue<'_> = lhs.into_float_value();
                let value: FloatValue<'_> = rhs.into_float_value();

                llvm_builder
                    .build_float_sub(old_value, value, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile '-' operation!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .into()
            }
        }

        _ => {
            if let TokenType::Plus
            | TokenType::Slash
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Arith
            | TokenType::BangEq
            | TokenType::EqEq
            | TokenType::LessEq
            | TokenType::Less
            | TokenType::Greater
            | TokenType::GreaterEq = operator
            {
                let lhs: BasicValueEnum = codegen::compile_as_value(context, lhs, cast);
                let rhs: BasicValueEnum = codegen::compile_as_value(context, rhs, cast);

                return compile_float_value_operation(
                    context,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    operator,
                    span,
                );
            }

            abort::abort_codegen(
                context,
                "Failed to compile float binary operation!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }
    }
}

pub fn compile_float_value_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let (lhs, rhs) = type_cast::compile_float_together_cast(context, lhs, rhs, span);

    match operator {
        TokenType::Plus => llvm_builder
            .build_float_add(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '+' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),
        TokenType::Minus => llvm_builder
            .build_float_sub(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '-' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),
        TokenType::Star => llvm_builder
            .build_float_mul(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '*' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),
        TokenType::Slash => llvm_builder
            .build_float_div(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '/' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),

        TokenType::Arith => llvm_builder
            .build_float_rem(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '%' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),

        op if op.is_logical_operator() => llvm_builder
            .build_float_compare(
                predicates::get_float_predicate(context, operator, span),
                lhs,
                rhs,
                "",
            )
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile comparison!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .into(),

        _ => abort::abort_codegen(
            context,
            "Failed to compile without a valid operator!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let span: Span = binary.3;

    if let (
        _,
        TokenType::Plus
        | TokenType::Slash
        | TokenType::Minus
        | TokenType::Star
        | TokenType::Arith
        | TokenType::PlusEq
        | TokenType::MinusEq
        | TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq,
        ..,
    ) = binary
    {
        let operator: &TokenType = binary.1;

        let lhs: &Ast<'_> = binary.0;
        let rhs: &Ast<'_> = binary.2;

        return compile_float_operation(context, lhs, rhs, cast, operator, span);
    }

    abort::abort_codegen(
        context,
        "Failed to compile float binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

#[inline]
fn compile_constant_float_value_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: FloatValue<'ctx>,
    rhs: FloatValue<'ctx>,
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let (lhs, rhs) = type_cast::compile_constant_float_together_cast(lhs, rhs);

    match operator {
        TokenType::Plus | TokenType::PlusEq => {
            if let Some(lhs_constant) = lhs.get_constant() {
                if let Some(rhs_constant) = rhs.get_constant() {
                    let lhs_number: f64 = lhs_constant.0;
                    let rhs_number: f64 = rhs_constant.0;

                    return lhs.get_type().const_float(lhs_number + rhs_number).into();
                }
            }

            lhs.get_type().const_zero().into()
        }

        TokenType::Minus | TokenType::MinusEq => {
            if let Some(lhs_constant) = lhs.get_constant() {
                if let Some(rhs_constant) = rhs.get_constant() {
                    let lhs_number: f64 = lhs_constant.0;
                    let rhs_number: f64 = rhs_constant.0;

                    return lhs.get_type().const_float(lhs_number - rhs_number).into();
                }
            }

            lhs.get_type().const_zero().into()
        }

        TokenType::Star => {
            if let Some(lhs_constant) = lhs.get_constant() {
                if let Some(rhs_constant) = rhs.get_constant() {
                    let lhs_number: f64 = lhs_constant.0;
                    let rhs_number: f64 = rhs_constant.0;

                    return lhs.get_type().const_float(lhs_number * rhs_number).into();
                }
            }

            lhs.get_type().const_zero().into()
        }

        TokenType::Slash => {
            if let Some(lhs_constant) = lhs.get_constant() {
                if let Some(rhs_constant) = rhs.get_constant() {
                    let lhs_number: f64 = lhs_constant.0;
                    let rhs_number: f64 = rhs_constant.0;

                    return lhs.get_type().const_float(lhs_number / rhs_number).into();
                }
            }

            lhs.get_type().const_zero().into()
        }

        TokenType::Arith => {
            if let Some(lhs_constant) = lhs.get_constant() {
                if let Some(rhs_constant) = rhs.get_constant() {
                    let lhs_number: f64 = lhs_constant.0;
                    let rhs_number: f64 = rhs_constant.0;

                    return lhs.get_type().const_float(lhs_number % rhs_number).into();
                }
            }

            lhs.get_type().const_zero().into()
        }

        op if op.is_logical_operator() => lhs
            .const_compare(
                predicates::get_float_predicate(context, operator, span),
                rhs,
            )
            .into(),

        _ => {
            abort::abort_codegen(
                context,
                "Failed to compile constant float binary operation!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }
    }
}

pub fn compile_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast: &Type,
) -> BasicValueEnum<'ctx> {
    let span: Span = binary.3;

    if let (
        _,
        TokenType::Plus
        | TokenType::Slash
        | TokenType::Minus
        | TokenType::Star
        | TokenType::PlusEq
        | TokenType::MinusEq
        | TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq,
        ..,
    ) = binary
    {
        let operator: &TokenType = binary.1;

        let lhs: BasicValueEnum = codegen::compile_constant_as_value(context, binary.0, cast);
        let rhs: BasicValueEnum = codegen::compile_constant_as_value(context, binary.2, cast);

        return compile_constant_float_value_operation(
            context,
            lhs.into_float_value(),
            rhs.into_float_value(),
            operator,
            span,
        );
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant float binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}
