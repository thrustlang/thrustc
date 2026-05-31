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
use crate::cast;
use crate::codegen;
use crate::context::LLVMCodeGenContext;
use crate::predicates;
use crate::traits::AstLLVMGetType;

use thrustc_entities::BinaryOperation;
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

fn compile_bool_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    operator: &TokenType,
    signatures: (bool, bool),
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    let (lhs_signed, rhs_signed) = signatures;

    if lhs.is_int_value() && rhs.is_int_value() {
        let (lhs, rhs) = cast::compile_int_together_cast(
            context,
            lhs.into_int_value(),
            rhs.into_int_value(),
            signatures,
            span,
        );

        return match operator {
            operator if operator.is_logical_operator() => llvm_builder
                .build_int_compare(
                    predicates::get_integer_predicate(
                        context, operator, lhs_signed, rhs_signed, span,
                    ),
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
                    );
                })
                .into(),
            operator if operator.is_logical_gate() => match operator {
                TokenType::And => llvm_builder
                    .build_and(lhs, rhs, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile '&&' operation!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .into(),
                TokenType::Or => llvm_builder
                    .build_or(lhs, rhs, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile '||' operation!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .into(),
                _ => abort::abort_codegen(
                    context,
                    "Failed to compile without a valid operator!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            },
            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    } else if lhs.is_float_value() && rhs.is_float_value() {
        let (lhs, rhs) = cast::compile_float_together_cast(
            context,
            lhs.into_float_value(),
            rhs.into_float_value(),
            span,
        );

        return match operator {
            operator if operator.is_logical_operator() => llvm_builder
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
                    );
                })
                .into(),

            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    } else if lhs.is_pointer_value() && rhs.is_pointer_value() {
        let lhs: PointerValue<'_> = lhs.into_pointer_value();
        let rhs: PointerValue<'_> = rhs.into_pointer_value();

        return match operator {
            op if op.is_logical_operator() => llvm_builder
                .build_int_compare(
                    predicates::get_pointer_predicate(context, operator, span),
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
                    );
                })
                .into(),
            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant boolean binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let span: Span = binary.3;

    if let (
        _,
        TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq
        | TokenType::And
        | TokenType::Or,
        ..,
    ) = binary
    {
        let operator: &'ctx TokenType = binary.1;

        let lhs: BasicValueEnum<'_> = codegen::compile_as_value(context, binary.0, cast_type);
        let rhs: BasicValueEnum<'_> = codegen::compile_as_value(context, binary.2, cast_type);

        let lhs_type: &Type = binary.0.get_type_for_llvm();
        let rhs_type: &Type = binary.2.get_type_for_llvm();

        return compile_bool_operation(
            context,
            lhs,
            rhs,
            operator,
            (
                lhs_type.is_signed_integer_type(),
                rhs_type.is_signed_integer_type(),
            ),
            span,
        );
    }

    abort::abort_codegen(
        context,
        "Failed to compile boolean binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

fn compile_constant_boolean_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    operator: &TokenType,
    signatures: (bool, bool),
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let (lhs_signed, rhs_signed) = signatures;

    if lhs.is_int_value() && rhs.is_int_value() {
        let (lhs, rhs) = cast::compile_constant_int_together_cast(
            lhs.into_int_value(),
            rhs.into_int_value(),
            signatures,
        );

        return match operator {
            op if op.is_logical_operator() => lhs
                .const_int_compare(
                    predicates::get_integer_predicate(
                        context, operator, lhs_signed, rhs_signed, span,
                    ),
                    rhs,
                )
                .into(),
            op if op.is_logical_gate() => match op {
                TokenType::And => {
                    if signatures.0 || signatures.1 {
                        if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                            if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                                return llvm_context
                                    .bool_type()
                                    .const_int(
                                        ((lhs_number != 0) && (rhs_number != 0)) as u64,
                                        false,
                                    )
                                    .into();
                            }
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return llvm_context
                                .bool_type()
                                .const_int(((lhs_number != 0) && (rhs_number != 0)) as u64, false)
                                .into();
                        }
                    }

                    return lhs.get_type().const_zero().into();
                }
                TokenType::Or => {
                    if signatures.0 || signatures.1 {
                        if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                            if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                                return llvm_context
                                    .bool_type()
                                    .const_int(
                                        ((lhs_number != 0) || (rhs_number != 0)) as u64,
                                        false,
                                    )
                                    .into();
                            }
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return llvm_context
                                .bool_type()
                                .const_int(((lhs_number != 0) || (rhs_number != 0)) as u64, false)
                                .into();
                        }
                    }

                    return lhs.get_type().const_zero().into();
                }

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile without a valid logical operator!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            },
            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    } else if lhs.is_float_value() && rhs.is_float_value() {
        let (lhs, rhs) = cast::compile_constant_float_together_cast(
            lhs.into_float_value(),
            rhs.into_float_value(),
        );

        return match operator {
            op if op.is_logical_operator() => lhs
                .const_compare(
                    predicates::get_float_predicate(context, operator, span),
                    rhs,
                )
                .into(),

            TokenType::And => {
                if signatures.0 || signatures.1 {
                    if let Some((lhs_number, _)) = lhs.get_constant() {
                        if let Some((rhs_number, _)) = rhs.get_constant() {
                            return llvm_context
                                .bool_type()
                                .const_int(
                                    ((lhs_number != 0.0) && (rhs_number != 0.0)) as u64,
                                    false,
                                )
                                .into();
                        }
                    }
                }

                if let Some((lhs_number, _)) = lhs.get_constant() {
                    if let Some((rhs_number, _)) = rhs.get_constant() {
                        return llvm_context
                            .bool_type()
                            .const_int(((lhs_number != 0.0) && (rhs_number != 0.0)) as u64, false)
                            .into();
                    }
                }

                return lhs.get_type().const_zero().into();
            }
            TokenType::Or => {
                if signatures.0 || signatures.1 {
                    if let Some((lhs_number, _)) = lhs.get_constant() {
                        if let Some((rhs_number, _)) = rhs.get_constant() {
                            return llvm_context
                                .bool_type()
                                .const_int(
                                    ((lhs_number != 0.0) || (rhs_number != 0.0)) as u64,
                                    false,
                                )
                                .into();
                        }
                    }
                }

                if let Some((lhs_number, _)) = lhs.get_constant() {
                    if let Some((rhs_number, _)) = rhs.get_constant() {
                        return llvm_context
                            .bool_type()
                            .const_int(((lhs_number != 0.0) || (rhs_number != 0.0)) as u64, false)
                            .into();
                    }
                }

                return lhs.get_type().const_zero().into();
            }

            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    } else if lhs.is_pointer_value() && rhs.is_pointer_value() {
        let lhs = lhs.into_pointer_value();
        let rhs = rhs.into_pointer_value();

        return match operator {
            op if op.is_logical_operator() => match op {
                TokenType::EqEq => llvm_context
                    .bool_type()
                    .const_int((lhs.is_null() == rhs.is_null()) as u64, false)
                    .into(),

                TokenType::BangEq => llvm_context
                    .bool_type()
                    .const_int((lhs.is_null() != rhs.is_null()) as u64, false)
                    .into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile a valid logical operator!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            },
            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant boolean binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

pub fn compile_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    binary: BinaryOperation<'ctx>,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let span: Span = binary.3;

    if let (
        _,
        TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq
        | TokenType::And
        | TokenType::Or,
        ..,
    ) = binary
    {
        let operator: &'ctx TokenType = binary.1;

        let lhs: BasicValueEnum<'_> =
            codegen::compile_constant_as_value(context, binary.0, cast_type);
        let rhs: BasicValueEnum<'_> =
            codegen::compile_constant_as_value(context, binary.2, cast_type);

        let lhs_type: &Type = binary.0.get_type_for_llvm();
        let rhs_type: &Type = binary.2.get_type_for_llvm();

        return compile_constant_boolean_operation(
            context,
            lhs,
            rhs,
            operator,
            (
                lhs_type.is_signed_integer_type(),
                rhs_type.is_signed_integer_type(),
            ),
            span,
        );
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant boolean binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}
