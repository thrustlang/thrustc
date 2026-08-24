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
#![allow(clippy::incompatible_msrv)]

use crate::abort;
use crate::atomic_operations::LLVMAtomicModificators;
use crate::codegen;
use crate::context::CodeGenLocation;
use crate::context::LLVMCodeGenContext;
use crate::memory;
use crate::predicates;
use crate::traits::AstLLVMGetType;
use crate::type_cast;
use crate::typegeneration;

use thrustc_ast::Ast;
use thrustc_ast::traits::AstMemoryExtensions;
use thrustc_backends::llvm::LLVMBackend;
use thrustc_code_location::Span;
use thrustc_entities::BinaryOperation;
use thrustc_options::CompilerOptions;
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::Type;

use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_typesystem::traits::TypeExtensions;
use thrustc_typesystem::traits::TypeIsExtensions;

fn compile_int_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: &'ctx Ast,
    rhs: &'ctx Ast,
    cast_type: Option<&Type>,
    signatures: (bool, bool, &Type, &Type),
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    match operator {
        TokenType::PlusEq
        | TokenType::MinusEq
        | TokenType::StarEq
        | TokenType::SlashEq
        | TokenType::ArithEq
        | TokenType::BAndEq
        | TokenType::BorEq
        | TokenType::XorEq
        | TokenType::LShiftEq
        | TokenType::RShiftEq => self::compile_compound_int_operation(
            context, lhs, rhs, cast_type, signatures, operator, span,
        ),

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
            | TokenType::GreaterEq
            | TokenType::LShift
            | TokenType::RShift
            | TokenType::And
            | TokenType::Or
            | TokenType::Xor
            | TokenType::Bor
            | TokenType::BAnd = operator
            {
                let lhs: BasicValueEnum = codegen::compile_as_value(context, lhs, cast_type);
                let rhs: BasicValueEnum = codegen::compile_as_value(context, rhs, cast_type);

                return self::compile_int_value_operation(
                    context, lhs, rhs, signatures, operator, span,
                );
            }

            abort::abort_codegen(
                context,
                "Failed to compile integer binary operation!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }
    }
}

fn compile_compound_int_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: &'ctx Ast,
    rhs: &'ctx Ast,
    cast_type: Option<&Type>,
    signatures: (bool, bool, &Type, &Type),
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    if lhs.is_memory_assigned_reference() {
        context.add_codegen_location(CodeGenLocation::LValue);

        let reference: BasicValueEnum<'_> = codegen::compile_as_ptr_value(context, lhs, cast_type);

        context.pop_current_codegen_location();

        let symbol: memory::SymbolAllocated<'_> = if let Ast::Reference { name, .. } = lhs {
            context.get_table().get_symbol(name)
        } else {
            abort::abort_codegen(
                context,
                "Failed to compile the compound operation!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        };

        let atomic_config: Option<LLVMAtomicModificators> =
            symbol.determinate_atomic_configuration();

        if let Some(config) = atomic_config {
            context.push_atomic_modificators(config);
        }

        if reference.is_pointer_value() {
            let ptr: PointerValue<'_> = reference.into_pointer_value();

            let old_value: IntValue<'_> =
                codegen::compile_as_value(context, lhs, cast_type).into_int_value();
            let value: IntValue<'_> =
                codegen::compile_as_value(context, rhs, cast_type).into_int_value();

            let (old_value, value) = type_cast::compile_int_together_cast(
                context,
                old_value,
                value,
                (signatures.0, signatures.1),
                span,
            );

            let new_value: BasicValueEnum<'_> = self::compiler_int_binary_instruction(
                context,
                old_value,
                value,
                (signatures.0, signatures.1),
                operator,
                span,
            );

            memory::store(context, ptr, new_value, span);

            if atomic_config.is_some() {
                context.pop_atomic_modificators();
            }

            new_value
        } else {
            abort::abort_codegen(
                context,
                "Failed to compile the compound operation!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    } else {
        let lhs: BasicValueEnum = codegen::compile_as_value(context, lhs, cast_type);
        let rhs: BasicValueEnum = codegen::compile_as_value(context, rhs, cast_type);

        self::compile_int_value_operation(context, lhs, rhs, signatures, operator, span)
    }
}

fn compile_int_value_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    signatures: (bool, bool, &Type, &Type),
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    if lhs.is_int_value() && rhs.is_int_value() {
        let lhs: IntValue = lhs.into_int_value();
        let rhs: IntValue = rhs.into_int_value();

        let signatures: (bool, bool) = (signatures.0, signatures.1);

        let (lhs, rhs) = type_cast::compile_int_together_cast(context, lhs, rhs, signatures, span);

        return self::compiler_int_binary_instruction(
            context, lhs, rhs, signatures, operator, span,
        );
    }

    if lhs.is_pointer_value() && rhs.is_pointer_value() {
        let lhs: PointerValue = lhs.into_pointer_value();
        let rhs: PointerValue = rhs.into_pointer_value();

        match operator {
            TokenType::Minus => {
                let lhs_type: &Type = signatures.2;
                let subtype: &Type = lhs_type.get_type_with_depth(1);

                let pointee_ty: BasicTypeEnum = typegeneration::generate_type(context, subtype);

                return llvm_builder
                    .build_ptr_diff(pointee_ty, lhs, rhs, "")
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
            }
            _ => abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        }
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant integer binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

fn compiler_int_binary_instruction<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    signatures: (bool, bool),
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let options: &CompilerOptions = context.get_compiler_options();
    let llvm_backend: &LLVMBackend = options.get_llvm_backend();

    match operator {
        TokenType::Plus | TokenType::PlusEq
            if (signatures.0 || signatures.1) && !llvm_backend.has_disable_safe_math() =>
        {
            llvm_builder
                .build_int_nsw_add(lhs, rhs, "")
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
        TokenType::Plus | TokenType::PlusEq if !llvm_backend.has_disable_safe_math() => {
            llvm_builder
                .build_int_nuw_add(lhs, rhs, "")
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
        TokenType::Plus | TokenType::PlusEq if llvm_backend.has_disable_safe_math() => llvm_builder
            .build_int_add(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '+' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Minus | TokenType::MinusEq
            if (signatures.0 || signatures.1) && !llvm_backend.has_disable_safe_math() =>
        {
            llvm_builder
                .build_int_nsw_sub(lhs, rhs, "")
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
        TokenType::Minus | TokenType::MinusEq if !llvm_backend.has_disable_safe_math() => {
            llvm_builder
                .build_int_nuw_sub(lhs, rhs, "")
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
        TokenType::Minus | TokenType::MinusEq if llvm_backend.has_disable_safe_math() => {
            llvm_builder
                .build_int_sub(lhs, rhs, "")
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
        TokenType::Star | TokenType::StarEq
            if (signatures.0 || signatures.1) && !llvm_backend.has_disable_safe_math() =>
        {
            llvm_builder
                .build_int_nsw_mul(lhs, rhs, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile '*' operation!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                })
                .into()
        }
        TokenType::Star | TokenType::StarEq if !llvm_backend.has_disable_safe_math() => {
            llvm_builder
                .build_int_nuw_mul(lhs, rhs, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile '*' operation!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                })
                .into()
        }
        TokenType::Star | TokenType::StarEq if llvm_backend.has_disable_safe_math() => llvm_builder
            .build_int_mul(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '*' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Slash | TokenType::SlashEq if signatures.0 || signatures.1 => llvm_builder
            .build_int_signed_div(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '/' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Slash | TokenType::SlashEq if !signatures.0 && !signatures.1 => llvm_builder
            .build_int_unsigned_div(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '/' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Slash | TokenType::SlashEq => llvm_builder
            .build_int_signed_div(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '/' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::LShift | TokenType::LShiftEq => llvm_builder
            .build_left_shift(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '<<' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::RShift | TokenType::RShiftEq => llvm_builder
            .build_right_shift(lhs, rhs, signatures.0 || signatures.1, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '>>' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Arith | TokenType::ArithEq if signatures.0 || signatures.1 => llvm_builder
            .build_int_signed_rem(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '%' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),

        TokenType::Arith | TokenType::ArithEq => llvm_builder
            .build_int_unsigned_rem(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '%' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),

        TokenType::Xor | TokenType::XorEq => llvm_builder
            .build_xor(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '^' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::Bor | TokenType::BorEq => llvm_builder
            .build_or(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '|' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),
        TokenType::BAnd | TokenType::BAndEq => llvm_builder
            .build_and(lhs, rhs, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile '&' operation!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            })
            .into(),

        op if op.is_logical_operator() => llvm_builder
            .build_int_compare(
                predicates::get_integer_predicate(
                    context,
                    operator,
                    signatures.0,
                    signatures.1,
                    span,
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

        op if op.is_logical_gate() => {
            if let TokenType::And = op {
                return llvm_builder
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
                    .into();
            }

            if let TokenType::Or = op {
                return llvm_builder
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
                    .into();
            }

            abort::abort_codegen(
                context,
                "Failed to compile without a valid operator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }

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
        | TokenType::StarEq
        | TokenType::SlashEq
        | TokenType::ArithEq
        | TokenType::BAndEq
        | TokenType::BorEq
        | TokenType::XorEq
        | TokenType::LShiftEq
        | TokenType::RShiftEq
        | TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq
        | TokenType::LShift
        | TokenType::RShift
        | TokenType::And
        | TokenType::Or
        | TokenType::Xor
        | TokenType::Bor
        | TokenType::BAnd,
        ..,
    ) = binary
    {
        let operator: &TokenType = binary.1;

        let lhs: &Ast<'_> = binary.0;
        let rhs: &Ast<'_> = binary.2;

        let lhs_type: &Type = binary.0.get_type_for_llvm();
        let rhs_type: &Type = binary.2.get_type_for_llvm();

        let lhs_is_signed: bool = lhs_type.is_signed_integer_type();
        let rhs_is_signed: bool = rhs_type.is_signed_integer_type();

        return self::compile_int_operation(
            context,
            lhs,
            rhs,
            cast,
            (lhs_is_signed, rhs_is_signed, lhs_type, rhs_type),
            operator,
            span,
        );
    }

    abort::abort_codegen(
        context,
        "Failed to compile integer binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}

fn compile_constant_int_value_operation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    lhs: BasicValueEnum<'ctx>,
    rhs: BasicValueEnum<'ctx>,
    signatures: (bool, bool),
    operator: &TokenType,
    span: Span,
) -> BasicValueEnum<'ctx> {
    if lhs.is_int_value() && rhs.is_int_value() {
        let lhs: IntValue = lhs.into_int_value();
        let rhs: IntValue = rhs.into_int_value();

        let (lhs, rhs) = type_cast::compile_constant_int_together_cast(lhs, rhs, signatures);

        return match operator {
            TokenType::Plus | TokenType::PlusEq => lhs.const_nsw_add(rhs).into(),
            TokenType::Minus | TokenType::MinusEq => lhs.const_nsw_sub(rhs).into(),
            TokenType::Star | TokenType::StarEq => lhs.const_nsw_mul(rhs).into(),
            TokenType::Slash | TokenType::SlashEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(
                                            lhs_number.overflowing_div(rhs_number).0,
                                        )
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    lhs_number
                                        .overflowing_div(rhs_number.try_into().unwrap_or_default())
                                        .0,
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(
                                            lhs_number
                                                .overflowing_div(
                                                    rhs_number.try_into().unwrap_or_default(),
                                                )
                                                .0,
                                        )
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(lhs_number.overflowing_div(rhs_number).0, false)
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }
            TokenType::LShift | TokenType::LShiftEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number.unbounded_shl(
                                            rhs_number.try_into().unwrap_or_default(),
                                        ))
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    lhs_number
                                        .unbounded_shl(rhs_number.try_into().unwrap_or_default()),
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number.unbounded_shl(
                                            rhs_number.try_into().unwrap_or_default(),
                                        ))
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(
                                lhs_number.unbounded_shl(rhs_number.try_into().unwrap_or_default()),
                                false,
                            )
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }
            TokenType::RShift | TokenType::RShiftEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number.unbounded_shr(
                                            rhs_number.try_into().unwrap_or_default(),
                                        ))
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    lhs_number
                                        .unbounded_shr(rhs_number.try_into().unwrap_or_default()),
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number.unbounded_shr(
                                            rhs_number.try_into().unwrap_or_default(),
                                        ))
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(
                                lhs_number.unbounded_shr(rhs_number.try_into().unwrap_or_default()),
                                false,
                            )
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }
            TokenType::Arith | TokenType::ArithEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number % rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            let casted_lhs = i64::try_from(lhs_number).unwrap_or_default();

                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(casted_lhs % rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            let casted_rhs: i64 = i64::try_from(rhs_number).unwrap_or_default();

                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number % casted_rhs)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(lhs_number % rhs_number, false)
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }
            TokenType::Xor | TokenType::XorEq => lhs.const_xor(rhs).into(),

            TokenType::Bor | TokenType::BorEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number | rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            let casted_lhs: i64 = i64::try_from(lhs_number).unwrap_or_default();

                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(casted_lhs | rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            let casted_rhs: i64 = i64::try_from(rhs_number).unwrap_or_default();

                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number | casted_rhs)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(lhs_number | rhs_number, false)
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }

            TokenType::BAnd | TokenType::BAndEq => {
                if signatures.0 || signatures.1 {
                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(lhs_number & rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                        if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                            let casted_lhs: i64 = i64::try_from(lhs_number).unwrap_or_default();

                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(casted_lhs & rhs_number)
                                    },
                                    true,
                                )
                                .into();
                        }
                    }

                    if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                        if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                            return lhs
                                .get_type()
                                .const_int(
                                    unsafe {
                                        std::mem::transmute::<i64, u64>(
                                            lhs_number
                                                & i64::try_from(rhs_number).unwrap_or_default(),
                                        )
                                    },
                                    true,
                                )
                                .into();
                        }
                    }
                }

                if let Some(lhs_number) = lhs.get_zero_extended_constant() {
                    if let Some(rhs_number) = rhs.get_zero_extended_constant() {
                        return lhs
                            .get_type()
                            .const_int(lhs_number & rhs_number, false)
                            .into();
                    }
                }

                lhs.get_type().const_zero().into()
            }

            op if op.is_logical_operator() => lhs
                .const_int_compare(
                    predicates::get_integer_predicate(
                        context,
                        operator,
                        signatures.0,
                        signatures.1,
                        span,
                    ),
                    rhs,
                )
                .into(),

            op if op.is_logical_gate() => {
                if let TokenType::And = op {
                    if signatures.0 || signatures.1 {
                        if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                            if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                                return lhs
                                    .get_type()
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
                            return lhs
                                .get_type()
                                .const_int(((lhs_number != 0) && (rhs_number != 0)) as u64, false)
                                .into();
                        }
                    }

                    return lhs.get_type().const_zero().into();
                }

                if let TokenType::Or = op {
                    if signatures.0 || signatures.1 {
                        if let Some(lhs_number) = lhs.get_sign_extended_constant() {
                            if let Some(rhs_number) = rhs.get_sign_extended_constant() {
                                return lhs
                                    .get_type()
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
                            return lhs
                                .get_type()
                                .const_int(((lhs_number != 0) || (rhs_number != 0)) as u64, false)
                                .into();
                        }
                    }

                    return lhs.get_type().const_zero().into();
                }

                abort::abort_codegen(
                    context,
                    "Failed to compile without a valid operator!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            }

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
        "Failed to compile constant integer binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
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
        | TokenType::Arith
        | TokenType::PlusEq
        | TokenType::MinusEq
        | TokenType::StarEq
        | TokenType::SlashEq
        | TokenType::ArithEq
        | TokenType::BAndEq
        | TokenType::BorEq
        | TokenType::XorEq
        | TokenType::LShiftEq
        | TokenType::RShiftEq
        | TokenType::BangEq
        | TokenType::EqEq
        | TokenType::LessEq
        | TokenType::Less
        | TokenType::Greater
        | TokenType::GreaterEq
        | TokenType::LShift
        | TokenType::RShift
        | TokenType::And
        | TokenType::Or
        | TokenType::Xor
        | TokenType::Bor
        | TokenType::BAnd,
        ..,
    ) = binary
    {
        let operator: &TokenType = binary.1;

        let lhs: BasicValueEnum = codegen::compile_constant_as_value(context, binary.0, cast);
        let rhs: BasicValueEnum = codegen::compile_constant_as_value(context, binary.2, cast);

        let lhs_type: &Type = binary.0.get_type_for_llvm();
        let rhs_type: &Type = binary.2.get_type_for_llvm();

        return self::compile_constant_int_value_operation(
            context,
            lhs,
            rhs,
            (
                rhs_type.is_signed_integer_type(),
                lhs_type.is_signed_integer_type(),
            ),
            operator,
            span,
        );
    }

    abort::abort_codegen(
        context,
        "Failed to compile constant integer binary operation!",
        span,
        std::path::PathBuf::from(file!()),
        line!(),
    );
}
