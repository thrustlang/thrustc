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

use thrustc_ast::Ast;
use thrustc_ast::traits::AstCodeLocation;
use thrustc_ast::traits::AstMemoryExtensions;
use thrustc_backends::llvm::LLVMBackend;
use thrustc_entities::UnaryOperation;
use thrustc_options::CompilerOptions;
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::abort;
use crate::cast;
use crate::codegen;
use crate::context::LLVMCodeGenContext;
use crate::memory::SymbolAllocated;

use crate::traits::AstLLVMGetType;
use crate::typegeneration;

use inkwell::types::FloatType;
use inkwell::values::PointerValue;
use inkwell::{
    builder::Builder,
    values::{BasicValueEnum, FloatValue, IntValue},
};

use std::path::PathBuf;

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    unary: UnaryOperation<'ctx>,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    match unary {
        (
            TokenType::PlusPlus | TokenType::MinusMinus,
            _,
            Ast::Reference {
                name, kind, span, ..
            },
        ) => self::compile_increment_decrement_ref(context, name, unary.0, kind, *span, cast_type),
        (TokenType::PlusPlus | TokenType::MinusMinus, _, expr) => {
            self::compile_increment_decrement(context, unary.0, expr, cast_type)
        }

        (TokenType::Bang, _, expr) => self::compile_logical_negation(context, expr, cast_type),
        (TokenType::Minus, _, expr) => self::compile_arithmetic_negation(context, expr, cast_type),
        (TokenType::Not, _, expr) => self::compile_bitwise_not(context, expr, cast_type),

        what => abort::abort_codegen(
            context,
            "Failed to compile the operation!",
            what.2.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

pub fn compile_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    unary: UnaryOperation<'ctx>,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    match unary {
        (TokenType::PlusPlus | TokenType::MinusMinus, _, expr) => {
            self::compile_increment_decrement_const(context, unary.0, expr, cast_type)
        }
        (TokenType::Bang, _, expr) => {
            self::compile_logical_negation_const(context, expr, cast_type)
        }
        (TokenType::Minus, _, expr) => {
            self::compile_arithmetic_negation_const(context, expr, cast_type)
        }
        (TokenType::Not, _, expr) => {
            self::compile_arithmetic_bitwise_not_const(context, expr, cast_type)
        }

        what => abort::abort_codegen(
            context,
            "Failed to compile the operation!",
            what.2.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

fn compile_increment_decrement_ref<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    operator: &TokenType,
    kind: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();
    let symbol: SymbolAllocated = context.get_table().get_symbol(name);

    match kind {
        kind if kind.is_integer_type() => {
            let old_value: IntValue = symbol.load(context).into_int_value();
            let modifier: IntValue = old_value.get_type().const_int(1, false);
            let is_signed: bool = kind.is_signed_integer_type();

            let options: &CompilerOptions = context.get_compiler_options();
            let llvm_backend: &LLVMBackend = options.get_llvm_backend();

            let result: BasicValueEnum = match operator {
                TokenType::PlusPlus if is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nsw_add(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::PlusPlus if !is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nuw_add(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }

                TokenType::PlusPlus if !llvm_backend.has_disable_safe_math() => llvm_builder
                    .build_int_add(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                TokenType::MinusMinus if is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nsw_sub(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::MinusMinus if !is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nuw_sub(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::MinusMinus if llvm_backend.has_disable_safe_math() => llvm_builder
                    .build_int_sub(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            };

            let result: BasicValueEnum =
                cast::try_smart_cast(context, cast_type, kind, result, span);

            symbol.store(context, result);

            result
        }
        _ => {
            let old_value: FloatValue = symbol.load(context).into_float_value();
            let modifier: FloatValue = typegeneration::generate_type(context, kind)
                .into_float_type()
                .const_float(1.0);

            let result: BasicValueEnum = match operator {
                TokenType::PlusPlus => llvm_builder
                    .build_float_add(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),
                TokenType::MinusMinus => llvm_builder
                    .build_float_sub(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            };

            let new_value: BasicValueEnum =
                cast::try_smart_cast(context, cast_type, kind, result, span);

            symbol.store(context, new_value);

            new_value
        }
    }
}

fn compile_increment_decrement<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    operator: &TokenType,
    expression: &'ctx Ast,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let value: BasicValueEnum = codegen::compile_as_value(context, expression, cast_type);
    let kind: &Type = expression.get_type_for_llvm();

    let span: Span = expression.get_span();

    match kind {
        kind if kind.is_integer_type() => {
            let old_value: IntValue = value.into_int_value();
            let modifier: IntValue = old_value.get_type().const_int(1, false);
            let is_signed: bool = kind.is_signed_integer_type();

            let options: &CompilerOptions = context.get_compiler_options();
            let llvm_backend: &LLVMBackend = options.get_llvm_backend();

            let result: BasicValueEnum = match operator {
                TokenType::PlusPlus if is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nsw_add(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::PlusPlus if !is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nuw_add(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::PlusPlus if llvm_backend.has_disable_safe_math() => llvm_builder
                    .build_int_add(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                TokenType::MinusMinus if is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nsw_sub(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::MinusMinus if !is_signed && !llvm_backend.has_disable_safe_math() => {
                    llvm_builder
                        .build_int_nuw_sub(old_value, modifier, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile the operation!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                }
                TokenType::MinusMinus if llvm_backend.has_disable_safe_math() => llvm_builder
                    .build_int_sub(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            };

            cast::try_smart_cast(context, cast_type, kind, result, span)
        }
        _ => {
            let old_value: FloatValue = value.into_float_value();
            let modifier: FloatValue = old_value.get_type().const_float(1.0);

            let result: BasicValueEnum = match operator {
                TokenType::PlusPlus => llvm_builder
                    .build_float_add(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                TokenType::MinusMinus => llvm_builder
                    .build_float_sub(old_value, modifier, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the operation!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            };

            cast::try_smart_cast(context, cast_type, kind, result, span)
        }
    }
}

fn compile_logical_negation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let kind: &Type = expr.get_type_for_llvm();

    let value: BasicValueEnum = if expr.is_memory_assigned_value().unwrap_or(false) {
        codegen::compile_as_ptr_value(context, expr, cast_type)
    } else {
        codegen::compile_as_value(context, expr, cast_type)
    };

    let span: Span = expr.get_span();

    match kind {
        kind if kind.is_bool_type() => {
            let int_value: IntValue = value.into_int_value();

            let new_value: IntValue = llvm_builder.build_not(int_value, "").unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

            cast::try_smart_cast(context, cast_type, kind, new_value.into(), span)
        }

        kind if kind.is_ptr_type() => {
            let ptr_value: PointerValue<'_> = value.into_pointer_value();

            let new_value: IntValue<'_> = llvm_builder
                .build_is_not_null(ptr_value, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile the operation!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    )
                });

            cast::try_smart_cast(context, cast_type, kind, new_value.into(), span)
        }

        _ => abort::abort_codegen(
            context,
            "Failed to compile the operation!",
            span,
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

fn compile_arithmetic_negation<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let value: BasicValueEnum = codegen::compile_as_value(context, expr, cast_type);
    let kind: &Type = expr.get_type_for_llvm();

    let span: Span = expr.get_span();

    match kind {
        kind if kind.is_integer_type() => {
            let int: IntValue = value.into_int_value();

            let result: IntValue<'_> = llvm_builder.build_int_neg(int, "").unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

            cast::try_smart_cast(context, cast_type, kind, result.into(), span)
        }

        _ => {
            let float: FloatValue = value.into_float_value();

            let result: FloatValue<'_> =
                llvm_builder.build_float_neg(float, "").unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile the operation!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    )
                });

            cast::try_smart_cast(context, cast_type, kind, result.into(), span)
        }
    }
}

fn compile_bitwise_not<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let value: BasicValueEnum = codegen::compile_as_value(context, expr, cast_type);
    let kind: &Type = expr.get_type_for_llvm();

    let span: Span = expr.get_span();

    match kind {
        kind if kind.is_integer_type() => {
            let int: IntValue = value.into_int_value();

            let result: IntValue = llvm_builder.build_not(int, "").unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

            cast::try_smart_cast(context, cast_type, kind, result.into(), span)
        }

        _ => {
            let ptr: PointerValue = value.into_pointer_value();

            let result: PointerValue<'_> = llvm_builder.build_not(ptr, "").unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

            cast::try_smart_cast(context, cast_type, kind, result.into(), span)
        }
    }
}

fn compile_increment_decrement_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    operator: &TokenType,
    expression: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let value: BasicValueEnum = codegen::compile_constant_as_value(context, expression, cast_type);

    let kind: &Type = expression.get_type_for_llvm();
    let span: Span = expression.get_span();

    match kind {
        kind if kind.is_integer_type() => {
            let int: IntValue = value.into_int_value();

            let modifier: IntValue = int.get_type().const_int(1, false);

            match operator {
                TokenType::PlusPlus => int.const_add(modifier).into(),
                TokenType::MinusMinus => int.const_sub(modifier).into(),

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            }
        }
        _ => {
            let float: FloatValue = value.into_float_value();

            match operator {
                TokenType::PlusPlus => {
                    if let Some(constant_float) = float.get_constant() {
                        let value: f64 = constant_float.0;
                        let new_value: f64 = value + 1.0;

                        return float.get_type().const_float(new_value).into();
                    }

                    float.into()
                }

                TokenType::MinusMinus => {
                    if let Some(constant_float) = float.get_constant() {
                        let value: f64 = constant_float.0;
                        let new_value: f64 = value - 1.0;

                        return float.get_type().const_float(new_value).into();
                    }

                    float.into()
                }

                _ => abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                ),
            }
        }
    }
}

fn compile_logical_negation_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let value: BasicValueEnum = codegen::compile_constant_as_value(context, expr, cast_type);

    let kind: &Type = expr.get_type_for_llvm();
    let span: Span = expr.get_span();

    match kind {
        kind if kind.is_bool_type() => {
            let int_value: IntValue = value.into_int_value();

            int_value.const_not().into()
        }

        _ => abort::abort_codegen(
            context,
            "Failed to compile the operation!",
            span,
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

fn compile_arithmetic_negation_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let value: BasicValueEnum = codegen::compile_constant_as_value(context, expr, cast_type);
    let kind: &Type = expr.get_type_for_llvm();

    match kind {
        kind if kind.is_integer_type() => value.into_int_value().const_neg().into(),
        _ => {
            let float_value: FloatValue = value.into_float_value();
            let float_type: FloatType = float_value.get_type();

            if let Some((value, ..)) = float_value.get_constant() {
                float_type.const_float(-value).into()
            } else {
                abort::abort_codegen(
                    context,
                    "Failed to compile the operation!",
                    expr.get_span(),
                    PathBuf::from(file!()),
                    line!(),
                )
            }
        }
    }
}

fn compile_arithmetic_bitwise_not_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    expr: &'ctx Ast,
    cast_type: &Type,
) -> BasicValueEnum<'ctx> {
    let value: BasicValueEnum = codegen::compile_constant_as_value(context, expr, cast_type);
    let kind: &Type = expr.get_type_for_llvm();

    match kind {
        kind if kind.is_integer_type() => value.into_int_value().const_not().into(),

        _ => abort::abort_codegen(
            context,
            "Failed to compile the operation!",
            expr.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}
