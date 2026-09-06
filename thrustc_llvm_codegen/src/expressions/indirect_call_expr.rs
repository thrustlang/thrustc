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

use inkwell::context::Context;
use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;
use thrustc_typesystem::traits::TypePointerExtensions;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::context::{CodeGenLocation, LLVMCodeGenContext};
use crate::traits::AstLLVMGetType;
use crate::{abort, codegen, type_cast, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    pointer: &'ctx Ast,
    args: &'ctx [Ast],
    function_type: &'ctx Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();
    let llvm_context: &Context = context.get_llvm_context();

    context.add_codegen_location(CodeGenLocation::RValue);

    let source_value: BasicValueEnum<'_> =
        codegen::compile_as_ptr_value(context, pointer, cast_type);

    context.pop_current_codegen_location();

    let function_ptr_value: PointerValue<'_> = source_value.into_pointer_value();

    let Type::Fn {
        parameter_types,
        return_type,
        modificator,
        ..
    } = function_type
    else {
        abort::abort_codegen(
            context,
            "Failed to compile indirect function call!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    };

    let is_var_args: bool = modificator.llvm().has_ignore();
    let has_abi: bool = context.has_abi();

    let uses_webassembly_abi: bool = matches!(
        context.get_abi(),
        Some(thrustc_llvm_abi_representation::LLVMABIRepresentation::WebAssemblyABI { .. })
    );

    let argument_types: Vec<Type> = args
        .iter()
        .map(|argument| argument.get_type_for_llvm().clone())
        .collect();

    let compiled_args: Vec<BasicValueEnum> = args
        .iter()
        .enumerate()
        .map(|(index, expr)| {
            let cast: Option<&Type> = parameter_types.get(index);

            context.add_codegen_location(CodeGenLocation::CallArgExpr);

            if let Some(cast_type) = cast {
                if cast_type.is_ptr_like_type() {
                    let value: BasicValueEnum<'_> =
                        codegen::compile_as_ptr_value(context, expr, cast);

                    context.pop_current_codegen_location();

                    value
                } else {
                    let value: BasicValueEnum<'_> = codegen::compile_as_value(context, expr, cast);

                    context.pop_current_codegen_location();

                    value
                }
            } else {
                let value: BasicValueEnum<'_> = codegen::compile_as_value(context, expr, cast);

                context.pop_current_codegen_location();

                value
            }
        })
        .collect();

    let (function_type, abi_configuration): (
        FunctionType<'_>,
        Option<thrustc_llvm_abi::LLVMABIConfiguration<'_>>,
    ) = if uses_webassembly_abi {
        let abi = context.get_abi().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to compile a WebAssembly indirect call without an ABI.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let (function_type, configuration) =
            thrustc_llvm_abi::create_anonymous_function_type(
                llvm_context,
                abi,
                return_type,
                parameter_types,
                is_var_args,
                context.get_codegen_location().to_abi_representation(),
            )
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to create a WebAssembly indirect call type.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        (function_type, Some(configuration))
    } else {
        (
            typegeneration::generate_type_function_type_to_function_type(
                context,
                return_type,
                parameter_types,
                is_var_args,
            ),
            None,
        )
    };

    let lowered_args: Vec<BasicMetadataValueEnum> = if uses_webassembly_abi {
        let abi = context.get_abi().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to lower a WebAssembly indirect call without an ABI.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        thrustc_llvm_abi::lower_anonymous_call_prologue(
            llvm_context,
            llvm_builder,
            abi,
            abi_configuration.as_ref().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to get a WebAssembly indirect call configuration.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            }),
            compiled_args,
            &argument_types,
            context.get_codegen_location().to_abi_representation(),
            span,
        )
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to lower WebAssembly indirect call arguments.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    } else {
        compiled_args.into_iter().map(Into::into).collect()
    };

    let function_value: BasicValueEnum<'_> = match llvm_builder.build_indirect_call(
        function_type,
        function_ptr_value,
        &lowered_args,
        "",
    ) {
        Ok(callsite) => {
            if uses_webassembly_abi {
                let abi = context.get_abi().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to finish a WebAssembly indirect call without an ABI.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

                let configuration = abi_configuration.as_ref().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to finish a WebAssembly indirect call without configuration.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

                let codegen_location = context.get_codegen_location().to_abi_representation();

                thrustc_llvm_abi::lower_call_conventions(
                    llvm_context,
                    abi,
                    configuration,
                    callsite,
                    &argument_types,
                    codegen_location,
                );

                thrustc_llvm_abi::lower_call_epilogue(
                    llvm_context,
                    llvm_builder,
                    abi,
                    configuration,
                    callsite,
                    &lowered_args,
                    codegen_location,
                    span,
                )
                .unwrap_or_else(|| {
                    context
                        .get_llvm_context()
                        .ptr_type(AddressSpace::default())
                        .const_null()
                        .into()
                })
            } else if has_abi {
                let args_types: &Vec<Type> = parameter_types;

                let abi: &thrustc_llvm_abi_representation::LLVMABIRepresentation<'_> =
                    context.get_abi().unwrap_or_else(|| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile the function anonymous call, expected an ABI!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let lowered: bool = thrustc_llvm_abi::lower_anonymous_call_epilogue(
                    llvm_context,
                    abi,
                    callsite,
                    return_type,
                    args_types,
                    context.get_codegen_location().to_abi_representation(),
                );

                if !lowered {
                    abort::abort_codegen(
                        context,
                        "Failed to compile the function anonymous call!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }

                if !return_type.is_void_type() {
                    callsite.try_as_basic_value().left().unwrap_or_else(|| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile indirect function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                } else {
                    context
                        .get_llvm_context()
                        .ptr_type(AddressSpace::default())
                        .const_null()
                        .into()
                }
            } else if !return_type.is_void_type() {
                callsite.try_as_basic_value().left().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile indirect function call!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
            } else {
                context
                    .get_llvm_context()
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into()
            }
        }
        Err(_) => abort::abort_codegen(
            context,
            "Failed to compile indirect function call!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    };

    type_cast::try_smart_cast(context, cast_type, return_type, function_value, span)
}
