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
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypePointerExtensions;

use crate::abort;
use crate::cast;
use crate::codegen;
use crate::context::CodeGenLocation;
use crate::context::LLVMCodeGenContext;
use crate::types::LLVMFunction;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    args: &'ctx [Ast],
    kind: &Type,
    cast: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();
    let llvm_context: &Context = context.get_llvm_context();

    let has_abi: bool = context.has_abi();

    let function: LLVMFunction = context.get_table().get_function(name);

    let (
        llvm_function,
        function_arg_types,
        call_convention,
        abi_configuration,
        attributes,
        is_variatic,
        span,
    ) = (
        function.0, function.2, function.3, function.4, function.5, function.6, function.7,
    );

    let other_call_convention: Option<u32> = attributes
        .iter()
        .find_map(|attribute| thrustc_llvm_attributes::interpret_as_callconvention(attribute));

    let mut build_standard_call = || -> BasicValueEnum {
        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .enumerate()
            .map(|(i, expr)| {
                let cast: Option<&Type> = function_arg_types.get(i);

                context.add_codegen_location(CodeGenLocation::CallArgExpr);

                if let Some(cast_type) = cast {
                    if cast_type.is_ptr_like_type() {
                        let value: BasicValueEnum<'_> =
                            codegen::compile_as_ptr_value(context, expr, cast);

                        context.pop_current_codegen_location();

                        value.into()
                    } else {
                        let value: BasicValueEnum<'_> =
                            codegen::compile_as_value(context, expr, cast);

                        context.pop_current_codegen_location();

                        value.into()
                    }
                } else {
                    let value: BasicValueEnum<'_> = codegen::compile_as_value(context, expr, cast);

                    context.pop_current_codegen_location();

                    value.into()
                }
            })
            .collect();

        let ret_value: BasicValueEnum<'_> =
            match llvm_builder.build_call(llvm_function, &compiled_args, "") {
                Ok(callsite) => {
                    if let Some(call_convention) = other_call_convention {
                        callsite.set_call_convention(call_convention);
                    } else {
                        callsite.set_call_convention(call_convention);
                    }

                    let is_void_type: bool = llvm_function.get_type().get_return_type().is_none();

                    if !is_void_type {
                        callsite.try_as_basic_value().left().unwrap_or_else(|| {
                            abort::abort_codegen(
                                context,
                                "Failed to compile function call!",
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
                    "Failed to compile the function call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            };

        cast::try_smart_cast(context, cast, kind, ret_value, span)
    };

    if !has_abi {
        build_standard_call()
    } else {
        if is_variatic {
            return build_standard_call();
        }

        let compiled_args: Vec<BasicValueEnum> = args
            .iter()
            .enumerate()
            .map(|(i, expr)| {
                let cast: Option<&Type> = function_arg_types.get(i);

                context.add_codegen_location(CodeGenLocation::CallArgExpr);

                if let Some(cast_type) = cast {
                    if cast_type.is_ptr_like_type() {
                        let value: BasicValueEnum<'_> =
                            codegen::compile_as_ptr_value(context, expr, cast);

                        context.pop_current_codegen_location();

                        value
                    } else {
                        let value: BasicValueEnum<'_> =
                            codegen::compile_as_value(context, expr, cast);

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

        let abi: &thrustc_llvm_abi_representation::LLVMABIRepresentation<'_> =
            context.get_abi().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the function call, expected an ABI!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let configuration: &thrustc_llvm_abi::LLVMABIConfiguration =
            abi_configuration.as_ref().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the function call, expected an ABI type configuration!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let codegen_location: thrustc_llvm_abi::LLVMABICodeGenLocation =
            context.get_codegen_location().to_abi_representation();

        let lowered_args: Vec<BasicMetadataValueEnum<'_>> =
            thrustc_llvm_abi::lower_abi_call_prologue(
                llvm_context,
                llvm_builder,
                abi,
                llvm_function,
                configuration,
                compiled_args,
                codegen_location,
                span,
            )
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile to lower the call arguments to a specific ABI!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let ret_value: BasicValueEnum =
            match llvm_builder.build_call(llvm_function, &lowered_args, "") {
                Ok(callsite) => {
                    let is_void_type: bool = callsite
                        .get_called_fn_value()
                        .get_type()
                        .get_return_type()
                        .is_none();

                    if let Some(call_convention) = other_call_convention {
                        callsite.set_call_convention(call_convention);
                    } else {
                        callsite.set_call_convention(call_convention);
                    }

                    let codegen_location: thrustc_llvm_abi::LLVMABICodeGenLocation =
                        context.get_codegen_location().to_abi_representation();

                    let result: Option<BasicValueEnum<'_>> =
                        thrustc_llvm_abi::lower_abi_call_epilogue(
                            llvm_context,
                            llvm_builder,
                            abi,
                            configuration,
                            callsite,
                            &lowered_args,
                            codegen_location,
                            span,
                        );

                    if result.is_none() && is_void_type {
                        llvm_context
                            .ptr_type(AddressSpace::default())
                            .const_null()
                            .into()
                    } else if let Some(value) = result {
                        value
                    } else {
                        abort::abort_codegen(
                            context,
                            "Failed to compile lower a function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    }
                }
                Err(_) => abort::abort_codegen(
                    context,
                    "Failed to compile the function call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            };

        cast::try_smart_cast(context, cast, kind, ret_value, span)
    }
}
