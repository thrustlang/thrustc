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
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::abort;
use crate::cast;
use crate::codegen;
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

    let (llvm_function, function_arg_types, function_convention, llvm_abi_type_config, span) =
        (function.0, function.2, function.3, function.4, function.5);

    if !has_abi {
        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .enumerate()
            .map(|(i, expr)| {
                let cast: Option<&Type> = function_arg_types.get(i);
                codegen::compile_as_value(context, expr, cast).into()
            })
            .collect();

        let ret_value: BasicValueEnum =
            match llvm_builder.build_call(llvm_function, &compiled_args, "") {
                Ok(call) => {
                    call.set_call_convention(function_convention);

                    if !kind.is_void_type() {
                        call.try_as_basic_value().left().unwrap_or_else(|| {
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
    } else {
        let compiled_args: Vec<BasicValueEnum> = args
            .iter()
            .enumerate()
            .map(|(i, expr)| {
                let cast: Option<&Type> = function_arg_types.get(i);
                codegen::compile_as_value(context, expr, cast)
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
            llvm_abi_type_config.as_ref().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the function call, expected an ABI type configuration!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let lowered_args: Vec<BasicMetadataValueEnum<'_>> = thrustc_llvm_abi::lower_function_call(
            llvm_context,
            llvm_builder,
            abi,
            llvm_function,
            configuration,
            &compiled_args,
        )
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to compile lower the function call to a specific ABI!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let ret_value: BasicValueEnum =
            match llvm_builder.build_call(llvm_function, &lowered_args, "") {
                Ok(call) => {
                    call.set_call_convention(function_convention);

                    if !kind.is_void_type() {
                        call.try_as_basic_value().left().unwrap_or_else(|| {
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
    }
}
