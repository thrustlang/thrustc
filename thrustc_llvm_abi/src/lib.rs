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

#![allow(non_camel_case_types)]
#![allow(clippy::large_enum_variant)]

use inkwell::{
    builder::Builder,
    context::Context,
    targets::TargetData,
    types::FunctionType,
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue},
};
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_llvm_x86_abi::{x86SystemVABIFunctionTypeConfiguration, x86SystemVABIType};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::{Type, type_layout::TargetInfo};

#[derive(Debug, Clone)]
pub enum LLVMABIType {
    x86SystemV(x86SystemVABIType),

    None,
}

#[derive(Debug, Clone)]
pub enum LLVMABIConfiguration {
    x86SystemVFunctionArgumentConfiguration(x86SystemVABIFunctionTypeConfiguration),

    None,
}

pub fn get_abi<'llvm_abi>(
    file: &'llvm_abi CompilationUnit,
    options: &'llvm_abi CompilerOptions,
    target_triple: &'llvm_abi LLVMTargetTriple,
    target_info: &'llvm_abi TargetInfo,
    target_data: &'llvm_abi TargetData,
) -> Option<LLVMABIRepresentation<'llvm_abi>> {
    if target_triple.has_sysv_abi() {
        return Some(LLVMABIRepresentation::x86SystemV {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        });
    }

    None
}

pub fn get_type<'llvm_abi>(
    abi: &'llvm_abi LLVMABIRepresentation<'llvm_abi>,
    ty: &Type,
) -> Option<LLVMABIType> {
    match abi {
        LLVMABIRepresentation::x86SystemV {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_x86_abi::X86SystemVABIContext =
                thrustc_llvm_x86_abi::X86SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                );

            let ty_classes: [thrustc_llvm_x86_abi::X86SystemVABITypeClass; 8] =
                thrustc_llvm_x86_abi::X86SystemVABITypeClass::get_system_v_type_class(
                    &mut abi_context,
                    ty,
                );

            let abi_ty: x86SystemVABIType =
                x86SystemVABIType::class_to_general_abi_strategy(&ty_classes, ty.clone());

            Some(LLVMABIType::x86SystemV(abi_ty))
        }

        _ => None,
    }
}

pub fn decompose_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &'llvm_abi LLVMABIRepresentation<'llvm_abi>,
    kind: &Type,
    parameter_types: &[Type],
    is_var_args: bool,
) -> Option<(FunctionType<'llvm_abi>, LLVMABIConfiguration)> {
    match abi {
        LLVMABIRepresentation::x86SystemV {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_x86_abi::X86SystemVABIContext =
                thrustc_llvm_x86_abi::X86SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                );

            let function_type: (
                FunctionType<'_>,
                thrustc_llvm_x86_abi::x86SystemVABIFunctionTypeConfiguration,
            ) = thrustc_llvm_x86_abi::decompose_function_type(
                llvm_context,
                &mut abi_context,
                kind,
                parameter_types,
                is_var_args,
            );

            Some((
                function_type.0,
                LLVMABIConfiguration::x86SystemVFunctionArgumentConfiguration(function_type.1),
            ))
        }

        _ => None,
    }
}

pub fn lower_function_call<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    configuration: LLVMABIConfiguration,
    args: &[BasicValueEnum<'llvm_abi>],
) -> Option<Vec<BasicMetadataValueEnum<'llvm_abi>>> {
    match abi {
        LLVMABIRepresentation::x86SystemV {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_x86_abi::X86SystemVABIContext =
                thrustc_llvm_x86_abi::X86SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                );

            let config: x86SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::x86SystemVFunctionArgumentConfiguration(config) => config,
                _ => unreachable!(),
            };

            let lowered_args: Vec<BasicMetadataValueEnum<'llvm_abi>> =
                thrustc_llvm_x86_abi::lower_function_call(
                    llvm_builder,
                    llvm_context,
                    &mut abi_context,
                    function_value,
                    &config,
                    args,
                );

            Some(lowered_args)
        }

        _ => None,
    }
}
