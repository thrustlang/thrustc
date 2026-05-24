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
use thrustc_abi::SpecificABI;
use thrustc_ast::Ast;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_llvm_x86_abi::{
    x86SystemVABIFunctionParameterConfiguration, x86SystemVABIFunctionTypeConfiguration,
    x86SystemVABIType,
};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::{Type, type_layout::TargetInfo};

#[derive(Debug, Clone)]
pub enum LLVMABIType<'llvm_abi> {
    x86SystemV(x86SystemVABIType<'llvm_abi>),

    None,
}

#[derive(Debug, Clone)]
pub enum LLVMABIConfiguration<'llvm_abi> {
    x86SystemVFunctionTypeConfiguration(x86SystemVABIFunctionTypeConfiguration<'llvm_abi>),
    x86SystemVFunctionParameterConfiguration(x86SystemVABIFunctionParameterConfiguration),

    None,
}

#[derive(Debug, Clone)]
pub struct LLVMABIFunctionLoweredParameter<'llvm_abi> {
    name: &'llvm_abi str,
    ascii_name: &'llvm_abi str,
    ty: &'llvm_abi Type,
    value: BasicValueEnum<'llvm_abi>,
    abi_configuration: LLVMABIConfiguration<'llvm_abi>,
}

pub fn get_abi<'llvm_abi>(
    specific: SpecificABI,
    file: &'llvm_abi CompilationUnit,
    options: &'llvm_abi CompilerOptions,
    target_triple: &'llvm_abi LLVMTargetTriple,
    target_info: &'llvm_abi TargetInfo,
    target_data: &'llvm_abi TargetData,
) -> Option<LLVMABIRepresentation<'llvm_abi>> {
    match specific {
        SpecificABI::SystemV => Some(LLVMABIRepresentation::x86SystemV {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        }),

        SpecificABI::None => {
            self::get_abi_automatic(file, options, target_triple, target_info, target_data)
        }
    }
}

fn get_abi_automatic<'llvm_abi>(
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
    ty: &'llvm_abi Type,
) -> Option<LLVMABIType<'llvm_abi>> {
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
                x86SystemVABIType::class_to_general_abi_strategy(&mut abi_context, &ty_classes, ty);

            Some(LLVMABIType::x86SystemV(abi_ty))
        }

        _ => None,
    }
}

pub fn decompose_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &'llvm_abi LLVMABIRepresentation<'llvm_abi>,
    kind: &'llvm_abi Type,
    parameters: &'llvm_abi [Ast<'llvm_abi>],
    is_var_args: bool,
) -> Option<(FunctionType<'llvm_abi>, LLVMABIConfiguration<'llvm_abi>)> {
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
                parameters,
                is_var_args,
            );

            Some((
                function_type.0,
                LLVMABIConfiguration::x86SystemVFunctionTypeConfiguration(function_type.1),
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
    configuration: &LLVMABIConfiguration,
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

            let configuration: &x86SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::x86SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            let lowered_args: Vec<BasicMetadataValueEnum<'llvm_abi>> =
                thrustc_llvm_x86_abi::lower_function_call(
                    llvm_builder,
                    llvm_context,
                    &mut abi_context,
                    function_value,
                    configuration,
                    args,
                );

            Some(lowered_args)
        }

        _ => None,
    }
}

pub fn lower_function_parameters<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
) -> Option<Vec<LLVMABIFunctionLoweredParameter<'llvm_abi>>> {
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

            let configuration: &x86SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::x86SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            let mut lowered_parameters: Vec<(
                &'llvm_abi str,
                &'llvm_abi str,
                &'llvm_abi Type,
                x86SystemVABIFunctionParameterConfiguration,
                BasicValueEnum<'_>,
            )> = thrustc_llvm_x86_abi::lower_function_parameters(
                llvm_builder,
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );

            let transformed_lowered_parameters: Vec<LLVMABIFunctionLoweredParameter> =
                lowered_parameters
                    .iter_mut()
                    .map(|lowered_parameter| {
                        let (name, ascii_name, ty, parameter_configuration, value) =
                            lowered_parameter;

                        let abi_parameter_configuration: LLVMABIConfiguration =
                            LLVMABIConfiguration::x86SystemVFunctionParameterConfiguration(
                                parameter_configuration.clone(),
                            );

                        LLVMABIFunctionLoweredParameter::new(
                            std::mem::take(name),
                            std::mem::take(ascii_name),
                            ty,
                            *value,
                            abi_parameter_configuration,
                        )
                    })
                    .collect();

            Some(transformed_lowered_parameters)
        }

        _ => None,
    }
}

impl<'llvm_abi> LLVMABIFunctionLoweredParameter<'llvm_abi> {
    pub fn new(
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        value: BasicValueEnum<'llvm_abi>,
        abi_configuration: LLVMABIConfiguration<'llvm_abi>,
    ) -> Self {
        Self {
            name,
            ascii_name,
            ty,
            value,
            abi_configuration,
        }
    }
}

impl<'lowered_parameter> LLVMABIFunctionLoweredParameter<'lowered_parameter> {
    #[inline]
    pub fn get_name(&self) -> &'lowered_parameter str {
        self.name
    }

    #[inline]
    pub fn get_ascii_name(&self) -> &'lowered_parameter str {
        self.ascii_name
    }

    #[inline]
    pub fn get_type(&self) -> &'lowered_parameter Type {
        self.ty
    }

    #[inline]
    pub fn get_value(&self) -> BasicValueEnum<'lowered_parameter> {
        self.value
    }

    #[inline]
    pub fn get_abi_configuration(&self) -> &LLVMABIConfiguration<'lowered_parameter> {
        &self.abi_configuration
    }
}
