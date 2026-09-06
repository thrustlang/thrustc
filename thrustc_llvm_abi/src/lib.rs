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
#![allow(clippy::too_many_arguments)]

use inkwell::{
    builder::Builder,
    context::Context,
    targets::TargetData,
    types::FunctionType,
    values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue},
};
use thrustc_abi::SpecificABI;
use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_nvidia_cuda_abi::{
    CudaABIAnonymousCallType, CudaABIFunctionTypeConfiguration, CudaCodeGenLocation,
};
use thrustc_llvm_system_v_abi::{
    SystemVABIFunctionParameterConfiguration, SystemVABIFunctionTypeConfiguration, SystemVABIType,
    SystemVCodeGenLocation,
};
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_llvm_webassembly_abi::{
    WebAssemblyABIFunctionTypeConfiguration, WebAssemblyCodeGenLocation,
};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::{Type, type_layout::TargetInfo};

mod abort;

#[derive(Debug, Clone)]
pub enum LLVMABIType<'llvm_abi> {
    SystemV(SystemVABIType<'llvm_abi>),

    None,
}

#[derive(Debug, Clone)]
pub enum LLVMABIConfiguration<'llvm_abi> {
    SystemVFunctionTypeConfiguration(SystemVABIFunctionTypeConfiguration<'llvm_abi>),
    CudaFunctionTypeConfiguration(CudaABIFunctionTypeConfiguration<'llvm_abi>),
    CudaABIAnonymousCallType {
        ty: &'llvm_abi Type,
        args_type: &'llvm_abi [Type],
    },
    WebAssemblyFunctionTypeConfiguration(WebAssemblyABIFunctionTypeConfiguration<'llvm_abi>),

    None,
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMABICodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

impl LLVMABICodeGenLocation {
    #[inline]
    pub fn to_system_v(&self) -> SystemVCodeGenLocation {
        match self {
            LLVMABICodeGenLocation::CallArgExpr => SystemVCodeGenLocation::CallArgExpr,
            LLVMABICodeGenLocation::LValue => SystemVCodeGenLocation::LValue,
            LLVMABICodeGenLocation::RValue => SystemVCodeGenLocation::RValue,
            LLVMABICodeGenLocation::None => SystemVCodeGenLocation::None,
        }
    }

    #[inline]
    pub fn to_nvidia_cuda(&self) -> CudaCodeGenLocation {
        match self {
            LLVMABICodeGenLocation::CallArgExpr => CudaCodeGenLocation::CallArgExpr,
            LLVMABICodeGenLocation::LValue => CudaCodeGenLocation::LValue,
            LLVMABICodeGenLocation::RValue => CudaCodeGenLocation::RValue,
            LLVMABICodeGenLocation::None => CudaCodeGenLocation::None,
        }
    }

    #[inline]
    pub fn to_webassembly(&self) -> WebAssemblyCodeGenLocation {
        match self {
            LLVMABICodeGenLocation::CallArgExpr => WebAssemblyCodeGenLocation::CallArgExpr,
            LLVMABICodeGenLocation::LValue => WebAssemblyCodeGenLocation::LValue,
            LLVMABICodeGenLocation::RValue => WebAssemblyCodeGenLocation::RValue,
            LLVMABICodeGenLocation::None => WebAssemblyCodeGenLocation::None,
        }
    }

    #[inline]
    pub fn is_direct_behavior(&self) -> bool {
        matches!(self, LLVMABICodeGenLocation::LValue)
    }

    #[inline]
    pub fn is_load_behavior(&self) -> bool {
        matches!(
            self,
            LLVMABICodeGenLocation::CallArgExpr | LLVMABICodeGenLocation::RValue
        )
    }
}

#[derive(Debug, Clone)]
pub struct LLVMABIFunctionLoweredParameter<'llvm_abi> {
    name: &'llvm_abi str,
    ascii_name: &'llvm_abi str,
    ty: &'llvm_abi Type,
    value: BasicValueEnum<'llvm_abi>,
    storage: LLVMABIFunctionParameterStorage,
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMABIFunctionParameterStorage {
    Value,
    Address,
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
        SpecificABI::SystemV => Some(LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        }),

        SpecificABI::NvidiaCuda => Some(LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        }),

        SpecificABI::WebAssembly if target_triple.is_wasm32_arch() => {
            Some(LLVMABIRepresentation::WebAssemblyABI {
                file,
                options,
                target_triple,
                target_info,
                target_data,
            })
        }

        SpecificABI::WebAssembly => None,

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
        return Some(LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        });
    }

    if target_triple.is_nvptx_arch() {
        return Some(LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        });
    }

    if target_triple.is_wasm32_arch() {
        return Some(LLVMABIRepresentation::WebAssemblyABI {
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
    codegen_location: LLVMABICodeGenLocation,
) -> Option<LLVMABIType<'llvm_abi>> {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let ty_classes: [thrustc_llvm_system_v_abi::SystemVABITypeClass; 8] =
                thrustc_llvm_system_v_abi::SystemVABITypeClass::get_system_v_type_class(
                    &mut abi_context,
                    ty,
                );

            let abi_ty: SystemVABIType =
                SystemVABIType::class_to_general_abi_strategy(&mut abi_context, &ty_classes, ty);

            Some(LLVMABIType::SystemV(abi_ty))
        }

        _ => None,
    }
}

pub fn create_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &'llvm_abi LLVMABIRepresentation<'llvm_abi>,
    kind: &'llvm_abi Type,
    parameters: &'llvm_abi [Ast<'llvm_abi>],
    is_var_args: bool,
    codegen_location: LLVMABICodeGenLocation,
) -> Option<(FunctionType<'llvm_abi>, LLVMABIConfiguration<'llvm_abi>)> {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let function_type: (
                FunctionType<'_>,
                thrustc_llvm_system_v_abi::SystemVABIFunctionTypeConfiguration,
            ) = thrustc_llvm_system_v_abi::generate_function_type(
                llvm_context,
                &mut abi_context,
                kind,
                parameters,
                is_var_args,
            );

            Some((
                function_type.0,
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(function_type.1),
            ))
        }

        LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_nvidia_cuda_abi::CudaABIContext<'_> =
                thrustc_llvm_nvidia_cuda_abi::CudaABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_nvidia_cuda(),
                );

            let function_type: (
                FunctionType<'_>,
                thrustc_llvm_nvidia_cuda_abi::CudaABIFunctionTypeConfiguration<'_>,
            ) = thrustc_llvm_nvidia_cuda_abi::generate_function_type(
                llvm_context,
                &mut abi_context,
                kind,
                parameters,
                is_var_args,
            );

            Some((
                function_type.0,
                LLVMABIConfiguration::CudaFunctionTypeConfiguration(function_type.1),
            ))
        }

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context = thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

            let (function_type, configuration) =
                thrustc_llvm_webassembly_abi::generate_function_type(
                    llvm_context,
                    &mut abi_context,
                    kind,
                    parameters,
                    is_var_args,
                );

            Some((
                function_type,
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration),
            ))
        }

        _ => None,
    }
}

pub fn lower_call_prologue<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    args: Vec<BasicValueEnum<'llvm_abi>>,
    argument_types: &[Type],
    codegen_location: LLVMABICodeGenLocation,
    span: Span,
) -> Option<Vec<BasicMetadataValueEnum<'llvm_abi>>> {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let configuration: &SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            let lowered_args: Vec<BasicMetadataValueEnum<'llvm_abi>> =
                thrustc_llvm_system_v_abi::lower_system_v_call_prologue(
                    llvm_builder,
                    llvm_context,
                    &mut abi_context,
                    function_value,
                    configuration,
                    args,
                    span,
                );

            Some(lowered_args)
        }

        LLVMABIRepresentation::CudaABI { .. } => {
            let lowered_args: Vec<BasicMetadataValueEnum<'llvm_abi>> =
                args.iter().map(|arg| (*arg).into()).collect();

            Some(lowered_args)
        }

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context = thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

            let configuration = match configuration {
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                    configuration
                }
                _ => unreachable!(),
            };

            Some(thrustc_llvm_webassembly_abi::lower_call_prologue(
                llvm_builder,
                llvm_context,
                &mut abi_context,
                configuration,
                args,
                argument_types,
                span,
            ))
        }

        _ => None,
    }
}

pub fn lower_call_epilogue<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    callsite: CallSiteValue<'llvm_abi>,
    lowered_args: &[BasicMetadataValueEnum<'llvm_abi>],
    codegen_location: LLVMABICodeGenLocation,
    span: Span,
) -> Option<BasicValueEnum<'llvm_abi>> {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_data,
            target_info,
            target_triple,
        } => {
            let is_void_type: bool = callsite
                .get_called_fn_value()
                .get_type()
                .get_return_type()
                .is_none();
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let configuration: &SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            if configuration.is_memory_return() {
                let lowered_value: BasicValueEnum<'_> =
                    thrustc_llvm_system_v_abi::lower_system_v_call_epilogue(
                        llvm_builder,
                        llvm_context,
                        &mut abi_context,
                        callsite,
                        lowered_args,
                        configuration,
                        span,
                    );

                Some(lowered_value)
            } else {
                if is_void_type {
                    None
                } else {
                    Some(callsite.try_as_basic_value().left().unwrap_or_else(|| {
                        abort::abort_system_v_abi_codegen(
                            &mut abi_context,
                            "Failed to compile function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    }))
                }
            }
        }

        LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let is_void_type: bool = callsite
                .get_called_fn_value()
                .get_type()
                .get_return_type()
                .is_none();
            let mut abi_context: thrustc_llvm_nvidia_cuda_abi::CudaABIContext<'_> =
                thrustc_llvm_nvidia_cuda_abi::CudaABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_nvidia_cuda(),
                );

            if is_void_type {
                None
            } else {
                Some(callsite.try_as_basic_value().left().unwrap_or_else(|| {
                    abort::abort_cuda_abi_codegen(
                        &mut abi_context,
                        "Failed to compile function call!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }))
            }
        }

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context = thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

            let configuration = match configuration {
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                    configuration
                }
                _ => unreachable!(),
            };

            thrustc_llvm_webassembly_abi::lower_call_epilogue(
                llvm_builder,
                llvm_context,
                &mut abi_context,
                callsite,
                lowered_args,
                configuration,
                span,
            )
        }

        _ => None,
    }
}

pub fn lower_call_conventions<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    callsite: CallSiteValue<'llvm_abi>,
    argument_types: &[Type],
    codegen_location: LLVMABICodeGenLocation,
) {
    if let LLVMABIRepresentation::WebAssemblyABI {
        file,
        options,
        target_info,
        target_data,
        ..
    } = abi
    {
        let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
            thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

        let configuration = match configuration {
            LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                configuration
            }
            _ => unreachable!(),
        };

        thrustc_llvm_webassembly_abi::lower_call_conventions(
            llvm_context,
            &mut abi_context,
            callsite,
            configuration,
            argument_types,
        );
    }
}

pub fn create_anonymous_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &'llvm_abi LLVMABIRepresentation<'llvm_abi>,
    return_type: &'llvm_abi Type,
    parameter_types: &'llvm_abi [Type],
    is_var_args: bool,
    codegen_location: LLVMABICodeGenLocation,
) -> Option<(FunctionType<'llvm_abi>, LLVMABIConfiguration<'llvm_abi>)> {
    if let LLVMABIRepresentation::WebAssemblyABI {
        file,
        options,
        target_info,
        target_data,
        ..
    } = abi
    {
        let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
            thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

        let (function_type, configuration) =
            thrustc_llvm_webassembly_abi::generate_anonymous_function_type(
                llvm_context,
                &mut abi_context,
                return_type,
                parameter_types,
                is_var_args,
            );

        Some((
            function_type,
            LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration),
        ))
    } else {
        None
    }
}

pub fn lower_anonymous_call_prologue<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    args: Vec<BasicValueEnum<'llvm_abi>>,
    argument_types: &[Type],
    codegen_location: LLVMABICodeGenLocation,
    span: Span,
) -> Option<Vec<BasicMetadataValueEnum<'llvm_abi>>> {
    if let LLVMABIRepresentation::WebAssemblyABI {
        file,
        options,
        target_info,
        target_data,
        ..
    } = abi
    {
        let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
            thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                file,
                options,
                (*target_info).clone(),
                target_data,
                codegen_location.to_webassembly(),
            );

        let configuration = match configuration {
            LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                configuration
            }
            _ => unreachable!(),
        };

        Some(thrustc_llvm_webassembly_abi::lower_call_prologue(
            llvm_builder,
            llvm_context,
            &mut abi_context,
            configuration,
            args,
            argument_types,
            span,
        ))
    } else {
        None
    }
}

pub fn lower_anonymous_call_epilogue<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    callsite: CallSiteValue<'llvm_abi>,
    ty: &'llvm_abi Type,
    args_type: &'llvm_abi [Type],
    codegen_location: LLVMABICodeGenLocation,
) -> bool {
    match abi {
        LLVMABIRepresentation::SystemVABI { .. } => true,
        LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_nvidia_cuda_abi::CudaABIContext<'_> =
                thrustc_llvm_nvidia_cuda_abi::CudaABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_nvidia_cuda(),
                );

            let configuration: CudaABIAnonymousCallType<'_> =
                CudaABIAnonymousCallType::new(ty, args_type);

            thrustc_llvm_nvidia_cuda_abi::lower_anonymous_call_epilogue(
                &mut abi_context,
                llvm_context,
                callsite,
                &configuration,
            )
        }

        _ => false,
    }
}

pub fn lower_function_parameters<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    codegen_location: LLVMABICodeGenLocation,
) -> Option<Vec<LLVMABIFunctionLoweredParameter<'llvm_abi>>> {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let configuration: &SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            let lowered_parameters: Vec<(
                &'llvm_abi str,
                &'llvm_abi str,
                &'llvm_abi Type,
                SystemVABIFunctionParameterConfiguration,
                BasicValueEnum<'_>,
            )> = thrustc_llvm_system_v_abi::lower_function_parameters(
                llvm_builder,
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );

            let transformed_lowered_parameters: Vec<LLVMABIFunctionLoweredParameter> =
                lowered_parameters
                    .iter()
                    .map(|lowered_parameter| {
                        let (name, ascii_name, ty, parameter_configuration, value) =
                            lowered_parameter;

                        let storage = match parameter_configuration {
                            SystemVABIFunctionParameterConfiguration::Normal => {
                                LLVMABIFunctionParameterStorage::Value
                            }
                            SystemVABIFunctionParameterConfiguration::FromMemory => {
                                LLVMABIFunctionParameterStorage::Address
                            }
                        };

                        LLVMABIFunctionLoweredParameter::new(name, ascii_name, ty, *value, storage)
                    })
                    .collect();

            Some(transformed_lowered_parameters)
        }

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
                thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                    file,
                    options,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_webassembly(),
                );

            let configuration = match configuration {
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                    configuration
                }
                _ => unreachable!(),
            };
            let lowered = thrustc_llvm_webassembly_abi::lower_function_parameters(
                llvm_builder,
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );

            Some(
                lowered
                    .into_iter()
                    .map(|(name, ascii_name, ty, parameter_configuration, value)| {
                        let storage = match parameter_configuration {
                            thrustc_llvm_webassembly_abi::WebAssemblyABIFunctionParameterConfiguration::Value => LLVMABIFunctionParameterStorage::Value,
                            thrustc_llvm_webassembly_abi::WebAssemblyABIFunctionParameterConfiguration::Address => LLVMABIFunctionParameterStorage::Address,
                        };

                        LLVMABIFunctionLoweredParameter::new(
                            name, ascii_name, ty, value, storage,
                        )
                    })
                    .collect(),
            )
        }

        _ => None,
    }
}

pub fn lower_terminator<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    return_value: Option<BasicValueEnum<'llvm_abi>>,
    codegen_location: LLVMABICodeGenLocation,
    span: Span,
) -> bool {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_data,
            target_info,
            target_triple,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let configuration: &SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            thrustc_llvm_system_v_abi::lower_function_terminator(
                llvm_context,
                llvm_builder,
                &mut abi_context,
                configuration,
                function_value,
                return_value,
                span,
            )
        }

        LLVMABIRepresentation::CudaABI { .. } => false,

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
                thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                    file,
                    options,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_webassembly(),
                );

            let configuration = match configuration {
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                    configuration
                }
                _ => unreachable!(),
            };

            thrustc_llvm_webassembly_abi::lower_function_terminator(
                llvm_context,
                llvm_builder,
                &mut abi_context,
                configuration,
                function_value,
                return_value,
                span,
            )
        }

        _ => false,
    }
}

pub fn lower_terminator_conventions<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    configuration: &LLVMABIConfiguration<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
) -> bool {
    match abi {
        LLVMABIRepresentation::SystemVABI { .. } => true,

        LLVMABIRepresentation::CudaABI { .. } => {
            let configuration: &CudaABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::CudaFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            thrustc_llvm_nvidia_cuda_abi::lower_terminator_conventions(
                llvm_context,
                function_value,
                configuration,
            );

            true
        }

        LLVMABIRepresentation::WebAssemblyABI { .. } => true,

        _ => false,
    }
}

pub fn lower_parameter_conventions<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi: &LLVMABIRepresentation<'llvm_abi>,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &LLVMABIConfiguration,
    codegen_location: LLVMABICodeGenLocation,
) -> bool {
    match abi {
        LLVMABIRepresentation::SystemVABI {
            file,
            options,
            target_data,
            target_info,
            target_triple,
        } => {
            let mut abi_context: thrustc_llvm_system_v_abi::SystemVABIContext =
                thrustc_llvm_system_v_abi::SystemVABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_system_v(),
                );

            let configuration: &SystemVABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::SystemVFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            thrustc_llvm_system_v_abi::lower_function_parameter_conventions(
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );

            true
        }

        LLVMABIRepresentation::CudaABI {
            file,
            options,
            target_triple,
            target_info,
            target_data,
        } => {
            let mut abi_context: thrustc_llvm_nvidia_cuda_abi::CudaABIContext<'_> =
                thrustc_llvm_nvidia_cuda_abi::CudaABIContext::new(
                    file,
                    options,
                    target_triple,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_nvidia_cuda(),
                );

            let configuration: &CudaABIFunctionTypeConfiguration = match configuration {
                LLVMABIConfiguration::CudaFunctionTypeConfiguration(config) => config,
                _ => unreachable!(),
            };

            thrustc_llvm_nvidia_cuda_abi::lower_function_parameter_conventions(
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );

            true
        }

        LLVMABIRepresentation::WebAssemblyABI {
            file,
            options,
            target_info,
            target_data,
            ..
        } => {
            let mut abi_context: thrustc_llvm_webassembly_abi::WebAssemblyABIContext<'_> =
                thrustc_llvm_webassembly_abi::WebAssemblyABIContext::new(
                    file,
                    options,
                    (*target_info).clone(),
                    target_data,
                    codegen_location.to_webassembly(),
                );

            let configuration = match configuration {
                LLVMABIConfiguration::WebAssemblyFunctionTypeConfiguration(configuration) => {
                    configuration
                }
                _ => unreachable!(),
            };

            thrustc_llvm_webassembly_abi::lower_function_conventions(
                llvm_context,
                &mut abi_context,
                function_value,
                configuration,
            );
            true
        }

        _ => false,
    }
}

impl<'llvm_abi> LLVMABIFunctionLoweredParameter<'llvm_abi> {
    pub fn new(
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        value: BasicValueEnum<'llvm_abi>,
        storage: LLVMABIFunctionParameterStorage,
    ) -> Self {
        Self {
            name,
            ascii_name,
            ty,
            value,
            storage,
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
    pub fn get_storage(&self) -> LLVMABIFunctionParameterStorage {
        self.storage
    }
}
