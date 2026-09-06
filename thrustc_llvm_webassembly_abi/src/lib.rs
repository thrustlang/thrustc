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

#![allow(clippy::too_many_arguments)]

use either::Either;
use inkwell::AddressSpace;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, PointerValue,
};
use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::{TypeCodeLocation, TypeIsExtensions};
use thrustc_typesystem::type_layout::{Layout, TargetInfo};
use thrustc_typesystem::type_modificators::StructureTypeModificator;

mod abort;

#[derive(Debug, Clone, Copy)]
pub enum WebAssemblyCodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

#[derive(Debug, Clone)]
pub enum WebAssemblyABIType<'abi> {
    Direct(&'abi Type),
    DirectAggregate {
        aggregate: &'abi Type,
        scalar: &'abi Type,
    },
    Indirect(&'abi Type),
    Ignore(&'abi Type),
}

#[derive(Debug, Clone)]
pub struct WebAssemblyABIParameterConfiguration<'abi> {
    pub name: &'abi str,
    pub ascii_name: &'abi str,

    pub ty: &'abi Type,
    pub passing: WebAssemblyABIType<'abi>,

    pub source_index: usize,
    pub llvm_index: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct WebAssemblyABIFunctionTypeConfiguration<'abi> {
    pub return_type: &'abi Type,
    pub return_passing: WebAssemblyABIType<'abi>,

    pub parameters: Vec<WebAssemblyABIParameterConfiguration<'abi>>,

    pub is_variadic: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum WebAssemblyABIFunctionParameterConfiguration {
    Value,

    Address,
}

#[derive(Debug)]
pub struct WebAssemblyABIContext<'abi> {
    target_info: TargetInfo,
    target_data: &'abi TargetData,

    codegen_location: WebAssemblyCodeGenLocation,

    diagnostician: Diagnostician,
}

impl<'abi> WebAssemblyABIContext<'abi> {
    pub fn new(
        file: &'abi CompilationUnit,
        options: &'abi CompilerOptions,
        target_info: TargetInfo,
        target_data: &'abi TargetData,
        codegen_location: WebAssemblyCodeGenLocation,
    ) -> Self {
        Self {
            target_info,
            target_data,
            codegen_location,
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'abi> WebAssemblyABIContext<'abi> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        &mut self.target_info
    }

    #[inline]
    pub fn get_target_data(&self) -> &TargetData {
        self.target_data
    }

    #[inline]
    pub fn get_codegen_location(&self) -> WebAssemblyCodeGenLocation {
        self.codegen_location
    }
}

pub fn generate_function_type<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    return_type: &'abi Type,
    parameters: &'abi [Ast<'abi>],
    is_variadic: bool,
) -> (
    FunctionType<'abi>,
    WebAssemblyABIFunctionTypeConfiguration<'abi>,
) {
    let mut source_parameters: Vec<(&str, &str, &Type)> = Vec::with_capacity(parameters.len());

    for parameter in parameters {
        if let Ast::FunctionParameter {
            name,
            ascii_name,
            kind,
            ..
        } = parameter
        {
            source_parameters.push((name, ascii_name, kind));
        }
    }

    self::generate_function_type_from_parts(
        llvm_context,
        abi_context,
        return_type,
        &source_parameters,
        is_variadic,
    )
}

pub fn generate_anonymous_function_type<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    return_type: &'abi Type,
    parameter_types: &'abi [Type],
    is_variadic: bool,
) -> (
    FunctionType<'abi>,
    WebAssemblyABIFunctionTypeConfiguration<'abi>,
) {
    let source_parameters: Vec<(&str, &str, &Type)> = parameter_types
        .iter()
        .map(|parameter| ("", "", parameter))
        .collect();

    self::generate_function_type_from_parts(
        llvm_context,
        abi_context,
        return_type,
        &source_parameters,
        is_variadic,
    )
}

pub fn lower_call_prologue<'abi>(
    llvm_builder: &'abi Builder<'abi>,
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
    args: Vec<BasicValueEnum<'abi>>,
    argument_types: &[Type],
    span: Span,
) -> Vec<BasicMetadataValueEnum<'abi>> {
    let mut lowered: Vec<BasicMetadataValueEnum<'abi>> =
        Vec::with_capacity(args.len().saturating_add(1));

    if matches!(
        configuration.return_passing,
        WebAssemblyABIType::Indirect(_)
    ) {
        let llvm_type: BasicTypeEnum =
            self::generate_type(llvm_context, abi_context, configuration.return_type);
        let buffer: PointerValue = llvm_builder
            .build_alloca(llvm_type, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to allocate the WebAssembly ABI return buffer.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        lowered.push(buffer.into());
    }

    for parameter in &configuration.parameters {
        let value: BasicValueEnum = *args.get(parameter.source_index).unwrap_or_else(|| {
            abort::abort_codegen(
                abi_context,
                "Failed to get a WebAssembly ABI call argument.",
                parameter.ty.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        match &parameter.passing {
            WebAssemblyABIType::Direct(_) => lowered.push(value.into()),
            WebAssemblyABIType::Ignore(_) => {}
            WebAssemblyABIType::Indirect(ty) => {
                if value.is_pointer_value() {
                    lowered.push(value.into());
                } else {
                    let llvm_type: BasicTypeEnum =
                        self::generate_type(llvm_context, abi_context, ty);
                    let buffer: PointerValue = llvm_builder
                        .build_alloca(llvm_type, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to allocate an indirect WebAssembly ABI argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    llvm_builder.build_store(buffer, value).unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store an indirect WebAssembly ABI argument.",
                            ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    lowered.push(buffer.into());
                }
            }
            WebAssemblyABIType::DirectAggregate { aggregate, scalar } => {
                let aggregate_type: BasicTypeEnum =
                    self::generate_type(llvm_context, abi_context, aggregate);
                let scalar_type: BasicTypeEnum =
                    self::generate_type(llvm_context, abi_context, scalar);
                let alignment: u32 =
                    match abi_context.get_mut_target_info().get_type_layout(aggregate) {
                        Either::Left(layout) => layout.alignof.max(1),
                        Either::Right(layout) => layout.alignof.max(1),
                    };
                let buffer: PointerValue = llvm_builder
                    .build_alloca(aggregate_type, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate a WebAssembly ABI singleton argument.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                llvm_builder
                    .build_store(buffer, value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store a WebAssembly ABI singleton argument.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton argument.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                let scalar_value: BasicValueEnum = llvm_builder
                    .build_load(scalar_type, buffer, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to extract a WebAssembly ABI singleton argument.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                scalar_value
                    .as_instruction_value()
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a WebAssembly ABI singleton load.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton load.",
                            aggregate.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                lowered.push(scalar_value.into());
            }
        }
    }

    for (index, value) in args.iter().enumerate().skip(configuration.parameters.len()) {
        let ty: &Type = argument_types.get(index).unwrap_or_else(|| {
            abort::abort_codegen(
                abi_context,
                "Failed to get a variadic WebAssembly ABI argument type.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        match self::classify_type(abi_context, ty) {
            WebAssemblyABIType::Indirect(indirect_type)
            | WebAssemblyABIType::DirectAggregate {
                aggregate: indirect_type,
                ..
            } => {
                if value.is_pointer_value() {
                    lowered.push((*value).into());
                } else {
                    let llvm_type: BasicTypeEnum =
                        self::generate_type(llvm_context, abi_context, indirect_type);
                    let buffer: PointerValue = llvm_builder
                        .build_alloca(llvm_type, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to allocate a variadic WebAssembly ABI argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                    llvm_builder
                        .build_store(buffer, *value)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to store a variadic WebAssembly ABI argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                    lowered.push(buffer.into());
                }
            }

            WebAssemblyABIType::Ignore(_) => {}

            WebAssemblyABIType::Direct(_) => {
                let layout: Layout = match abi_context.get_mut_target_info().get_type_layout(ty) {
                    Either::Left(layout) => layout.into_layout(),
                    Either::Right(layout) => layout.into_layout(),
                };

                let promoted: BasicValueEnum = if layout.width <= 16 && ty.is_signed_integer_type()
                {
                    llvm_builder
                        .build_int_s_extend(value.into_int_value(), llvm_context.i32_type(), "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to sign-extend a variadic WebAssembly ABI argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                } else if layout.width <= 16
                    && (ty.is_unsigned_integer_type() || ty.is_bool_type() || ty.is_char_type())
                {
                    llvm_builder
                        .build_int_z_extend(value.into_int_value(), llvm_context.i32_type(), "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to zero-extend a variadic WebAssembly ABI argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                } else if matches!(ty, Type::F32 { .. }) {
                    llvm_builder
                        .build_float_ext(value.into_float_value(), llvm_context.f64_type(), "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to promote a variadic WebAssembly ABI float argument.",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .into()
                } else {
                    *value
                };

                lowered.push(promoted.into());
            }
        }
    }

    lowered
}

pub fn lower_call_epilogue<'abi>(
    llvm_builder: &'abi Builder<'abi>,
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    callsite: CallSiteValue<'abi>,
    lowered_args: &[BasicMetadataValueEnum<'abi>],
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
    span: Span,
) -> Option<BasicValueEnum<'abi>> {
    match &configuration.return_passing {
        WebAssemblyABIType::Ignore(ty) => {
            if matches!(ty, Type::Void { .. }) {
                None
            } else {
                Some(self::generate_type(llvm_context, abi_context, ty).const_zero())
            }
        }
        WebAssemblyABIType::Direct(_) => callsite.try_as_basic_value().left(),
        WebAssemblyABIType::Indirect(ty) => {
            let buffer: PointerValue = lowered_args
                .first()
                .and_then(|value| BasicValueEnum::try_from(*value).ok())
                .filter(|value| value.is_pointer_value())
                .map(BasicValueEnum::into_pointer_value)
                .unwrap_or_else(|| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the WebAssembly ABI return buffer.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            if matches!(
                abi_context.get_codegen_location(),
                WebAssemblyCodeGenLocation::LValue
            ) {
                Some(buffer.into())
            } else {
                let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);
                Some(
                    llvm_builder
                        .build_load(llvm_type, buffer, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to load the WebAssembly ABI return buffer.",
                                span,
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        }),
                )
            }
        }
        WebAssemblyABIType::DirectAggregate { aggregate, .. } => {
            let scalar_value: BasicValueEnum =
                callsite.try_as_basic_value().left().unwrap_or_else(|| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get a WebAssembly ABI singleton return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            let aggregate_type: BasicTypeEnum =
                self::generate_type(llvm_context, abi_context, aggregate);

            let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(aggregate)
            {
                Either::Left(layout) => layout.alignof.max(1),
                Either::Right(layout) => layout.alignof.max(1),
            };

            let buffer: PointerValue = llvm_builder
                .build_alloca(aggregate_type, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to allocate a WebAssembly ABI singleton return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            llvm_builder
                .build_store(buffer, aggregate_type.const_zero())
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to initialize a WebAssembly ABI singleton return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
                .set_alignment(alignment)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to align a WebAssembly ABI singleton initialization.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            llvm_builder
                .build_store(buffer, scalar_value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to reconstruct a WebAssembly ABI singleton return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
                .set_alignment(alignment)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to align a WebAssembly ABI singleton reconstruction.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            if matches!(
                abi_context.get_codegen_location(),
                WebAssemblyCodeGenLocation::LValue
            ) {
                Some(buffer.into())
            } else {
                let value = llvm_builder
                    .build_load(aggregate_type, buffer, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a WebAssembly ABI singleton return value.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                value
                    .as_instruction_value()
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a WebAssembly ABI singleton return load.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton return load.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                Some(value)
            }
        }
    }
}

pub fn lower_function_parameters<'abi>(
    llvm_builder: &'abi Builder<'abi>,
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    function_value: FunctionValue<'abi>,
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
) -> Vec<(
    &'abi str,
    &'abi str,
    &'abi Type,
    WebAssemblyABIFunctionParameterConfiguration,
    BasicValueEnum<'abi>,
)> {
    let llvm_parameters: Vec<BasicValueEnum> = function_value.get_params();
    let mut lowered: Vec<(
        &str,
        &str,
        &Type,
        WebAssemblyABIFunctionParameterConfiguration,
        BasicValueEnum<'_>,
    )> = Vec::with_capacity(configuration.parameters.len());

    for parameter in &configuration.parameters {
        match &parameter.passing {
            WebAssemblyABIType::Direct(_) => {
                let value: BasicValueEnum = *llvm_parameters
                    .get(parameter.llvm_index.unwrap_or_default() as usize)
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a direct WebAssembly ABI parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                lowered.push((
                    parameter.name,
                    parameter.ascii_name,
                    parameter.ty,
                    WebAssemblyABIFunctionParameterConfiguration::Value,
                    value,
                ));
            }
            WebAssemblyABIType::Indirect(_) => {
                let value: BasicValueEnum = *llvm_parameters
                    .get(parameter.llvm_index.unwrap_or_default() as usize)
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get an indirect WebAssembly ABI parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                lowered.push((
                    parameter.name,
                    parameter.ascii_name,
                    parameter.ty,
                    WebAssemblyABIFunctionParameterConfiguration::Address,
                    value,
                ));
            }
            WebAssemblyABIType::Ignore(ty) => lowered.push((
                parameter.name,
                parameter.ascii_name,
                parameter.ty,
                WebAssemblyABIFunctionParameterConfiguration::Value,
                self::generate_type(llvm_context, abi_context, ty).const_zero(),
            )),

            WebAssemblyABIType::DirectAggregate { aggregate, .. } => {
                let scalar_value: BasicValueEnum = *llvm_parameters
                    .get(parameter.llvm_index.unwrap_or_default() as usize)
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a WebAssembly ABI singleton parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let aggregate_type: BasicTypeEnum =
                    self::generate_type(llvm_context, abi_context, aggregate);

                let alignment: u32 =
                    match abi_context.get_mut_target_info().get_type_layout(aggregate) {
                        Either::Left(layout) => layout.alignof.max(1),
                        Either::Right(layout) => layout.alignof.max(1),
                    };

                let buffer: PointerValue = llvm_builder
                    .build_alloca(aggregate_type, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate a WebAssembly ABI singleton parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                llvm_builder
                    .build_store(buffer, aggregate_type.const_zero())
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to initialize a WebAssembly ABI singleton parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton parameter initialization.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                llvm_builder
                    .build_store(buffer, scalar_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to reconstruct a WebAssembly ABI singleton parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton parameter reconstruction.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let aggregate_value: BasicValueEnum = llvm_builder
                    .build_load(aggregate_type, buffer, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a WebAssembly ABI singleton parameter.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                aggregate_value
                    .as_instruction_value()
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a WebAssembly ABI singleton parameter load.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton parameter load.",
                            parameter.ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                lowered.push((
                    parameter.name,
                    parameter.ascii_name,
                    parameter.ty,
                    WebAssemblyABIFunctionParameterConfiguration::Value,
                    aggregate_value,
                ));
            }
        }
    }

    lowered
}

pub fn lower_function_terminator<'abi>(
    llvm_context: &'abi Context,
    llvm_builder: &'abi Builder<'abi>,
    abi_context: &mut WebAssemblyABIContext,
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
    function_value: FunctionValue<'abi>,
    return_value: Option<BasicValueEnum<'abi>>,
    span: Span,
) -> bool {
    match &configuration.return_passing {
        WebAssemblyABIType::Direct(_) => false,
        WebAssemblyABIType::Ignore(_) => {
            llvm_builder.build_return(None).unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to emit an ignored WebAssembly ABI return.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            true
        }
        WebAssemblyABIType::Indirect(ty) => {
            let destination: PointerValue = function_value
                .get_first_param()
                .filter(|value| value.is_pointer_value())
                .map(BasicValueEnum::into_pointer_value)
                .unwrap_or_else(|| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the WebAssembly ABI sret parameter.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            let mut value: BasicValueEnum = return_value.unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Missing an indirect WebAssembly ABI return value.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            if value.is_pointer_value() {
                let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);

                value = llvm_builder
                    .build_load(llvm_type, value.into_pointer_value(), "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load an indirect WebAssembly ABI return value.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
            }

            llvm_builder
                .build_store(destination, value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to store an indirect WebAssembly ABI return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            llvm_builder.build_return(None).unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to emit an indirect WebAssembly ABI return.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            true
        }
        WebAssemblyABIType::DirectAggregate { aggregate, scalar } => {
            let mut aggregate_value: BasicValueEnum = return_value.unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Missing a WebAssembly ABI singleton return value.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            let aggregate_type: BasicTypeEnum =
                self::generate_type(llvm_context, abi_context, aggregate);

            let scalar_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, scalar);

            let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(aggregate)
            {
                Either::Left(layout) => layout.alignof.max(1),
                Either::Right(layout) => layout.alignof.max(1),
            };

            let buffer: PointerValue = if aggregate_value.is_pointer_value() {
                aggregate_value.into_pointer_value()
            } else {
                let buffer: PointerValue = llvm_builder
                    .build_alloca(aggregate_type, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate a WebAssembly ABI singleton return value.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                llvm_builder
                    .build_store(buffer, aggregate_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store a WebAssembly ABI singleton return value.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to align a WebAssembly ABI singleton return store.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
                buffer
            };

            aggregate_value = llvm_builder
                .build_load(scalar_type, buffer, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to extract a WebAssembly ABI singleton return value.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            aggregate_value
                .as_instruction_value()
                .unwrap_or_else(|| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get a WebAssembly ABI singleton return extraction.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
                .set_alignment(alignment)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to align a WebAssembly ABI singleton return extraction.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            llvm_builder
                .build_return(Some(&aggregate_value))
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to emit a WebAssembly ABI singleton return.",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });
            true
        }
    }
}

pub fn lower_function_conventions<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    function_value: FunctionValue<'abi>,
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
) {
    if let WebAssemblyABIType::Indirect(ty) = &configuration.return_passing {
        let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);
        let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(ty) {
            Either::Left(layout) => layout.alignof.max(1),
            Either::Right(layout) => layout.alignof.max(1),
        };

        let sret_id: u32 = Attribute::get_named_enum_kind_id("sret");
        let sret: Attribute =
            llvm_context.create_type_attribute(sret_id, llvm_type.as_any_type_enum());

        function_value.add_attribute(AttributeLoc::Param(0), sret);
        function_value.set_param_alignment(0, alignment);
    } else {
        let attribute_type: Option<&Type> = match &configuration.return_passing {
            WebAssemblyABIType::Direct(ty) => Some(ty),
            WebAssemblyABIType::DirectAggregate { scalar, .. } => Some(scalar),
            _ => None,
        };

        if let Some(ty) = attribute_type {
            let layout: Layout = match abi_context.get_mut_target_info().get_type_layout(ty) {
                Either::Left(layout) => layout.into_layout(),
                Either::Right(layout) => layout.into_layout(),
            };

            if layout.width <= 16 && ty.is_signed_integer_type() {
                let signext_id: u32 = Attribute::get_named_enum_kind_id("signext");

                function_value.add_attribute(
                    AttributeLoc::Return,
                    llvm_context.create_enum_attribute(signext_id, 0),
                );
            } else if layout.width <= 16
                && (ty.is_unsigned_integer_type() || ty.is_bool_type() || ty.is_char_type())
            {
                let zeroext_id: u32 = Attribute::get_named_enum_kind_id("zeroext");

                function_value.add_attribute(
                    AttributeLoc::Return,
                    llvm_context.create_enum_attribute(zeroext_id, 0),
                );
            }
        }
    }

    for parameter in &configuration.parameters {
        let Some(index) = parameter.llvm_index else {
            continue;
        };

        match &parameter.passing {
            WebAssemblyABIType::Indirect(ty) => {
                let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);
                let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(ty) {
                    Either::Left(layout) => layout.alignof.max(1),
                    Either::Right(layout) => layout.alignof.max(1),
                };

                let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");
                let byval: Attribute =
                    llvm_context.create_type_attribute(byval_id, llvm_type.as_any_type_enum());

                function_value.add_attribute(AttributeLoc::Param(index), byval);
                function_value.set_param_alignment(index, alignment);
            }

            WebAssemblyABIType::Direct(ty)
            | WebAssemblyABIType::DirectAggregate { scalar: ty, .. } => {
                let layout: Layout = match abi_context.get_mut_target_info().get_type_layout(ty) {
                    Either::Left(layout) => layout.into_layout(),
                    Either::Right(layout) => layout.into_layout(),
                };

                if layout.width <= 16 && ty.is_signed_integer_type() {
                    let signext_id: u32 = Attribute::get_named_enum_kind_id("signext");

                    function_value.add_attribute(
                        AttributeLoc::Param(index),
                        llvm_context.create_enum_attribute(signext_id, 0),
                    );
                } else if layout.width <= 16
                    && (ty.is_unsigned_integer_type() || ty.is_bool_type() || ty.is_char_type())
                {
                    let zeroext_id: u32 = Attribute::get_named_enum_kind_id("zeroext");

                    function_value.add_attribute(
                        AttributeLoc::Param(index),
                        llvm_context.create_enum_attribute(zeroext_id, 0),
                    );
                }
            }

            WebAssemblyABIType::Ignore(_) => {}
        }
    }
}

pub fn lower_call_conventions<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    callsite: CallSiteValue<'abi>,
    configuration: &WebAssemblyABIFunctionTypeConfiguration<'abi>,
    argument_types: &[Type],
) {
    let return_attribute_type: Option<&Type> = match &configuration.return_passing {
        WebAssemblyABIType::Direct(ty) => Some(ty),
        WebAssemblyABIType::DirectAggregate { scalar, .. } => Some(scalar),
        _ => None,
    };

    if let Some(ty) = return_attribute_type {
        let layout: Layout = match abi_context.get_mut_target_info().get_type_layout(ty) {
            Either::Left(layout) => layout.into_layout(),
            Either::Right(layout) => layout.into_layout(),
        };

        if layout.width <= 16 && ty.is_signed_integer_type() {
            let signext_id: u32 = Attribute::get_named_enum_kind_id("signext");

            callsite.add_attribute(
                AttributeLoc::Return,
                llvm_context.create_enum_attribute(signext_id, 0),
            );
        } else if layout.width <= 16
            && (ty.is_unsigned_integer_type() || ty.is_bool_type() || ty.is_char_type())
        {
            let zeroext_id: u32 = Attribute::get_named_enum_kind_id("zeroext");

            callsite.add_attribute(
                AttributeLoc::Return,
                llvm_context.create_enum_attribute(zeroext_id, 0),
            );
        }
    }

    if let WebAssemblyABIType::Indirect(ty) = &configuration.return_passing {
        let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);
        let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(ty) {
            Either::Left(layout) => layout.alignof.max(1),
            Either::Right(layout) => layout.alignof.max(1),
        };

        let sret_id: u32 = Attribute::get_named_enum_kind_id("sret");

        callsite.add_attribute(
            AttributeLoc::Param(0),
            llvm_context.create_type_attribute(sret_id, llvm_type.as_any_type_enum()),
        );
        callsite.set_alignment_attribute(AttributeLoc::Param(0), alignment);
    }

    for parameter in &configuration.parameters {
        let Some(index) = parameter.llvm_index else {
            continue;
        };

        match &parameter.passing {
            WebAssemblyABIType::Indirect(ty) => {
                let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);
                let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(ty) {
                    Either::Left(layout) => layout.alignof.max(1),
                    Either::Right(layout) => layout.alignof.max(1),
                };

                let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                callsite.add_attribute(
                    AttributeLoc::Param(index),
                    llvm_context.create_type_attribute(byval_id, llvm_type.as_any_type_enum()),
                );

                callsite.set_alignment_attribute(AttributeLoc::Param(index), alignment);
            }

            WebAssemblyABIType::Direct(ty)
            | WebAssemblyABIType::DirectAggregate { scalar: ty, .. } => {
                let layout: Layout = match abi_context.get_mut_target_info().get_type_layout(ty) {
                    Either::Left(layout) => layout.into_layout(),
                    Either::Right(layout) => layout.into_layout(),
                };

                if layout.width <= 16 && ty.is_signed_integer_type() {
                    let signext_id: u32 = Attribute::get_named_enum_kind_id("signext");

                    callsite.add_attribute(
                        AttributeLoc::Param(index),
                        llvm_context.create_enum_attribute(signext_id, 0),
                    );
                } else if layout.width <= 16
                    && (ty.is_unsigned_integer_type() || ty.is_bool_type() || ty.is_char_type())
                {
                    let zeroext_id: u32 = Attribute::get_named_enum_kind_id("zeroext");

                    callsite.add_attribute(
                        AttributeLoc::Param(index),
                        llvm_context.create_enum_attribute(zeroext_id, 0),
                    );
                }
            }

            WebAssemblyABIType::Ignore(_) => {}
        }
    }

    let mut lowered_index: u32 = configuration
        .parameters
        .iter()
        .filter_map(|parameter| parameter.llvm_index)
        .max()
        .map_or_else(
            || {
                if matches!(
                    configuration.return_passing,
                    WebAssemblyABIType::Indirect(_)
                ) {
                    1
                } else {
                    0
                }
            },
            |index| index.saturating_add(1),
        );

    for ty in argument_types.iter().skip(configuration.parameters.len()) {
        if matches!(
            self::classify_type(abi_context, ty),
            WebAssemblyABIType::Ignore(_)
        ) {
            continue;
        }

        if matches!(
            self::classify_type(abi_context, ty),
            WebAssemblyABIType::Indirect(_) | WebAssemblyABIType::DirectAggregate { .. }
        ) {
            let llvm_type: BasicTypeEnum = self::generate_type(llvm_context, abi_context, ty);

            let alignment: u32 = match abi_context.get_mut_target_info().get_type_layout(ty) {
                Either::Left(layout) => layout.alignof.max(1),
                Either::Right(layout) => layout.alignof.max(1),
            };

            let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

            callsite.add_attribute(
                AttributeLoc::Param(lowered_index),
                llvm_context.create_type_attribute(byval_id, llvm_type.as_any_type_enum()),
            );

            callsite.set_alignment_attribute(AttributeLoc::Param(lowered_index), alignment);
        }

        lowered_index = lowered_index.saturating_add(1);
    }
}

fn generate_function_type_from_parts<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    return_type: &'abi Type,
    source_parameters: &[(&'abi str, &'abi str, &'abi Type)],
    is_variadic: bool,
) -> (
    FunctionType<'abi>,
    WebAssemblyABIFunctionTypeConfiguration<'abi>,
) {
    let return_passing: WebAssemblyABIType = self::classify_type(abi_context, return_type);

    let mut llvm_parameter_types: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(source_parameters.len().saturating_add(1));

    let mut llvm_index: u32 = 0;

    if matches!(return_passing, WebAssemblyABIType::Indirect(_)) {
        llvm_parameter_types.push(llvm_context.ptr_type(AddressSpace::default()).into());
        llvm_index = 1;
    }

    let mut parameters: Vec<WebAssemblyABIParameterConfiguration<'_>> =
        Vec::with_capacity(source_parameters.len());

    for (source_index, (name, ascii_name, ty)) in source_parameters.iter().enumerate() {
        let passing: WebAssemblyABIType = self::classify_type(abi_context, ty);

        let parameter_index: Option<u32> = match &passing {
            WebAssemblyABIType::Direct(direct_type) => {
                llvm_parameter_types
                    .push(self::generate_type(llvm_context, abi_context, direct_type).into());

                let index: u32 = llvm_index;

                llvm_index = llvm_index.saturating_add(1);

                Some(index)
            }

            WebAssemblyABIType::DirectAggregate { scalar, .. } => {
                llvm_parameter_types
                    .push(self::generate_type(llvm_context, abi_context, scalar).into());

                let index: u32 = llvm_index;

                llvm_index = llvm_index.saturating_add(1);

                Some(index)
            }

            WebAssemblyABIType::Indirect(_) => {
                llvm_parameter_types.push(llvm_context.ptr_type(AddressSpace::default()).into());

                let index: u32 = llvm_index;

                llvm_index = llvm_index.saturating_add(1);

                Some(index)
            }

            WebAssemblyABIType::Ignore(_) => None,
        };

        parameters.push(WebAssemblyABIParameterConfiguration {
            name,
            ascii_name,
            ty,
            passing,
            source_index,
            llvm_index: parameter_index,
        });
    }

    let function_type: FunctionType = match &return_passing {
        WebAssemblyABIType::Ignore(_) | WebAssemblyABIType::Indirect(_) => llvm_context
            .void_type()
            .fn_type(&llvm_parameter_types, is_variadic),
        WebAssemblyABIType::Direct(ty) => self::generate_type(llvm_context, abi_context, ty)
            .fn_type(&llvm_parameter_types, is_variadic),

        WebAssemblyABIType::DirectAggregate { scalar, .. } => {
            self::generate_type(llvm_context, abi_context, scalar)
                .fn_type(&llvm_parameter_types, is_variadic)
        }
    };

    (
        function_type,
        WebAssemblyABIFunctionTypeConfiguration {
            return_type,
            return_passing,
            parameters,
            is_variadic,
        },
    )
}

fn classify_type<'abi>(
    abi_context: &mut WebAssemblyABIContext,
    ty: &'abi Type,
) -> WebAssemblyABIType<'abi> {
    match ty {
        Type::Const(inner, _) => self::classify_type(abi_context, inner),

        Type::Void { .. } => WebAssemblyABIType::Ignore(ty),

        Type::Struct { .. } => match self::find_single_scalar(ty) {
            SingleScalar::Empty => WebAssemblyABIType::Ignore(ty),
            SingleScalar::Multiple => WebAssemblyABIType::Indirect(ty),

            SingleScalar::One(scalar) => {
                let aggregate_layout: Layout =
                    match abi_context.get_mut_target_info().get_type_layout(ty) {
                        Either::Left(layout) => layout.into_layout(),
                        Either::Right(layout) => layout.into_layout(),
                    };
                let scalar_layout: Layout =
                    match abi_context.get_mut_target_info().get_type_layout(scalar) {
                        Either::Left(layout) => layout.into_layout(),
                        Either::Right(layout) => layout.into_layout(),
                    };

                if aggregate_layout.abi_align > scalar_layout.abi_align {
                    WebAssemblyABIType::Indirect(ty)
                } else {
                    WebAssemblyABIType::DirectAggregate {
                        aggregate: ty,
                        scalar,
                    }
                }
            }
        },

        Type::FixedArray { .. } => WebAssemblyABIType::Indirect(ty),
        Type::Unresolved { .. } => WebAssemblyABIType::Ignore(ty),

        _ => WebAssemblyABIType::Direct(ty),
    }
}

#[derive(Debug)]
enum SingleScalar<'abi> {
    Empty,

    One(&'abi Type),

    Multiple,
}

fn find_single_scalar(ty: &Type) -> SingleScalar<'_> {
    match ty {
        Type::Const(inner, _) => self::find_single_scalar(inner),

        Type::Struct { fields, .. } => {
            let mut found: Option<&Type> = None;

            for field in fields {
                match self::find_single_scalar(field) {
                    SingleScalar::Empty => {}
                    SingleScalar::Multiple => return SingleScalar::Multiple,
                    SingleScalar::One(_) if found.is_some() => {
                        return SingleScalar::Multiple;
                    }
                    SingleScalar::One(scalar) => found = Some(scalar),
                }
            }

            found.map_or(SingleScalar::Empty, SingleScalar::One)
        }

        Type::FixedArray {
            base_type, size, ..
        } => {
            if *size == 0 {
                SingleScalar::Empty
            } else if *size == 1 {
                self::find_single_scalar(base_type)
            } else {
                match self::find_single_scalar(base_type) {
                    SingleScalar::Empty => SingleScalar::Empty,
                    _ => SingleScalar::Multiple,
                }
            }
        }

        Type::Void { .. } => SingleScalar::Empty,

        _ => SingleScalar::One(ty),
    }
}

fn generate_type<'abi>(
    llvm_context: &'abi Context,
    abi_context: &mut WebAssemblyABIContext,
    ty: &Type,
) -> BasicTypeEnum<'abi> {
    match ty {
        Type::S8 { .. } | Type::U8 { .. } | Type::Char { .. } => llvm_context.i8_type().into(),

        Type::S16 { .. } | Type::U16 { .. } => llvm_context.i16_type().into(),

        Type::S32 { .. } | Type::U32 { .. } => llvm_context.i32_type().into(),

        Type::S64 { .. } | Type::U64 { .. } => llvm_context.i64_type().into(),

        Type::U128 { .. } => llvm_context.i128_type().into(),

        Type::USize { .. } | Type::SSize { .. } => llvm_context
            .ptr_sized_int_type(abi_context.get_target_data(), None)
            .into(),

        Type::Bool { .. } => llvm_context.bool_type().into(),

        Type::F32 { .. } => llvm_context.f32_type().into(),

        Type::F64 { .. } => llvm_context.f64_type().into(),

        Type::F128 { .. } => llvm_context.f128_type().into(),

        Type::FX8680 { .. } => llvm_context.x86_f80_type().into(),

        Type::FPPC128 { .. } => llvm_context.ppc_f128_type().into(),

        Type::Array {
            infered_type: Some((infered_type, ..)),
            ..
        } => self::generate_type(llvm_context, abi_context, infered_type),

        Type::Ptr {
            address_space: Some(address_space),
            ..
        } => llvm_context
            .ptr_type(AddressSpace::from(*address_space))
            .into(),

        Type::Ptr { .. } | Type::Fn { .. } | Type::Array { .. } => {
            llvm_context.ptr_type(AddressSpace::default()).into()
        }

        Type::Const(inner, _) => self::generate_type(llvm_context, abi_context, inner),

        Type::Struct {
            fields, metadata, ..
        } => {
            let field_types: Vec<BasicTypeEnum> = fields
                .iter()
                .map(|field| self::generate_type(llvm_context, abi_context, field))
                .collect();
            let modifications: &StructureTypeModificator = metadata.get_struct_type_modificator();

            llvm_context
                .struct_type(&field_types, modifications.llvm().is_packed())
                .into()
        }

        Type::FixedArray {
            base_type, size, ..
        } => self::generate_type(llvm_context, abi_context, base_type)
            .array_type(*size)
            .into(),

        any => abort::abort_codegen(
            abi_context,
            &format!("Failed to compile '{}' as a WebAssembly ABI type.", any),
            any.get_span(),
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}
