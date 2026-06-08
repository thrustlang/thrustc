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

mod abort;

use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    context::Context,
    targets::TargetData,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue},
};
use thrustc_ast::Ast;
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeIsExtensions, TypePointerExtensions},
    type_layout::TargetInfo,
};

#[derive(Debug)]
pub struct CudaABIContext<'system_v_abi> {
    target_triple: &'system_v_abi LLVMTargetTriple,
    diagnostician: Diagnostician,
    target_info: TargetInfo,
    target_data: &'system_v_abi TargetData,
    codegen_location: CudaCodeGenLocation,
}

#[derive(Debug, Clone, Copy)]
pub enum CudaCodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

impl<'system_v_abi> CudaABIContext<'system_v_abi> {
    pub fn new(
        file: &CompilationUnit,
        options: &CompilerOptions,
        target_triple: &'system_v_abi LLVMTargetTriple,
        target_info: TargetInfo,
        target_data: &'system_v_abi TargetData,
        codegen_location: CudaCodeGenLocation,
    ) -> Self {
        Self {
            target_triple,
            diagnostician: Diagnostician::new(file, options),
            target_info,
            target_data,
            codegen_location,
        }
    }
}

impl CudaABIContext<'_> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        &mut self.target_info
    }
}

impl CudaABIContext<'_> {
    #[inline]
    pub fn get_target_data(&self) -> &TargetData {
        self.target_data
    }

    #[inline]
    pub fn get_codegen_location(&self) -> CudaCodeGenLocation {
        self.codegen_location
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CudaABIFunctionTypeArgumentConfigurationAttribute {
    ZeroExt,
    SignExt,

    None,
}

#[derive(Debug, Clone, Copy)]
pub struct CudaABIFunctionTypeArgumentConfiguration<'llvm_abi> {
    name: &'llvm_abi str,
    ty: &'llvm_abi Type,
    attribute: CudaABIFunctionTypeArgumentConfigurationAttribute,
    index: usize,
}

impl CudaABIFunctionTypeArgumentConfiguration<'_> {
    #[inline]
    pub fn get_index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Clone)]
pub struct CudaABIFunctionTypeConfiguration<'llvm_abi> {
    parameter_types: Vec<CudaABIFunctionTypeArgumentConfiguration<'llvm_abi>>,
    is_variatic: bool,
}

impl<'llvm_abi> CudaABIFunctionTypeConfiguration<'llvm_abi> {
    pub fn new(is_variatic: bool) -> Self {
        Self {
            parameter_types: Vec::new(),
            is_variatic,
        }
    }
}

impl<'llvm_abi> CudaABIFunctionTypeConfiguration<'llvm_abi> {
    pub fn set_parameter_types_configuration(
        &mut self,
        parameter_types: Vec<CudaABIFunctionTypeArgumentConfiguration<'llvm_abi>>,
    ) {
        self.parameter_types = parameter_types;
    }
}

pub fn generate_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut CudaABIContext,
    return_type: &'llvm_abi Type,
    parameters: &'llvm_abi [Ast<'llvm_abi>],
    is_variatic: bool,
) -> (
    FunctionType<'llvm_abi>,
    CudaABIFunctionTypeConfiguration<'llvm_abi>,
) {
    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum<'llvm_abi>> =
        Vec::with_capacity(parameters.len());

    let mut configuration: CudaABIFunctionTypeConfiguration =
        CudaABIFunctionTypeConfiguration::new(is_variatic);

    let mut configuration_parameter_types: Vec<CudaABIFunctionTypeArgumentConfiguration> =
        Vec::with_capacity(parameters.len());

    for (idx, parameter) in parameters.iter().enumerate() {
        match parameter {
            Ast::FunctionParameter { name, kind: ty, .. } => {
                let llvm_ty: BasicTypeEnum<'_> = self::generate_type(llvm_context, abi_context, ty);

                let is_signed_integer_value: bool = ty.is_signed_integer_type();
                let is_unsigned_integer_value: bool = ty.is_unsigned_integer_type();

                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(r#ty);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                let ty_width: u32 = layout.width;

                let attribute: CudaABIFunctionTypeArgumentConfigurationAttribute = {
                    if ty_width <= 32 {
                        if is_signed_integer_value {
                            CudaABIFunctionTypeArgumentConfigurationAttribute::SignExt
                        } else if is_unsigned_integer_value {
                            CudaABIFunctionTypeArgumentConfigurationAttribute::ZeroExt
                        } else {
                            CudaABIFunctionTypeArgumentConfigurationAttribute::None
                        }
                    } else {
                        CudaABIFunctionTypeArgumentConfigurationAttribute::None
                    }
                };

                configuration_parameter_types.push(CudaABIFunctionTypeArgumentConfiguration {
                    name,
                    ty,
                    attribute,
                    index: idx,
                });

                llvm_parameters_types.push(llvm_ty.into());
            }

            Ast::AssemblerFunctionParameter { .. } => (),
            Ast::IntrinsicParameter { .. } => (),

            _ => (),
        }
    }

    if return_type.is_void_type() {
        configuration.set_parameter_types_configuration(configuration_parameter_types);

        (
            llvm_context
                .void_type()
                .fn_type(&llvm_parameters_types, is_variatic),
            configuration,
        )
    } else {
        configuration.set_parameter_types_configuration(configuration_parameter_types);

        let llvm_return_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, return_type);

        (
            llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
            configuration,
        )
    }
}

pub fn lower_function_parameter_conventions<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut CudaABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &CudaABIFunctionTypeConfiguration,
) {
    let function_parameters: Vec<BasicValueEnum<'_>> = function_value.get_params();

    let ordered_configurations: Vec<&CudaABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let _ = ordered_configurations.is_sorted_by_key(|config| config.get_index());

    for parameter_configuration in ordered_configurations.iter() {
        let parameter_attribute: CudaABIFunctionTypeArgumentConfigurationAttribute =
            parameter_configuration.attribute;
        let parameter_ty: &Type = parameter_configuration.ty;
        let parameter_index: usize = parameter_configuration.get_index();

        if matches!(
            parameter_attribute,
            CudaABIFunctionTypeArgumentConfigurationAttribute::ZeroExt
        ) {
            let Some(_) = function_parameters.get(parameter_index) else {
                abort::abort_codegen(
                    abi_context,
                    "Failed to get the function parameter value from the function declaration for ABI lowering!",
                    parameter_ty.get_span(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            };

            let zeroext_id: u32 = Attribute::get_named_enum_kind_id("zeroext");
            let zeroext_attribute: Attribute = llvm_context.create_enum_attribute(zeroext_id, 0);

            function_value.add_attribute(
                AttributeLoc::Param((parameter_index).try_into().unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to parse parameter index on Cuda ABI lowering!",
                        parameter_ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })),
                zeroext_attribute,
            );
        }

        if matches!(
            parameter_attribute,
            CudaABIFunctionTypeArgumentConfigurationAttribute::SignExt
        ) {
            let Some(_) = function_parameters.get(parameter_index) else {
                abort::abort_codegen(
                    abi_context,
                    "Failed to get the function parameter value from the function declaration for ABI lowering!",
                    parameter_ty.get_span(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            };

            let zeroext_id: u32 = Attribute::get_named_enum_kind_id("signext");
            let signext_attribute: Attribute = llvm_context.create_enum_attribute(zeroext_id, 0);

            function_value.add_attribute(
                AttributeLoc::Param((parameter_index).try_into().unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to parse parameter index on Cuda ABI lowering!",
                        parameter_ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })),
                signext_attribute,
            );
        }
    }
}

pub fn generate_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut CudaABIContext,
    ty: &Type,
) -> BasicTypeEnum<'llvm_abi> {
    match ty {
        t if t.is_integer_type() || t.is_char_type() || t.is_bool_type() => match t {
            Type::S8 { .. } | Type::U8 { .. } | Type::Char(..) => llvm_context.i8_type().into(),
            Type::S16 { .. } | Type::U16 { .. } => llvm_context.i16_type().into(),
            Type::S32 { .. } | Type::U32 { .. } => llvm_context.i32_type().into(),
            Type::S64 { .. } | Type::U64 { .. } => llvm_context.i64_type().into(),
            Type::U128 { .. } => llvm_context.i128_type().into(),
            Type::USize { .. } | Type::SSize { .. } => llvm_context
                .ptr_sized_int_type(abi_context.get_target_data(), None)
                .into(),

            Type::Bool { .. } => llvm_context.bool_type().into(),
            Type::Const(subtype, ..) => self::generate_type(llvm_context, abi_context, subtype),

            any => abort::abort_codegen(
                abi_context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        },

        t if t.is_float_type() => match t {
            Type::F32 { .. } => llvm_context.f32_type().into(),
            Type::F64 { .. } => llvm_context.f64_type().into(),
            Type::F128 { .. } => llvm_context.f128_type().into(),
            Type::FX8680 { .. } => llvm_context.x86_f80_type().into(),
            Type::FPPC128 { .. } => llvm_context.ppc_f128_type().into(),

            Type::Const(subtype, ..) => self::generate_type(llvm_context, abi_context, subtype),

            any => abort::abort_codegen(
                abi_context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        },

        Type::Array {
            infered_type: Some((infered_type, ..)),
            ..
        } => self::generate_type(llvm_context, abi_context, infered_type),

        t if t.is_ptr_type() => {
            if let Type::Ptr {
                address_space: Some(address_space),
                ..
            } = t
            {
                llvm_context
                    .ptr_type(AddressSpace::from(*address_space))
                    .into()
            } else {
                llvm_context.ptr_type(AddressSpace::default()).into()
            }
        }

        t if t.is_ptr_like_type() => llvm_context.ptr_type(AddressSpace::default()).into(),

        Type::Const(subtype, ..) => self::generate_type(llvm_context, abi_context, subtype),

        Type::Struct {
            fields, modifier, ..
        } => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::with_capacity(u8::MAX as usize);

            let packed: bool = modifier.llvm().is_packed();

            {
                for ty in fields.iter() {
                    field_types.push(self::generate_type(llvm_context, abi_context, ty));
                }
            }

            llvm_context.struct_type(&field_types, packed).into()
        }

        Type::FixedArray {
            base_type, size, ..
        } => {
            let array_type: BasicTypeEnum =
                self::generate_type(llvm_context, abi_context, base_type);
            array_type.array_type(*size).into()
        }

        any => abort::abort_codegen(
            abi_context,
            &format!("Failed to compile '{}' as a type!", any),
            any.get_span(),
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}
