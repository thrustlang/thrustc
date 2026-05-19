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

mod abort;

use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    builder::{Builder, BuilderError},
    context::Context,
    targets::TargetData,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue},
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
pub struct X86SystemVABIContext<'system_v_abi> {
    target_triple: &'system_v_abi LLVMTargetTriple,
    diagnostician: Diagnostician,
    target_info: TargetInfo,
    target_data: &'system_v_abi TargetData,
}

impl<'system_v_abi> X86SystemVABIContext<'system_v_abi> {
    pub fn new(
        file: &CompilationUnit,
        options: &CompilerOptions,
        target_triple: &'system_v_abi LLVMTargetTriple,
        target_info: TargetInfo,
        target_data: &'system_v_abi TargetData,
    ) -> Self {
        Self {
            target_triple,
            diagnostician: Diagnostician::new(file, options),
            target_info,
            target_data,
        }
    }
}

impl X86SystemVABIContext<'_> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        &mut self.target_info
    }
}

impl X86SystemVABIContext<'_> {
    #[inline]
    pub fn get_target_data(&self) -> &TargetData {
        &self.target_data
    }
}

// https://gitlab.com/x86-psABIs/x86-64-ABI - System V
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum X86SystemVABITypeClass {
    INTEGER,
    SSE,
    SSEUP,
    X87,
    X87UP,
    COMPLEX_X87,
    NO_CLASS,
    MEMORY,
}

impl std::fmt::Display for X86SystemVABITypeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class_str: &str = match self {
            X86SystemVABITypeClass::INTEGER => "INTEGER",
            X86SystemVABITypeClass::SSE => "SSE",
            X86SystemVABITypeClass::SSEUP => "SSEUP",
            X86SystemVABITypeClass::X87 => "X87",
            X86SystemVABITypeClass::X87UP => "X87UP",
            X86SystemVABITypeClass::COMPLEX_X87 => "COMPLEX_X87",
            X86SystemVABITypeClass::NO_CLASS => "NO_CLASS",
            X86SystemVABITypeClass::MEMORY => "MEMORY",
        };

        write!(f, "{}", class_str)
    }
}

pub const X86_SYSTEMV_ABI_TWO_INTEGERS: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_ONE_INTEGER: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_F32_F64: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::SSE,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_F128: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::SSE,
    X86SystemVABITypeClass::SSEUP,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_STACK: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::MEMORY,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

// https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig#L412

impl X86SystemVABITypeClass {
    pub fn combine(
        accum: X86SystemVABITypeClass,
        current: X86SystemVABITypeClass,
    ) -> X86SystemVABITypeClass {
        if accum == current {
            return accum;
        }

        if accum == X86SystemVABITypeClass::NO_CLASS {
            return current;
        }

        if accum == X86SystemVABITypeClass::MEMORY || current == X86SystemVABITypeClass::MEMORY {
            return X86SystemVABITypeClass::MEMORY;
        }

        if accum == X86SystemVABITypeClass::INTEGER || current == X86SystemVABITypeClass::INTEGER {
            return X86SystemVABITypeClass::INTEGER;
        }

        X86SystemVABITypeClass::SSE
    }

    pub fn get_system_v_type_class(
        abi_context: &mut X86SystemVABIContext,
        ty: &Type,
    ) -> [X86SystemVABITypeClass; 8] {
        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

        match ty {
            Type::Const(subtype, ..) => Self::get_system_v_type_class(abi_context, subtype),

            Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::Char(..)
            | Type::Bool(..) => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::SSize { .. } | Type::USize { .. }
                if layout.alignof == 8 || layout.alignof == 4 =>
            {
                X86_SYSTEM_V_ABI_ONE_INTEGER
            }

            Type::SSize { .. } | Type::USize { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::U128 { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::F32 { .. } | Type::F64 { .. } => X86_SYSTEM_V_ABI_F32_F64,

            Type::F128 { .. } => X86_SYSTEM_V_ABI_F128,

            t if t.is_ptr_like_type() => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::FixedArray(..) => {
                let abi_size: u32 = layout.abi_size;

                if abi_size <= 8 {
                    X86_SYSTEM_V_ABI_ONE_INTEGER
                } else if abi_size <= 16 {
                    X86_SYSTEMV_ABI_TWO_INTEGERS
                } else {
                    X86_SYSTEM_V_ABI_STACK
                }
            }

            Type::Struct { fields, .. } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size > 64 {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [X86SystemVABITypeClass; 8] =
                    [X86SystemVABITypeClass::NO_CLASS; 8];

                for (i, field_type) in fields.iter().enumerate() {
                    let field_offset_bytes: u32 = layout.field_offsets[i] / 8;

                    let field_classes: [X86SystemVABITypeClass; 8] =
                        Self::get_system_v_type_class(abi_context, field_type);

                    for (sub_idx, _) in field_classes.iter().enumerate() {
                        let field_class: X86SystemVABITypeClass = field_classes[sub_idx];

                        if matches!(field_class, X86SystemVABITypeClass::NO_CLASS) {
                            continue;
                        }

                        let target_eightbyte_idx: u32 = (field_offset_bytes / 8) + sub_idx as u32;

                        if target_eightbyte_idx < 8 {
                            current_classes[target_eightbyte_idx as usize] = Self::combine(
                                current_classes[target_eightbyte_idx as usize],
                                field_class,
                            );
                        }
                    }
                }

                for (idx, _) in current_classes.iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::MEMORY) {
                        return X86_SYSTEM_V_ABI_STACK;
                    }
                }

                // https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig
                /*

                   "If the size of the aggregate exceeds two eightbytes and the first eight-
                    byte isn’t SSE or any other eightbyte isn’t SSEUP, the whole argument
                    is passed in memory."

                */
                if abi_size > 16
                    && (current_classes
                        .first()
                        .is_some_and(|c| !matches!(c, X86SystemVABITypeClass::SSE))
                        || current_classes
                            .get(1)
                            .iter()
                            .any(|c| !matches!(c, X86SystemVABITypeClass::SSEUP)))
                {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx - 1] {
                            X86SystemVABITypeClass::SSE | X86SystemVABITypeClass::SSEUP => {
                                continue;
                            }
                            _ => {
                                current_classes[idx] = X86SystemVABITypeClass::SSE;
                            }
                        }
                    }
                }

                current_classes
            }

            any => abort::abort_codegen(
                abi_context,
                &format!(
                    "Unsupported type for x86 System V ABI classification: '{}'.",
                    any
                ),
                any.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum x86SystemVABIType {
    Same(Type),
    ToMemory(Type),
    DecomposeAndExpand(Vec<Type>),
    Ignore,
}

impl x86SystemVABIType {
    #[inline]
    pub fn is_the_same(&self) -> bool {
        matches!(self, x86SystemVABIType::Same(_))
    }

    #[inline]
    pub fn is_ignore(&self) -> bool {
        matches!(self, x86SystemVABIType::Ignore)
    }

    #[inline]
    pub fn is_to_memory(&self) -> bool {
        matches!(self, x86SystemVABIType::ToMemory(_))
    }

    #[inline]
    pub fn is_decompose_and_expand(&self) -> bool {
        matches!(self, x86SystemVABIType::DecomposeAndExpand(_))
    }
}

impl x86SystemVABIType {
    pub fn class_to_general_abi_strategy(
        classes: &[X86SystemVABITypeClass; 8],
        ty: Type,
    ) -> x86SystemVABIType {
        if classes.contains(&X86SystemVABITypeClass::MEMORY) {
            return x86SystemVABIType::ToMemory(ty);
        }

        let used: usize = classes
            .iter()
            .take_while(|&&c| c != X86SystemVABITypeClass::NO_CLASS)
            .count();

        if used == 0 {
            return x86SystemVABIType::Ignore;
        }

        match used {
            1 => match classes[0] {
                X86SystemVABITypeClass::INTEGER | X86SystemVABITypeClass::SSE => {
                    x86SystemVABIType::Same(ty)
                }
                _ => x86SystemVABIType::Same(ty),
            },

            2 => match (classes[0], classes[1]) {
                (X86SystemVABITypeClass::INTEGER, X86SystemVABITypeClass::INTEGER) => {
                    x86SystemVABIType::Same(ty)
                }

                (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSE) => {
                    x86SystemVABIType::Same(ty)
                }

                (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSEUP) => {
                    x86SystemVABIType::Same(ty)
                }

                _ => {
                    if let Type::Struct { fields, .. } = &ty {
                        x86SystemVABIType::DecomposeAndExpand(fields.clone())
                    } else {
                        x86SystemVABIType::ToMemory(ty)
                    }
                }
            },

            _ => x86SystemVABIType::ToMemory(ty),
        }
    }

    fn is_valid_sse_sequence(classes: &[X86SystemVABITypeClass; 8]) -> bool {
        let mut seen_sse: bool = false;

        for &c in classes {
            match c {
                X86SystemVABITypeClass::SSE | X86SystemVABITypeClass::SSEUP => seen_sse = true,
                X86SystemVABITypeClass::NO_CLASS => continue,
                _ if seen_sse => return false,
                _ => {}
            }
        }

        true
    }
}

#[derive(Debug, Clone, Copy)]
pub enum x86SystemVABIFunctionTypeArgumentConfigurationAttributes {
    byVal,
}

#[derive(Debug)]
pub enum x86SystemVABIFunctionTypeArgumentConfiguration {
    Same {
        ty: Type,
        index: usize,
    },
    ToMemory {
        ty: Type,
        index: usize,
        attributes: x86SystemVABIFunctionTypeArgumentConfigurationAttributes,
    },
    DecomposeAndExpand {
        old_type: Type,
        decomposed_indexes: Vec<usize>,
        index: usize,
    },
    Ignore {
        ty: Type,
        index: usize,
    },
}

#[derive(Debug)]
pub struct x86SystemVABIFunctionTypeConfiguration {
    parameter_types: Vec<x86SystemVABIFunctionTypeArgumentConfiguration>,
    is_variatic: bool,
}

impl x86SystemVABIFunctionTypeConfiguration {
    #[inline]
    pub fn new(is_variatic: bool) -> Self {
        Self {
            parameter_types: Vec::new(),
            is_variatic,
        }
    }
}

impl x86SystemVABIFunctionTypeConfiguration {
    #[inline]
    pub fn get_is_variatic(&self) -> bool {
        self.is_variatic
    }
}

impl x86SystemVABIFunctionTypeConfiguration {
    #[inline]
    pub fn get_mut_configuration_parameter_types(
        &mut self,
    ) -> &mut Vec<x86SystemVABIFunctionTypeArgumentConfiguration> {
        &mut self.parameter_types
    }
}

pub fn lower_function_call<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    function_value: &FunctionValue<'llvm_abi>,
    configuration: &x86SystemVABIFunctionTypeConfiguration,
    args: &'llvm_abi [BasicValueEnum],
) {
    let function_value: FunctionValue<'_> = *function_value;
    let function_type: FunctionType = function_value.get_type();
    let callee_args_values: Vec<BasicValueEnum> = function_value.get_params();
    let callee_args_types: Vec<BasicTypeEnum<'_>> = function_type.get_param_types();

    let ordered_configurations: Vec<&x86SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let _ = ordered_configurations.is_sorted_by_key(|config| match config {
        x86SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    assert!(args.len() != ordered_configurations.len());

    let mut processed_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

    for (arg_value, arg_config) in args.iter().zip(ordered_configurations.iter()) {
        match arg_config {
            x86SystemVABIFunctionTypeArgumentConfiguration::Ignore { .. } => {
                processed_args.push((*arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::Same { .. } => {
                processed_args.push((*arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                ty,
                index,
                attributes,
            } => {
                let mut arg_value: BasicValueEnum<'_> = *arg_value;

                let is_ptr_value: bool = arg_value.is_pointer_value();

                if !is_ptr_value {
                    let stack_ptr: inkwell::values::PointerValue<'_> = llvm_builder
                        .build_alloca(arg_value.get_type(), "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to compile allocate a paramater onto the stack!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let alignment: u32 = abi_context
                        .get_target_data()
                        .get_preferred_alignment(&arg_value.get_type());

                    if let Some(instruction) = stack_ptr.as_instruction() {
                        instruction.set_alignment(alignment).unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set type alignment!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        });
                    }

                    llvm_builder
                        .build_store(stack_ptr, arg_value)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to store a value in memory!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        });

                    if matches!(
                        attributes,
                        x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal
                    ) {
                        let Some(_) = callee_args_values.get(*index) else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the function parameter value from the function declaration!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        };

                        let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                        let byval_attribute: Attribute = llvm_context.create_type_attribute(
                            byval_id,
                            arg_value.get_type().as_any_type_enum(),
                        );

                        function_value.add_attribute(
                            AttributeLoc::Param((*index).try_into().unwrap_or_default()),
                            byval_attribute,
                        );

                        function_value.set_param_alignment(
                            (*index).try_into().unwrap_or_default(),
                            alignment,
                        );
                    }

                    arg_value = stack_ptr.into();
                }

                processed_args.push((arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                old_type,
                decomposed_indexes,
                ..
            } => {
                if !arg_value.is_struct_value() {
                    abort::abort_codegen(
                        abi_context,
                        "Expected a struct value to decompose and expand, but got a non-struct value!",
                        old_type.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    );
                }

                let struct_value: inkwell::values::StructValue<'_> = arg_value.into_struct_value();

                assert!(
                    struct_value.count_fields() != decomposed_indexes.len().try_into().unwrap()
                );

                let mut extracted_fields_values: Vec<BasicValueEnum> = Vec::new();

                for field_idx in 0..struct_value.count_fields() {
                    let field_value: BasicValueEnum<'_> = llvm_builder
                        .build_extract_value(struct_value, field_idx, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to extract a value from a struct!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        });

                    extracted_fields_values.push(field_value);
                }

                for field_value in extracted_fields_values.iter() {
                    processed_args.push((*field_value).into());
                }
            }
        }
    }
}

pub fn decompose_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    return_type: &Type,
    parameter_types: &[Type],
    is_variatic: bool,
) -> (
    FunctionType<'llvm_abi>,
    x86SystemVABIFunctionTypeConfiguration,
) {
    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(parameter_types.len());

    let mut configuration: x86SystemVABIFunctionTypeConfiguration =
        x86SystemVABIFunctionTypeConfiguration::new(is_variatic);

    let configuration_parameter_types: &mut Vec<x86SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.get_mut_configuration_parameter_types();

    for (idx, ty) in parameter_types.iter().enumerate() {
        let ty_claseses: [X86SystemVABITypeClass; 8] =
            X86SystemVABITypeClass::get_system_v_type_class(abi_context, ty);

        let abi_ty: x86SystemVABIType =
            x86SystemVABIType::class_to_general_abi_strategy(&ty_claseses, ty.clone());

        match abi_ty {
            x86SystemVABIType::Ignore => {
                let llvm_ty: BasicTypeEnum<'_> =
                    self::decompose_type(llvm_context, abi_context, ty);

                configuration_parameter_types.push(
                    x86SystemVABIFunctionTypeArgumentConfiguration::Ignore {
                        ty: ty.clone(),
                        index: idx,
                    },
                );

                llvm_parameters_types.push(llvm_ty.into());
            }

            x86SystemVABIType::Same(ty) => {
                let llvm_ty: BasicTypeEnum<'_> =
                    self::decompose_type(llvm_context, abi_context, &ty);

                configuration_parameter_types.push(
                    x86SystemVABIFunctionTypeArgumentConfiguration::Same {
                        ty: ty.clone(),
                        index: idx,
                    },
                );

                llvm_parameters_types.push(llvm_ty.into());
            }

            x86SystemVABIType::ToMemory(_) => {
                configuration_parameter_types.push(
                    x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                        ty: ty.clone(),
                        index: idx,
                        attributes: x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal,
                    },
                );

                llvm_parameters_types.push(llvm_context.ptr_type(AddressSpace::default()).into());
            }

            x86SystemVABIType::DecomposeAndExpand(field_types) => {
                let mut decomposed_types: Vec<BasicMetadataTypeEnum> = Vec::new();
                let mut llvm_parameters_last_index: usize =
                    llvm_parameters_types.len().saturating_sub(1);

                let mut finish_decompose_process: bool = false;

                for field_type in field_types.iter() {
                    let ty_claseses: [X86SystemVABITypeClass; 8] =
                        X86SystemVABITypeClass::get_system_v_type_class(abi_context, field_type);

                    let abi_ty: x86SystemVABIType =
                        x86SystemVABIType::class_to_general_abi_strategy(
                            &ty_claseses,
                            field_type.clone(),
                        );

                    if abi_ty.is_decompose_and_expand() || abi_ty.is_to_memory() {
                        configuration_parameter_types.push(
                            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                                ty: ty.clone(),
                                index: idx,
                                attributes:
                                    x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal,
                            },
                        );

                        llvm_parameters_types
                            .push(llvm_context.ptr_type(AddressSpace::default()).into());

                        finish_decompose_process = true;
                        break;
                    } else {
                        let llvm_ty: BasicTypeEnum<'_> =
                            self::decompose_type(llvm_context, abi_context, field_type);

                        decomposed_types.push(llvm_ty.into());
                    }
                }

                if finish_decompose_process {
                    continue;
                }

                let mut decomposed_indexes: Vec<usize> = Vec::new();

                for _ in decomposed_types.iter() {
                    decomposed_indexes.push(llvm_parameters_last_index + 1);
                    llvm_parameters_last_index += 1;
                }

                configuration_parameter_types.push(
                    x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                        old_type: ty.clone(),
                        decomposed_indexes,
                        index: idx,
                    },
                );

                llvm_parameters_types.extend(decomposed_types.iter());
            }
        }
    }

    if return_type.is_void_type() {
        (
            llvm_context
                .void_type()
                .fn_type(&llvm_parameters_types, is_variatic),
            configuration,
        )
    } else {
        let return_ty_classes: [X86SystemVABITypeClass; 8] =
            X86SystemVABITypeClass::get_system_v_type_class(abi_context, return_type);

        let abi_return_ty: x86SystemVABIType = x86SystemVABIType::class_to_general_abi_strategy(
            &return_ty_classes,
            return_type.clone(),
        );

        match abi_return_ty {
            x86SystemVABIType::Ignore => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::decompose_type(llvm_context, abi_context, return_type);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            x86SystemVABIType::Same(ty) => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::decompose_type(llvm_context, abi_context, &ty);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            x86SystemVABIType::DecomposeAndExpand(..) | x86SystemVABIType::ToMemory(..) => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::decompose_type(llvm_context, abi_context, return_type);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }
        }
    }
}

pub fn decompose_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
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

            Type::Bool(..) => llvm_context.bool_type().into(),
            Type::Const(subtype, ..) => self::decompose_type(llvm_context, abi_context, subtype),

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

            Type::Const(subtype, ..) => self::decompose_type(llvm_context, abi_context, subtype),

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
        } => self::decompose_type(llvm_context, abi_context, infered_type),

        t if t.is_ptr_like_type() => llvm_context.ptr_type(AddressSpace::default()).into(),

        Type::Const(subtype, ..) => self::decompose_type(llvm_context, abi_context, subtype),

        Type::Struct {
            fields, modifier, ..
        } => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::with_capacity(u8::MAX as usize);

            let packed: bool = modifier.llvm().is_packed();

            {
                for ty in fields.iter() {
                    field_types.push(self::decompose_type(llvm_context, abi_context, ty));
                }
            }

            llvm_context.struct_type(&field_types, packed).into()
        }

        Type::FixedArray(type_, size, ..) => {
            let array_type: BasicTypeEnum = self::decompose_type(llvm_context, abi_context, type_);
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
