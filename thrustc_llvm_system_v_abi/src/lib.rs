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
#![allow(clippy::collapsible_match)]

mod abort;

use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    targets::TargetData,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
        InstructionValue, PointerValue,
    },
};
use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::{
    Type,
    traits::{
        DereferenceExtensions, TypeCodeLocation, TypeExtensions, TypeFixedArrayEntensions,
        TypeIsExtensions, TypePointerExtensions,
    },
    type_layout::TargetInfo,
    type_modificators::StructureTypeModificator,
};

#[derive(Debug)]
pub struct SystemVABIContext<'system_v_abi> {
    target_triple: &'system_v_abi LLVMTargetTriple,
    diagnostician: Diagnostician,
    target_info: TargetInfo,
    target_data: &'system_v_abi TargetData,
    codegen_location: SystemVCodeGenLocation,
    abi_variant: X64ABIVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum X64ABIVariant {
    SystemV,
    Windows,
}

#[derive(Debug, Clone, Copy)]
pub enum SystemVCodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

impl<'system_v_abi> SystemVABIContext<'system_v_abi> {
    pub fn new(
        file: &CompilationUnit,
        options: &CompilerOptions,
        target_triple: &'system_v_abi LLVMTargetTriple,
        target_info: TargetInfo,
        target_data: &'system_v_abi TargetData,
        codegen_location: SystemVCodeGenLocation,
    ) -> Self {
        let abi_variant = if target_triple.is_windows_based() {
            X64ABIVariant::Windows
        } else {
            X64ABIVariant::SystemV
        };

        Self {
            target_triple,
            diagnostician: Diagnostician::new(file, options),
            target_info,
            target_data,
            codegen_location,
            abi_variant,
        }
    }
}

impl SystemVABIContext<'_> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        &mut self.target_info
    }
}

impl SystemVABIContext<'_> {
    #[inline]
    pub fn get_target_data(&self) -> &TargetData {
        self.target_data
    }

    #[inline]
    pub fn get_codegen_location(&self) -> SystemVCodeGenLocation {
        self.codegen_location
    }
}

// https://gitlab.com/x86-psABIs/x86-64-ABI - System V
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SystemVABITypeClass {
    INTEGER,
    SSE,
    SSEUP,
    X87,
    X87UP,
    COMPLEX_X87,
    NO_CLASS,
    MEMORY,
}

impl std::fmt::Display for SystemVABITypeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class_str: &str = match self {
            SystemVABITypeClass::INTEGER => "INTEGER",
            SystemVABITypeClass::SSE => "SSE",
            SystemVABITypeClass::SSEUP => "SSEUP",
            SystemVABITypeClass::X87 => "X87",
            SystemVABITypeClass::X87UP => "X87UP",
            SystemVABITypeClass::COMPLEX_X87 => "COMPLEX_X87",
            SystemVABITypeClass::NO_CLASS => "NO_CLASS",
            SystemVABITypeClass::MEMORY => "MEMORY",
        };

        write!(f, "{}", class_str)
    }
}

pub const SYSTEM_V_ABI_TWO_INTEGERS: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::INTEGER,
    SystemVABITypeClass::INTEGER,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_ONE_INTEGER: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::INTEGER,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_MANTISSA: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::X87,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_MANTISSA_UP: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::X87UP,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_F32_F64: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::SSE,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_F128: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::SSE,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_STACK: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::MEMORY,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

// https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig#L412

impl SystemVABITypeClass {
    pub fn combine(
        accum: SystemVABITypeClass,
        current: SystemVABITypeClass,
    ) -> SystemVABITypeClass {
        if accum == current {
            return accum;
        }

        if accum == SystemVABITypeClass::NO_CLASS {
            return current;
        }

        if accum == SystemVABITypeClass::MEMORY || current == SystemVABITypeClass::MEMORY {
            return SystemVABITypeClass::MEMORY;
        }

        if accum == SystemVABITypeClass::X87 || current == SystemVABITypeClass::X87 {
            return SystemVABITypeClass::MEMORY;
        }

        if accum == SystemVABITypeClass::X87UP || current == SystemVABITypeClass::X87UP {
            return SystemVABITypeClass::MEMORY;
        }

        if accum == SystemVABITypeClass::INTEGER || current == SystemVABITypeClass::INTEGER {
            return SystemVABITypeClass::INTEGER;
        }

        SystemVABITypeClass::SSE
    }

    pub fn get_system_v_type_class(
        abi_context: &mut SystemVABIContext,
        ty: &Type,
    ) -> [SystemVABITypeClass; 8] {
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
            | Type::Char { .. }
            | Type::Bool { .. } => SYSTEM_V_ABI_ONE_INTEGER,

            Type::SSize { .. } | Type::USize { .. } if layout.sizeof <= 8 || layout.sizeof <= 4 => {
                SYSTEM_V_ABI_ONE_INTEGER
            }

            Type::SSize { .. } | Type::USize { .. } => SYSTEM_V_ABI_TWO_INTEGERS,

            Type::U128 { .. } => SYSTEM_V_ABI_TWO_INTEGERS,

            Type::FX8680 { .. } => SYSTEM_V_ABI_MANTISSA,

            Type::F32 { .. } | Type::F64 { .. } => SYSTEM_V_ABI_F32_F64,

            Type::F128 { .. } | Type::FPPC128 { .. } => SYSTEM_V_ABI_F128,

            t if t.is_ptr_like_type() => SYSTEM_V_ABI_ONE_INTEGER,

            Type::FixedArray { base_type, .. } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size > 16 {
                    return SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [SystemVABITypeClass; 8] =
                    [SystemVABITypeClass::NO_CLASS; 8];

                let subty_classes: [SystemVABITypeClass; 8] =
                    Self::get_system_v_type_class(abi_context, base_type);

                for &elem_offset_bits in layout.field_offsets.iter() {
                    let base_eightbyte: usize = ((elem_offset_bits / 8) / 8) as usize;

                    for (sub_idx, _) in subty_classes.iter().enumerate() {
                        let current_subty_class: SystemVABITypeClass = subty_classes[sub_idx];

                        if matches!(current_subty_class, SystemVABITypeClass::NO_CLASS) {
                            continue;
                        }

                        let target: usize = base_eightbyte + sub_idx;

                        if target >= 8 {
                            return SYSTEM_V_ABI_STACK;
                        }

                        current_classes[target] =
                            Self::combine(current_classes[target], current_subty_class);
                    }
                }

                if current_classes.contains(&SystemVABITypeClass::MEMORY) {
                    return SYSTEM_V_ABI_STACK;
                }

                /*
                    "if X87UP is not preceded by X87, the whole argument is passed in memory."
                */
                for (idx, class) in current_classes.iter().enumerate() {
                    if matches!(class, SystemVABITypeClass::X87UP)
                        && idx > 0
                        && !matches!(
                            current_classes[idx.saturating_sub(1)],
                            SystemVABITypeClass::X87
                        )
                    {
                        return SYSTEM_V_ABI_STACK;
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
                        .is_some_and(|c| !matches!(c, SystemVABITypeClass::SSE))
                        || current_classes
                            .iter()
                            .skip(1)
                            .any(|c| !matches!(c, SystemVABITypeClass::SSEUP)))
                {
                    return SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx.saturating_sub(1)] {
                            SystemVABITypeClass::SSE | SystemVABITypeClass::SSEUP => {
                                continue;
                            }
                            _ => {
                                current_classes[idx] = SystemVABITypeClass::SSE;
                            }
                        }
                    }
                }

                current_classes
            }

            Type::Struct {
                fields, metadata, ..
            } => {
                let modifications: &StructureTypeModificator =
                    metadata.get_struct_type_modificator();

                let abi_size: u32 = layout.abi_size;

                if abi_size > 16 {
                    return SYSTEM_V_ABI_STACK;
                }

                if modifications.llvm().is_packed() {
                    return SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [SystemVABITypeClass; 8] =
                    [SystemVABITypeClass::NO_CLASS; 8];

                for (i, field_type) in fields.iter().enumerate() {
                    let field_classes: [SystemVABITypeClass; 8] =
                        Self::get_system_v_type_class(abi_context, field_type);

                    let field_offset_bytes: u32 =
                        layout.field_offsets.get(i).copied().unwrap_or(0) / 8;
                    let base_eightbyte: usize = (field_offset_bytes / 8) as usize;

                    for (sub_idx, _) in field_classes.iter().enumerate() {
                        let current_field_class: SystemVABITypeClass = field_classes[sub_idx];

                        if matches!(current_field_class, SystemVABITypeClass::NO_CLASS) {
                            continue;
                        }

                        let target: usize = base_eightbyte + sub_idx;

                        if target >= 8 {
                            return SYSTEM_V_ABI_STACK;
                        }

                        current_classes[target] =
                            Self::combine(current_classes[target], current_field_class);
                    }
                }

                if current_classes.contains(&SystemVABITypeClass::MEMORY) {
                    return SYSTEM_V_ABI_STACK;
                }

                /*
                    "if X87UP is not preceded by X87, the whole argument is passed in memory."
                */
                for (idx, class) in current_classes.iter().enumerate() {
                    if matches!(class, SystemVABITypeClass::X87UP)
                        && idx > 0
                        && !matches!(
                            current_classes[idx.saturating_sub(1)],
                            SystemVABITypeClass::X87
                        )
                    {
                        return SYSTEM_V_ABI_STACK;
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
                        .is_some_and(|c| !matches!(c, SystemVABITypeClass::SSE))
                        || current_classes
                            .iter()
                            .skip(1)
                            .any(|c| !matches!(c, SystemVABITypeClass::SSEUP)))
                {
                    return SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx.saturating_sub(1)] {
                            SystemVABITypeClass::SSE | SystemVABITypeClass::SSEUP => {
                                continue;
                            }
                            _ => {
                                current_classes[idx] = SystemVABITypeClass::SSE;
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
pub enum SystemVABIType<'llvm_abi> {
    Same(&'llvm_abi Type),
    ToMemory(&'llvm_abi Type),
    Coerce(&'llvm_abi Type, u32),
    DecomposeAndExpand(Vec<Type>, SystemVABITypeDecomposeAndExpandVariant),
    Ignore,
}

#[derive(Debug, Clone, Copy)]
pub enum SystemVABITypeDecomposeAndExpandVariant {
    DecomposeAndExpandStructure,
    DecomposeAndExpandInteger128,
    DecomposeAndExpandArray,
}

impl SystemVABITypeDecomposeAndExpandVariant {
    #[inline]
    pub fn is_decompose_and_expand_structure(&self) -> bool {
        matches!(
            self,
            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandStructure
        )
    }

    #[inline]
    pub fn is_decompose_and_expand_integer128(&self) -> bool {
        matches!(
            self,
            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandInteger128
        )
    }

    #[inline]
    pub fn is_decompose_and_expand_array(&self) -> bool {
        matches!(
            self,
            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray
        )
    }
}

impl SystemVABIType<'_> {
    #[inline]
    pub fn is_the_same(&self) -> bool {
        matches!(self, SystemVABIType::Same(_))
    }

    #[inline]
    pub fn is_ignore(&self) -> bool {
        matches!(self, SystemVABIType::Ignore)
    }

    #[inline]
    pub fn is_to_memory(&self) -> bool {
        matches!(self, SystemVABIType::ToMemory(_))
    }

    #[inline]
    pub fn is_decompose_and_expand(&self) -> bool {
        matches!(self, SystemVABIType::DecomposeAndExpand(..))
    }

    #[inline]
    pub fn is_coerce(&self) -> bool {
        matches!(self, SystemVABIType::Coerce(..))
    }
}

impl<'llvm_abi> SystemVABIType<'llvm_abi> {
    pub fn class_to_general_abi_strategy(
        abi_context: &mut SystemVABIContext,
        classes: &[SystemVABITypeClass; 8],
        ty: &'llvm_abi Type,
    ) -> SystemVABIType<'llvm_abi> {
        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

        if abi_context.abi_variant == X64ABIVariant::Windows {
            if ty.is_struct_type() || ty.is_fixed_array_type() {
                if layout.abi_size > 8 {
                    return SystemVABIType::ToMemory(ty);
                } else {
                    return SystemVABIType::Coerce(ty, layout.width);
                }
            }

            return SystemVABIType::Same(ty);
        }

        if classes.contains(&SystemVABITypeClass::MEMORY) {
            return SystemVABIType::ToMemory(ty);
        }

        let used: usize = classes
            .iter()
            .take_while(|&&c| c != SystemVABITypeClass::NO_CLASS)
            .count();

        if used == 0 {
            return SystemVABIType::Ignore;
        }

        if classes.contains(&SystemVABITypeClass::MEMORY) {
            return SystemVABIType::ToMemory(ty);
        }

        match used {
            1 => match classes[0] {
                SystemVABITypeClass::INTEGER if ty.is_fixed_array_type() => {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    if array_fixed_ty.is_array_type()
                        || array_fixed_ty.is_fixed_array_type()
                        || array_fixed_ty.is_struct_type()
                        || array_fixed_ty.is_ptr_like_type()
                    {
                        return SystemVABIType::ToMemory(ty);
                    }

                    let is_integer: bool = array_fixed_ty.is_integer_type();

                    if is_integer {
                        let first_integer_ty: Type = if array_fixed_ty.is_signed_integer_type() {
                            Type::S64 {
                                span: ty.get_span(),
                            }
                        } else {
                            Type::U64 {
                                span: ty.get_span(),
                            }
                        };

                        let second_integer_ty: Type = first_integer_ty.clone();

                        if layout.abi_size == 8 {
                            return SystemVABIType::DecomposeAndExpand(
                                vec![first_integer_ty],
                                SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_integer_ty, second_integer_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                SystemVABITypeClass::SSE if ty.is_fixed_array_type() => {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_span(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_span(),
                            }
                        };

                        let second_float_ty: Type = first_float_ty.clone();

                        if layout.abi_size == 8 {
                            return SystemVABIType::DecomposeAndExpand(
                                vec![first_float_ty],
                                SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_float_ty, second_float_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                SystemVABITypeClass::MEMORY => SystemVABIType::ToMemory(ty),
                SystemVABITypeClass::INTEGER
                    if ty.is_struct_type() || ty.is_fixed_array_type() =>
                {
                    SystemVABIType::Coerce(ty, layout.width)
                }
                SystemVABITypeClass::INTEGER | SystemVABITypeClass::SSE => SystemVABIType::Same(ty),

                _ => SystemVABIType::Same(ty),
            },

            2 => match (classes[0], classes[1]) {
                (SystemVABITypeClass::INTEGER, SystemVABITypeClass::INTEGER)
                    if ty.is_fixed_array_type() =>
                {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    if array_fixed_ty.is_array_type()
                        || array_fixed_ty.is_fixed_array_type()
                        || array_fixed_ty.is_struct_type()
                        || array_fixed_ty.is_ptr_like_type()
                    {
                        return SystemVABIType::ToMemory(ty);
                    }

                    let is_integer: bool = array_fixed_ty.is_integer_type();

                    if is_integer {
                        let first_integer_ty: Type = if array_fixed_ty.is_signed_integer_type() {
                            Type::S64 {
                                span: ty.get_span(),
                            }
                        } else {
                            Type::U64 {
                                span: ty.get_span(),
                            }
                        };

                        let second_integer_ty: Type = first_integer_ty.clone();

                        if layout.abi_size == 8 {
                            return SystemVABIType::DecomposeAndExpand(
                                vec![first_integer_ty],
                                SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_integer_ty, second_integer_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::INTEGER, SystemVABITypeClass::INTEGER) => {
                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::SSE, SystemVABITypeClass::SSE)
                    if ty.is_fixed_array_type() =>
                {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_span(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_span(),
                            }
                        };

                        let second_float_ty: Type = first_float_ty.clone();

                        if layout.abi_size == 8 {
                            return SystemVABIType::DecomposeAndExpand(
                                vec![first_float_ty],
                                SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_float_ty, second_float_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::SSE, SystemVABITypeClass::SSE) => {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_span(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_span(),
                            }
                        };

                        let second_float_ty: Type = first_float_ty.clone();

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_float_ty, second_float_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::SSE, SystemVABITypeClass::SSEUP)
                    if ty.is_fixed_array_type() =>
                {
                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::SSE, SystemVABITypeClass::SSEUP) => SystemVABIType::Same(ty),

                (SystemVABITypeClass::MEMORY, _) => SystemVABIType::ToMemory(ty),

                _ => {
                    if let Type::Struct { fields, .. } = &ty {
                        SystemVABIType::DecomposeAndExpand(
                            fields.clone(),
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandStructure,
                        )
                    } else {
                        SystemVABIType::ToMemory(ty)
                    }
                }
            },

            _ => SystemVABIType::ToMemory(ty),
        }
    }

    fn is_valid_sse_sequence(classes: &[SystemVABITypeClass; 8]) -> bool {
        let mut seen_sse: bool = false;

        for &c in classes {
            match c {
                SystemVABITypeClass::SSE | SystemVABITypeClass::SSEUP => seen_sse = true,
                SystemVABITypeClass::NO_CLASS => continue,
                _ if seen_sse => return false,
                _ => {}
            }
        }

        true
    }
}

#[derive(Debug, Clone)]
pub enum SystemVABIFunctionParameterConfiguration {
    Normal,
    FromMemory,
}

#[derive(Debug, Clone)]
pub enum SystemVABIFunctionTypeArgumentConfigurationAttributes {
    byVal(Type),
    InReg,
    Sret(Type),

    None,
}

#[derive(Debug, Clone)]
pub enum SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi> {
    Same {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        index: usize,
    },
    ToMemory {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        index: usize,
        attribute: SystemVABIFunctionTypeArgumentConfigurationAttributes,
        is_sret: bool,
    },
    DecomposeAndExpand {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        old_type: &'llvm_abi Type,
        struct_field_indexes: Vec<usize>,
        decomposed_indexes: Vec<usize>,
        array_decomposed_types: Vec<Type>,
        variant: SystemVABITypeDecomposeAndExpandVariant,
        index: usize,
    },
    Coerce {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        original_ty: &'llvm_abi Type,
        coerced_width_bits: u32,
        index: usize,
    },
    Ignore {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        index: usize,
    },
}

#[derive(Debug, Clone)]
pub struct SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    parameter_types: Vec<SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi>>,
    is_memory_return: bool,
    is_variatic: bool,
}

impl<'llvm_abi> SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    #[inline]
    pub fn new(is_variatic: bool, is_memory_return: bool) -> Self {
        Self {
            parameter_types: Vec::new(),
            is_variatic,
            is_memory_return,
        }
    }
}

impl<'llvm_abi> SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    pub fn set_parameter_types_configuration(
        &mut self,
        parameter_types: Vec<SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi>>,
    ) {
        self.parameter_types = parameter_types;
    }
}

impl SystemVABIFunctionTypeConfiguration<'_> {
    #[inline]
    pub fn is_variatic(&self) -> bool {
        self.is_variatic
    }

    #[inline]
    pub fn is_memory_return(&self) -> bool {
        self.is_memory_return
    }
}

impl SystemVABIFunctionTypeConfiguration<'_> {
    #[inline]
    pub fn set_memory_return(&mut self, value: bool) {
        self.is_memory_return = value
    }
}

impl<'llvm_abi> SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    #[inline]
    pub fn get_mut_configuration_parameter_types(
        &mut self,
    ) -> &mut Vec<SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi>> {
        &mut self.parameter_types
    }
}

pub fn lower_function_parameters<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &SystemVABIFunctionTypeConfiguration<'llvm_abi>,
) -> Vec<(
    &'llvm_abi str,
    &'llvm_abi str,
    &'llvm_abi Type,
    SystemVABIFunctionParameterConfiguration,
    BasicValueEnum<'llvm_abi>,
)> {
    let function_value: FunctionValue<'_> = function_value;
    let function_parameters: Vec<BasicValueEnum<'_>> = function_value.get_params();

    let ordered_configurations: Vec<&SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let _ = ordered_configurations.is_sorted_by_key(|config| match config {
        SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Coerce { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    let mut processed_parameters: Vec<(
        &'llvm_abi str,
        &'llvm_abi str,
        &'llvm_abi Type,
        SystemVABIFunctionParameterConfiguration,
        BasicValueEnum<'_>,
    )> = Vec::new();

    for arg_config in ordered_configurations.iter() {
        match arg_config {
            SystemVABIFunctionTypeArgumentConfiguration::Ignore {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_parameters.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        SystemVABIFunctionParameterConfiguration::Normal,
                        (*value),
                    ));
                } else {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the parameter value from the function declaration for System V ABI!",
                        ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::Same {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_parameters.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        SystemVABIFunctionParameterConfiguration::Normal,
                        (*value),
                    ));
                } else {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the parameter value from the function declaration for System V ABI!",
                        ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::Coerce {
                name,
                ascii_name,
                original_ty,
                index,
                ..
            } => {
                if let Some(coerced_value) = function_parameters.get(*index) {
                    let original_llvm_ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, original_ty);

                    let ptr: PointerValue<'_> =
                        llvm_builder.build_alloca(original_llvm_ty, "").unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to allocate memory for a coerced parameter in System V ABI!",
                                original_ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let alignment: u32 = abi_context
                        .get_target_data()
                        .get_preferred_alignment(&original_llvm_ty);

                    llvm_builder
                        .build_store(ptr, *coerced_value)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to store a coerced parameter value in memory for System V ABI!",
                                original_ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .set_alignment(alignment)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set the alignment of a store instruction for a coerced parameter in System V ABI!",
                                original_ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let loaded_value: BasicValueEnum<'_> = llvm_builder
                        .build_load(original_llvm_ty, ptr, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to load a coerced parameter from memory for System V ABI!",
                                original_ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    processed_parameters.push((
                        name,
                        ascii_name,
                        original_ty,
                        SystemVABIFunctionParameterConfiguration::Normal,
                        loaded_value,
                    ));
                } else {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the coerced parameter value from the function declaration for System V ABI!",
                        original_ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_parameters.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        SystemVABIFunctionParameterConfiguration::FromMemory,
                        *value,
                    ));
                } else {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to get the parameter value from the function declaration for System V ABI!",
                        ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                name,
                ascii_name,
                old_type,
                struct_field_indexes,
                decomposed_indexes,
                variant,
                ..
            } => {
                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(old_type);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                if variant.is_decompose_and_expand_structure() {
                    let ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, old_type);

                    let ptr: PointerValue<'_> = llvm_builder.build_alloca(ty, "").unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate memory for a decomposed and expanded parameter in System V ABI!",
                            old_type.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    let alignment: u32 = abi_context.get_target_data().get_preferred_alignment(&ty);

                    assert!(struct_field_indexes.len() == decomposed_indexes.len());

                    for (field_idx, decomposed_idx) in
                        struct_field_indexes.iter().zip(decomposed_indexes.iter())
                    {
                        if let Some(decomposed_value) = function_parameters.get(*decomposed_idx) {
                            let element_ptr: PointerValue<'_> = llvm_builder.build_struct_gep(ty, ptr, (*field_idx) as u32, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to build a GEP instruction for a decomposed and expanded parameter in System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            let store: InstructionValue<'_> = llvm_builder.build_store(element_ptr, *decomposed_value).unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to store a decomposed and expanded parameter value in memory for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            store.set_alignment(alignment).unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        }
                    }

                    processed_parameters.push((
                        name,
                        ascii_name,
                        old_type,
                        SystemVABIFunctionParameterConfiguration::FromMemory,
                        ptr.into(),
                    ));
                }

                if variant.is_decompose_and_expand_array() {
                    let ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, old_type);

                    let ptr: PointerValue<'_> =
                        llvm_builder.build_alloca(ty, "").unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to allocate memory for a decomposed and expanded parameter in System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let alignment: u32 = abi_context.get_target_data().get_preferred_alignment(&ty);

                    assert!(!decomposed_indexes.is_empty());

                    if layout.abi_size == 8 {
                        if let Some(first_decomposed_value) =
                            function_parameters.get(decomposed_indexes[0])
                        {
                            llvm_builder
                                .build_store(ptr, *first_decomposed_value)
                                .unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to store a decomposed and expanded parameter value in memory for System V ABI!",
                                        old_type.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                })
                                .set_alignment(alignment)
                                .unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                        old_type.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                });
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        }
                    } else {
                        if let Some(first_decomposed_value) =
                            function_parameters.get(decomposed_indexes[0])
                        {
                            llvm_builder
                            .build_store(ptr, *first_decomposed_value)
                            .unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to store a decomposed and expanded parameter value in memory for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            })
                            .set_alignment(alignment)
                            .unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            if let Some(second_decomposed_value) =
                                function_parameters.get(decomposed_indexes[1])
                            {
                                let second_element_ptr: PointerValue<'_> = unsafe {
                                    llvm_builder.build_gep(
                                        second_decomposed_value.get_type(),
                                        ptr,
                                        &[
                                            llvm_context.i32_type().const_int(1, false),
                                        ],
                                        "",
                                    )
                                }.unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to build a GEP instruction for the second element of a decomposed and expanded array parameter in System V ABI!",
                                        old_type.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                });

                                llvm_builder
                                    .build_store(second_element_ptr, *second_decomposed_value)
                                    .unwrap_or_else(|_| {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to store the second element of a decomposed and expanded array parameter in memory for System V ABI!",
                                            old_type.get_span(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        )
                                    })
                                    .set_alignment(alignment)
                                    .unwrap_or_else(|_| {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to set the alignment of a store instruction for the second element of a decomposed and expanded array parameter in System V ABI!",
                                            old_type.get_span(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        )
                                    });

                                processed_parameters.push((
                                    name,
                                    ascii_name,
                                    old_type,
                                    SystemVABIFunctionParameterConfiguration::FromMemory,
                                    ptr.into(),
                                ));
                            } else {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to get the second decomposed and expanded parameter value from the function declaration for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            }
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        }
                    }
                }
            }
        }
    }

    processed_parameters
}

pub fn lower_system_v_call_prologue<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &SystemVABIFunctionTypeConfiguration,
    args: Vec<BasicValueEnum<'llvm_abi>>,
    span: Span,
) -> Vec<BasicMetadataValueEnum<'llvm_abi>> {
    let function_value: FunctionValue<'_> = function_value;
    let callee_args_values: Vec<BasicValueEnum> = function_value.get_params();

    let mut ordered_configuration: Vec<&SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let mut processed_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

    if configuration.is_memory_return() {
        let buffer_type: &Type = ordered_configuration
            .iter()
            .find_map(|config| match config {
                SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                    ty, is_sret: true, ..
                } => Some(*ty),
                _ => None,
            })
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Expected a return type from memory on ABI lowering!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let llvm_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, buffer_type);

        let ptr: PointerValue<'_> = llvm_builder.build_alloca(llvm_ty, "").unwrap_or_else(|_| {
            abort::abort_codegen(
                abi_context,
                "Failed to allocate a buffer for return value on a function call on ABI lowering!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        ordered_configuration.remove(0);
        processed_args.push(ptr.into());
    }

    let _ = ordered_configuration.is_sorted_by_key(|config| match config {
        SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Coerce { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    assert!(args.len() == ordered_configuration.len());

    for (arg_value, arg_config) in args.iter().zip(ordered_configuration.iter()) {
        match arg_config {
            SystemVABIFunctionTypeArgumentConfiguration::Ignore { .. } => {
                processed_args.push((*arg_value).into());
            }

            SystemVABIFunctionTypeArgumentConfiguration::Same { .. } => {
                processed_args.push((*arg_value).into());
            }

            SystemVABIFunctionTypeArgumentConfiguration::Coerce {
                original_ty,
                coerced_width_bits,
                ..
            } => {
                let original_llvm_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, original_ty);

                let coerced_llvm_ty: BasicTypeEnum<'_> =
                    llvm_context.custom_width_int_type(*coerced_width_bits).into();

                let ptr: PointerValue<'_> = llvm_builder
                    .build_alloca(original_llvm_ty, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate memory for a coerced argument in System V ABI!",
                            original_ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let alignment: u32 = abi_context
                    .get_target_data()
                    .get_preferred_alignment(&original_llvm_ty);

                llvm_builder
                    .build_store(ptr, *arg_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store a coerced argument value in memory for System V ABI!",
                            original_ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set the alignment of a store instruction for a coerced argument in System V ABI!",
                            original_ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let coerced_value: BasicValueEnum<'_> = llvm_builder
                    .build_load(coerced_llvm_ty, ptr, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a coerced argument from memory for System V ABI!",
                            original_ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                processed_args.push(coerced_value.into());
            }

            SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                ty,
                index,
                attribute,
                is_sret,
                ..
            } => {
                let mut arg_value: BasicValueEnum<'_> = *arg_value;

                let is_ptr_value: bool = arg_value.is_pointer_value();

                if *is_sret && !is_ptr_value {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to configure a big return on System V ABI lowering!",
                        ty.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }

                if *is_sret {
                    match attribute {
                        SystemVABIFunctionTypeArgumentConfigurationAttributes::Sret(ty) => {
                            let Some(_) = callee_args_values.get(*index) else {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to get the function parameter value from the function declaration for ABI lowering!",
                                    ty.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            };

                            let llvm_ty: BasicTypeEnum<'_> =
                                self::generate_type(llvm_context, abi_context, ty);

                            let alignment: u32 = abi_context
                                .get_target_data()
                                .get_preferred_alignment(&llvm_ty);

                            let sret_id: u32 = Attribute::get_named_enum_kind_id("sret");

                            let sret_attribute: Attribute = llvm_context
                                .create_type_attribute(sret_id, llvm_ty.as_any_type_enum());

                            function_value.add_attribute(
                                AttributeLoc::Param((*index).try_into().unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to parse parameter index on System-V ABI lowering!",
                                        ty.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                })),
                                sret_attribute,
                            );

                            function_value.set_param_alignment(
                                (*index).try_into().unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to set parameter memory alignment on System-V ABI lowering!",
                                        ty.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                }),
                                alignment,
                            );
                        }

                        // Fallback
                        _ => {
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

                                if let  SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(ty) = attribute {
                                    let Some(_) = callee_args_values.get(*index) else {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                                            ty.get_span(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        );
                                    };

                                    let llvm_ty: BasicTypeEnum<'_> = self::generate_type(llvm_context, abi_context, ty);

                                    let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                                    let byval_attribute: Attribute = llvm_context
                                        .create_type_attribute(
                                            byval_id,
                                            llvm_ty.as_any_type_enum(),
                                        );

                                    function_value.add_attribute(
                                        AttributeLoc::Param(
                                            (*index).try_into().unwrap_or_else(|_| {
                                                abort::abort_codegen(
                                                    abi_context,
                                                    "Failed to parse parameter index on System-V ABI lowering!",
                                                    ty.get_span(),
                                                    std::path::PathBuf::from(file!()),
                                                    line!(),
                                                )
                                            }),
                                        ),
                                        byval_attribute,
                                    );

                                    function_value.set_param_alignment(
                                        (*index).try_into().unwrap_or_else(|_| {
                                            abort::abort_codegen(
                                                abi_context,
                                                "Failed to set parameter memory alignment on System-V ABI lowering!",
                                                ty.get_span(),
                                                std::path::PathBuf::from(file!()),
                                                line!(),
                                            )
                                        }),
                                        alignment,
                                    );
                                }

                                arg_value = stack_ptr.into();
                            }
                        }
                    }
                } else {
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

                        if let SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(ty) =
                            attribute
                        {
                            let Some(_) = callee_args_values.get(*index) else {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to get the function parameter value from the function declaration for ABI lowering!",
                                    ty.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            };

                            let llvm_ty: BasicTypeEnum<'_> =
                                self::generate_type(llvm_context, abi_context, ty);

                            let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                            let byval_attribute: Attribute = llvm_context
                                .create_type_attribute(byval_id, llvm_ty.as_any_type_enum());

                            function_value.add_attribute(
                                AttributeLoc::Param((*index).try_into().unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to parse parameter index on System-V ABI lowering!",
                                        ty.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                })),
                                byval_attribute,
                            );

                            function_value.set_param_alignment(
                                (*index).try_into().unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to set parameter memory alignment on System-V ABI lowering!",
                                        ty.get_span(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                }),
                                alignment,
                            );
                        }

                        arg_value = stack_ptr.into();
                    }
                }

                processed_args.push((arg_value).into());
            }

            SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                old_type,
                array_decomposed_types,
                variant,
                ..
            } => {
                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(old_type);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                if variant.is_decompose_and_expand_structure() {
                    if !arg_value.is_struct_value() {
                        abort::abort_codegen(
                            abi_context,
                            "Expected a struct value to decompose and expand, but got a non-struct value!",
                            old_type.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    }

                    let struct_value: inkwell::values::StructValue<'_> =
                        arg_value.into_struct_value();

                    let mut extracted_fields_values: Vec<BasicValueEnum> = Vec::new();

                    for field_idx in 0..=struct_value.count_fields() {
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

                if variant.is_decompose_and_expand_array() {
                    if !arg_value.is_array_value() {
                        abort::abort_codegen(
                            abi_context,
                            "Expected an array value to decompose and expand, but got a non-array value!",
                            old_type.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    }

                    let mut ptr: PointerValue<'_> = llvm_builder
                        .build_alloca(arg_value.get_type(), "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to allocate memory for a decomposed and expanded array parameter in System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let alignment: u32 = abi_context
                        .get_target_data()
                        .get_preferred_alignment(&arg_value.get_type());

                    let array_value: inkwell::values::ArrayValue<'_> = arg_value.into_array_value();

                    let store_instruction: InstructionValue<'_> = llvm_builder
                        .build_store(ptr, array_value)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to store an array value in memory for System V ABI!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    store_instruction.set_alignment(alignment).unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set the alignment of a store instruction for a decomposed and expanded array parameter in System V ABI!",
                            old_type.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    if layout.abi_size == 8 {
                        let first_element_decomposed_ty: &Type = array_decomposed_types.first().unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the first decomposed type for an array decomposition and expansion!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                        let first_element_decomposed_llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(
                                llvm_context,
                                abi_context,
                                first_element_decomposed_ty,
                            );

                        let value: BasicValueEnum<'_> =
                            llvm_builder.build_load(first_element_decomposed_llvm_ty, ptr, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        processed_args.push(value.into());
                    } else {
                        let first_element_decomposed_ty: &Type = array_decomposed_types.first().unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the first decomposed type for an array decomposition and expansion!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                        let second_element_decomposed_ty: &Type = array_decomposed_types.get(1).unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the second decomposed type for an array decomposition and expansion!",
                                old_type.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                        let first_element_decomposed_llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(
                                llvm_context,
                                abi_context,
                                first_element_decomposed_ty,
                            );

                        let second_element_decomposed_llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(
                                llvm_context,
                                abi_context,
                                second_element_decomposed_ty,
                            );

                        let first_value: BasicValueEnum<'_> =
                            llvm_builder.build_load(first_element_decomposed_llvm_ty, ptr, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        ptr = self::address_space_to_normal(
                            abi_context,
                            llvm_builder,
                            llvm_context,
                            ptr,
                            old_type.get_span(),
                        );

                        let ptr_to_second_element: PointerValue<'_> = unsafe {
                            llvm_builder.build_in_bounds_gep(second_element_decomposed_llvm_ty, ptr, &[llvm_context.i32_type().const_int(1, false)], "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to build a GEP instruction to get the second element of a decomposed and expanded array parameter in System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            })
                        };

                        let second_value: BasicValueEnum<'_> =
                            llvm_builder.build_load(second_element_decomposed_llvm_ty, ptr_to_second_element, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_span(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        processed_args.push(first_value.into());
                        processed_args.push(second_value.into());
                    }
                }
            }
        }
    }

    processed_args
}

pub fn lower_system_v_call_epilogue<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    callsite: CallSiteValue<'llvm_abi>,
    lowered_args: &[BasicMetadataValueEnum<'llvm_abi>],
    configuration: &SystemVABIFunctionTypeConfiguration,
    span: Span,
) -> BasicValueEnum<'llvm_abi> {
    let function_value: FunctionValue<'_> = callsite.get_called_fn_value();
    let is_void_type: bool = function_value.get_type().get_return_type().is_none();

    if configuration.is_memory_return() && is_void_type {
        let memory_ptr_arg: BasicMetadataValueEnum<'_> =
            *lowered_args.first().unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to lower the call epilogue!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        if !memory_ptr_arg.is_pointer_value() {
            abort::abort_codegen(
                abi_context,
                "Failed to lower the call epilogue!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }

        let ptr: PointerValue<'_> = memory_ptr_arg.into_pointer_value();

        let ty: &&Type = configuration
            .parameter_types
            .iter()
            .find_map(|config| match config {
                SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                    ty, is_sret: true, ..
                } => Some(ty),
                _ => None,
            })
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile lower a function call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let codegen_location: SystemVCodeGenLocation = abi_context.get_codegen_location();

        if matches!(codegen_location, SystemVCodeGenLocation::LValue) {
            return ptr.into();
        }

        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(r#ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

        let llvm_ty: BasicTypeEnum<'_> = self::generate_type(llvm_context, abi_context, ty);

        let value: BasicValueEnum<'_> =
            llvm_builder
                .build_load(llvm_ty, ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to compile lower a function call!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

        let alignment: u32 = layout.alignof;

        let load_instruction: InstructionValue<'_> =
            value.as_instruction_value().unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile lower a function call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        load_instruction
            .set_alignment(alignment)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to set alignment to an instruction!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        value
    } else {
        callsite.try_as_basic_value().left().unwrap_or_else(|| {
            abort::abort_codegen(
                abi_context,
                "Failed to compile lower a function call!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }
}

pub fn lower_function_terminator<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi_context: &mut SystemVABIContext,
    configuration: &SystemVABIFunctionTypeConfiguration,
    function_value: FunctionValue<'llvm_abi>,
    return_value: Option<BasicValueEnum<'llvm_abi>>,
    span: Span,
) -> bool {
    if configuration.is_memory_return() {
        if let Some(return_value) = return_value {
            let is_ptr_value: bool = return_value.is_pointer_value();
            let is_any_other_value: bool = !is_ptr_value;

            if is_ptr_value {
                let memory_ptr_arg: BasicValueEnum<'_> =
                    function_value.get_first_param().unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get the memory pointer return value!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                if !memory_ptr_arg.is_pointer_value() {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to lower the return instruction!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }

                let ptr: PointerValue<'_> = return_value.into_pointer_value();

                let ty: &&Type = configuration
                    .parameter_types
                    .iter()
                    .find_map(|config| match config {
                        SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                            ty,
                            is_sret: true,
                            ..
                        } => Some(ty),
                        _ => None,
                    })
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to compile lower a function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(r#ty);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                let llvm_ty: BasicTypeEnum<'_> = self::generate_type(llvm_context, abi_context, ty);

                let value: BasicValueEnum<'_> = llvm_builder
                    .build_load(llvm_ty, ptr, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to compile lower a function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let alignment: u32 = layout.alignof;

                let load_instruction: InstructionValue<'_> =
                    value.as_instruction_value().unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to compile lower a function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                load_instruction
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set alignment to an instruction!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let store_instruction: InstructionValue<'_> = llvm_builder
                    .build_store(memory_ptr_arg.into_pointer_value(), value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set alignment to an instruction!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                store_instruction
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set alignment to an instruction!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
            }

            if is_any_other_value {
                let memory_ptr_arg: BasicValueEnum<'_> =
                    function_value.get_first_param().unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get the memory pointer return value!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let ty: &&Type = configuration
                    .parameter_types
                    .iter()
                    .find_map(|config| match config {
                        SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                            ty,
                            is_sret: true,
                            ..
                        } => Some(ty),
                        _ => None,
                    })
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to compile lower a function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(r#ty);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                let alignment: u32 = layout.alignof;

                let store_instruction: InstructionValue<'_> = llvm_builder
                    .build_store(memory_ptr_arg.into_pointer_value(), return_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set alignment to an instruction!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                store_instruction
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set alignment to an instruction!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
            }
        }

        if llvm_builder.build_return(None).is_err() {
            abort::abort_codegen(
                abi_context,
                "Failed to compile a function terminator!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }

        return true;
    }

    false
}

pub fn generate_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    return_type: &'llvm_abi Type,
    parameters: &'llvm_abi [Ast<'llvm_abi>],
    is_variatic: bool,
) -> (
    FunctionType<'llvm_abi>,
    SystemVABIFunctionTypeConfiguration<'llvm_abi>,
) {
    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum<'llvm_abi>> =
        Vec::with_capacity(parameters.len());

    let mut configuration: SystemVABIFunctionTypeConfiguration =
        SystemVABIFunctionTypeConfiguration::new(is_variatic, false);

    let mut configuration_parameter_types: Vec<SystemVABIFunctionTypeArgumentConfiguration> =
        Vec::with_capacity(parameters.len());

    let mut is_memory_return: bool = false;
    let mut idx: usize = 0;

    let mut abi_return_ty: SystemVABIType = SystemVABIType::Ignore;

    if !return_type.is_void_type() {
        let return_ty_classes: [SystemVABITypeClass; 8] =
            SystemVABITypeClass::get_system_v_type_class(abi_context, return_type);

        let abi_return_ty_: SystemVABIType = SystemVABIType::class_to_general_abi_strategy(
            abi_context,
            &return_ty_classes,
            return_type,
        );

        abi_return_ty = abi_return_ty_;

        if abi_return_ty.is_to_memory() {
            is_memory_return = true;
            configuration.set_memory_return(true);
        }

        if is_memory_return {
            llvm_parameters_types.insert(0, llvm_context.ptr_type(AddressSpace::default()).into());

            configuration_parameter_types.push(
                SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                    name: "",
                    ascii_name: "",
                    ty: return_type,
                    index: idx,
                    attribute: SystemVABIFunctionTypeArgumentConfigurationAttributes::Sret(
                        return_type.clone(),
                    ),
                    is_sret: true,
                },
            );

            idx = idx.saturating_add(1);
        }
    }

    for parameter in parameters.iter() {
        match parameter {
            Ast::FunctionParameter {
                name,
                ascii_name,
                kind: ty,
                ..
            } => {
                let ty_classes: [SystemVABITypeClass; 8] =
                    SystemVABITypeClass::get_system_v_type_class(abi_context, ty);

                let abi_ty: SystemVABIType =
                    SystemVABIType::class_to_general_abi_strategy(abi_context, &ty_classes, ty);

                match abi_ty {
                    SystemVABIType::Ignore => {
                        let llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(llvm_context, abi_context, ty);

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::Ignore {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                            },
                        );

                        llvm_parameters_types.push(llvm_ty.into());
                    }

                    SystemVABIType::Same(ty) => {
                        let llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(llvm_context, abi_context, ty);

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::Same {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                            },
                        );

                        llvm_parameters_types.push(llvm_ty.into());
                    }

                    SystemVABIType::Coerce(original_ty, coerced_width_bits) => {
                        let llvm_coerced_ty: BasicTypeEnum<'_> =
                            llvm_context.custom_width_int_type(coerced_width_bits).into();

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::Coerce {
                                name,
                                ascii_name,
                                original_ty,
                                coerced_width_bits,
                                index: idx,
                            },
                        );

                        llvm_parameters_types.push(llvm_coerced_ty.into());
                    }

                    SystemVABIType::ToMemory(ty) => {
                        let byval_ty: Type = ty.dereference();

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                                attribute:
                                    SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(
                                        byval_ty,
                                    ),
                                is_sret: false,
                            },
                        );

                        llvm_parameters_types
                            .push(llvm_context.ptr_type(AddressSpace::default()).into());
                    }

                    SystemVABIType::DecomposeAndExpand(field_types, ref variant) => {
                        if variant.is_decompose_and_expand_structure() {
                            let mut decomposed_types: Vec<BasicMetadataTypeEnum> = Vec::new();
                            let mut struct_field_indexes: Vec<usize> = Vec::new();

                            let mut llvm_parameters_last_index: usize =
                                llvm_parameters_types.len().saturating_sub(1);

                            let mut finish_decompose_process: bool = false;

                            for (field_idx, field_type) in field_types.iter().enumerate() {
                                let ty_claseses: [SystemVABITypeClass; 8] =
                                    SystemVABITypeClass::get_system_v_type_class(
                                        abi_context,
                                        field_type,
                                    );

                                let abi_ty: SystemVABIType =
                                    SystemVABIType::class_to_general_abi_strategy(
                                        abi_context,
                                        &ty_claseses,
                                        field_type,
                                    );

                                if abi_ty.is_decompose_and_expand() || abi_ty.is_to_memory() {
                                    configuration_parameter_types
                                            .push(SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                                            name,
                                            ascii_name,
                                            ty,
                                            index: idx,
                                            attribute:
                                                SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(ty.clone()),
                                            is_sret: false,
                                        });

                                    llvm_parameters_types.push(
                                        llvm_context.ptr_type(AddressSpace::default()).into(),
                                    );

                                    finish_decompose_process = true;
                                    break;
                                } else {
                                    let llvm_ty: BasicTypeEnum<'_> =
                                        self::generate_type(llvm_context, abi_context, field_type);

                                    struct_field_indexes.push(field_idx);
                                    decomposed_types.push(llvm_ty.into());
                                }
                            }

                            if finish_decompose_process {
                                continue;
                            }

                            let mut decomposed_indexes: Vec<usize> = Vec::new();

                            for _ in decomposed_types.iter() {
                                decomposed_indexes.push(llvm_parameters_last_index);
                                llvm_parameters_last_index += 1;
                            }

                            configuration_parameter_types.push(
                                SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                                    name,
                                    ascii_name,
                                    old_type: ty,
                                    struct_field_indexes,
                                    array_decomposed_types: Vec::new(),
                                    decomposed_indexes,
                                    variant: *variant,
                                    index: idx,
                                },
                            );

                            llvm_parameters_types.extend(decomposed_types.iter());
                        }

                        if variant.is_decompose_and_expand_array() {
                            let mut decomposed_types: Vec<BasicMetadataTypeEnum> = Vec::new();

                            let mut llvm_parameters_last_index: usize =
                                llvm_parameters_types.len().saturating_sub(1);

                            for field_type in field_types.iter() {
                                let llvm_ty: BasicTypeEnum<'_> =
                                    self::generate_type(llvm_context, abi_context, field_type);

                                decomposed_types.push(llvm_ty.into());
                            }

                            let mut decomposed_indexes: Vec<usize> = Vec::new();

                            for _ in decomposed_types.iter() {
                                decomposed_indexes.push(llvm_parameters_last_index);
                                llvm_parameters_last_index += 1;
                            }

                            configuration_parameter_types.push(
                                SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
                                    name,
                                    ascii_name,
                                    old_type: ty,
                                    struct_field_indexes: Vec::new(),
                                    array_decomposed_types: field_types.clone(),
                                    decomposed_indexes,
                                    variant: *variant,
                                    index: idx,
                                },
                            );

                            llvm_parameters_types.extend(decomposed_types.iter());
                        }
                    }
                }

                idx += 1;
            }

            Ast::AssemblerFunctionParameter { .. } => (),
            Ast::CompilerIntrinsicParameter { .. } => (),

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

        match abi_return_ty {
            SystemVABIType::Ignore => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, return_type);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            SystemVABIType::Same(ty) => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, ty);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            SystemVABIType::Coerce(_, coerced_width_bits) => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    llvm_context.custom_width_int_type(coerced_width_bits).into();

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            SystemVABIType::ToMemory(..) => {
                if is_memory_return {
                    (
                        llvm_context
                            .void_type()
                            .fn_type(&llvm_parameters_types, is_variatic),
                        configuration,
                    )
                } else {
                    let llvm_return_ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, return_type);

                    (
                        llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                        configuration,
                    )
                }
            }

            SystemVABIType::DecomposeAndExpand(..) => {
                let llvm_return_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, return_type);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }
        }
    }
}

pub fn lower_function_parameter_conventions<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &SystemVABIFunctionTypeConfiguration,
) {
    let function_parameters: Vec<BasicValueEnum<'_>> = function_value.get_params();

    let ordered_configurations: Vec<&SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let _ = ordered_configurations.is_sorted_by_key(|config| match config {
        SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Coerce { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    for parameter_configuration in ordered_configurations.iter() {
        match parameter_configuration {
            SystemVABIFunctionTypeArgumentConfiguration::Ignore { .. } => {}
            SystemVABIFunctionTypeArgumentConfiguration::Same { .. } => {}
            SystemVABIFunctionTypeArgumentConfiguration::Coerce { .. } => {}

            SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                ty,
                index,
                attribute,
                ..
            } => {
                let type_layout: either::Either<
                    thrustc_typesystem::type_layout::TypeLayout,
                    thrustc_typesystem::type_layout::StructTypeLayout,
                > = abi_context.get_mut_target_info().get_type_layout(r#ty);

                let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
                    either::Either::Left(ty) => ty.into_layout(),
                    either::Either::Right(ty) => ty.into_layout(),
                };

                let alignment: u32 = layout.alignof;

                if let SystemVABIFunctionTypeArgumentConfigurationAttributes::Sret(ty) = attribute {
                    let Some(_) = function_parameters.get(*index) else {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                            ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    };

                    let llvm_ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, ty);

                    let alignment: u32 = abi_context
                        .get_target_data()
                        .get_preferred_alignment(&llvm_ty);

                    let sret_id: u32 = Attribute::get_named_enum_kind_id("sret");

                    let sret_attribute: Attribute =
                        llvm_context.create_type_attribute(sret_id, llvm_ty.as_any_type_enum());

                    function_value.add_attribute(
                        AttributeLoc::Param((*index).try_into().unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to parse parameter index on System-V ABI lowering!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })),
                        sret_attribute,
                    );

                    function_value
                        .set_param_alignment((*index).try_into().unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set parameter memory alignment on System-V ABI lowering!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        }), alignment);
                }

                if let SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(ty) = attribute
                {
                    let Some(_) = function_parameters.get(*index) else {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                            ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    };

                    let llvm_ty: BasicTypeEnum<'_> =
                        self::generate_type(llvm_context, abi_context, ty);

                    let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                    let byval_attribute: Attribute =
                        llvm_context.create_type_attribute(byval_id, llvm_ty.as_any_type_enum());

                    function_value.add_attribute(
                        AttributeLoc::Param((*index).try_into().unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to parse parameter index on System-V ABI lowering!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })),
                        byval_attribute,
                    );

                    function_value
                        .set_param_alignment((*index).try_into().unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set parameter memory alignment on System-V ABI lowering!",
                                ty.get_span(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        }), alignment);
                }
            }

            _ => (),
        }
    }
}

pub fn generate_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut SystemVABIContext,
    ty: &Type,
) -> BasicTypeEnum<'llvm_abi> {
    match ty {
        t if t.is_integer_type() || t.is_char_type() || t.is_bool_type() => match t {
            Type::S8 { .. } | Type::U8 { .. } | Type::Char { .. } => llvm_context.i8_type().into(),
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

        t if t.is_ptr_like_type() => {
            let address_space: Option<u16> = t.get_address_space();

            if let Some(address_space) = address_space {
                llvm_context
                    .ptr_type(AddressSpace::from(address_space))
                    .into()
            } else {
                llvm_context.ptr_type(AddressSpace::default()).into()
            }
        }

        Type::Const(subtype, ..) => self::generate_type(llvm_context, abi_context, subtype),

        Type::Struct {
            fields, metadata, ..
        } => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::with_capacity(u8::MAX as usize);

            let modifications: &StructureTypeModificator = metadata.get_struct_type_modificator();

            let packed: bool = modifications.llvm().is_packed();

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

#[inline]
pub fn constant_address_space_to_normal<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    ptr: PointerValue<'llvm_abi>,
) -> PointerValue<'llvm_abi> {
    ptr.const_address_space_cast(llvm_context.ptr_type(AddressSpace::default()))
}

#[inline]
pub fn address_space_to_normal<'llvm_abi>(
    abi_context: &mut SystemVABIContext,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    ptr: PointerValue<'llvm_abi>,
    span: Span,
) -> PointerValue<'llvm_abi> {
    llvm_builder
        .build_address_space_cast(ptr, llvm_context.ptr_type(AddressSpace::default()), "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                abi_context,
                "Failed to compile a address space cast!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        })
}
