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
    builder::Builder,
    context::Context,
    targets::TargetData,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, InstructionValue,
        PointerValue,
    },
};
use thrustc_ast::Ast;
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeFixedArrayEntensions, TypeIsExtensions, TypePointerExtensions},
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
        self.target_data
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
            | Type::Bool { .. } => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::SSize { .. } | Type::USize { .. } if layout.sizeof <= 8 || layout.sizeof <= 4 => {
                X86_SYSTEM_V_ABI_ONE_INTEGER
            }

            Type::SSize { .. } | Type::USize { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::U128 { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::F32 { .. } | Type::F64 { .. } => X86_SYSTEM_V_ABI_F32_F64,

            Type::F128 { .. } => X86_SYSTEM_V_ABI_F128,

            t if t.is_ptr_like_type() => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::FixedArray { base_type, .. } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size > 16 {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [X86SystemVABITypeClass; 8] =
                    [X86SystemVABITypeClass::NO_CLASS; 8];

                let subty_classes: [X86SystemVABITypeClass; 8] =
                    Self::get_system_v_type_class(abi_context, base_type);

                for field_offset_bits in layout.field_offsets.iter() {
                    let field_offset_bytes: u32 = field_offset_bits / 8;
                    let subty_class: X86SystemVABITypeClass = subty_classes[0];

                    if matches!(subty_class, X86SystemVABITypeClass::NO_CLASS) {
                        break;
                    }

                    let target_eightbyte_idx: u32 = field_offset_bytes / 8;

                    if target_eightbyte_idx < 8 {
                        current_classes[target_eightbyte_idx as usize] = Self::combine(
                            current_classes[target_eightbyte_idx as usize],
                            subty_class,
                        );
                    }
                }

                if current_classes.contains(&X86SystemVABITypeClass::MEMORY) {
                    return X86_SYSTEM_V_ABI_STACK;
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
                            .iter()
                            .skip(1)
                            .any(|c| !matches!(c, X86SystemVABITypeClass::SSEUP)))
                {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx.saturating_sub(1)] {
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

            Type::Struct {
                fields, modifier, ..
            } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size > 16 {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                if modifier.llvm().is_packed() {
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

                if current_classes.contains(&X86SystemVABITypeClass::MEMORY) {
                    return X86_SYSTEM_V_ABI_STACK;
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
                            .iter()
                            .skip(1)
                            .any(|c| !matches!(c, X86SystemVABITypeClass::SSEUP)))
                {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx.saturating_sub(1)] {
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
pub enum x86SystemVABIType<'llvm_abi> {
    Same(&'llvm_abi Type),
    ToMemory(&'llvm_abi Type),
    DecomposeAndExpand(Vec<Type>, x86SystemVABITypeDecomposeAndExpandVariant),
    Ignore,
}

#[derive(Debug, Clone, Copy)]
pub enum x86SystemVABITypeDecomposeAndExpandVariant {
    DecomposeAndExpandStructure,
    DecomposeAndExpandInteger128,
    DecomposeAndExpandArray,
}

impl x86SystemVABITypeDecomposeAndExpandVariant {
    #[inline]
    pub fn is_decompose_and_expand_structure(&self) -> bool {
        matches!(
            self,
            x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandStructure
        )
    }

    #[inline]
    pub fn is_decompose_and_expand_integer128(&self) -> bool {
        matches!(
            self,
            x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandInteger128
        )
    }

    #[inline]
    pub fn is_decompose_and_expand_array(&self) -> bool {
        matches!(
            self,
            x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray
        )
    }
}

impl x86SystemVABIType<'_> {
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
        matches!(self, x86SystemVABIType::DecomposeAndExpand(..))
    }
}

impl<'llvm_abi> x86SystemVABIType<'llvm_abi> {
    pub fn class_to_general_abi_strategy(
        abi_context: &mut X86SystemVABIContext,
        classes: &[X86SystemVABITypeClass; 8],
        ty: &'llvm_abi Type,
    ) -> x86SystemVABIType<'llvm_abi> {
        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

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
                X86SystemVABITypeClass::INTEGER if ty.is_fixed_array_type() => {
                    let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                    if array_fixed_ty.is_array_type()
                        || array_fixed_ty.is_fixed_array_type()
                        || array_fixed_ty.is_struct_type()
                        || array_fixed_ty.is_ptr_like_type()
                    {
                        return x86SystemVABIType::ToMemory(ty);
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
                            return x86SystemVABIType::DecomposeAndExpand(
                                vec![first_integer_ty],
                                x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return x86SystemVABIType::DecomposeAndExpand(
                            vec![first_integer_ty, second_integer_ty],
                            x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    x86SystemVABIType::Same(ty)
                }

                X86SystemVABITypeClass::SSE if ty.is_fixed_array_type() => {
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
                            return x86SystemVABIType::DecomposeAndExpand(
                                vec![first_float_ty],
                                x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        return x86SystemVABIType::DecomposeAndExpand(
                            vec![first_float_ty, second_float_ty],
                            x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    x86SystemVABIType::Same(ty)
                }

                X86SystemVABITypeClass::INTEGER | X86SystemVABITypeClass::SSE => {
                    x86SystemVABIType::Same(ty)
                }
                _ => x86SystemVABIType::Same(ty),
            },

            2 => {
                match (classes[0], classes[1]) {
                    (X86SystemVABITypeClass::INTEGER, X86SystemVABITypeClass::INTEGER)
                        if ty.is_fixed_array_type() =>
                    {
                        let array_fixed_ty: Type = ty.get_fixed_array_base_type();

                        if array_fixed_ty.is_array_type()
                            || array_fixed_ty.is_fixed_array_type()
                            || array_fixed_ty.is_struct_type()
                            || array_fixed_ty.is_ptr_like_type()
                        {
                            return x86SystemVABIType::ToMemory(ty);
                        }

                        let is_integer: bool = array_fixed_ty.is_integer_type();

                        if is_integer {
                            let first_integer_ty: Type = if array_fixed_ty.is_signed_integer_type()
                            {
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
                                return x86SystemVABIType::DecomposeAndExpand(
                                    vec![first_integer_ty],
                                    x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                                );
                            }

                            return x86SystemVABIType::DecomposeAndExpand(
                                vec![first_integer_ty, second_integer_ty],
                                x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        x86SystemVABIType::Same(ty)
                    }

                    (X86SystemVABITypeClass::INTEGER, X86SystemVABITypeClass::INTEGER) => {
                        x86SystemVABIType::Same(ty)
                    }

                    (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSE)
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
                                return x86SystemVABIType::DecomposeAndExpand(
                                    vec![first_float_ty],
                                    x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                                );
                            }

                            return x86SystemVABIType::DecomposeAndExpand(
                                vec![first_float_ty, second_float_ty],
                                x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        x86SystemVABIType::Same(ty)
                    }

                    (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSE) => {
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

                            return x86SystemVABIType::DecomposeAndExpand(
                                vec![first_float_ty, second_float_ty],
                                x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                            );
                        }

                        x86SystemVABIType::Same(ty)
                    }

                    (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSEUP)
                        if ty.is_fixed_array_type() =>
                    {
                        x86SystemVABIType::Same(ty)
                    }

                    (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSEUP) => {
                        x86SystemVABIType::Same(ty)
                    }

                    _ => {
                        if let Type::Struct { fields, .. } = &ty {
                            x86SystemVABIType::DecomposeAndExpand(fields.clone(), x86SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandStructure)
                        } else {
                            x86SystemVABIType::ToMemory(ty)
                        }
                    }
                }
            }

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

#[derive(Debug, Clone)]
pub enum x86SystemVABIFunctionParameterConfiguration {
    Normal,
    FromMemory,
}

#[derive(Debug, Clone)]
pub enum x86SystemVABIFunctionTypeArgumentConfigurationAttributes {
    byVal,
    InReg,
    Sret(Type),

    None,
}

#[derive(Debug, Clone)]
pub enum x86SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi> {
    Same {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        index: usize,
        attribute: x86SystemVABIFunctionTypeArgumentConfigurationAttributes,
    },
    ToMemory {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        index: usize,
        attribute: x86SystemVABIFunctionTypeArgumentConfigurationAttributes,
        is_sret: bool,
    },
    DecomposeAndExpand {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        old_type: &'llvm_abi Type,
        struct_field_indexes: Vec<usize>,
        decomposed_indexes: Vec<usize>,
        array_decomposed_types: Vec<Type>,
        variant: x86SystemVABITypeDecomposeAndExpandVariant,
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
pub struct x86SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    parameter_types: Vec<x86SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi>>,
    is_memory_return: bool,
    is_variatic: bool,
}

impl<'llvm_abi> x86SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    #[inline]
    pub fn new(is_variatic: bool, is_memory_return: bool) -> Self {
        Self {
            parameter_types: Vec::new(),
            is_variatic,
            is_memory_return,
        }
    }
}

impl x86SystemVABIFunctionTypeConfiguration<'_> {
    #[inline]
    pub fn is_variatic(&self) -> bool {
        self.is_variatic
    }

    #[inline]
    pub fn is_memory_return(&self) -> bool {
        self.is_memory_return
    }
}

impl x86SystemVABIFunctionTypeConfiguration<'_> {
    #[inline]
    pub fn set_memory_return(&mut self, value: bool) {
        self.is_memory_return = value
    }
}

impl<'llvm_abi> x86SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    #[inline]
    pub fn get_mut_configuration_parameter_types(
        &mut self,
    ) -> &mut Vec<x86SystemVABIFunctionTypeArgumentConfiguration<'llvm_abi>> {
        &mut self.parameter_types
    }
}

pub fn lower_function_parameters<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &x86SystemVABIFunctionTypeConfiguration<'llvm_abi>,
) -> Vec<(
    &'llvm_abi str,
    &'llvm_abi str,
    &'llvm_abi Type,
    x86SystemVABIFunctionParameterConfiguration,
    BasicValueEnum<'llvm_abi>,
)> {
    let function_value: FunctionValue<'_> = function_value;
    let function_params: Vec<BasicValueEnum<'_>> = function_value.get_params();

    let ordered_configurations: Vec<&x86SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let _ = ordered_configurations.is_sorted_by_key(|config| match config {
        x86SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    let mut processed_parameters: Vec<(
        &'llvm_abi str,
        &'llvm_abi str,
        &'llvm_abi Type,
        x86SystemVABIFunctionParameterConfiguration,
        BasicValueEnum<'_>,
    )> = Vec::new();

    for arg_config in ordered_configurations.iter() {
        match arg_config {
            x86SystemVABIFunctionTypeArgumentConfiguration::Ignore {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_params.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        x86SystemVABIFunctionParameterConfiguration::Normal,
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

            x86SystemVABIFunctionTypeArgumentConfiguration::Same {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_params.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        x86SystemVABIFunctionParameterConfiguration::Normal,
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

            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                name,
                ascii_name,
                ty,
                index,
                ..
            } => {
                if let Some(value) = function_params.get(*index) {
                    processed_parameters.push((
                        name,
                        ascii_name,
                        ty,
                        x86SystemVABIFunctionParameterConfiguration::FromMemory,
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

            x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
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
                        self::decompose_type(llvm_context, abi_context, old_type);

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
                        if let Some(decomposed_value) = function_params.get(*decomposed_idx) {
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
                        x86SystemVABIFunctionParameterConfiguration::FromMemory,
                        ptr.into(),
                    ));
                }

                if variant.is_decompose_and_expand_array() {
                    let ty: BasicTypeEnum<'_> =
                        self::decompose_type(llvm_context, abi_context, old_type);

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
                            function_params.get(decomposed_indexes[0])
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
                            function_params.get(decomposed_indexes[0])
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
                                function_params.get(decomposed_indexes[1])
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
                                    x86SystemVABIFunctionParameterConfiguration::FromMemory,
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

pub fn lower_function_call<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    function_value: FunctionValue<'llvm_abi>,
    configuration: &x86SystemVABIFunctionTypeConfiguration,
    mut args: Vec<BasicValueEnum<'llvm_abi>>,
    span: Span,
) -> Vec<BasicMetadataValueEnum<'llvm_abi>> {
    let function_value: FunctionValue<'_> = function_value;
    let callee_args_values: Vec<BasicValueEnum> = function_value.get_params();

    let mut ordered_configuration: Vec<&x86SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.parameter_types.iter().collect();

    let mut processed_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

    if configuration.is_memory_return() {
        let buffer_type: &Type = ordered_configuration
            .iter()
            .find_map(|config| match config {
                x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                    ty,
                    is_sret: true,
                    ..
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
            self::decompose_type(llvm_context, abi_context, buffer_type);

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
        x86SystemVABIFunctionTypeArgumentConfiguration::Same { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::Ignore { index, .. } => *index,
        x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand { index, .. } => *index,
    });

    assert!(args.len() == ordered_configuration.len());

    for (arg_value, arg_config) in args.iter().zip(ordered_configuration.iter()) {
        match arg_config {
            x86SystemVABIFunctionTypeArgumentConfiguration::Ignore { .. } => {
                processed_args.push((*arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::Same {
                ty,
                attribute,
                index,
                ..
            } => {
                if matches!(
                    attribute,
                    x86SystemVABIFunctionTypeArgumentConfigurationAttributes::InReg
                ) {
                    let Some(_) = callee_args_values.get(*index) else {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                            ty.get_span(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    };

                    let inreg_id: u32 = Attribute::get_named_enum_kind_id("inreg");

                    let inreg_id_attribute: Attribute =
                        llvm_context.create_enum_attribute(inreg_id, 0);

                    function_value.add_attribute(
                        AttributeLoc::Param((*index).try_into().unwrap_or_default()),
                        inreg_id_attribute,
                    );
                }

                processed_args.push((*arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
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
                        x86SystemVABIFunctionTypeArgumentConfigurationAttributes::Sret(ty) => {
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
                                self::decompose_type(llvm_context, abi_context, ty);

                            let alignment: u32 = abi_context
                                .get_target_data()
                                .get_preferred_alignment(&llvm_ty);

                            let sret_id: u32 = Attribute::get_named_enum_kind_id("sret");

                            let sret_attribute: Attribute = llvm_context
                                .create_type_attribute(sret_id, llvm_ty.as_any_type_enum());

                            function_value.add_attribute(
                                AttributeLoc::Param((*index).try_into().unwrap_or_default()),
                                sret_attribute,
                            );

                            function_value.set_param_alignment(
                                (*index).try_into().unwrap_or_default(),
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

                                if matches!(
                                    attribute,
                                    x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal
                                ) {
                                    let Some(_) = callee_args_values.get(*index) else {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                                            ty.get_span(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        );
                                    };

                                    let byval_id: u32 = Attribute::get_named_enum_kind_id("byval");

                                    let byval_attribute: Attribute = llvm_context
                                        .create_type_attribute(
                                            byval_id,
                                            arg_value.get_type().as_any_type_enum(),
                                        );

                                    function_value.add_attribute(
                                        AttributeLoc::Param(
                                            (*index).try_into().unwrap_or_default(),
                                        ),
                                        byval_attribute,
                                    );

                                    function_value.set_param_alignment(
                                        (*index).try_into().unwrap_or_default(),
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

                        if matches!(
                            attribute,
                            x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal
                        ) {
                            let Some(_) = callee_args_values.get(*index) else {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to get the function parameter value from the function declaration for ABI lowering!",
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
                }

                processed_args.push((arg_value).into());
            }

            x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
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
                            self::decompose_type(
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
                            self::decompose_type(
                                llvm_context,
                                abi_context,
                                first_element_decomposed_ty,
                            );

                        let second_element_decomposed_llvm_ty: BasicTypeEnum<'_> =
                            self::decompose_type(
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

pub fn lower_call_return<'llvm_abi>(
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    callsite: CallSiteValue<'llvm_abi>,
    lowered_args: &[BasicMetadataValueEnum<'llvm_abi>],
    configuration: &x86SystemVABIFunctionTypeConfiguration,
    span: Span,
) -> BasicValueEnum<'llvm_abi> {
    let called_function: FunctionValue<'_> = callsite.get_called_fn_value();
    let is_void_type: bool = called_function.get_type().get_return_type().is_none();

    if configuration.is_memory_return() && is_void_type {
        todo!()

        /*
            let first_arg: &BasicMetadataValueEnum<'_> = lowered_args.first().unwrap();
            let ptr: PointerValue<'_> = first_arg.into_pointer_value();
            let ty: &&Type = configuration
                .parameter_types
                .iter()
                .find_map(|config| match config {
                    x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                        ty,
                        is_sret: true,
                        ..
                    } => Some(ty),
                    _ => None,
                })
                .unwrap();

            let llvm_ptr_ty: BasicTypeEnum<'_> = self::decompose_type(llvm_context, abi_context, ty);

            let value: BasicValueEnum<'_> = llvm_builder.build_load(llvm_ptr_ty, ptr, "").unwrap();

            return value;
        */
    }

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

pub fn lower_return<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    llvm_builder: &'llvm_abi Builder<'llvm_abi>,
    abi_context: &mut X86SystemVABIContext,
    configuration: &x86SystemVABIFunctionTypeConfiguration,
    return_value: Option<BasicValueEnum<'llvm_abi>>,
    span: Span,
) -> bool {
    if configuration.is_memory_return() {
        if let Some(return_value) = return_value {}

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

pub fn decompose_function_type<'llvm_abi>(
    llvm_context: &'llvm_abi Context,
    abi_context: &mut X86SystemVABIContext,
    return_type: &'llvm_abi Type,
    parameters: &'llvm_abi [Ast<'llvm_abi>],
    is_variatic: bool,
) -> (
    FunctionType<'llvm_abi>,
    x86SystemVABIFunctionTypeConfiguration<'llvm_abi>,
) {
    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum<'llvm_abi>> =
        Vec::with_capacity(parameters.len());

    let mut configuration: x86SystemVABIFunctionTypeConfiguration =
        x86SystemVABIFunctionTypeConfiguration::new(is_variatic, false);

    let return_ty_classes: [X86SystemVABITypeClass; 8] =
        X86SystemVABITypeClass::get_system_v_type_class(abi_context, return_type);

    let mut is_memory_return: bool = false;
    let mut idx: usize = 0;

    let abi_return_ty: x86SystemVABIType = x86SystemVABIType::class_to_general_abi_strategy(
        abi_context,
        &return_ty_classes,
        return_type,
    );

    if abi_return_ty.is_to_memory() {
        is_memory_return = true;
        configuration.set_memory_return(true);
    }

    let configuration_parameter_types: &mut Vec<x86SystemVABIFunctionTypeArgumentConfiguration> =
        configuration.get_mut_configuration_parameter_types();

    if is_memory_return {
        llvm_parameters_types.insert(0, llvm_context.ptr_type(AddressSpace::default()).into());

        configuration_parameter_types.push(
            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                name: "",
                ascii_name: "",
                ty: return_type,
                index: idx,
                attribute: x86SystemVABIFunctionTypeArgumentConfigurationAttributes::Sret(
                    return_type.clone(),
                ),
                is_sret: true,
            },
        );

        idx = idx.saturating_add(1);
    }

    for parameter in parameters.iter() {
        match parameter {
            Ast::FunctionParameter {
                name,
                ascii_name,
                kind: ty,
                ..
            } => {
                let ty_claseses: [X86SystemVABITypeClass; 8] =
                    X86SystemVABITypeClass::get_system_v_type_class(abi_context, ty);

                let abi_ty: x86SystemVABIType =
                    x86SystemVABIType::class_to_general_abi_strategy(abi_context, &ty_claseses, ty);

                match abi_ty {
                    x86SystemVABIType::Ignore => {
                        let llvm_ty: BasicTypeEnum<'_> =
                            self::decompose_type(llvm_context, abi_context, ty);

                        configuration_parameter_types.push(
                            x86SystemVABIFunctionTypeArgumentConfiguration::Ignore {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                            },
                        );

                        llvm_parameters_types.push(llvm_ty.into());
                    }

                    x86SystemVABIType::Same(ty) => {
                        let llvm_ty: BasicTypeEnum<'_> =
                            self::decompose_type(llvm_context, abi_context, ty);

                        configuration_parameter_types.push(
                            x86SystemVABIFunctionTypeArgumentConfiguration::Same {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                                attribute:
                                    x86SystemVABIFunctionTypeArgumentConfigurationAttributes::InReg,
                            },
                        );

                        llvm_parameters_types.push(llvm_ty.into());
                    }

                    x86SystemVABIType::ToMemory(_) => {
                        configuration_parameter_types.push(
                            x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                                name,
                                ascii_name,
                                ty,
                                index: idx,
                                attribute:
                                    x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal,
                                is_sret: false,
                            },
                        );

                        llvm_parameters_types
                            .push(llvm_context.ptr_type(AddressSpace::default()).into());
                    }

                    x86SystemVABIType::DecomposeAndExpand(field_types, ref variant) => {
                        if variant.is_decompose_and_expand_structure() {
                            let mut decomposed_types: Vec<BasicMetadataTypeEnum> = Vec::new();
                            let mut struct_field_indexes: Vec<usize> = Vec::new();

                            let mut llvm_parameters_last_index: usize =
                                llvm_parameters_types.len().saturating_sub(1);

                            let mut finish_decompose_process: bool = false;

                            for (field_idx, field_type) in field_types.iter().enumerate() {
                                let ty_claseses: [X86SystemVABITypeClass; 8] =
                                    X86SystemVABITypeClass::get_system_v_type_class(
                                        abi_context,
                                        field_type,
                                    );

                                let abi_ty: x86SystemVABIType =
                                    x86SystemVABIType::class_to_general_abi_strategy(
                                        abi_context,
                                        &ty_claseses,
                                        field_type,
                                    );

                                if abi_ty.is_decompose_and_expand() || abi_ty.is_to_memory() {
                                    configuration_parameter_types
                                            .push(x86SystemVABIFunctionTypeArgumentConfiguration::ToMemory {
                                            name,
                                            ascii_name,
                                            ty,
                                            index: idx,
                                            attribute:
                                                x86SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal,
                                            is_sret: false,
                                        });

                                    llvm_parameters_types.push(
                                        llvm_context.ptr_type(AddressSpace::default()).into(),
                                    );

                                    finish_decompose_process = true;
                                    break;
                                } else {
                                    let llvm_ty: BasicTypeEnum<'_> =
                                        self::decompose_type(llvm_context, abi_context, field_type);

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
                                x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
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
                                    self::decompose_type(llvm_context, abi_context, field_type);

                                decomposed_types.push(llvm_ty.into());
                            }

                            let mut decomposed_indexes: Vec<usize> = Vec::new();

                            for _ in decomposed_types.iter() {
                                decomposed_indexes.push(llvm_parameters_last_index);
                                llvm_parameters_last_index += 1;
                            }

                            configuration_parameter_types.push(
                                x86SystemVABIFunctionTypeArgumentConfiguration::DecomposeAndExpand {
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

            Ast::IntrinsicParameter { .. } => (),

            _ => (),
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
                    self::decompose_type(llvm_context, abi_context, ty);

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            x86SystemVABIType::ToMemory(..) => {
                if is_memory_return {
                    (
                        llvm_context
                            .void_type()
                            .fn_type(&llvm_parameters_types, is_variatic),
                        configuration,
                    )
                } else {
                    let llvm_return_ty: BasicTypeEnum<'_> =
                        self::decompose_type(llvm_context, abi_context, return_type);

                    (
                        llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                        configuration,
                    )
                }
            }

            x86SystemVABIType::DecomposeAndExpand(..) => {
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

            Type::Bool { .. } => llvm_context.bool_type().into(),
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

        Type::FixedArray {
            base_type, size, ..
        } => {
            let array_type: BasicTypeEnum =
                self::decompose_type(llvm_context, abi_context, base_type);
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
    abi_context: &mut X86SystemVABIContext,
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
