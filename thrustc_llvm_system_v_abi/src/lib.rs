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
#![allow(clippy::if_same_then_else)]

mod abort;

use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    targets::TargetData,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, VectorType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
        InstructionValue, IntValue, PointerValue,
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
        ConstantTypeExtensions, DereferenceExtensions, TypeCodeLocation, TypeExtensions,
        TypeFixedArrayEntensions, TypeIsExtensions, TypePointerExtensions,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SystemVABITypeContext {
    Arg,
    Ret,
    Other,
}

#[derive(Debug, Clone, Copy)]
pub enum SystemVCodeGenLocation {
    LValue,
    RValue,

    CallArgExpr,

    None,
}

impl SystemVCodeGenLocation {
    #[inline]
    pub fn is_direct_behavior(&self) -> bool {
        matches!(self, SystemVCodeGenLocation::LValue)
    }

    #[inline]
    pub fn is_load_behavior(&self) -> bool {
        matches!(
            self,
            SystemVCodeGenLocation::CallArgExpr | SystemVCodeGenLocation::RValue
        )
    }
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
        let abi_variant: X64ABIVariant = if target_triple.is_windows_based() {
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
    pub fn get_target_triple(&self) -> &LLVMTargetTriple {
        self.target_triple
    }

    #[inline]
    pub fn get_codegen_location(&self) -> SystemVCodeGenLocation {
        self.codegen_location
    }
}

// FLOAT, FLOAT_COMBINE and INTEGER_PER_ELEMENT mirror Zig's Class variants in src/codegen/x86_64/abi.zig.
// https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig
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
    FLOAT,
    FLOAT_COMBINE,
    INTEGER_PER_ELEMENT,
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
            SystemVABITypeClass::FLOAT => "FLOAT",
            SystemVABITypeClass::FLOAT_COMBINE => "FLOAT_COMBINE",
            SystemVABITypeClass::INTEGER_PER_ELEMENT => "INTEGER_PER_ELEMENT",
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

pub const SYSTEM_V_ABI_FLOAT: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::FLOAT,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_F80: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::X87,
    SystemVABITypeClass::X87UP,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_COMPLEX_X87: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::X87,
    SystemVABITypeClass::X87UP,
    SystemVABITypeClass::X87,
    SystemVABITypeClass::X87UP,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_INTEGER_PER_ELEMENT: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::INTEGER_PER_ELEMENT,
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

// clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) splits named vectors up to the AVX native width into SSE/SSEUP; apply that split only for explicit AVX or AVX-512F so enabled vectors stay in registers. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
pub const SYSTEM_V_ABI_NATIVE_VECTOR_32B: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::SSE,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
    SystemVABITypeClass::NO_CLASS,
];

pub const SYSTEM_V_ABI_NATIVE_VECTOR_64B: [SystemVABITypeClass; 8] = [
    SystemVABITypeClass::SSE,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
    SystemVABITypeClass::SSEUP,
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
            if accum == SystemVABITypeClass::FLOAT {
                return SystemVABITypeClass::FLOAT_COMBINE;
            }

            return accum;
        }

        if accum == SystemVABITypeClass::NO_CLASS {
            return current;
        }

        if accum == SystemVABITypeClass::MEMORY || current == SystemVABITypeClass::MEMORY {
            return SystemVABITypeClass::MEMORY;
        }

        if accum == SystemVABITypeClass::INTEGER || current == SystemVABITypeClass::INTEGER {
            return SystemVABITypeClass::INTEGER;
        }

        if matches!(accum, SystemVABITypeClass::X87 | SystemVABITypeClass::X87UP)
            || matches!(
                current,
                SystemVABITypeClass::X87 | SystemVABITypeClass::X87UP
            )
        {
            return SystemVABITypeClass::MEMORY;
        }

        SystemVABITypeClass::SSE
    }

    pub fn get_system_v_type_class(
        abi_context: &mut SystemVABIContext,
        ty: &Type,
        context: SystemVABITypeContext,
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
            Type::Const(subtype, ..) => {
                Self::get_system_v_type_class(abi_context, subtype, context)
            }

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

            Type::FX8680 { .. } => SYSTEM_V_ABI_F80,

            // Zig maps f32 to FLOAT and f64 to SSE. https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig
            Type::F32 { .. } => SYSTEM_V_ABI_FLOAT,

            Type::F64 { .. } => SYSTEM_V_ABI_F32_F64,

            Type::F128 { .. } | Type::FPPC128 { .. } => SYSTEM_V_ABI_F128,

            Type::NativeVector { element_type, .. } => {
                let bits: u32 = layout.width;

                let is_x86_64: bool = abi_context.get_target_triple().is_x86_64_arch();
                let has_avx: bool = abi_context.get_target_triple().get_cpu().has_avx();
                let has_avx512f: bool = abi_context.get_target_triple().get_cpu().has_avx512f();

                if matches!(element_type.as_ref(), Type::Bool { .. }) {
                    if !is_x86_64 {
                        return SYSTEM_V_ABI_STACK;
                    }

                    if bits <= 32 {
                        return SYSTEM_V_ABI_ONE_INTEGER;
                    }

                    if bits <= 64 {
                        return SYSTEM_V_ABI_F32_F64;
                    }

                    if matches!(context, SystemVABITypeContext::Other) {
                        return SYSTEM_V_ABI_STACK;
                    }

                    if bits <= 128 || (bits <= 256 && has_avx) || (bits <= 512 && has_avx512f) {
                        return SYSTEM_V_ABI_INTEGER_PER_ELEMENT;
                    }

                    return SYSTEM_V_ABI_STACK;
                }

                if matches!(element_type.as_ref(), Type::FX8680 { .. }) {
                    if !is_x86_64 {
                        return SYSTEM_V_ABI_STACK;
                    }

                    if bits <= 80 {
                        return SYSTEM_V_ABI_F80;
                    }

                    if bits <= 160 {
                        return SYSTEM_V_ABI_COMPLEX_X87;
                    }

                    return SYSTEM_V_ABI_STACK;
                }

                if bits <= 64 {
                    return SYSTEM_V_ABI_F32_F64;
                }

                if bits <= 128 {
                    return SYSTEM_V_ABI_F128;
                }

                if !is_x86_64 {
                    return SYSTEM_V_ABI_STACK;
                }

                if matches!(context, SystemVABITypeContext::Arg) && !has_avx {
                    return SYSTEM_V_ABI_STACK;
                }

                // ziglang/zig src/codegen/x86_64/abi.zig @ 738d2be9d6b6ef3ff3559130c05159ef53336224, vector branch: pick the register width from the vector bit size and context.
                // Register widths: 1-2 eightbytes use XMM, 3-4 use YMM, and 5-8 use ZMM.
                // Zig derives those counts in abi.zig lines 224-254, one eightbyte per 64 bits of vector width: https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig#L224-L254
                // Zig takes the rule from the AMD64 psABI section 3.2.3: __m256 is four eightbytes and __m512 is eight, with the low eightbyte SSE and the rest SSEUP.
                // https://gitlab.com/x86-psABIs/x86-64-ABI
                // An eightbyte is one 64-bit chunk of the value; the vector bits are split into these chunks for classification.
                // The branch requires AVX for argument vectors above 128 bits and AVX-512F above 256 bits.
                // Fill the covered eightbytes with SSEUP and leave the remaining slots as NO_CLASS.
                let max_eightbytes: usize = if bits <= 192 {
                    3
                } else if bits <= 256 {
                    4
                } else if bits <= 320 {
                    5
                } else if bits <= 384 {
                    6
                } else if bits <= 448 {
                    7
                } else if bits <= 512 {
                    8
                } else if matches!(context, SystemVABITypeContext::Ret)
                    && bits
                        <= if has_avx512f {
                            2048
                        } else if has_avx {
                            1024
                        } else {
                            512
                        }
                {
                    8
                } else {
                    return SYSTEM_V_ABI_STACK;
                };

                if max_eightbytes > 4
                    && matches!(context, SystemVABITypeContext::Arg)
                    && !has_avx512f
                {
                    return SYSTEM_V_ABI_STACK;
                }

                let mut classes: [SystemVABITypeClass; 8] = [SystemVABITypeClass::NO_CLASS; 8];

                classes[0] = SystemVABITypeClass::SSE;

                for class in classes.iter_mut().take(max_eightbytes).skip(1) {
                    *class = SystemVABITypeClass::SSEUP;
                }

                classes
            }

            t if t.is_ptr_like_type() => SYSTEM_V_ABI_ONE_INTEGER,

            // Zig classifies arrays up to 16 bytes as INTEGER and sends larger arrays to memory. https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig
            Type::FixedArray { .. } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size <= 8 {
                    return SYSTEM_V_ABI_ONE_INTEGER;
                }

                if abi_size <= 16 {
                    return SYSTEM_V_ABI_TWO_INTEGERS;
                }

                SYSTEM_V_ABI_STACK
            }

            Type::Struct {
                fields, metadata, ..
            } => {
                let modifications: &StructureTypeModificator =
                    metadata.get_struct_type_modificator();

                let abi_size: u32 = layout.abi_size;

                if abi_size > 64 {
                    return SYSTEM_V_ABI_STACK;
                }

                if modifications.llvm().is_packed() {
                    return SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [SystemVABITypeClass; 8] =
                    [SystemVABITypeClass::NO_CLASS; 8];

                for (i, field_type) in fields.iter().enumerate() {
                    let field_classes: [SystemVABITypeClass; 8] = Self::get_system_v_type_class(
                        abi_context,
                        field_type,
                        SystemVABITypeContext::Other,
                    );

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
                        || current_classes.iter().skip(1).any(|c| {
                            !matches!(
                                c,
                                SystemVABITypeClass::SSEUP | SystemVABITypeClass::NO_CLASS
                            )
                        }))
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

                if matches!(
                    context,
                    SystemVABITypeContext::Arg | SystemVABITypeContext::Ret
                ) && abi_size > 16
                {
                    let has_avx: bool = abi_context.get_target_triple().get_cpu().has_avx();
                    let has_avx512f: bool = abi_context.get_target_triple().get_cpu().has_avx512f();

                    if !has_avx {
                        return SYSTEM_V_ABI_STACK;
                    }

                    if abi_size > 32 && !has_avx512f {
                        return SYSTEM_V_ABI_STACK;
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
                any.get_code_location(),
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
    IntegerPerElement(&'llvm_abi Type, u32),
    CoerceFloatCombine(&'llvm_abi Type, u32),
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

    #[inline]
    pub fn is_integer_per_element(&self) -> bool {
        matches!(self, SystemVABIType::IntegerPerElement(..))
    }

    #[inline]
    pub fn is_coerce_float_combine(&self) -> bool {
        matches!(self, SystemVABIType::CoerceFloatCombine(..))
    }
}

impl<'llvm_abi> SystemVABIType<'llvm_abi> {
    pub fn class_to_general_abi_strategy(
        abi_context: &mut SystemVABIContext,
        classes: &[SystemVABITypeClass; 8],
        ty: &'llvm_abi Type,
    ) -> SystemVABIType<'llvm_abi> {
        let non_constant_ty: Type = ty.remove_all_constant_type();

        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

        if abi_context.abi_variant == X64ABIVariant::Windows {
            if non_constant_ty.is_struct_type() || non_constant_ty.is_fixed_array_type() {
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

        if let Type::Struct { fields, .. } = &non_constant_ty {
            if used > 0
                && fields
                    .iter()
                    .all(|field| matches!(field.remove_all_constant_type(), Type::F32 { .. }))
                && classes[..used]
                    .iter()
                    .all(|class| *class == SystemVABITypeClass::FLOAT_COMBINE)
            {
                return SystemVABIType::CoerceFloatCombine(ty, used as u32);
            }
        }

        let is_sse_aggregate: bool = matches!(
            non_constant_ty,
            Type::NativeVector { .. } | Type::Struct { .. }
        );

        if is_sse_aggregate
            && used > 0
            && classes[..used].iter().all(|class| {
                matches!(
                    class,
                    SystemVABITypeClass::SSE
                        | SystemVABITypeClass::SSEUP
                        | SystemVABITypeClass::FLOAT
                        | SystemVABITypeClass::FLOAT_COMBINE
                )
            })
        {
            return SystemVABIType::Same(ty);
        }

        match used {
            1 => match classes[0] {
                SystemVABITypeClass::INTEGER if non_constant_ty.is_fixed_array_type() => {
                    let array_fixed_ty: Type = non_constant_ty.get_fixed_array_base_type();

                    if array_fixed_ty.is_array_type()
                        || array_fixed_ty.is_fixed_array_type()
                        || array_fixed_ty.is_struct_type()
                        || array_fixed_ty.is_ptr_like_type()
                    {
                        return SystemVABIType::ToMemory(ty);
                    }

                    let is_integer: bool = array_fixed_ty.is_integer_type();

                    if is_integer {
                        fn integer_chunk_type(bits: u32, span: Span) -> Type {
                            match bits {
                                0..=8 => Type::U8 { span },
                                9..=16 => Type::U16 { span },
                                17..=32 => Type::U32 { span },
                                _ => Type::U64 { span },
                            }
                        }

                        if layout.abi_size <= 8 {
                            return SystemVABIType::Coerce(ty, layout.width);
                        }

                        let first_integer_ty: Type = integer_chunk_type(64, ty.get_code_location());
                        let second_integer_ty: Type = integer_chunk_type(
                            layout.width.saturating_sub(64),
                            ty.get_code_location(),
                        );

                        return SystemVABIType::DecomposeAndExpand(
                            vec![first_integer_ty, second_integer_ty],
                            SystemVABITypeDecomposeAndExpandVariant::DecomposeAndExpandArray,
                        );
                    }

                    SystemVABIType::Same(ty)
                }

                SystemVABITypeClass::SSE if non_constant_ty.is_fixed_array_type() => {
                    let array_fixed_ty: Type = non_constant_ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_code_location(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_code_location(),
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
                    if non_constant_ty.is_struct_type()
                        || non_constant_ty.is_fixed_array_type() =>
                {
                    SystemVABIType::Coerce(ty, layout.width)
                }
                SystemVABITypeClass::INTEGER
                | SystemVABITypeClass::SSE
                | SystemVABITypeClass::FLOAT
                | SystemVABITypeClass::FLOAT_COMBINE => SystemVABIType::Same(ty),

                SystemVABITypeClass::INTEGER_PER_ELEMENT => {
                    if let Type::NativeVector { element_count, .. } = &non_constant_ty {
                        SystemVABIType::IntegerPerElement(ty, *element_count)
                    } else {
                        SystemVABIType::ToMemory(ty)
                    }
                }

                _ => SystemVABIType::Same(ty),
            },

            2 => match (classes[0], classes[1]) {
                (SystemVABITypeClass::INTEGER, SystemVABITypeClass::INTEGER)
                    if non_constant_ty.is_fixed_array_type() =>
                {
                    fn integer_chunk_type(bits: u32, span: Span) -> Type {
                        match bits {
                            0..=8 => Type::U8 { span },
                            9..=16 => Type::U16 { span },
                            17..=32 => Type::U32 { span },
                            _ => Type::U64 { span },
                        }
                    }

                    let array_fixed_ty: Type = non_constant_ty.get_fixed_array_base_type();

                    if array_fixed_ty.is_array_type()
                        || array_fixed_ty.is_fixed_array_type()
                        || array_fixed_ty.is_struct_type()
                        || array_fixed_ty.is_ptr_like_type()
                    {
                        return SystemVABIType::ToMemory(ty);
                    }

                    let is_integer: bool = array_fixed_ty.is_integer_type();

                    if is_integer {
                        if layout.abi_size <= 8 {
                            return SystemVABIType::Coerce(ty, layout.width);
                        }

                        let first_integer_ty: Type = integer_chunk_type(64, ty.get_code_location());

                        // REASON: clang System-V ABI = i64 + i32 e.g
                        let second_integer_ty: Type = integer_chunk_type(
                            layout.width.saturating_sub(64),
                            ty.get_code_location(),
                        );

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
                    if non_constant_ty.is_fixed_array_type() =>
                {
                    let array_fixed_ty: Type = non_constant_ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_code_location(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_code_location(),
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
                    let array_fixed_ty: Type = non_constant_ty.get_fixed_array_base_type();

                    let is_float: bool = array_fixed_ty.is_float_type();

                    if is_float {
                        let first_float_ty: Type = if let Type::F32 { .. } = array_fixed_ty {
                            Type::F32 {
                                span: ty.get_code_location(),
                            }
                        } else {
                            Type::F64 {
                                span: ty.get_code_location(),
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
                    if non_constant_ty.is_fixed_array_type() =>
                {
                    SystemVABIType::Same(ty)
                }

                (SystemVABITypeClass::SSE, SystemVABITypeClass::SSEUP) => SystemVABIType::Same(ty),

                (SystemVABITypeClass::MEMORY, _) => SystemVABIType::ToMemory(ty),

                _ => {
                    if let Type::Struct { fields, .. } = &non_constant_ty {
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
    IntegerPerElement {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        element_count: u32,
        first_index: usize,
        index: usize,
    },
    CoerceFloatCombine {
        name: &'llvm_abi str,
        ascii_name: &'llvm_abi str,
        ty: &'llvm_abi Type,
        chunk_count: u32,
        first_index: usize,
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
    return_type: Option<SystemVABIType<'llvm_abi>>,
    is_memory_return: bool,
    is_variatic: bool,
}

impl<'llvm_abi> SystemVABIFunctionTypeConfiguration<'llvm_abi> {
    #[inline]
    pub fn new(is_variatic: bool, is_memory_return: bool) -> Self {
        Self {
            parameter_types: Vec::new(),
            return_type: None,
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

    #[inline]
    pub fn return_type(&self) -> Option<&SystemVABIType<'_>> {
        self.return_type.as_ref()
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
    pub fn set_return_type(&mut self, return_type: Option<SystemVABIType<'llvm_abi>>) {
        self.return_type = return_type;
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
        SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine { index, .. } => *index,
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
                        ty.get_code_location(),
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
                        ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine {
                name,
                ascii_name,
                ty,
                chunk_count,
                first_index,
                ..
            } => {
                let original_llvm_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, ty);
                let ptr: PointerValue<'_> =
                    llvm_builder.build_alloca(original_llvm_ty, "").unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate memory for a float-combine parameter in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let chunk_ty: VectorType<'_> = llvm_context.f32_type().vec_type(2);
                let chunk_ptr_ty = llvm_context.ptr_type(AddressSpace::default());
                let chunk_ptr: PointerValue<'_> = llvm_builder
                    .build_pointer_cast(ptr, chunk_ptr_ty, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to cast a float-combine parameter pointer in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                for chunk in 0..*chunk_count {
                    let chunk_index: IntValue<'_> =
                        llvm_context.i32_type().const_int(chunk as u64, false);
                    let slot: PointerValue<'_> = unsafe {
                        llvm_builder.build_in_bounds_gep(chunk_ty, chunk_ptr, &[chunk_index], "")
                    }
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to address a float-combine parameter chunk in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    let parameter_index: usize = first_index.saturating_add(chunk as usize);

                    let Some(chunk_value) = function_parameters.get(parameter_index) else {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get a float-combine parameter chunk in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    };

                    llvm_builder
                        .build_store(slot, *chunk_value)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to store a float-combine parameter chunk in System V ABI!",
                                ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                }

                let value: BasicValueEnum<'_> = llvm_builder
                    .build_load(original_llvm_ty, ptr, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a float-combine parameter in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                processed_parameters.push((
                    name,
                    ascii_name,
                    ty,
                    SystemVABIFunctionParameterConfiguration::Normal,
                    value,
                ));
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
                                original_ty.get_code_location(),
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
                                original_ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        })
                        .set_alignment(alignment)
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set the alignment of a store instruction for a coerced parameter in System V ABI!",
                                original_ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    let loaded_value_instruction = llvm_builder
                        .build_load(original_llvm_ty, ptr, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to load a coerced parameter from memory for System V ABI!",
                                original_ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    loaded_value_instruction
                        .as_instruction_value()
                        .and_then(|instruction| instruction.set_alignment(alignment).ok());

                    let loaded_value: BasicValueEnum<'_> = loaded_value_instruction;

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
                        original_ty.get_code_location(),
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
                        ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement {
                name,
                ascii_name,
                ty,
                element_count,
                first_index,
                ..
            } => {
                let vector_type: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, ty);
                let mut vector_value: inkwell::values::VectorValue<'_> =
                    vector_type.into_vector_type().get_undef();

                // lane = element

                for lane in 0..*element_count {
                    let parameter_index: usize = first_index.saturating_add(lane as usize);

                    let Some(lane_value) = function_parameters.get(parameter_index) else {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to get an integer-per-element parameter value from the function declaration for System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    };

                    vector_value = llvm_builder
                        .build_insert_element(
                            vector_value,
                            *lane_value,
                            llvm_context.i32_type().const_int(lane as u64, false),
                            "",
                        )
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to build a vector element for an integer-per-element parameter in System V ABI!",
                                ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                }

                processed_parameters.push((
                    name,
                    ascii_name,
                    ty,
                    SystemVABIFunctionParameterConfiguration::Normal,
                    vector_value.as_basic_value_enum(),
                ));
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
                            old_type.get_code_location(),
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
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            let store: InstructionValue<'_> = llvm_builder.build_store(element_ptr, *decomposed_value).unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to store a decomposed and expanded parameter value in memory for System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            store.set_alignment(alignment).unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                old_type.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_code_location(),
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
                                old_type.get_code_location(),
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
                                        old_type.get_code_location(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                })
                                .set_alignment(alignment)
                                .unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                        old_type.get_code_location(),
                                        std::path::PathBuf::from(file!()),
                                        line!(),
                                    )
                                });
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_code_location(),
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
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            })
                            .set_alignment(alignment)
                            .unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to set the alignment of a store instruction for a decomposed and expanded parameter in System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                            if let Some(second_decomposed_value) =
                                function_parameters.get(decomposed_indexes[1])
                            {
                                let second_element_ptr: PointerValue<'_> = unsafe {
                                    llvm_builder.build_gep(
                                        llvm_context.i8_type(),
                                        ptr,
                                        &[llvm_context.i32_type().const_int(8, false)],
                                        "",
                                    )
                                }.unwrap_or_else(|_| {
                                    abort::abort_codegen(
                                        abi_context,
                                        "Failed to build a GEP instruction for the second element of a decomposed and expanded array parameter in System V ABI!",
                                        old_type.get_code_location(),
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
                                            old_type.get_code_location(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        )
                                    })
                                    .set_alignment(alignment)
                                    .unwrap_or_else(|_| {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to set the alignment of a store instruction for the second element of a decomposed and expanded array parameter in System V ABI!",
                                            old_type.get_code_location(),
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
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                );
                            }
                        } else {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the decomposed and expanded parameter value from the function declaration for System V ABI!",
                                old_type.get_code_location(),
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
        SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine { index, .. } => *index,
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

                let coerced_llvm_ty: BasicTypeEnum<'_> = llvm_context
                    .custom_width_int_type(*coerced_width_bits)
                    .into();

                let ptr: PointerValue<'_> = llvm_builder
                    .build_alloca(original_llvm_ty, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate memory for a coerced argument in System V ABI!",
                            original_ty.get_code_location(),
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
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .set_alignment(alignment)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set the alignment of a store instruction for a coerced argument in System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let coerced_value_instruction = llvm_builder
                    .build_load(coerced_llvm_ty, ptr, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a coerced argument from memory for System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                coerced_value_instruction
                    .as_instruction_value()
                    .and_then(|instruction| instruction.set_alignment(alignment).ok());

                let coerced_value: BasicValueEnum<'_> = coerced_value_instruction;

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
                        ty.get_code_location(),
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
                                    ty.get_code_location(),
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
                                        ty.get_code_location(),
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
                                        ty.get_code_location(),
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
                                            ty.get_code_location(),
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
                                            ty.get_code_location(),
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
                                            ty.get_code_location(),
                                            std::path::PathBuf::from(file!()),
                                            line!(),
                                        );
                                    });

                                if let  SystemVABIFunctionTypeArgumentConfigurationAttributes::byVal(ty) = attribute {
                                    let Some(_) = callee_args_values.get(*index) else {
                                        abort::abort_codegen(
                                            abi_context,
                                            "Failed to get the function parameter value from the function declaration for ABI lowering!",
                                            ty.get_code_location(),
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
                                                    ty.get_code_location(),
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
                                                ty.get_code_location(),
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
                                    ty.get_code_location(),
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
                                    ty.get_code_location(),
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
                                    ty.get_code_location(),
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
                                    ty.get_code_location(),
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
                                        ty.get_code_location(),
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
                                        ty.get_code_location(),
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

            SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine {
                ty,
                chunk_count,
                ..
            } => {
                let original_llvm_ty: BasicTypeEnum<'_> =
                    self::generate_type(llvm_context, abi_context, ty);
                let ptr: PointerValue<'_> =
                    llvm_builder.build_alloca(original_llvm_ty, "").unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to allocate memory for a float-combine argument in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                llvm_builder
                    .build_store(ptr, *arg_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store a float-combine argument in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let chunk_ty: VectorType<'_> = llvm_context.f32_type().vec_type(2);
                let chunk_ptr_ty = llvm_context.ptr_type(AddressSpace::default());
                let chunk_ptr: PointerValue<'_> = llvm_builder
                    .build_pointer_cast(ptr, chunk_ptr_ty, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to cast a float-combine argument pointer in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                for chunk in 0..*chunk_count {
                    let chunk_index: IntValue<'_> =
                        llvm_context.i32_type().const_int(chunk as u64, false);
                    let slot: PointerValue<'_> = unsafe {
                        llvm_builder.build_in_bounds_gep(chunk_ty, chunk_ptr, &[chunk_index], "")
                    }
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to address a float-combine argument chunk in System V ABI!",
                            ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    let chunk_value: BasicValueEnum<'_> = llvm_builder
                        .build_load(chunk_ty, slot, "")
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to load a float-combine argument chunk in System V ABI!",
                                ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    processed_args.push(chunk_value.into());
                }
            }

            SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement {
                ty,
                element_count,
                ..
            } => {
                let vector_value: inkwell::values::VectorValue<'_> =
                    (*arg_value).into_vector_value();

                for lane in 0..*element_count {
                    let lane_value: BasicValueEnum<'_> = llvm_builder
                        .build_extract_element(
                            vector_value,
                            llvm_context.i32_type().const_int(lane as u64, false),
                            "",
                        )
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to extract a vector element for an integer-per-element argument in System V ABI!",
                                ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    processed_args.push(lane_value.into());
                }
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
                            old_type.get_code_location(),
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
                                    old_type.get_code_location(),
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
                            old_type.get_code_location(),
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
                                old_type.get_code_location(),
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
                                old_type.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                    store_instruction.set_alignment(alignment).unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to set the alignment of a store instruction for a decomposed and expanded array parameter in System V ABI!",
                            old_type.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    if layout.abi_size == 8 {
                        let first_element_decomposed_ty: &Type = array_decomposed_types.first().unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the first decomposed type for an array decomposition and expansion!",
                                old_type.get_code_location(),
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

                        let value_instruction =
                            llvm_builder.build_load(first_element_decomposed_llvm_ty, ptr, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        value_instruction
                            .as_instruction_value()
                            .and_then(|instruction| instruction.set_alignment(alignment).ok());

                        let value: BasicValueEnum<'_> = value_instruction;

                        processed_args.push(value.into());
                    } else {
                        let first_element_decomposed_ty: &Type = array_decomposed_types.first().unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the first decomposed type for an array decomposition and expansion!",
                                old_type.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            )
                        });

                        let second_element_decomposed_ty: &Type = array_decomposed_types.get(1).unwrap_or_else(|| {
                            abort::abort_codegen(
                                abi_context,
                                "Failed to get the second decomposed type for an array decomposition and expansion!",
                                old_type.get_code_location(),
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

                        let first_value_instruction: BasicValueEnum<'_> =
                            llvm_builder.build_load(first_element_decomposed_llvm_ty, ptr, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        first_value_instruction
                            .as_instruction_value()
                            .and_then(|instruction| instruction.set_alignment(alignment).ok());

                        let first_value: BasicValueEnum<'_> = first_value_instruction;

                        ptr = self::address_space_to_normal(
                            abi_context,
                            llvm_builder,
                            llvm_context,
                            ptr,
                            old_type.get_code_location(),
                        );

                        let ptr_to_second_element: PointerValue<'_> = unsafe {
                            llvm_builder.build_in_bounds_gep(llvm_context.i8_type(), ptr, &[llvm_context.i32_type().const_int(8, false)], "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to build a GEP instruction to get the second element of a decomposed and expanded array parameter in System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            })
                        };

                        let second_value_instruction: BasicValueEnum<'_> =
                            llvm_builder.build_load(second_element_decomposed_llvm_ty, ptr_to_second_element, "").unwrap_or_else(|_| {
                                abort::abort_codegen(
                                    abi_context,
                                    "Failed to load a decomposed and expanded array element from memory for System V ABI!",
                                    old_type.get_code_location(),
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            });

                        second_value_instruction
                            .as_instruction_value()
                            .and_then(|instruction| instruction.set_alignment(alignment).ok());

                        let second_value: BasicValueEnum<'_> = second_value_instruction;

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

    if let Some(SystemVABIType::CoerceFloatCombine(original_ty, chunk_count)) =
        configuration.return_type()
    {
        let returned_value: BasicValueEnum<'_> =
            callsite.try_as_basic_value().left().unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile lower a float-combine return value!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let original_llvm_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, original_ty);

        let ptr: PointerValue<'_> = llvm_builder
            .build_alloca(original_llvm_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to allocate memory for a float-combine return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let chunk_ty: VectorType<'_> = llvm_context.f32_type().vec_type(2);
        let chunk_ptr_ty = llvm_context.ptr_type(AddressSpace::default());
        let chunk_ptr: PointerValue<'_> = llvm_builder
            .build_pointer_cast(ptr, chunk_ptr_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to cast a float-combine return pointer in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        if *chunk_count == 1 {
            llvm_builder
                .build_store(chunk_ptr, returned_value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to store a float-combine return chunk in System V ABI!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });
        } else {
            let returned_struct: inkwell::values::StructValue<'_> =
                returned_value.into_struct_value();

            for chunk in 0..*chunk_count {
                let chunk_value: BasicValueEnum<'_> = llvm_builder
                    .build_extract_value(returned_struct, chunk, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to extract a float-combine return chunk in System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let chunk_index: IntValue<'_> =
                    llvm_context.i32_type().const_int(chunk as u64, false);
                let slot: PointerValue<'_> = unsafe {
                    llvm_builder.build_in_bounds_gep(chunk_ty, chunk_ptr, &[chunk_index], "")
                }
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to address a float-combine return chunk in System V ABI!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

                llvm_builder
                    .build_store(slot, chunk_value)
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to store a float-combine return chunk in System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });
            }
        }

        return llvm_builder
            .build_load(original_llvm_ty, ptr, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to load a float-combine return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    if let Some(SystemVABIType::Coerce(original_ty, _)) = configuration.return_type() {
        let returned_value: BasicValueEnum<'_> =
            callsite.try_as_basic_value().left().unwrap_or_else(|| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile lower a function call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let original_llvm_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, original_ty);

        let ptr: PointerValue<'_> = llvm_builder
            .build_alloca(original_llvm_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to allocate memory for a coerced return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let alignment: u32 = abi_context
            .get_target_data()
            .get_preferred_alignment(&original_llvm_ty);

        let store_instruction: InstructionValue<'_> = llvm_builder
            .build_store(ptr, returned_value)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to store a coerced return value in memory for System V ABI!",
                    original_ty.get_code_location(),
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
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let loaded_value: BasicValueEnum<'_> = llvm_builder
            .build_load(original_llvm_ty, ptr, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to load a coerced return value from memory for System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        if let Some(instruction) = loaded_value.as_instruction_value() {
            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to set alignment to an instruction!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
        }

        return loaded_value;
    }

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

        if codegen_location.is_direct_behavior() {
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
    if let Some(SystemVABIType::CoerceFloatCombine(original_ty, chunk_count)) =
        configuration.return_type()
    {
        let Some(return_value) = return_value else {
            return false;
        };

        let original_llvm_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, original_ty);
        let ptr: PointerValue<'_> = llvm_builder
            .build_alloca(original_llvm_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to allocate memory for a float-combine return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        llvm_builder
            .build_store(ptr, return_value)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to store a float-combine return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let chunk_ty: VectorType<'_> = llvm_context.f32_type().vec_type(2);
        let chunk_ptr_ty = llvm_context.ptr_type(AddressSpace::default());
        let chunk_ptr: PointerValue<'_> = llvm_builder
            .build_pointer_cast(ptr, chunk_ptr_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to cast a float-combine return pointer in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        if *chunk_count == 1 {
            let chunk_value: BasicValueEnum<'_> = llvm_builder
                .build_load(chunk_ty, chunk_ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to load a float-combine return chunk in System V ABI!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            if llvm_builder.build_return(Some(&chunk_value)).is_err() {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile a float-combine return terminator in System V ABI!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            }
        } else {
            let chunk_types: Vec<BasicTypeEnum<'_>> =
                (0..*chunk_count).map(|_| chunk_ty.into()).collect();
            let return_struct_ty = llvm_context.struct_type(&chunk_types, false);
            let mut return_struct = return_struct_ty.get_undef();

            for chunk in 0..*chunk_count {
                let chunk_index: IntValue<'_> =
                    llvm_context.i32_type().const_int(chunk as u64, false);
                let slot: PointerValue<'_> = unsafe {
                    llvm_builder.build_in_bounds_gep(chunk_ty, chunk_ptr, &[chunk_index], "")
                }
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to address a float-combine return chunk in System V ABI!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

                let chunk_value: BasicValueEnum<'_> = llvm_builder
                    .build_load(chunk_ty, slot, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to load a float-combine return chunk in System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                return_struct = llvm_builder
                    .build_insert_value(return_struct, chunk_value, chunk, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            abi_context,
                            "Failed to build a float-combine return struct in System V ABI!",
                            original_ty.get_code_location(),
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                    .into_struct_value();
            }

            if llvm_builder.build_return(Some(&return_struct)).is_err() {
                abort::abort_codegen(
                    abi_context,
                    "Failed to compile a float-combine return terminator in System V ABI!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                );
            }
        }

        return true;
    }

    if let Some(SystemVABIType::Coerce(original_ty, coerced_width_bits)) =
        configuration.return_type()
    {
        let Some(return_value) = return_value else {
            return false;
        };

        let original_llvm_ty: BasicTypeEnum<'_> =
            self::generate_type(llvm_context, abi_context, original_ty);

        let ptr: PointerValue<'_> = llvm_builder
            .build_alloca(original_llvm_ty, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to allocate memory for a coerced return value in System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let alignment: u32 = abi_context
            .get_target_data()
            .get_preferred_alignment(&original_llvm_ty);

        let source_value: BasicValueEnum<'_> = if return_value.is_pointer_value() {
            let loaded_value: BasicValueEnum<'_> = llvm_builder
                .build_load(original_llvm_ty, return_value.into_pointer_value(), "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to load a pointed coerced return value for System V ABI!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

            if let Some(instruction) = loaded_value.as_instruction_value() {
                instruction.set_alignment(alignment).unwrap_or_else(|_| {
                    abort::abort_codegen(
                        abi_context,
                        "Failed to set alignment to an instruction!",
                        original_ty.get_code_location(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });
            }

            loaded_value
        } else {
            return_value
        };

        let store_instruction: InstructionValue<'_> = llvm_builder
            .build_store(ptr, source_value)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to store a coerced return value in memory for System V ABI!",
                    original_ty.get_code_location(),
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
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let coerced_value: BasicValueEnum<'_> = llvm_builder
            .build_load(
                llvm_context.custom_width_int_type(*coerced_width_bits),
                ptr,
                "",
            )
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to load a coerced integer return value from memory for System V ABI!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        if let Some(instruction) = coerced_value.as_instruction_value() {
            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    abi_context,
                    "Failed to set alignment to an instruction!",
                    original_ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
        }

        if llvm_builder.build_return(Some(&coerced_value)).is_err() {
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
            SystemVABITypeClass::get_system_v_type_class(
                abi_context,
                return_type,
                SystemVABITypeContext::Ret,
            );

        let abi_return_ty_: SystemVABIType = SystemVABIType::class_to_general_abi_strategy(
            abi_context,
            &return_ty_classes,
            return_type,
        );

        abi_return_ty = if abi_return_ty_.is_integer_per_element() {
            SystemVABIType::ToMemory(return_type)
        } else {
            abi_return_ty_
        };
        configuration.set_return_type(Some(abi_return_ty.clone()));

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
                    SystemVABITypeClass::get_system_v_type_class(
                        abi_context,
                        ty,
                        SystemVABITypeContext::Arg,
                    );

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
                        let llvm_coerced_ty: BasicTypeEnum<'_> = llvm_context
                            .custom_width_int_type(coerced_width_bits)
                            .into();

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

                    SystemVABIType::IntegerPerElement(vector_ty, element_count) => {
                        let Type::NativeVector { element_type, .. } = vector_ty else {
                            abort::abort_codegen(
                                abi_context,
                                "Expected a NativeVector to lower an integer-per-element argument!",
                                vector_ty.get_code_location(),
                                std::path::PathBuf::from(file!()),
                                line!(),
                            );
                        };

                        let first_index: usize = llvm_parameters_types.len();
                        let element_llvm_ty: BasicTypeEnum<'_> =
                            self::generate_type(llvm_context, abi_context, element_type);

                        for _ in 0..element_count {
                            llvm_parameters_types.push(element_llvm_ty.into());
                        }

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement {
                                name,
                                ascii_name,
                                ty,
                                element_count,
                                first_index,
                                index: idx,
                            },
                        );
                    }

                    SystemVABIType::CoerceFloatCombine(original_ty, chunk_count) => {
                        let first_index: usize = llvm_parameters_types.len();
                        let chunk_ty: BasicTypeEnum<'_> =
                            llvm_context.f32_type().vec_type(2).into();

                        for _ in 0..chunk_count {
                            llvm_parameters_types.push(chunk_ty.into());
                        }

                        configuration_parameter_types.push(
                            SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine {
                                name,
                                ascii_name,
                                ty: original_ty,
                                chunk_count,
                                first_index,
                                index: idx,
                            },
                        );
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
                                        SystemVABITypeContext::Other,
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

            SystemVABIType::CoerceFloatCombine(_, chunk_count) => {
                let chunk_ty: BasicTypeEnum<'_> = llvm_context.f32_type().vec_type(2).into();

                let llvm_return_ty: BasicTypeEnum<'_> = if chunk_count == 1 {
                    chunk_ty
                } else {
                    let chunk_types: Vec<BasicTypeEnum<'_>> =
                        (0..chunk_count).map(|_| chunk_ty).collect();

                    llvm_context.struct_type(&chunk_types, false).into()
                };

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            SystemVABIType::Coerce(_, coerced_width_bits) => {
                let llvm_return_ty: BasicTypeEnum<'_> = llvm_context
                    .custom_width_int_type(coerced_width_bits)
                    .into();

                (
                    llvm_return_ty.fn_type(&llvm_parameters_types, is_variatic),
                    configuration,
                )
            }

            SystemVABIType::IntegerPerElement(..) | SystemVABIType::ToMemory(..) => {
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
        SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement { index, .. } => *index,
        SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine { index, .. } => *index,
    });

    for parameter_configuration in ordered_configurations.iter() {
        match parameter_configuration {
            SystemVABIFunctionTypeArgumentConfiguration::Ignore { .. } => {}
            SystemVABIFunctionTypeArgumentConfiguration::Same { .. } => {}
            SystemVABIFunctionTypeArgumentConfiguration::IntegerPerElement { .. } => {}
            SystemVABIFunctionTypeArgumentConfiguration::CoerceFloatCombine { .. } => {}
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
                            ty.get_code_location(),
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
                                ty.get_code_location(),
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
                                ty.get_code_location(),
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
                            ty.get_code_location(),
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
                                ty.get_code_location(),
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
                                ty.get_code_location(),
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
                any.get_code_location(),
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
                any.get_code_location(),
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

        Type::NativeVector {
            element_type,
            element_count,
            ..
        } => {
            let vector_type: BasicTypeEnum =
                self::generate_type(llvm_context, abi_context, element_type);

            match vector_type {
                BasicTypeEnum::FloatType(ty) => ty.vec_type(*element_count).into(),
                BasicTypeEnum::IntType(ty) => ty.vec_type(*element_count).into(),
                BasicTypeEnum::PointerType(ty) => ty.vec_type(*element_count).into(),

                any => abort::abort_codegen(
                    abi_context,
                    &format!(
                        "Failed to compile '{:?}' as a native vector element type!",
                        any
                    ),
                    ty.get_code_location(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                ),
            }
        }

        any => abort::abort_codegen(
            abi_context,
            &format!("Failed to compile '{}' as a type!", any),
            any.get_code_location(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use thrustc_llvm_target_triple::LLVMTargetCPU;

    fn classify_type_on_linux(
        ty: &Type,
        is_x86_64: bool,
        has_avx: bool,
        has_avx512f: bool,
    ) -> [SystemVABITypeClass; 8] {
        let target: &str = if is_x86_64 {
            "x86_64-unknown-linux-gnu"
        } else {
            "aarch64-unknown-linux-gnu"
        };

        let features: String = match (has_avx, has_avx512f) {
            (true, true) => "+avx,+avx512f".to_string(),
            (true, false) => "+avx".to_string(),
            (false, true) => "+avx512f".to_string(),
            (false, false) => String::new(),
        };

        let cpu: LLVMTargetCPU = LLVMTargetCPU::new("test".to_string(), features);
        let target_triple: LLVMTargetTriple =
            LLVMTargetTriple::new_with_cpu(target.to_string(), cpu);
        let target_info: TargetInfo = TargetInfo::new(target_triple.clone());
        let target_data: TargetData = TargetData::create(
            "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128",
        );
        let file: CompilationUnit = CompilationUnit::new(
            "test.thrust".to_string(),
            std::path::PathBuf::from("test.thrust"),
            String::new(),
            "test".to_string(),
        );

        let options: CompilerOptions = CompilerOptions::new();

        let mut abi_context: SystemVABIContext<'_> = SystemVABIContext::new(
            &file,
            &options,
            &target_triple,
            target_info,
            &target_data,
            SystemVCodeGenLocation::None,
        );

        SystemVABITypeClass::get_system_v_type_class(
            &mut abi_context,
            ty,
            SystemVABITypeContext::Arg,
        )
    }

    fn classify_native_vector_on_linux(
        abi_size: u32,
        is_x86_64: bool,
        has_avx: bool,
        has_avx512f: bool,
    ) -> [SystemVABITypeClass; 8] {
        let span: Span = Span::nothing();

        let element_type: Type = Type::F32 { span };
        let element_count: u32 = abi_size / 4;
        let ty: Type = Type::NativeVector {
            element_type: Box::new(element_type),
            element_count,
            span,
        };

        self::classify_type_on_linux(&ty, is_x86_64, has_avx, has_avx512f)
    }

    fn classify_single_vector_struct_on_linux(
        abi_size: u32,
        is_x86_64: bool,
        has_avx: bool,
        has_avx512f: bool,
    ) -> [SystemVABITypeClass; 8] {
        let span: Span = Span::nothing();

        let element_type: Type = Type::F32 { span };
        let element_count: u32 = abi_size / 4;
        let vector_type: Type = Type::NativeVector {
            element_type: Box::new(element_type),
            element_count,
            span,
        };
        let ty: Type = Type::Struct {
            name: "V".to_string(),
            fields: vec![vector_type],
            metadata: Default::default(),
            span,
        };

        self::classify_type_on_linux(&ty, is_x86_64, has_avx, has_avx512f)
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) gates named 256-bit vectors on AVX. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
    // The test pins the explicit AVX check, so AVX-512F alone cannot enable 32-byte register passing.
    #[test]
    fn native_vector_32b_needs_explicit_avx() {
        assert_eq!(
            self::classify_native_vector_on_linux(32, true, true, false),
            SYSTEM_V_ABI_NATIVE_VECTOR_32B
        );

        assert_eq!(
            self::classify_native_vector_on_linux(32, true, false, true),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_native_vector_on_linux(32, false, true, true),
            SYSTEM_V_ABI_STACK
        );
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) gates named 512-bit vectors on AVX-512F. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
    // The test pins that gate without treating AVX as enough for 64-byte register passing.
    #[test]
    fn native_vector_64b_needs_explicit_avx512f() {
        assert_eq!(
            self::classify_native_vector_on_linux(64, true, true, true),
            SYSTEM_V_ABI_NATIVE_VECTOR_64B
        );

        assert_eq!(
            self::classify_native_vector_on_linux(64, true, true, false),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_native_vector_on_linux(64, false, true, true),
            SYSTEM_V_ABI_STACK
        );
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) splits wider named vectors into SSEUP chunks. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
    // The test covers the intermediate widths that AVX and AVX-512F enable.
    #[test]
    fn native_vector_intermediate_sizes_use_sseup() {
        let classes_24: [SystemVABITypeClass; 8] =
            self::classify_native_vector_on_linux(24, true, true, false);

        assert_eq!(classes_24[0], SystemVABITypeClass::SSE);

        assert_eq!(
            classes_24[1..3],
            [SystemVABITypeClass::SSEUP, SystemVABITypeClass::SSEUP]
        );

        assert_eq!(classes_24[3], SystemVABITypeClass::NO_CLASS);

        let classes_40: [SystemVABITypeClass; 8] =
            self::classify_native_vector_on_linux(40, true, true, true);

        assert_eq!(
            classes_40[1..5],
            [
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP
            ]
        );

        assert_eq!(classes_40[5], SystemVABITypeClass::NO_CLASS);

        let classes_48: [SystemVABITypeClass; 8] =
            self::classify_native_vector_on_linux(48, true, true, true);

        assert_eq!(
            classes_48[1..6],
            [
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP
            ]
        );

        assert_eq!(classes_48[6], SystemVABITypeClass::NO_CLASS);

        let classes_56: [SystemVABITypeClass; 8] =
            self::classify_native_vector_on_linux(56, true, true, true);

        assert_eq!(
            classes_56[1..7],
            [
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP,
                SystemVABITypeClass::SSEUP
            ]
        );

        assert_eq!(classes_56[7], SystemVABITypeClass::NO_CLASS);
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) keeps unsupported widths in memory. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
    // The test pins the feature and architecture gates for intermediate widths.
    #[test]
    fn native_vector_intermediate_sizes_require_features() {
        assert_eq!(
            self::classify_native_vector_on_linux(24, true, false, true),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_native_vector_on_linux(40, true, true, false),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_native_vector_on_linux(48, true, true, false),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_native_vector_on_linux(56, false, true, true),
            SYSTEM_V_ABI_STACK
        );
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1869 (LLVM 17.0.6) handles 128-bit vectors without AVX. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1869
    // The test keeps that path independent from the new width checks.
    #[test]
    fn native_vector_16b_is_unchanged() {
        assert_eq!(
            self::classify_native_vector_on_linux(16, false, false, false),
            SYSTEM_V_ABI_F128
        );
    }

    // clang/lib/CodeGen/Targets/X86.cpp:1868-1893 (LLVM 17.0.6) classifies the vector field as the struct ABI. https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/clang/lib/CodeGen/Targets/X86.cpp#L1868-L1893
    // The test covers the single-vector struct that the high-level wrapper needs.
    #[test]
    fn single_vector_struct_uses_vector_classes() {
        assert_eq!(
            self::classify_single_vector_struct_on_linux(32, true, true, false),
            SYSTEM_V_ABI_NATIVE_VECTOR_32B
        );

        assert_eq!(
            self::classify_single_vector_struct_on_linux(64, true, true, true),
            SYSTEM_V_ABI_NATIVE_VECTOR_64B
        );

        assert_eq!(
            self::classify_single_vector_struct_on_linux(64, true, false, true),
            SYSTEM_V_ABI_STACK
        );

        assert_eq!(
            self::classify_single_vector_struct_on_linux(32, true, false, true),
            SYSTEM_V_ABI_STACK
        );
    }
}
