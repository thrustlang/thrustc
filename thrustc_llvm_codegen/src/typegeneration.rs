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

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::debug_info::AsDIScope;
use inkwell::debug_info::DIFlagsConstants;
use inkwell::debug_info::DIType;
use inkwell::targets::TargetData;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;

use thrustc_ast::Ast;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeCodeLocation;
use thrustc_typesystem::traits::TypeIsExtensions;
use thrustc_typesystem::traits::TypePointerExtensions;

use std::path::PathBuf;

use crate::abort;
use crate::context::LLVMCodeGenContext;
use crate::debug_context::LLVMDebugContext;

#[inline]
pub fn compile_as_function_type<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    kind: &Type,
    parameters: &[Ast],
    is_var_args: bool,
) -> FunctionType<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(parameters.len());

    for parameter in parameters {
        match parameter {
            Ast::FunctionParameter { kind, .. }
            | Ast::IntrinsicParameter { kind, .. }
            | Ast::AssemblerFunctionParameter { kind, .. } => {
                let generated_type: BasicTypeEnum<'_> = self::generate_type(context, kind);
                llvm_parameters_types.push(generated_type.into());
            }

            _ => {}
        }
    }

    if kind.is_void_type() {
        llvm_context
            .void_type()
            .fn_type(&llvm_parameters_types, is_var_args)
    } else {
        self::generate_type(context, kind).fn_type(&llvm_parameters_types, is_var_args)
    }
}

#[inline]
pub fn generate_type_function_type_to_function_type<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    kind: &Type,
    parameter_types: &[Type],
    is_var_args: bool,
) -> FunctionType<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let mut llvm_parameters_types: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(parameter_types.len());

    for ty in parameter_types.iter() {
        let generated_type: BasicTypeEnum<'_> = self::generate_type(context, ty);
        llvm_parameters_types.push(generated_type.into())
    }

    if kind.is_void_type() {
        llvm_context
            .void_type()
            .fn_type(&llvm_parameters_types, is_var_args)
    } else {
        self::generate_type(context, kind).fn_type(&llvm_parameters_types, is_var_args)
    }
}

pub fn generate_type<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    kind: &Type,
) -> BasicTypeEnum<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    match kind {
        t if t.is_integer_type() || t.is_char_type() || t.is_bool_type() => match kind {
            Type::S8 { .. } | Type::U8 { .. } | Type::Char(..) => llvm_context.i8_type().into(),
            Type::S16 { .. } | Type::U16 { .. } => llvm_context.i16_type().into(),
            Type::S32 { .. } | Type::U32 { .. } => llvm_context.i32_type().into(),
            Type::S64 { .. } | Type::U64 { .. } => llvm_context.i64_type().into(),
            Type::U128 { .. } => llvm_context.i128_type().into(),
            Type::USize { .. } | Type::SSize { .. } => llvm_context
                .ptr_sized_int_type(context.get_target_data(), None)
                .into(),

            Type::Bool(..) => llvm_context.bool_type().into(),
            Type::Const(subtype, ..) => self::generate_type(context, subtype),

            any => abort::abort_codegen(
                context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                PathBuf::from(file!()),
                line!(),
            ),
        },

        t if t.is_float_type() => match kind {
            Type::F32 { .. } => llvm_context.f32_type().into(),
            Type::F64 { .. } => llvm_context.f64_type().into(),
            Type::F128 { .. } => llvm_context.f128_type().into(),
            Type::FX8680 { .. } => llvm_context.x86_f80_type().into(),
            Type::FPPC128 { .. } => llvm_context.ppc_f128_type().into(),

            Type::Const(subtype, ..) => self::generate_type(context, subtype),

            any => abort::abort_codegen(
                context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                PathBuf::from(file!()),
                line!(),
            ),
        },

        Type::Array {
            infered_type: Some((infered_type, ..)),
            ..
        } => self::generate_type(context, infered_type),

        t if t.is_ptr_like_type() => llvm_context.ptr_type(AddressSpace::default()).into(),

        Type::Const(subtype, ..) => self::generate_type(context, subtype),

        Type::Struct {
            fields, modifier, ..
        } => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::with_capacity(u8::MAX as usize);

            let packed: bool = modifier.llvm().is_packed();

            {
                for ty in fields.iter() {
                    field_types.push(self::generate_type(context, ty));
                }
            }

            llvm_context.struct_type(&field_types, packed).into()
        }

        Type::FixedArray(type_, size, ..) => {
            let array_type: BasicTypeEnum = self::generate_type(context, type_);
            array_type.array_type(*size).into()
        }

        any => abort::abort_codegen(
            context,
            &format!("Failed to compile '{}' as a type!", any),
            any.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

#[inline]
pub fn generate_load_type<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    kind: &Type,
) -> BasicTypeEnum<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    match kind {
        t if t.is_integer_type() || t.is_char_type() || t.is_bool_type() => match kind {
            Type::S8 { .. } | Type::U8 { .. } | Type::Char(..) => llvm_context.i8_type().into(),
            Type::S16 { .. } | Type::U16 { .. } => llvm_context.i16_type().into(),
            Type::S32 { .. } | Type::U32 { .. } => llvm_context.i32_type().into(),
            Type::S64 { .. } | Type::U64 { .. } => llvm_context.i64_type().into(),
            Type::U128 { .. } => llvm_context.i128_type().into(),
            Type::USize { .. } | Type::SSize { .. } => llvm_context
                .ptr_sized_int_type(context.get_target_data(), None)
                .into(),

            Type::Bool(..) => llvm_context.bool_type().into(),
            Type::Const(subtype, ..) => self::generate_load_type(context, subtype),

            any => abort::abort_codegen(
                context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                PathBuf::from(file!()),
                line!(),
            ),
        },

        t if t.is_float_type() => match kind {
            Type::F32 { .. } => llvm_context.f32_type().into(),
            Type::F64 { .. } => llvm_context.f64_type().into(),
            Type::F128 { .. } => llvm_context.f128_type().into(),
            Type::FX8680 { .. } => llvm_context.x86_f80_type().into(),
            Type::FPPC128 { .. } => llvm_context.ppc_f128_type().into(),

            Type::Const(subtype, ..) => self::generate_load_type(context, subtype),

            any => abort::abort_codegen(
                context,
                &format!("Failed to compile '{}' as a type!", any),
                any.get_span(),
                PathBuf::from(file!()),
                line!(),
            ),
        },

        t if t.is_ptr_like_type() => llvm_context.ptr_type(AddressSpace::default()).into(),

        Type::Const(subtype, ..) => self::generate_load_type(context, subtype),

        Type::Struct {
            fields, modifier, ..
        } => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::with_capacity(u8::MAX as usize);

            let packed: bool = modifier.llvm().is_packed();

            {
                for ty in fields.iter() {
                    field_types.push(self::generate_load_type(context, ty));
                }
            }

            llvm_context.struct_type(&field_types, packed).into()
        }

        Type::FixedArray(subtype, size, ..) => {
            let array_type: BasicTypeEnum = self::generate_load_type(context, subtype);
            array_type.array_type(*size).into()
        }

        any => abort::abort_codegen(
            context,
            &format!("Failed to compile '{}' as a type!", any),
            any.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

pub fn compile_as_dbg_type<'ctx>(
    context: &mut LLVMDebugContext<'_, 'ctx>,
    from_type: &Type,
    llvm_type: BasicTypeEnum<'ctx>,
) -> DIType<'ctx> {
    let target_data: TargetData = context.get_target_data();

    match llvm_type {
        BasicTypeEnum::ArrayType(array_ty) => {
            let mut subscripts: Vec<std::ops::Range<i64>> = Vec::with_capacity(u8::MAX as usize);

            fn calculate_inner_size<'ctx>(
                array_ty: BasicTypeEnum<'ctx>,
                calculated_size: &mut Vec<std::ops::Range<i64>>,
            ) {
                if let BasicTypeEnum::ArrayType(array_ty) = array_ty {
                    let size: i64 = array_ty.len().saturating_sub(1) as i64;

                    calculated_size.push(0..size);

                    calculate_inner_size(array_ty.get_element_type(), calculated_size)
                }
            }

            calculate_inner_size(array_ty.get_element_type(), &mut subscripts);

            let inner_type: DIType<'_> =
                self::compile_as_dbg_type(context, from_type, array_ty.get_element_type());

            context
                .get_debug_builder()
                .create_array_type(
                    inner_type,
                    target_data.get_bit_size(&array_ty),
                    target_data.get_abi_alignment(&array_ty),
                    &subscripts,
                )
                .as_type()
        }

        BasicTypeEnum::FloatType(fp_ty) => context
            .get_debug_builder()
            .create_basic_type(
                format!("{}", from_type).trim(),
                target_data.get_bit_size(&fp_ty),
                0x00,
                DIFlagsConstants::PUBLIC,
            )
            .unwrap_or_else(|_| {
                abort::abort_codegen_dbg(
                    context,
                    &format!("Failed to compile '{}' as a debuggable type!", from_type),
                    from_type.get_span(),
                    PathBuf::from(file!()),
                    line!(),
                )
            })
            .as_type(),

        BasicTypeEnum::IntType(int_ty) => context
            .get_debug_builder()
            .create_basic_type(
                format!("{}", from_type).trim(),
                target_data.get_bit_size(&int_ty),
                0x00,
                DIFlagsConstants::PUBLIC,
            )
            .unwrap_or_else(|_| {
                abort::abort_codegen_dbg(
                    context,
                    &format!("Failed to compile '{}' as a debuggable type!", from_type),
                    from_type.get_span(),
                    PathBuf::from(file!()),
                    line!(),
                )
            })
            .as_type(),

        BasicTypeEnum::PointerType(pt_ty) => {
            let inner_type: DIType = self::compile_as_dbg_type(context, from_type, pt_ty.into());

            context
                .get_debug_builder()
                .create_pointer_type(
                    format!("{}", from_type).trim(),
                    inner_type,
                    target_data.get_bit_size(&pt_ty),
                    target_data.get_abi_alignment(&pt_ty),
                    AddressSpace::default(),
                )
                .as_type()
        }

        BasicTypeEnum::StructType(struct_ty) => {
            let elements: Vec<DIType<'_>> = struct_ty
                .get_field_types_iter()
                .map(|field_type| self::compile_as_dbg_type(context, from_type, field_type))
                .collect();

            let line: u32 = from_type.get_span().get_line();

            context
                .get_debug_builder()
                .create_struct_type(
                    context.get_debug_unit().get_file().as_debug_info_scope(),
                    format!("{}", from_type).trim(),
                    context.get_debug_unit().get_file(),
                    line,
                    target_data.get_bit_size(&struct_ty),
                    target_data.get_abi_alignment(&struct_ty),
                    DIFlagsConstants::PUBLIC,
                    None,
                    &elements,
                    0,
                    None,
                    &uuid::Uuid::new_v4().to_string(),
                )
                .as_type()
        }

        _ => abort::abort_codegen_dbg(
            context,
            &format!("Failed to compile '{}' as a debuggable type!", from_type),
            from_type.get_span(),
            PathBuf::from(file!()),
            line!(),
        ),
    }
}

#[inline]
pub fn generate_pointer_arithmetic_type<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    kind: &Type,
) -> BasicTypeEnum<'ctx> {
    match kind {
        Type::Const(subtype, ..) => self::generate_pointer_arithmetic_type(context, subtype),
        Type::Array {
            infered_type: Some((infered_type, _)),
            ..
        } => self::generate_type(context, infered_type),
        Type::Array {
            base_type: subtype, ..
        } => self::generate_type(context, subtype),
        Type::Ptr(Some(subtype), ..) => self::generate_type(context, subtype),

        _ => self::generate_type(context, kind),
    }
}
