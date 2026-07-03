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

use thrustc_ast::Ast;
use thrustc_span::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::{TypeArrayEntensions, TypeExtensions};
use thrustc_typesystem::type_metadata::FixedArrayTypeMetadata;

use crate::context::LLVMCodeGenContext;
use crate::memory::{self, LLVMAllocationSite};
use crate::pointer_anchor::PointerAnchor;
use crate::traits::AstLLVMGetType;
use crate::{abort, codegen, type_cast, typegeneration};

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

#[inline]
pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    array_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    match context.get_pointer_anchor() {
        Some(anchor) if !anchor.is_triggered() => {
            self::compile_array_with_anchor(context, items, array_type, span, cast_type, *anchor)
        }
        _ => self::compile_array_without_anchor(context, items, array_type, span, cast_type),
    }
}

pub fn compile_const<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    array_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let base_type: Type = array_type.get_array_base_type();
    let array_type: BasicTypeEnum = typegeneration::generate_type(context, &base_type);

    let values: Vec<BasicValueEnum> = items
        .iter()
        .map(|item| {
            let value_type: &Type = item.get_type_for_llvm();
            let value: BasicValueEnum =
                codegen::compile_constant_as_value(context, item, &base_type);

            type_cast::try_smart_constant_cast(context, &base_type, value_type, value)
        })
        .collect();

    match array_type {
        t if t.is_int_type() => t
            .into_int_type()
            .const_array(
                &values
                    .iter()
                    .map(|v| v.into_int_value())
                    .collect::<Vec<_>>(),
            )
            .into(),
        t if t.is_float_type() => t
            .into_float_type()
            .const_array(
                &values
                    .iter()
                    .map(|v| v.into_float_value())
                    .collect::<Vec<_>>(),
            )
            .into(),
        t if t.is_array_type() => t
            .into_array_type()
            .const_array(
                &values
                    .iter()
                    .map(|v| v.into_array_value())
                    .collect::<Vec<_>>(),
            )
            .into(),
        t if t.is_struct_type() => t
            .into_struct_type()
            .const_array(
                &values
                    .iter()
                    .map(|v| v.into_struct_value())
                    .collect::<Vec<_>>(),
            )
            .into(),
        t if t.is_pointer_type() => t
            .into_pointer_type()
            .const_array(
                &values
                    .iter()
                    .map(|v| v.into_pointer_value())
                    .collect::<Vec<_>>(),
            )
            .into(),
        _ => {
            abort::abort_codegen(
                context,
                "Failed to compile the constant array!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        }
    }
}

fn compile_array_without_anchor<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    array_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let array_type: &Type = cast_type.unwrap_or(array_type);
    let base_type: Type = array_type.get_array_base_type();

    let array_size: u32 = u32::try_from(items.len()).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to compile the array!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    let metadata: FixedArrayTypeMetadata =
        FixedArrayTypeMetadata::new(array_type.get_address_space());

    let fixed_array_type: Type = Type::FixedArray {
        base_type: base_type.clone().into(),
        size: array_size,
        metadata,
        span,
    };

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, &fixed_array_type);

    let array_ptr: PointerValue =
        memory::allocate_in(context, LLVMAllocationSite::Stack, &fixed_array_type, span);

    if items.is_empty() {
        memory::store(context, array_ptr, llvm_type.const_zero(), span);
        array_ptr.into()
    } else {
        let items: Vec<BasicValueEnum> = items
            .iter()
            .map(|item| codegen::compile_as_value(context, item, Some(&base_type)))
            .collect();

        for (n, value) in items.iter().enumerate() {
            let idx: u64 = u64::try_from(n).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to parse the build index!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            let index: IntValue = llvm_context.i32_type().const_int(idx, false);

            let ptr: PointerValue = memory::gep_anon(
                context,
                array_ptr,
                &fixed_array_type,
                &[llvm_context.i32_type().const_zero(), index],
                span,
            );

            memory::store(context, ptr, *value, span);
        }

        array_ptr.into()
    }
}

fn compile_array_with_anchor<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    array_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
    anchor: PointerAnchor<'ctx>,
) -> BasicValueEnum<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let anchor_ptr: PointerValue = anchor.get_pointer();

    let array_size: u32 = u32::try_from(items.len()).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to compile the array!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    let array_type: &Type = cast_type.unwrap_or(array_type);
    let base_type: Type = array_type.get_array_base_type();

    let metadata: FixedArrayTypeMetadata =
        FixedArrayTypeMetadata::new(array_type.get_address_space());

    let fixed_array_type: Type = Type::FixedArray {
        base_type: base_type.clone().into(),
        size: array_size,
        metadata,
        span,
    };

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, &fixed_array_type);

    if let Some(anchor) = context.get_mut_pointer_anchor() {
        anchor.trigger();
    }

    if items.is_empty() {
        memory::store(context, anchor_ptr, llvm_type.const_zero(), span);
        anchor_ptr.into()
    } else {
        let items: Vec<BasicValueEnum> = items
            .iter()
            .map(|item| codegen::compile_as_value(context, item, Some(&base_type)))
            .collect();

        let ptr_value: Option<PointerValue> = items
            .iter()
            .enumerate()
            .map(|(n, value)| {
                let idx: u64 = u64::try_from(n).unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to compile the array!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });

                let index: IntValue = llvm_context.i32_type().const_int(idx, false);

                let ptr: PointerValue = memory::gep_anon(
                    context,
                    anchor_ptr,
                    &fixed_array_type,
                    &[llvm_context.i32_type().const_zero(), index],
                    span,
                );

                memory::store(context, ptr, *value, span);

                ptr
            })
            .last();

        ptr_value
            .unwrap_or(
                context
                    .get_llvm_context()
                    .ptr_type(AddressSpace::default())
                    .const_null(),
            )
            .into()
    }
}
