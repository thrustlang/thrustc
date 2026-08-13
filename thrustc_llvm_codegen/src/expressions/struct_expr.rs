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
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};
use thrustc_ast::ast_logic_data::ConstructorData;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeStructExtensions;

use crate::context::LLVMCodeGenContext;
use crate::memory::LLVMAllocationSite;
use crate::pointer_anchor::PointerAnchor;
use crate::{abort, codegen, memory, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    data: &'ctx ConstructorData,
    struct_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    match context.get_pointer_anchor() {
        Some(anchor) if !anchor.is_triggered() => {
            self::compile_with_anchor(context, data, struct_type, span, *anchor)
        }
        _ => self::compile_without_anchor(context, data, struct_type, span),
    }
}

fn compile_with_anchor<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    data: &'ctx ConstructorData,
    struct_type: &Type,
    span: Span,
    anchor: PointerAnchor<'ctx>,
) -> BasicValueEnum<'ctx> {
    context.mark_pointer_anchor();

    let ptr_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, struct_type);
    let ptr_value: PointerValue<'_> = anchor.get_pointer();

    let fields_types: &[Type] = struct_type.get_struct_fields().unwrap_or_else(|| {
        abort::abort_codegen(
            context,
            "Failed get structure type fields!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    let fields: Vec<_> = data
        .iter()
        .zip(fields_types)
        .map(|((_, field, ..), field_target_type)| {
            codegen::compile_as_value(context, field, Some(field_target_type))
        })
        .collect();

    for (idx, value) in fields.iter().enumerate() {
        let index: u32 = idx.try_into().unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get the field pointer!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let field_ptr_value: PointerValue<'_> = context
            .get_llvm_builder()
            .build_struct_gep(ptr_type, ptr_value, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to get the field pointer!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        memory::store(context, field_ptr_value, *value, span);
    }

    context
        .get_llvm_context()
        .ptr_type(AddressSpace::default())
        .const_null()
        .into()
}

fn compile_without_anchor<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    data: &'ctx ConstructorData,
    struct_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let ptr_type: BasicTypeEnum<'_> = typegeneration::generate_type(context, struct_type);
    let ptr_value: PointerValue<'_> =
        memory::allocate_in(context, LLVMAllocationSite::Stack, struct_type, span);

    let fields_types: &[Type] = struct_type.get_struct_fields().unwrap_or_else(|| {
        abort::abort_codegen(
            context,
            "Failed get structure type fields!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    let fields: Vec<_> = data
        .iter()
        .zip(fields_types)
        .map(|((_, field, _, _), field_target_type)| {
            codegen::compile_as_value(context, field, Some(field_target_type))
        })
        .collect();

    for (idx, value) in fields.iter().enumerate() {
        let index: u32 = idx.try_into().unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get the field pointer!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let field_ptr_value: PointerValue<'_> = context
            .get_llvm_builder()
            .build_struct_gep(ptr_type, ptr_value, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to get the field pointer!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        memory::store(context, field_ptr_value, *value, span);
    }

    memory::load(context, ptr_value, struct_type, span)
}
