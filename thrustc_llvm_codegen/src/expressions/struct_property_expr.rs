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

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use inkwell::{builder::Builder, values::BasicValueEnum};
use thrustc_ast::Ast;
use thrustc_ast::ast_logic_data::PropertyData;
use thrustc_ast::ast_metadata::PropertyMetadata;
use thrustc_ast::traits::AstMemoryExtensions;
use thrustc_ast::traits::{
    AstCodeLocation, AstPropertyDataExtensions, AstPropertyDataFieldExtensions,
};
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;
use thrustc_typesystem::traits::TypePointerExtensions;

use crate::context::{CodeGenLocation, LLVMCodeGenContext};
use crate::traits::AstLLVMGetType;
use crate::{abort, codegen, memory, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    source: &'ctx Ast<'ctx>,
    data: &'ctx PropertyData,
    metadata: &PropertyMetadata,
) -> BasicValueEnum<'ctx> {
    let source_type: &Type = source.get_type_for_llvm();

    let is_allocated: bool = source.is_memory_assigned_value().unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to compile the property!",
            source.get_span(),
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    if (is_allocated && source_type.is_struct_type()) || source_type.is_ptr_composite_type() {
        let ptr: BasicValueEnum = if metadata.is_deref() {
            context.add_codegen_location(CodeGenLocation::LValue);
            let ptr: BasicValueEnum = self::compile_gep_property(context, source, data);
            context.pop_current_codegen_location();

            ptr
        } else {
            self::compile_gep_property(context, source, data)
        };

        if metadata.is_deref() {
            self::compile_deref_property(context, source, data, ptr)
        } else {
            ptr
        }
    } else {
        self::compile_extract_property(context, source, data)
    }
}

fn compile_deref_property<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    source: &'ctx Ast<'ctx>,
    data: &'ctx PropertyData,
    ptr: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let codegen_location: CodeGenLocation = context.get_codegen_location();

    if codegen_location.is_direct_behavior() {
        return ptr;
    }

    let span: Span = source.get_span();

    let value_type: Type = data
        .last()
        .map(|field| field.get_property_type())
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to compile the dereferenced property!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    memory::dereference(context, ptr.into_pointer_value(), &value_type, span)
}

fn compile_extract_property<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    source: &'ctx Ast<'ctx>,
    data: &'ctx PropertyData,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let span: Span = source.get_span();

    let mut property: BasicValueEnum = {
        let value: BasicValueEnum = codegen::compile_as_value(context, source, None);

        let index: u32 = data
            .get_first_property()
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the property!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .get_index();

        llvm_builder
            .build_extract_value(value.into_struct_value(), index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the property!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
    };

    for field in data.iter().skip(1) {
        let index: u32 = field.get_index();

        property = llvm_builder
            .build_extract_value(property.into_struct_value(), index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the property!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    property
}

fn compile_gep_property<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    source: &'ctx Ast<'ctx>,
    data: &'ctx PropertyData,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let span: Span = source.get_span();

    context.add_codegen_location(CodeGenLocation::LValue);
    let source_value: BasicValueEnum<'_> = codegen::compile_as_ptr_value(context, source, None);
    context.pop_current_codegen_location();

    let ptr_value: PointerValue = source_value.into_pointer_value();
    let ptr_type: &Type = source.get_type_for_llvm();

    let index: u32 = data
        .get_first_property()
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to compile the property!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .get_index();

    let mut address: PointerValue =
        memory::gep_struct_anon(context, ptr_value, ptr_type, index, span);

    for field in data.iter().skip(1) {
        let base_type: Type = field.get_base_type();
        let index: u32 = field.get_index();

        let property_type: BasicTypeEnum =
            typegeneration::generate_pointer_arithmetic_type(context, &base_type);

        address = llvm_builder
            .build_struct_gep(property_type, address, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to gep a value from pointer!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    if context.get_codegen_location().is_load_behavior() {
        return memory::load_pointer(context, address, span);
    }

    address.into()
}
