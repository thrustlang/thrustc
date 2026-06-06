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
use thrustc_typesystem::traits::TypeIsExtensions;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::FunctionType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};

use crate::context::LLVMCodeGenContext;
use crate::{abort, cast, codegen, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    pointer: &'ctx Ast,
    args: &'ctx [Ast],
    function_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();
    let source_value: BasicValueEnum<'_> =
        codegen::compile_as_ptr_value(context, pointer, cast_type);
    let function_ptr_value: PointerValue<'_> = source_value.into_pointer_value();

    if let Type::Fn(parameter_types, kind, modificator, ..) = function_type {
        let is_var_args: bool = modificator.llvm().has_ignore();

        let function_type: FunctionType<'_> =
            typegeneration::generate_type_function_type_to_function_type(
                context,
                kind,
                parameter_types,
                is_var_args,
            );

        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .enumerate()
            .map(|(index, expr)| {
                let cast_type: Option<&Type> = parameter_types.get(index);
                codegen::compile_as_value(context, expr, cast_type).into()
            })
            .collect();

        let function_value: BasicValueEnum<'_> = match llvm_builder.build_indirect_call(
            function_type,
            function_ptr_value,
            &compiled_args,
            "",
        ) {
            Ok(call) => {
                if !kind.is_void_type() {
                    call.try_as_basic_value().left().unwrap_or_else(|| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile indirect function call!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    })
                } else {
                    context
                        .get_llvm_context()
                        .ptr_type(AddressSpace::default())
                        .const_null()
                        .into()
                }
            }
            Err(_) => abort::abort_codegen(
                context,
                "Failed to compile indirect function call!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            ),
        };

        cast::try_smart_cast(context, cast_type, kind, function_value, span)
    } else {
        abort::abort_codegen(
            context,
            "Failed to compile indirect function call!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    }
}
