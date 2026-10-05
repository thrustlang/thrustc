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
use inkwell::values::{BasicValueEnum, IntValue, VectorValue};

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::context::LLVMCodeGenContext;
use crate::traits::AstLLVMGetType;
use crate::{abort, codegen, type_cast, typegeneration};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    vector_type: &Type,
    span: Span,
    cast_type: Option<&Type>,
) -> BasicValueEnum<'ctx> {
    let vector_type: &Type = cast_type.unwrap_or(vector_type);

    let Type::NativeVector { element_type, .. } = vector_type else {
        abort::abort_codegen(
            context,
            "Expected native vector type for native vector literal.",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    };

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, vector_type);
    let mut vector: VectorValue = llvm_type.const_zero().into_vector_value();

    for (index, item) in items.iter().enumerate() {
        let value: BasicValueEnum = codegen::compile_as_value(context, item, Some(element_type));

        let value_type: &Type = item.get_type_for_llvm();
        let value: BasicValueEnum =
            type_cast::try_smart_cast(context, Some(element_type), value_type, value, span);

        let index: u64 = u64::try_from(index).unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to parse the native vector index.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        let index: IntValue = context
            .get_llvm_context()
            .i32_type()
            .const_int(index, false);

        vector = context
            .get_llvm_builder()
            .build_insert_element(vector, value, index, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to insert native vector element.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    vector.into()
}

pub fn compile_constant<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    items: &'ctx [Ast],
    vector_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    self::compile(context, items, vector_type, span, Some(vector_type))
}
