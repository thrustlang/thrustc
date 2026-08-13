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

use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use thrustc_ast::{Ast, traits::AstCodeLocation};
use thrustc_code_location::Span;
use thrustc_typesystem::{
    Type,
    traits::{InfererTypeExtensions, TypePointerExtensions},
};

use crate::{
    codegen,
    context::{CodeGenLocation, LLVMCodeGenContext},
    expressions, memory,
    traits::AstLLVMGetType,
};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    source: &'ctx Ast<'ctx>,
    index: &'ctx Ast<'ctx>,
) -> BasicValueEnum<'ctx> {
    context.add_codegen_location(CodeGenLocation::RValue);
    let source_value: BasicValueEnum<'_> = codegen::compile_as_ptr_value(context, source, None);
    context.pop_current_codegen_location();

    let ptr_value: PointerValue<'_> = source_value.into_pointer_value();

    let mut ptr_type: &Type = source.get_type_for_llvm();
    let infered_inner_type: Type = ptr_type.get_inferer_inner_type();

    let ordered_indexes: Vec<IntValue> = {
        let span: Span = index.get_span();

        let has_inferer_inner_type: bool = ptr_type.has_infered_inner_type();

        if has_inferer_inner_type {
            ptr_type = &infered_inner_type;
        }

        let is_ptr_aggv_type: bool = ptr_type.is_ptr_aggregate_value_like_type();
        let is_ptr_like_type: bool = ptr_type.is_ptr_like_type();

        let indexes: Vec<IntValue> = if is_ptr_aggv_type {
            let base_type: Type = Type::U32 { span };

            let base: IntValue = expressions::literal_integer_expr::compile(
                context,
                &base_type,
                0,
                index.get_span(),
            );

            let depth_type: Type = Type::U32 { span };

            let depth: IntValue =
                codegen::compile_as_value(context, index, Some(&depth_type)).into_int_value();

            vec![base, depth]
        } else if is_ptr_like_type {
            let base_type: Type = Type::U64 { span };

            let base: IntValue =
                codegen::compile_as_value(context, index, Some(&base_type)).into_int_value();

            vec![base]
        } else {
            let base_type: Type = Type::U32 { span };

            let base: IntValue = expressions::literal_integer_expr::compile(
                context,
                &base_type,
                0,
                index.get_span(),
            );

            let depth_type: Type = Type::U32 { span };

            let depth: IntValue =
                codegen::compile_as_value(context, index, Some(&depth_type)).into_int_value();

            vec![base, depth]
        };

        indexes
    };

    memory::gep_anon(
        context,
        ptr_value,
        ptr_type,
        &ordered_indexes,
        source.get_span(),
    )
    .into()
}
