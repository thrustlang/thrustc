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

use inkwell::{context::Context, targets::TargetData, values::IntValue};
use thrustc_span::Span;
use thrustc_typesystem::Type;

use crate::{abort, context::LLVMCodeGenContext};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ty: &Type,
    value: u64,
    signed: bool,
    span: Span,
) -> IntValue<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();
    let target_data: &TargetData = context.get_target_data();

    match ty {
        Type::Char(..) => llvm_context.i8_type().const_int(value, signed).const_neg(),
        Type::S8 { .. } if signed => llvm_context.i8_type().const_int(value, signed).const_neg(),
        Type::S8 { .. } => llvm_context.i8_type().const_int(value, signed),
        Type::S16 { .. } if signed => llvm_context.i16_type().const_int(value, signed).const_neg(),
        Type::S16 { .. } => llvm_context.i16_type().const_int(value, signed),
        Type::S32 { .. } if signed => llvm_context.i32_type().const_int(value, signed).const_neg(),
        Type::S32 { .. } => llvm_context.i32_type().const_int(value, signed),
        Type::S64 { .. } if signed => llvm_context.i64_type().const_int(value, signed).const_neg(),
        Type::S64 { .. } => llvm_context.i64_type().const_int(value, signed),
        Type::U8 { .. } => llvm_context.i8_type().const_int(value, false),
        Type::U16 { .. } => llvm_context.i16_type().const_int(value, false),
        Type::U32 { .. } => llvm_context.i32_type().const_int(value, false),
        Type::U64 { .. } => llvm_context.i64_type().const_int(value, false),
        Type::U128 { .. } if signed => llvm_context
            .i128_type()
            .const_int(value, signed)
            .const_neg(),
        Type::U128 { .. } => llvm_context.i128_type().const_int(value, signed),
        Type::Bool { .. } => llvm_context.bool_type().const_int(value, false),
        Type::USize { .. } => llvm_context
            .ptr_sized_int_type(target_data, None)
            .const_int(value, false),
        Type::SSize { .. } => llvm_context
            .ptr_sized_int_type(target_data, None)
            .const_int(value, true),

        what => abort::abort_codegen(
            context,
            &format!("Failed to compile '{}' as integer value!", what),
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}
