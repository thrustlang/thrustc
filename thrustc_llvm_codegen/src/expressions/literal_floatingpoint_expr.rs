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

use inkwell::{context::Context, values::FloatValue};

use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::{abort, context::LLVMCodeGenContext};

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ty: &Type,
    value: f64,
    span: Span,
) -> FloatValue<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    match ty {
        Type::F32 { .. } => llvm_context.f32_type().const_float(value),
        Type::F64 { .. } => llvm_context.f64_type().const_float(value),
        Type::FX8680 { .. } => llvm_context.x86_f80_type().const_float(value),
        Type::F128 { .. } => llvm_context.f128_type().const_float(value),
        Type::FPPC128 { .. } => llvm_context.ppc_f128_type().const_float(value),

        what => abort::abort_codegen(
            context,
            &format!("Failed to compile '{}' as float value!", what),
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}
