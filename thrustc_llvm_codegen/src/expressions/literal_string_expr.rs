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

use inkwell::values::PointerValue;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::ArrayType;
use inkwell::values::GlobalValue;
use thrustc_span::Span;

use crate::abort;
use crate::context::LLVMCodeGenContext;
use crate::utils;

pub fn compile<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    bytes: &'ctx [u8],
    null_terminated: bool,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();
    let llvm_context: &Context = context.get_llvm_context();

    let parsed_size: u32 = u32::try_from(bytes.len()).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to compile string literal!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    let fixed_cstr_size: u32 = if !bytes.is_empty() && null_terminated {
        parsed_size + 1
    } else {
        parsed_size
    };

    let cstr_type: ArrayType = llvm_context.i8_type().array_type(fixed_cstr_size);

    let cstr_name: String = format!(
        "cstr{}",
        utils::generate_string(context, utils::SHORT_RANGE_OBFUSCATION)
    );

    let cstr: GlobalValue =
        llvm_module.add_global(cstr_type, Some(AddressSpace::default()), &cstr_name);

    cstr.set_alignment(
        context
            .get_target_data()
            .get_preferred_alignment_of_global(&cstr),
    );

    cstr.set_linkage(Linkage::LinkerPrivate);
    cstr.set_initializer(&llvm_context.const_string(bytes, null_terminated));
    cstr.set_constant(true);

    if context.get_expressions_optimizations().has_unnamed_addr() {
        cstr.set_unnamed_addr(true);
    }

    cstr.as_pointer_value()
}
