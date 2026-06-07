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

use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::values::FunctionValue;

use thrustc_span::Span;

use crate::context::LLVMCodeGenContext;
use crate::traits::LLVMFunctionExtensions;
use crate::{abort, utils};

#[inline]
pub fn move_terminator_to_end(context: &mut LLVMCodeGenContext, span: Span) {
    let function: FunctionValue = context.get_current_function(span).get_value();

    let last_block: BasicBlock = context.get_last_builder_block(span);

    if let Some(parent) = function.get_last_basic_block() {
        last_block.move_after(parent).unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to move the IR block!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            );
        });
    }
}

#[inline]
pub fn move_specific_after_the_last(
    context: &mut LLVMCodeGenContext,
    block: BasicBlock,
    span: Span,
) {
    let last_block: BasicBlock = context.get_last_builder_block(span);

    block.move_after(last_block).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to move the IR block!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    });
}

#[inline]
pub fn append_block<'ctx>(
    context: &LLVMCodeGenContext<'_, 'ctx>,
    function: FunctionValue<'ctx>,
) -> BasicBlock<'ctx> {
    let llvm_context: &Context = context.get_llvm_context();

    let obfuscated_name: &str = &utils::generate_string(context, utils::SHORT_RANGE_OBFUSCATION);

    llvm_context.append_basic_block(function, obfuscated_name)
}
