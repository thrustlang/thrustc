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
use thrustc_ast::traits::AstCodeLocation;
use thrustc_code_location::Span;

use crate::abort;
use crate::block;
use crate::codegen::LLVMCodegen;
use crate::traits::LLVMFunctionExtensions;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::values::FunctionValue;

pub fn compile<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, node: &'ctx Ast<'ctx>) {
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let llvm_function: FunctionValue = codegen
        .get_mut_context()
        .get_current_function(node.get_span())
        .get_value();

    let Ast::Loop { block, span, .. } = node else {
        return;
    };

    let block_span: Span = block.get_span();

    let start: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let exit: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

    llvm_builder
        .build_unconditional_branch(start)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                codegen.get_mut_context(),
                "Failed to compile loop terminator to start!",
                *span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.position_at_end(start);

    if codegen.get_context().get_loop_ctx().get_all_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_breakall_branch(exit);

        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_continueall_branch(start);
    }

    codegen
        .get_mut_context()
        .get_mut_loop_context()
        .add_continue_branch(start);

    codegen
        .get_mut_context()
        .get_mut_loop_context()
        .add_break_branch(exit);

    codegen.codegen_block(block);

    if codegen
        .get_mut_context()
        .get_last_builder_block(block_span)
        .get_terminator()
        .is_none()
    {
        llvm_builder
            .build_unconditional_branch(start)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to compile loop body terminator to start!",
                    *span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    codegen.get_mut_context().get_mut_loop_context().pop();

    if codegen.get_context().get_loop_ctx().get_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .pop_superior_branch();
    }

    llvm_builder.position_at_end(exit);
}
