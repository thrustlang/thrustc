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
use inkwell::builder::Builder;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;

use thrustc_ast::Ast;
use thrustc_ast::traits::AstCodeLocation;
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::abort;
use crate::block;
use crate::codegen;
use crate::codegen::LLVMCodegen;
use crate::traits::AstLLVMGetType;
use crate::traits::LLVMFunctionExtensions;

pub fn compile<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, node: &'ctx Ast<'ctx>) {
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let llvm_function: FunctionValue = codegen
        .get_mut_context()
        .get_current_function(node.get_span())
        .get_value();

    let Ast::While {
        variable,
        condition,
        block,
        ..
    } = node
    else {
        return;
    };

    let block_span: Span = block.get_span();

    let cond: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let body: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let exit: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

    if let Some(node) = variable {
        codegen.codegen_variables(node);
    }

    llvm_builder
        .build_unconditional_branch(cond)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                codegen.get_mut_context(),
                "Failed to compile while loop terminator to condition!",
                condition.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.position_at_end(cond);

    self::short_circuit_comparison(codegen, condition, body, exit, llvm_function);

    llvm_builder.position_at_end(body);

    if codegen.get_context().get_loop_ctx().get_all_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_breakall_branch(exit);

        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_continueall_branch(cond);
    }

    codegen
        .get_mut_context()
        .get_mut_loop_context()
        .add_continue_branch(cond);

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
            .build_unconditional_branch(cond)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to compile while loop body terminator to comparison!",
                    condition.get_span(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    llvm_builder.position_at_end(exit);

    if codegen.get_context().get_loop_ctx().get_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .pop_superior_branch();
    }

    codegen.get_mut_context().get_mut_loop_context().pop();
}

fn short_circuit_comparison<'ctx>(
    codegen: &mut LLVMCodegen<'_, 'ctx>,
    condition: &'ctx Ast<'ctx>,
    target_body: BasicBlock<'ctx>,
    target_exit: BasicBlock<'ctx>,
    llvm_function: FunctionValue<'ctx>,
) {
    let llvm_builder: &Builder<'_> = codegen.get_context().get_llvm_builder();

    if let Ast::Group { node, .. } = condition {
        self::short_circuit_comparison(codegen, node, target_body, target_exit, llvm_function);
        return;
    }

    if let Ast::BinaryOp {
        left,
        right,
        operator,
        ..
    } = condition
    {
        if *operator == TokenType::And {
            let next_cond_block: BasicBlock<'_> =
                block::append_block(codegen.get_context(), llvm_function);

            self::short_circuit_comparison(
                codegen,
                left,
                next_cond_block,
                target_exit,
                llvm_function,
            );

            llvm_builder.position_at_end(next_cond_block);

            self::short_circuit_comparison(codegen, right, target_body, target_exit, llvm_function);

            return;
        }

        if *operator == TokenType::Or {
            let next_cond_block: BasicBlock<'_> =
                block::append_block(codegen.get_context(), llvm_function);

            self::short_circuit_comparison(
                codegen,
                left,
                target_body,
                next_cond_block,
                llvm_function,
            );

            llvm_builder.position_at_end(next_cond_block);

            self::short_circuit_comparison(codegen, right, target_body, target_exit, llvm_function);

            return;
        }
    }

    let condition_type: &Type = condition.get_type_for_llvm();

    let comparison: IntValue<'_> =
        codegen::compile_as_value(codegen.get_mut_context(), condition, Some(condition_type))
            .into_int_value();

    llvm_builder
        .build_conditional_branch(comparison, target_body, target_exit)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                codegen.get_mut_context(),
                "Failed to compile for loop comparison to body!",
                condition.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });
}
