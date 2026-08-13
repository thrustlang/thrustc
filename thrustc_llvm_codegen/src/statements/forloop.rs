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
use thrustc_ast::traits::AstExpressionExtensions;
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::abort;
use crate::block;
use crate::codegen;
use crate::codegen::LLVMCodegen;
use crate::traits::AstLLVMGetType;
use crate::traits::LLVMFunctionExtensions;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;

pub fn compile<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, node: &'ctx Ast<'ctx>) {
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let llvm_function: FunctionValue = codegen
        .get_mut_context()
        .get_current_function(node.get_span())
        .get_value();

    let Ast::For {
        local,
        condition,
        actions,
        block,
        span,
        ..
    } = node
    else {
        return;
    };

    let block_span: Span = block.get_span();

    let start: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let cond: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let steps: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let body: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let exit: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

    llvm_builder
        .build_unconditional_branch(start)
        .unwrap_or_else(|_| {
            llvm_builder
                .build_unconditional_branch(start)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        codegen.get_mut_context(),
                        "Failed to compile of starter!",
                        *span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                })
        });

    llvm_builder.position_at_end(start);

    codegen.codegen_variables(local);

    llvm_builder
        .build_unconditional_branch(cond)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                codegen.get_mut_context(),
                "Failed to compile for loop start to condition!",
                block.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.position_at_end(steps);

    if !actions.is_unary_before_operation() {
        let _ = codegen::compile_as_value(codegen.get_mut_context(), actions, None);
    }

    llvm_builder
        .build_unconditional_branch(cond)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                codegen.get_mut_context(),
                "Failed to compile for loop start terminator to condition!",
                block.get_span(),
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.position_at_end(cond);

    self::short_circuit_comparison(codegen, condition, body, exit, llvm_function);

    block::move_specific_after_the_last(codegen.get_mut_context(), body, *span);

    llvm_builder.position_at_end(body);

    if codegen.get_context().get_loop_ctx().get_all_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_breakall_branch(exit);

        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .set_continueall_branch(steps);
    }

    codegen
        .get_mut_context()
        .get_mut_loop_context()
        .add_continue_branch(steps);

    codegen
        .get_mut_context()
        .get_mut_loop_context()
        .add_break_branch(exit);

    if actions.is_unary_before_operation() {
        let _ = codegen::compile_as_value(codegen.get_mut_context(), actions, None);
    }

    codegen.codegen_block(block);

    if codegen
        .get_mut_context()
        .get_last_builder_block(block_span)
        .get_terminator()
        .is_none()
    {
        llvm_builder
            .build_unconditional_branch(steps)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to compile for loop body terminator to actions!",
                    condition.get_span(),
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    block::move_specific_after_the_last(codegen.get_mut_context(), steps, *span);

    codegen.get_mut_context().get_mut_loop_context().pop();

    if codegen.get_context().get_loop_ctx().get_branch_depth() == 0 {
        codegen
            .get_mut_context()
            .get_mut_loop_context()
            .pop_superior_branch();
    }

    llvm_builder.position_at_end(exit);
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
