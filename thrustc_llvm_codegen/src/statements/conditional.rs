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
use thrustc_ast::traits::AstCodeBlockEntensions;
use thrustc_ast::traits::AstCodeLocation;
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::abort;
use crate::block;
use crate::codegen;
use crate::codegen::LLVMCodegen;
use crate::traits::AstLLVMGetType;
use crate::traits::LLVMFunctionExtensions;

pub fn compile<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, node: &'ctx Ast<'ctx>) {
    let Ast::If {
        condition,
        then_branch,
        else_if_branch,
        else_branch,
        ..
    } = node
    else {
        return;
    };

    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let llvm_function: FunctionValue = codegen
        .get_mut_context()
        .get_current_function(node.get_span())
        .get_value();

    let block_span: Span = then_branch.get_span();

    let then_block: BasicBlock = block::append_block(codegen.get_context(), llvm_function);
    let merge_block: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

    let is_if_returns: bool = then_branch.has_terminator();
    let is_elif_returns: bool = else_if_branch.iter().all(|node| {
        if let Ast::Elif { block, .. } = node {
            block.has_terminator()
        } else {
            false
        }
    });

    let is_else_returns: bool = else_branch
        .as_ref()
        .is_some_and(|otherwise| match &**otherwise {
            Ast::Else { block, .. } => block.has_terminator(),
            _ => false,
        });

    let is_if_else_returns: bool = is_if_returns && is_else_returns && else_if_branch.is_empty();
    let is_full_returns: bool = is_if_returns && is_elif_returns && is_else_returns;

    let next_block: BasicBlock = if (!else_if_branch.is_empty() || else_branch.is_some())
        && !(is_if_else_returns || is_full_returns)
    {
        block::append_block(codegen.get_context(), llvm_function)
    } else {
        merge_block
    };

    self::short_circuit_comparison(codegen, condition, then_block, next_block, llvm_function);

    llvm_builder.position_at_end(then_block);

    codegen.codegen_block(then_branch);

    if codegen
        .get_mut_context()
        .get_last_builder_block(block_span)
        .get_terminator()
        .is_none()
    {
        llvm_builder
            .build_unconditional_branch(merge_block)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to if terminator!",
                    block_span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    if !else_if_branch.is_empty() {
        let span: Span = else_if_branch
            .first()
            .unwrap_or_else(|| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to get elif code location!",
                    block_span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
            .get_span();

        self::compile_elseif(codegen, else_if_branch, next_block, merge_block, span);
    }

    if let Some(node) = else_branch {
        self::compile_else(codegen, node, next_block, merge_block);
    }

    llvm_builder.position_at_end(merge_block);
}

fn compile_elseif<'ctx>(
    codegen: &mut LLVMCodegen<'_, 'ctx>,
    else_if: &'ctx [Ast<'ctx>],
    first_block: BasicBlock<'ctx>,
    merge: BasicBlock<'ctx>,
    span: Span,
) {
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let llvm_function: FunctionValue = codegen
        .get_mut_context()
        .get_current_function(span)
        .get_value();

    let mut current: BasicBlock = first_block;

    for (idx, elseif) in else_if.iter().enumerate() {
        let Ast::Elif {
            condition, block, ..
        } = elseif
        else {
            continue;
        };

        let block_span: Span = block.get_span();
        let is_last: bool = idx == else_if.len().saturating_sub(1);

        llvm_builder.position_at_end(current);

        let then_block: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

        let next_block: BasicBlock = if is_last {
            merge
        } else {
            block::append_block(codegen.get_context(), llvm_function)
        };

        self::short_circuit_comparison(codegen, condition, then_block, next_block, llvm_function);

        llvm_builder.position_at_end(then_block);

        codegen.codegen_block(block);

        if codegen
            .get_mut_context()
            .get_last_builder_block(block_span)
            .get_terminator()
            .is_none()
        {
            llvm_builder
                .build_unconditional_branch(merge)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        codegen.get_mut_context(),
                        "Failed to elif terminator!",
                        block.get_span(),
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });
        }

        current = next_block;
    }

    llvm_builder.position_at_end(current);
}

pub fn compile_else<'ctx>(
    codegen: &mut LLVMCodegen<'_, 'ctx>,
    r#else: &'ctx Ast<'ctx>,
    next: BasicBlock<'ctx>,
    merge: BasicBlock<'ctx>,
) {
    let llvm_builder: &Builder = codegen.get_mut_context().get_llvm_builder();

    let Ast::Else { block, .. } = r#else else {
        return;
    };

    let block_span: Span = block.get_span();

    llvm_builder.position_at_end(next);

    codegen.codegen_block(block);

    if codegen
        .get_mut_context()
        .get_last_builder_block(block_span)
        .get_terminator()
        .is_none()
    {
        llvm_builder
            .build_unconditional_branch(merge)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    codegen.get_mut_context(),
                    "Failed to compile else block!",
                    block_span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }
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
