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

#![allow(stable_features)]

use thrustc_ast::Ast;

use crate::{codegen::LLVMCodegen, context::LLVMCodeGenContext, metadata::LLVMMetadata};

mod abort;
mod atomic_operations;
mod attributebuilder;
mod block;
mod branch_context;
mod cast;
mod codegen;
mod compiler_builtins;
pub mod context;
pub mod debug_context;
mod expressions;
mod heap_memory;
mod impls;
pub mod jit;
mod memory;
mod metadata;
pub mod optimizer;
mod pointer_anchor;
mod predicates;
mod stack_memory;
mod statements;
mod static_memory;
mod table;
mod toplevel;
mod traits;
mod typegeneration;
mod types;
mod utils;

pub struct LLVMCompiler;

impl<'a, 'ctx> LLVMCompiler {
    #[inline]
    pub fn compile(context: &'a mut LLVMCodeGenContext<'a, 'ctx>, ast: &'ctx [Ast<'ctx>]) {
        LLVMMetadata::setup_platform_independent_metadata(context);
        LLVMCodegen::codegen(context, ast);
    }
}
