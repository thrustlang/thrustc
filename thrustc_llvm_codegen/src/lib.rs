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
mod anchor;
mod atomic_operations;
mod attributebuilder;
mod block;
mod branch_context;
mod builtins;
mod cast;
mod codegen;
pub mod context;
pub mod debug_context;
mod declarations;
mod expressions;
mod heap;
mod impls;
pub mod jit;
mod memory;
mod metadata;
pub mod optimizer;
mod predicates;
mod stack;
mod statements;
mod r#static;
mod table;
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
