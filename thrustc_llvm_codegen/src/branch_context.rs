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

#[derive(Debug)]
pub struct LLVMLoopContext<'ctx> {
    break_branches: Vec<BasicBlock<'ctx>>,
    continue_branches: Vec<BasicBlock<'ctx>>,
    continueall_branch: Option<BasicBlock<'ctx>>,
    breakerall_branch: Option<BasicBlock<'ctx>>,
}

impl<'ctx> LLVMLoopContext<'ctx> {
    #[inline]
    pub fn new() -> LLVMLoopContext<'ctx> {
        LLVMLoopContext {
            break_branches: Vec::with_capacity(u8::MAX as usize),
            continue_branches: Vec::with_capacity(u8::MAX as usize),
            continueall_branch: None,
            breakerall_branch: None,
        }
    }
}

impl<'ctx> LLVMLoopContext<'ctx> {
    #[inline]
    pub fn add_break_branch(&mut self, branch: BasicBlock<'ctx>) {
        self.break_branches.push(branch);
    }

    #[inline]
    pub fn add_continue_branch(&mut self, branch: BasicBlock<'ctx>) {
        self.continue_branches.push(branch);
    }

    #[inline]
    pub fn set_continueall_branch(&mut self, branch: BasicBlock<'ctx>) {
        self.continueall_branch = Some(branch);
    }

    #[inline]
    pub fn set_breakall_branch(&mut self, branch: BasicBlock<'ctx>) {
        self.breakerall_branch = Some(branch);
    }
}

impl<'ctx> LLVMLoopContext<'ctx> {
    #[inline]
    pub fn get_last_break_branch(&self) -> BasicBlock<'ctx> {
        *self.break_branches.last().unwrap_or_else(|| {
            self::codegen_abort("Loop control flow 'breaker' branch couldn't be obtained.");
        })
    }

    #[inline]
    pub fn get_last_continue_branch(&self) -> BasicBlock<'ctx> {
        *self.continue_branches.last().unwrap_or_else(|| {
            self::codegen_abort("Loop control flow 'continue' branch couldn't be obtained.");
        })
    }

    #[inline]
    pub fn get_breakall_branch(&self) -> BasicBlock<'ctx> {
        self.breakerall_branch.unwrap_or_else(|| {
            self::codegen_abort("Loop control flow 'breakall' branch couldn't be obtained.");
        })
    }

    #[inline]
    pub fn get_continueall_branch(&self) -> BasicBlock<'ctx> {
        self.continueall_branch.unwrap_or_else(|| {
            self::codegen_abort("Loop control flow 'continueall' branch couldn't be obtained.");
        })
    }
}

impl<'ctx> LLVMLoopContext<'ctx> {
    #[inline]
    pub fn get_all_branch_depth(&self) -> usize {
        self.continue_branches.len()
            + self.break_branches.len()
            + self.continueall_branch.is_some() as usize
            + self.breakerall_branch.is_some() as usize
    }

    #[inline]
    pub fn get_branch_depth(&self) -> usize {
        self.continue_branches.len() + self.break_branches.len()
    }
}

impl LLVMLoopContext<'_> {
    #[inline]
    pub fn pop(&mut self) {
        self.break_branches.pop();
        self.continue_branches.pop();
    }

    #[inline]
    pub fn pop_superior_branch(&mut self) {
        self.continueall_branch = None;
        self.breakerall_branch = None;
    }
}

fn codegen_abort<T: std::fmt::Display>(message: T) -> ! {
    thrustc_logging::print_backend_bug(
        thrustc_logging::LoggingType::BackendBug,
        &format!("{}", message),
    );
}
