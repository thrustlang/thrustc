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

use inkwell::builder::Builder;
use inkwell::targets::TargetData;
use thrustc_diagnostician::Diagnostician;
use thrustc_options::{CompilationUnit, CompilerOptions};

use crate::modificators::LLVMAtomicModificators;

#[derive(Debug)]
pub struct LLVMAtomicCodeGenContext<'atomic_ctx> {
    builder: &'atomic_ctx Builder<'atomic_ctx>,
    target_data: &'atomic_ctx TargetData,
    diagnostician: Diagnostician,
    atomic_modificators: Vec<LLVMAtomicModificators>,
}

impl<'atomic_ctx> LLVMAtomicCodeGenContext<'atomic_ctx> {
    pub fn new(
        builder: &'atomic_ctx Builder<'atomic_ctx>,
        target_data: &'atomic_ctx TargetData,
        file: &CompilationUnit,
        options: &CompilerOptions,
    ) -> Self {
        let diagnostician: Diagnostician = Diagnostician::new(file, options);

        Self {
            builder,
            target_data,
            diagnostician,
            atomic_modificators: Vec::new(),
        }
    }
}

impl<'atomic_ctx> LLVMAtomicCodeGenContext<'atomic_ctx> {
    #[inline]
    pub fn get_builder(&self) -> &'atomic_ctx Builder<'atomic_ctx> {
        self.builder
    }

    #[inline]
    pub fn get_target_data(&self) -> &'atomic_ctx TargetData {
        self.target_data
    }

    #[inline]
    pub fn get_atomic_modificators(&self) -> Option<LLVMAtomicModificators> {
        self.atomic_modificators.last().copied()
    }
}

impl LLVMAtomicCodeGenContext<'_> {
    #[inline]
    pub fn push_atomic_modificators(&mut self, modificators: LLVMAtomicModificators) {
        self.atomic_modificators.push(modificators);
    }

    #[inline]
    pub fn pop_atomic_modificators(&mut self) {
        self.atomic_modificators.pop();
    }
}

impl<'atomic_ctx> LLVMAtomicCodeGenContext<'atomic_ctx> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }
}
