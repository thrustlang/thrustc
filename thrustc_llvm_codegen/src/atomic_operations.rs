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

use inkwell::{AtomicOrdering, values::InstructionValue};
use thrustc_code_location::Span;

use crate::{abort, context::LLVMCodeGenContext};

#[derive(Debug, Clone, Copy)]
pub struct LLVMAtomicModificators {
    pub atomic_volatile: bool,
    pub atomic_ord: Option<AtomicOrdering>,
}

#[inline]
pub fn set_atomic_behavior_load_instruction<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    instruction: InstructionValue<'ctx>,
    modificators: LLVMAtomicModificators,
    span: Span,
) {
    if modificators.atomic_volatile {
        instruction.set_volatile(true).unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile an atomic behavior!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });
    }

    let Some(ordering) = modificators.atomic_ord else {
        return;
    };

    if !matches!(ordering, AtomicOrdering::NotAtomic) {
        /*
           pub fn set_atomic_ordering(self, ordering: AtomicOrdering) -> Result<(), &'static str> {
               // Although fence and atomicrmw both have an ordering, the LLVM C API
               // does not support them. The cmpxchg instruction has two orderings and
               // does not work with this API.
               if !self.is_a_load_inst() && !self.is_a_store_inst() {
                   return Err("Value is not a load or store instruction.");
               }
               match ordering {
                   AtomicOrdering::Release if self.is_a_load_inst() => {
                       return Err("The release ordering is not valid on load instructions.")
                   },
                   AtomicOrdering::AcquireRelease => {
                       return Err("The acq_rel ordering is not valid on load or store instructions.")
                   },
                   AtomicOrdering::Acquire if self.is_a_store_inst() => {
                       return Err("The acquire ordering is not valid on store instructions.")
                   },
                   _ => {},
               };
               unsafe { LLVMSetOrdering(self.as_value_ref(), ordering.into()) };
               Ok(())
           }
        */

        if matches!(ordering, AtomicOrdering::Release) {
            return;
        }

        if matches!(ordering, AtomicOrdering::AcquireRelease) {
            return;
        }

        instruction
            .set_atomic_ordering(ordering)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to compile an atomic behavior!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }
}
