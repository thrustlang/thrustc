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
use thrustc_span::Span;

use crate::{abort, context::LLVMCodeGenContext};

#[derive(Debug, Clone, Copy)]
pub struct LLVMAtomicModificators {
    pub atomic_volatile: bool,
    pub atomic_ord: Option<AtomicOrdering>,
}

#[inline]
pub fn set_atomic_behavior<'ctx>(
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

    if let Some(ordering) = modificators.atomic_ord {
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
