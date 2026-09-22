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

use std::path::PathBuf;

use inkwell::AtomicOrdering;
use inkwell::values::InstructionValue;
use thrustc_code_location::Span;

use crate::abort;
use crate::context::LLVMAtomicCodeGenContext;
use crate::modificators::LLVMAtomicModificators;

pub fn set_atomic_behavior_load_instruction<'ctx>(
    context: &mut LLVMAtomicCodeGenContext,
    instruction: InstructionValue<'ctx>,
    modificators: LLVMAtomicModificators,
    span: Span,
) {
    if modificators.get_atomic_volatile() {
        instruction.set_volatile(true).unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to compile an atomic behavior!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });
    }

    let Some(ordering) = modificators.get_atomic_ord() else {
        return;
    };

    if !matches!(ordering, AtomicOrdering::NotAtomic) {
        if matches!(ordering, AtomicOrdering::Release) {
            return;
        }

        if matches!(ordering, AtomicOrdering::AcquireRelease) {
            return;
        }

        instruction
            .set_atomic_ordering(ordering)
            .unwrap_or_else(|_| {
                abort::abort_atomic_codegen(
                    context,
                    "Failed to compile an atomic behavior!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });
    }
}

pub fn set_atomic_behavior_store_instruction<'ctx>(
    context: &mut LLVMAtomicCodeGenContext,
    instruction: InstructionValue<'ctx>,
    modificators: LLVMAtomicModificators,
    span: Span,
) {
    if modificators.get_atomic_volatile() {
        instruction.set_volatile(true).unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to compile an atomic behavior!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });
    }

    let Some(ordering) = modificators.get_atomic_ord() else {
        return;
    };

    if !matches!(ordering, AtomicOrdering::NotAtomic) {
        if matches!(ordering, AtomicOrdering::AcquireRelease) {
            return;
        }

        if matches!(ordering, AtomicOrdering::Acquire) {
            return;
        }

        instruction
            .set_atomic_ordering(ordering)
            .unwrap_or_else(|_| {
                abort::abort_atomic_codegen(
                    context,
                    "Failed to compile an atomic behavior!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });
    }
}