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

#![allow(clippy::too_many_arguments)]

use std::path::PathBuf;

use inkwell::values::{BasicValueEnum, IntValue, PointerValue, StructValue};

use crate::abort;
use crate::context::LLVMAtomicCodeGenContext;

pub fn generate_cmpxchg<'ctx>(
    context: &mut LLVMAtomicCodeGenContext<'ctx>,
    ptr: PointerValue<'ctx>,
    expected_ptr: PointerValue<'ctx>,
    expected: BasicValueEnum<'ctx>,
    new_value: BasicValueEnum<'ctx>,
    success: inkwell::AtomicOrdering,
    failure: inkwell::AtomicOrdering,
    span: thrustc_code_location::Span,
) -> IntValue<'ctx> {
    let builder: &inkwell::builder::Builder<'ctx> = context.get_builder();

    let swapped: StructValue<'ctx> = builder
        .build_cmpxchg(ptr, expected, new_value, success, failure)
        .unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to build an atomic compare-and-swap instruction!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    let previous: BasicValueEnum<'ctx> = builder
        .build_extract_value(swapped, 0, "")
        .unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to extract the previous value of an atomic compare-and-swap!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    let flag: BasicValueEnum<'ctx> =
        builder
            .build_extract_value(swapped, 1, "")
            .unwrap_or_else(|_| {
                abort::abort_atomic_codegen(
                    context,
                    "Failed to extract the success flag of an atomic compare-and-swap!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

    builder
        .build_store(expected_ptr, previous)
        .unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to store the previous value of an atomic compare-and-swap!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    flag.into_int_value()
}

pub fn generate_atomicrmw<'ctx>(
    context: &mut LLVMAtomicCodeGenContext<'ctx>,
    op: inkwell::AtomicRMWBinOp,
    ptr: PointerValue<'ctx>,
    value: IntValue<'ctx>,
    ordering: inkwell::AtomicOrdering,
    span: thrustc_code_location::Span,
) -> IntValue<'ctx> {
    let builder: &inkwell::builder::Builder<'ctx> = context.get_builder();

    builder
        .build_atomicrmw(op, ptr, value, ordering)
        .unwrap_or_else(|_| {
            abort::abort_atomic_codegen(
                context,
                "Failed to build an atomic read-modify-write instruction!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        })
}
