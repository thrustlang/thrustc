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

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_attributes::{ThrustAttribute, ThrustAttributes};
use thrustc_span::Span;
use thrustc_typesystem::Type;

use std::path::PathBuf;

use crate::context::LLVMCodeGenContext;
use crate::{abort, heap_memory, typegeneration};

pub fn allocate_variable<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    llvm_name: &str,
    kind: &Type,
    attributes: &ThrustAttributes,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_name: String = format!("local.{}", llvm_name);
    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, kind);

    context.mark_dbg_location(span);

    if attributes.has_heap_attr() {
        heap_memory::try_allocate_at_heap(context, &llvm_name, llvm_type, attributes, span)
    } else {
        self::try_allocate_at_stack(context, &llvm_name, llvm_type, attributes, span)
    }
}

#[inline]
fn try_allocate_at_stack<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    llvm_name: &str,
    llvm_type: BasicTypeEnum<'ctx>,
    attributes: &ThrustAttributes,
    span: Span,
) -> PointerValue<'ctx> {
    let target_data: &inkwell::targets::TargetData = context.get_target_data();

    if let Ok(ptr) = context
        .get_llvm_builder()
        .build_alloca(llvm_type, llvm_name)
    {
        if let Some(align_attr) =
            attributes.get_attr(thrustc_attributes::ThrustAttributeComparator::Align)
        {
            if let Some(instruction) = ptr.as_instruction() {
                if let ThrustAttribute::Align(value, ..) = align_attr {
                    let preferred_aligment: u32 = target_data.get_preferred_alignment(&llvm_type);

                    instruction
                        .set_alignment(value.try_into().unwrap_or(preferred_aligment))
                        .unwrap_or_else(|_| {
                            abort::abort_codegen(
                                context,
                                "Failed to set type alignment!",
                                span,
                                PathBuf::from(file!()),
                                line!(),
                            );
                        });
                }
            }
        }

        return ptr;
    }

    abort::abort_codegen(
        context,
        "Failed to allocate at stack!",
        span,
        PathBuf::from(file!()),
        line!(),
    );
}
