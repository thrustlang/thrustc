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

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use thrustc_llvm_target_triple::LLVMTargetTriple;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VaListKind {
    CharPtr,
    VoidPtr,
    X86_64ABI,
    AArch64ABI,
    PowerABI,
    SystemZABI,
    AAPCSABI,
    HexagonABI,
}

pub fn resolve_va_list_kind(triple: &LLVMTargetTriple) -> VaListKind {
    if triple.is_windows_based() || triple.is_darwin_os() {
        return VaListKind::CharPtr;
    }

    if triple.is_x86_64_arch() {
        return VaListKind::X86_64ABI;
    }

    if triple.is_aarch64_arch() {
        return VaListKind::AArch64ABI;
    }

    if triple.is_arm_family() {
        return VaListKind::AAPCSABI;
    }

    if triple.is_hexagon_arch() {
        return VaListKind::HexagonABI;
    }

    if triple.get_arch().contains("riscv") {
        return VaListKind::VoidPtr;
    }

    if triple.is_systemz_arch() {
        return VaListKind::SystemZABI;
    }

    if triple.is_ppc64_arch() {
        return VaListKind::CharPtr;
    }

    if triple.is_ppc_arch() && triple.is_object_format_elf() {
        return VaListKind::PowerABI;
    }

    if triple.is_loongarch64_arch() {
        return VaListKind::VoidPtr;
    }

    if triple.is_mips64_arch() || triple.get_arch().contains("mips") {
        return VaListKind::CharPtr;
    }

    if triple.is_sparc_arch() {
        return VaListKind::CharPtr;
    }

    VaListKind::CharPtr
}

pub fn build_va_list_llvm_type<'ctx>(
    context: &'ctx Context,
    kind: VaListKind,
) -> BasicTypeEnum<'ctx> {
    let default_pointer: BasicTypeEnum = context.ptr_type(AddressSpace::default()).into();

    match kind {
        VaListKind::CharPtr | VaListKind::VoidPtr => default_pointer,

        VaListKind::X86_64ABI => context
            .struct_type(
                &[
                    context.i32_type().into(),
                    context.i32_type().into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            )
            .into(),

        VaListKind::AArch64ABI => context
            .struct_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.i32_type().into(),
                    context.i32_type().into(),
                ],
                false,
            )
            .into(),

        VaListKind::PowerABI => context
            .struct_type(
                &[
                    context.i8_type().into(),
                    context.i8_type().into(),
                    context.i16_type().into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            )
            .into(),

        VaListKind::SystemZABI => context
            .struct_type(
                &[
                    context.i64_type().into(),
                    context.i64_type().into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            )
            .into(),

        VaListKind::AAPCSABI => context
            .struct_type(
                &[context.ptr_type(AddressSpace::default()).into()],
                false,
            )
            .into(),

        VaListKind::HexagonABI => context
            .struct_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            )
            .into(),
    }
}