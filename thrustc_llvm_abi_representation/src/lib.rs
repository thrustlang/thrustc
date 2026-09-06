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

#![allow(clippy::large_enum_variant)]
#![allow(non_camel_case_types)]

use inkwell::targets::TargetData;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::type_layout::TargetInfo;

#[derive(Debug)]
pub enum LLVMABIRepresentation<'llvm_abi> {
    SystemVABI {
        file: &'llvm_abi CompilationUnit,
        options: &'llvm_abi CompilerOptions,
        target_triple: &'llvm_abi LLVMTargetTriple,
        target_info: &'llvm_abi TargetInfo,
        target_data: &'llvm_abi TargetData,
    },

    CudaABI {
        file: &'llvm_abi CompilationUnit,
        options: &'llvm_abi CompilerOptions,
        target_triple: &'llvm_abi LLVMTargetTriple,
        target_info: &'llvm_abi TargetInfo,
        target_data: &'llvm_abi TargetData,
    },

    WebAssemblyABI {
        file: &'llvm_abi CompilationUnit,
        options: &'llvm_abi CompilerOptions,
        target_triple: &'llvm_abi LLVMTargetTriple,
        target_info: &'llvm_abi TargetInfo,
        target_data: &'llvm_abi TargetData,
    },

    None,
}
