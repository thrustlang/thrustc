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

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_llvm_target_triple::traits::LLVMTargetTripleSupport;
use thrustc_typesystem::{
    Type,
    traits::{ConstantTypeExtensions, TypeCodeLocation},
};

use crate::TypeChecker;

pub fn check_target_type_support<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    ty: &Type,
) {
    let ty: Type = ty.remove_all_constant_type();
    let llvm_is_used: bool = typechecker.get_compiler_options().llvm();

    if matches!(ty, Type::FX8680 { .. }) && llvm_is_used {
        let compiler_options: &thrustc_options::CompilerOptions =
            typechecker.get_compiler_options();

        let llvm_backend: &thrustc_backends::llvm::LLVMBackend =
            compiler_options.get_llvm_backend();

        let normalized_target_triple: &thrustc_llvm_target_triple::LLVMTargetTriple =
            llvm_backend.get_target().get_normalized_target_triple();

        let support: bool = normalized_target_triple.support_80_bits_floating_point();

        if !support {
            typechecker.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0039,
                "Unsupported type".into(),
                "Type isn't supported on the current target architecture.".into(),
                None,
                ty.get_span(),
            ));
        }
    }

    if matches!(ty, Type::FPPC128 { .. }) && llvm_is_used {
        let compiler_options: &thrustc_options::CompilerOptions =
            typechecker.get_compiler_options();
        let llvm_backend: &thrustc_backends::llvm::LLVMBackend =
            compiler_options.get_llvm_backend();

        let normalized_target_triple: &thrustc_llvm_target_triple::LLVMTargetTriple =
            llvm_backend.get_target().get_normalized_target_triple();

        let support: bool = normalized_target_triple.support_128_bits_ppc_floating_point();

        if !support {
            typechecker.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0039,
                "Unsupported type".into(),
                "Type isn't supported on the current target architecture.".into(),
                None,
                ty.get_span(),
            ));
        }
    }
}
