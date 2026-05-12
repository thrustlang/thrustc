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

#![allow(clippy::collapsible_match)]

use thrustc_ast::Ast;
use thrustc_attributes::{
    ThrustAttribute, ThrustAttributeComparator, ThrustAttributes,
    traits::ThrustAttributesExtensions,
};
use thrustc_backends::llvm::LLVMBackend;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_llvm_attributes::{
    LLVMAttribute, LLVMAttributeComparator, LLVMAttributes, traits::LLVMAttributesExtensions,
};
use thrustc_llvm_callconventions::LLVMCallConvention;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;

#[derive(Debug)]
pub struct LLVMCallConventionsChecker<'call_conv_checker> {
    ast: &'call_conv_checker [Ast<'call_conv_checker>],
    options: &'call_conv_checker CompilerOptions,
    errors: Vec<CompilationIssue>,
    diagnostician: Diagnostician,
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMCallConventionAplicant {
    Function,
    Instrinsic,
}

impl<'call_conv_checker> LLVMCallConventionsChecker<'call_conv_checker> {
    #[inline]
    pub fn new(
        ast: &'call_conv_checker [Ast<'call_conv_checker>],
        options: &'call_conv_checker CompilerOptions,
        file: &'call_conv_checker CompilationUnit,
    ) -> Self {
        Self {
            ast,
            options,
            errors: Vec::with_capacity(u8::MAX as usize),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'call_conv_checker> LLVMCallConventionsChecker<'call_conv_checker> {
    pub fn analyze(&mut self) -> bool {
        {
            for node in self.ast.iter() {
                self.visit_node(node);
            }
        }

        self.verify()
    }
}

impl LLVMCallConventionsChecker<'_> {
    fn visit_node(&mut self, node: &Ast) {
        match node {
            Ast::Function { attributes, .. } => {
                self.analyze_applicant(attributes, LLVMCallConventionAplicant::Function);
            }
            Ast::Intrinsic { attributes, .. } => {
                self.analyze_applicant(attributes, LLVMCallConventionAplicant::Instrinsic);
            }

            _ => (),
        }
    }
}

impl LLVMCallConventionsChecker<'_> {
    fn analyze_applicant(
        &mut self,
        attributes: &ThrustAttributes,
        applicant: LLVMCallConventionAplicant,
    ) {
        let llvm_attributes: LLVMAttributes =
            thrustc_llvm_attributes::into_llvm_attributes(attributes);
        let llvm_backend: &LLVMBackend = self.get_compiler_options().get_llvm_backend();

        let triple: (String, String, String, String) =
            llvm_backend.get_target().dissamble_target_triple();

        match applicant {
            LLVMCallConventionAplicant::Function => {
                if let Some(LLVMAttribute::Convention(call_conv)) =
                    llvm_attributes.get_attr(LLVMAttributeComparator::Convention)
                {
                    if let Some(ThrustAttribute::Convention(_, span)) =
                        attributes.get_attr(ThrustAttributeComparator::Convention)
                    {
                        self.analyze_calling_convention(triple, call_conv, applicant, span);
                    }
                }
            }
            LLVMCallConventionAplicant::Instrinsic => {
                if let Some(LLVMAttribute::Convention(call_conv)) =
                    llvm_attributes.get_attr(LLVMAttributeComparator::Convention)
                {
                    if let Some(ThrustAttribute::Convention(_, span)) =
                        attributes.get_attr(ThrustAttributeComparator::Convention)
                    {
                        self.analyze_calling_convention(triple, call_conv, applicant, span);
                    }
                }
            }
        }
    }
}

impl LLVMCallConventionsChecker<'_> {
    fn analyze_calling_convention(
        &mut self,
        target_triple: (String, String, String, String),
        call_conv: LLVMCallConvention,
        applicant: LLVMCallConventionAplicant,
        span: Span,
    ) {
        const X86_64_CALL_CONVENTIONS: &[LLVMCallConvention] = &[
            LLVMCallConvention::X86_StdCall,
            LLVMCallConvention::X86_FastCall,
            LLVMCallConvention::X86_ThisCall,
            LLVMCallConvention::X86_64_SysV,
            LLVMCallConvention::X86_INTR,
            LLVMCallConvention::X86_VectorCall,
            LLVMCallConvention::X86_RegCall,
        ];

        const ARM_CALL_CONVENTIONS: &[LLVMCallConvention] = &[
            LLVMCallConvention::ARM_AAPCS,
            LLVMCallConvention::ARM_AAPCS_VFP,
            LLVMCallConvention::ARM_APCS,
            LLVMCallConvention::ARM64EC_Thunk_Native,
            LLVMCallConvention::ARM64EC_Thunk_X64,
        ];

        const RISCV_CALL_CONVENTIONS: &[LLVMCallConvention] = &[
            LLVMCallConvention::RISCV_VLSCall_1024,
            LLVMCallConvention::RISCV_VLSCall_128,
            LLVMCallConvention::RISCV_VLSCall_16384,
            LLVMCallConvention::RISCV_VLSCall_2048,
            LLVMCallConvention::RISCV_VLSCall_256,
            LLVMCallConvention::RISCV_VLSCall_32,
            LLVMCallConvention::RISCV_VLSCall_32768,
            LLVMCallConvention::RISCV_VLSCall_4096,
            LLVMCallConvention::RISCV_VLSCall_512,
            LLVMCallConvention::RISCV_VLSCall_64,
            LLVMCallConvention::RISCV_VLSCall_65536,
            LLVMCallConvention::RISCV_VLSCall_8192,
            LLVMCallConvention::RISCV_VectorCall,
        ];

        const AARCH64_CALL_CONVENTIONS: &[LLVMCallConvention] = &[
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0,
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1,
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2,
            LLVMCallConvention::AArch64_SVE_VectorCall,
            LLVMCallConvention::AArch64_VectorCall,
        ];

        const AMDGPU_CALL_CONVENTIONS: &[LLVMCallConvention] = &[
            LLVMCallConvention::AMDGPU_CS,
            LLVMCallConvention::AMDGPU_CS_Chain,
            LLVMCallConvention::AMDGPU_CS_ChainPreserve,
            LLVMCallConvention::AMDGPU_ES,
            LLVMCallConvention::AMDGPU_GS,
            LLVMCallConvention::AMDGPU_Gfx,
            LLVMCallConvention::AMDGPU_Gfx_WholeWave,
            LLVMCallConvention::AMDGPU_HS,
            LLVMCallConvention::AMDGPU_KERNEL,
            LLVMCallConvention::AMDGPU_LS,
            LLVMCallConvention::AMDGPU_PS,
            LLVMCallConvention::AMDGPU_VS,
        ];

        const WASM_CALL_CONVENTIONS: &[LLVMCallConvention] =
            &[LLVMCallConvention::WASM_EmscriptenInvoke];

        let formatted_target_triple: String = format!(
            "{}-{}-{}-{}",
            target_triple.0, target_triple.1, target_triple.2, target_triple.3
        );
        let lower_arch: String = target_triple.0.to_lowercase();
        let arch: &str = lower_arch.trim();

        match applicant {
            LLVMCallConventionAplicant::Function | LLVMCallConventionAplicant::Instrinsic
                if call_conv.is_specific_target_conv() =>
            {
                match arch {
                    arch if arch.contains("x86") => {
                        if !X86_64_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = X86_64_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any: '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }
                    arch if arch.contains("arm") => {
                        if !ARM_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = ARM_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any: '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }
                    arch if arch.contains("riscv") => {
                        if !RISCV_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = RISCV_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }
                    arch if arch.contains("aarch64") => {
                        if !AARCH64_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = AARCH64_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }
                    arch if arch.starts_with("amd") => {
                        if !AMDGPU_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = AMDGPU_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }

                    arch if arch.contains("wasm") => {
                        if !WASM_CALL_CONVENTIONS.contains(&call_conv) {
                            let transformed: Vec<String> = WASM_CALL_CONVENTIONS
                                .iter()
                                .map(|callconv| callconv.to_string())
                                .collect();

                            let displayed: String = transformed.join(", ");

                            self.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0024,
                                format!(
                                    "Unsupported calling convention '{}' for target '{}'",
                                    arch, formatted_target_triple
                                ),
                                format!("You can use any '{}'.", displayed),
                                None,
                                span,
                            ));
                        }
                    }

                    _ => (),
                }
            }

            _ => (),
        }
    }
}

impl LLVMCallConventionsChecker<'_> {
    #[inline]
    fn get_compiler_options(&self) -> &CompilerOptions {
        self.options
    }
}

impl LLVMCallConventionsChecker<'_> {
    #[inline]
    fn add_error_report(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }
}

impl LLVMCallConventionsChecker<'_> {
    #[inline]
    fn verify(&mut self) -> bool {
        if !self.errors.is_empty() {
            self.errors.iter().for_each(|error| {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            });

            true
        } else {
            false
        }
    }
}
