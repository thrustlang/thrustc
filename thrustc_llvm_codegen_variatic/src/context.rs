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
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;

use crate::va_list_kind::VaListKind;
use crate::va_list_kind::build_va_list_llvm_type;
use crate::va_list_kind::resolve_va_list_kind;

#[derive(Debug)]
pub struct LLVMVariaticContext<'a, 'ctx> {
    llvm_module: &'a Module<'ctx>,
    llvm_context: &'ctx Context,
    llvm_builder: &'ctx Builder<'ctx>,
    diagnostician: Diagnostician,
    va_list_kind: VaListKind,
    current_va_list: Option<PointerValue<'ctx>>,
    current_va_arg_count: Option<IntValue<'ctx>>,
}

impl<'a, 'ctx> LLVMVariaticContext<'a, 'ctx> {
    pub fn new(
        llvm_module: &'a Module<'ctx>,
        llvm_context: &'ctx Context,
        llvm_builder: &'ctx Builder<'ctx>,
        target_triple: &LLVMTargetTriple,
        file: &'ctx CompilationUnit,
        options: &'ctx CompilerOptions,
    ) -> Self {
        let va_list_kind: VaListKind = resolve_va_list_kind(target_triple);

        Self {
            llvm_module,
            llvm_context,
            llvm_builder,
            diagnostician: Diagnostician::new(file, options),
            va_list_kind,
            current_va_list: None,
            current_va_arg_count: None,
        }
    }
}

impl<'a, 'ctx> LLVMVariaticContext<'a, 'ctx> {
    pub fn emit_va_start(&mut self, span: Span) -> PointerValue<'ctx> {
        let va_list_type: BasicTypeEnum<'ctx> =
            self::build_va_list_llvm_type(self.llvm_context, self.va_list_kind);

        let va_list: PointerValue<'ctx> = self
            .llvm_builder
            .build_alloca(va_list_type, "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to allocate the variable arguments list!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let va_start_intrinsic: FunctionValue<'ctx> = self
            .llvm_module
            .get_function("llvm.va_start")
            .unwrap_or_else(|| {
                self.llvm_module.add_function(
                    "llvm.va_start",
                    self.llvm_context.void_type().fn_type(
                        &[self.llvm_context.ptr_type(AddressSpace::default()).into()],
                        false,
                    ),
                    None,
                )
            });

        self.llvm_builder
            .build_call(va_start_intrinsic, &[va_list.into()], "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to compile the 'llvm.va_start' intrinsic call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        self.current_va_list = Some(va_list);

        va_list
    }

    pub fn emit_va_end(&mut self, span: Span) {
        let va_list: PointerValue<'ctx> = self.get_current_va_list(span);

        let va_end_intrinsic: FunctionValue<'ctx> = self
            .llvm_module
            .get_function("llvm.va_end")
            .unwrap_or_else(|| {
                self.llvm_module.add_function(
                    "llvm.va_end",
                    self.llvm_context.void_type().fn_type(
                        &[self.llvm_context.ptr_type(AddressSpace::default()).into()],
                        false,
                    ),
                    None,
                )
            });

        self.llvm_builder
            .build_call(va_end_intrinsic, &[va_list.into()], "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to compile the 'llvm.va_end' intrinsic call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }

    pub fn emit_va_arg(&mut self, ty: BasicTypeEnum<'ctx>, span: Span) -> BasicValueEnum<'ctx> {
        let va_list: PointerValue<'ctx> = self.get_current_va_list(span);

        self.emit_va_arg_from(va_list, ty, span)
    }

    pub fn emit_va_arg_from(
        &mut self,
        list: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        self.llvm_builder
            .build_va_arg(list, ty, "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to compile the 'va_arg' instruction!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            })
    }

    pub fn emit_va_copy(&mut self, source: PointerValue<'ctx>, span: Span) -> PointerValue<'ctx> {
        let va_list_type: BasicTypeEnum<'ctx> =
            self::build_va_list_llvm_type(self.llvm_context, self.va_list_kind);

        let copy: PointerValue<'ctx> = self
            .llvm_builder
            .build_alloca(va_list_type, "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to allocate a variable arguments copy!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        let va_copy_intrinsic: FunctionValue<'ctx> = self
            .llvm_module
            .get_function("llvm.va_copy")
            .unwrap_or_else(|| {
                self.llvm_module.add_function(
                    "llvm.va_copy",
                    self.llvm_context.void_type().fn_type(
                        &[
                            self.llvm_context.ptr_type(AddressSpace::default()).into(),
                            self.llvm_context.ptr_type(AddressSpace::default()).into(),
                        ],
                        false,
                    ),
                    None,
                )
            });

        self.llvm_builder
            .build_call(va_copy_intrinsic, &[copy.into(), source.into()], "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to compile the 'llvm.va_copy' intrinsic call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

        copy
    }

    pub fn emit_va_end_on(&mut self, list: PointerValue<'ctx>, span: Span) {
        let va_end_intrinsic: FunctionValue<'ctx> = self
            .llvm_module
            .get_function("llvm.va_end")
            .unwrap_or_else(|| {
                self.llvm_module.add_function(
                    "llvm.va_end",
                    self.llvm_context.void_type().fn_type(
                        &[self.llvm_context.ptr_type(AddressSpace::default()).into()],
                        false,
                    ),
                    None,
                )
            });

        self.llvm_builder
            .build_call(va_end_intrinsic, &[list.into()], "")
            .unwrap_or_else(|_| {
                crate::abort::abort_variatic_codegen(
                    self,
                    "Failed to compile the 'llvm.va_end' intrinsic call!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
    }
}

impl<'a, 'ctx> LLVMVariaticContext<'a, 'ctx> {
    #[inline]
    pub fn get_mut_diagnostician(&mut self) -> &mut Diagnostician {
        &mut self.diagnostician
    }

    #[inline]
    pub fn get_va_list_kind(&self) -> VaListKind {
        self.va_list_kind
    }

    #[inline]
    pub fn has_current_va_list(&self) -> bool {
        self.current_va_list.is_some()
    }

    #[inline]
    pub fn unset_current_va_list(&mut self) {
        self.current_va_list = None;
    }

    #[inline]
    pub fn set_current_va_arg_count(&mut self, count: IntValue<'ctx>) {
        self.current_va_arg_count = Some(count);
    }

    pub fn get_current_va_arg_count(&mut self, span: Span) -> IntValue<'ctx> {
        self.current_va_arg_count.unwrap_or_else(|| {
            crate::abort::abort_variatic_codegen(
                self,
                "Failed to get the current variable arguments count! The 'arbitraryArgsCount' builtin is only available inside a variadic function with a body.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }
}

impl<'a, 'ctx> LLVMVariaticContext<'a, 'ctx> {
    pub fn get_current_va_list(&mut self, span: Span) -> PointerValue<'ctx> {
        self.current_va_list.unwrap_or_else(|| {
            crate::abort::abort_variatic_codegen(
                self,
                "Failed to get the current variable arguments list! The 'arbitraryArg' and 'arbitraryArgs' builtins are only available inside a variadic function with a body.",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
    }
}
