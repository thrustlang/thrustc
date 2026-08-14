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

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::InstructionValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_ast::ast_metadata::LLVMConstantMetadata;
use thrustc_ast::ast_metadata::LLVMDereferenceMetadata;
use thrustc_ast::ast_metadata::LLVMLocalMetadata;
use thrustc_ast::ast_metadata::LLVMStaticMetadata;

use thrustc_code_location::Span;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::ConstantTypeExtensions;
use thrustc_typesystem::traits::TypeExtensions;

use crate::abort;
use crate::atomic_operations;
use crate::atomic_operations::LLVMAtomicModificators;
use crate::context::LLVMCodeGenContext;
use crate::typegeneration;

#[derive(Debug, Clone, Copy)]
pub enum SymbolAllocated<'ctx> {
    Local {
        ptr: PointerValue<'ctx>,
        kind: &'ctx Type,
        metadata: LLVMLocalMetadata,
        attributes: SymbolAttributes,
        span: Span,
    },
    Static {
        ptr: PointerValue<'ctx>,
        value: Option<BasicValueEnum<'ctx>>,
        kind: &'ctx Type,
        metadata: LLVMStaticMetadata,
        span: Span,
    },
    Constant {
        ptr: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        kind: &'ctx Type,
        metadata: LLVMConstantMetadata,
        span: Span,
    },
    LowLevelInstruction {
        value: BasicValueEnum<'ctx>,
        kind: &'ctx Type,
        span: Span,
    },
    AllocatedParameter {
        ptr: PointerValue<'ctx>,
        kind: &'ctx Type,
        span: Span,
    },
    Parameter {
        value: BasicValueEnum<'ctx>,
        kind: &'ctx Type,
        span: Span,
    },
    Function {
        ptr: PointerValue<'ctx>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolToAllocate {
    AllocatedParameter,
    Parameter,
    LowLevelInstruction,
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMAllocationSite {
    Heap,
    Stack,
    Static,
}

impl<'ctx> SymbolAllocated<'ctx> {
    #[inline]
    pub fn new(
        allocate: SymbolToAllocate,
        kind: &'ctx Type,
        value: BasicValueEnum<'ctx>,
        span: Span,
    ) -> Self {
        match allocate {
            SymbolToAllocate::AllocatedParameter => Self::AllocatedParameter {
                ptr: value.into_pointer_value(),
                kind,
                span,
            },
            SymbolToAllocate::Parameter => Self::Parameter { value, kind, span },
            SymbolToAllocate::LowLevelInstruction => {
                Self::LowLevelInstruction { value, kind, span }
            }
        }
    }

    #[inline]
    pub fn new_function(ptr: PointerValue<'ctx>, span: Span) -> Self {
        Self::Function { ptr, span }
    }

    #[inline]
    pub fn new_local(
        ptr: PointerValue<'ctx>,
        kind: &'ctx Type,
        metadata: LLVMLocalMetadata,
        attributes: SymbolAttributes,
        span: Span,
    ) -> Self {
        Self::Local {
            ptr,
            kind,
            metadata,
            attributes,
            span,
        }
    }

    #[inline]
    pub fn new_constant(
        ptr: BasicValueEnum<'ctx>,
        kind: &'ctx Type,
        value: BasicValueEnum<'ctx>,
        metadata: LLVMConstantMetadata,
        span: Span,
    ) -> Self {
        Self::Constant {
            ptr: ptr.into_pointer_value(),
            value,
            kind,
            metadata,
            span,
        }
    }

    #[inline]
    pub fn new_static(
        ptr: BasicValueEnum<'ctx>,
        kind: &'ctx Type,
        value: Option<BasicValueEnum<'ctx>>,
        metadata: LLVMStaticMetadata,
        span: Span,
    ) -> Self {
        Self::Static {
            ptr: ptr.into_pointer_value(),
            value,
            kind,
            metadata,
            span,
        }
    }
}

impl<'ctx> SymbolAllocated<'ctx> {
    pub fn load(&self, context: &mut LLVMCodeGenContext<'_, 'ctx>) -> BasicValueEnum<'ctx> {
        let llvm_builder: &Builder = context.get_llvm_builder();

        let inner_type: &Type = self.get_symbol_type(context);
        let inner_type: Type = inner_type.remove_all_constant_type();

        let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, &inner_type);

        let span: Span = self.get_symbol_span();

        let alignment: u32 = context
            .get_target_data()
            .get_preferred_alignment(&llvm_type);

        context.mark_dbg_location(span);

        if let Self::Local {
            ptr,
            metadata,
            attributes,
            ..
        } = self
        {
            let loaded_value = llvm_builder
                .build_load(llvm_type, *ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to build load instruction",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let instruction: InstructionValue<'_> =
                loaded_value.as_instruction_value().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to transform a loaded value into an instruction value!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                atomic_volatile: metadata.volatile,
                atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
            };

            atomic_operations::set_atomic_behavior_load_instruction(
                context,
                instruction,
                atomic_config,
                span,
            );

            let alignment: u32 = attributes
                .get_explicit_memory_alignment()
                .and_then(|a| a.try_into().ok())
                .unwrap_or(alignment);

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set alignment on load instruction",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return loaded_value;
        }

        if let Self::Constant { ptr, metadata, .. } = self {
            let loaded_value = llvm_builder
                .build_load(llvm_type, *ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to build load instruction",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let instruction: InstructionValue<'_> =
                loaded_value.as_instruction_value().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to transform a loaded value into an instruction value!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                atomic_volatile: metadata.volatile,
                atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
            };

            atomic_operations::set_atomic_behavior_load_instruction(
                context,
                instruction,
                atomic_config,
                span,
            );

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set alignment on load instruction",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return loaded_value;
        }

        if let Self::Static { ptr, metadata, .. } = self {
            let loaded_value = llvm_builder
                .build_load(llvm_type, *ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to build load instruction",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let instruction: InstructionValue<'_> =
                loaded_value.as_instruction_value().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to transform a loaded value into an instruction value!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                atomic_volatile: metadata.volatile,
                atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
            };

            atomic_operations::set_atomic_behavior_load_instruction(
                context,
                instruction,
                atomic_config,
                span,
            );

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return loaded_value;
        }

        if let Self::LowLevelInstruction { value, .. } = self {
            return *value;
        }

        if let Self::AllocatedParameter { ptr, span, .. } = self {
            let loaded_value: BasicValueEnum<'_> = llvm_builder
                .build_load(llvm_type, *ptr, "")
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to build load instruction",
                        *span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let instruction: InstructionValue<'_> =
                loaded_value.as_instruction_value().unwrap_or_else(|| {
                    abort::abort_codegen(
                        context,
                        "Failed to transform a loaded value into an instruction value!",
                        *span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    *span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return loaded_value;
        }

        if let Self::Parameter { value, .. } = self {
            return *value;
        }

        abort::abort_codegen(
            context,
            "Failed to load a value from memory!",
            self.get_symbol_span(),
            PathBuf::from(file!()),
            line!(),
        );
    }

    pub fn store(
        &self,
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        new_value: BasicValueEnum<'ctx>,
    ) {
        let llvm_builder: &Builder = context.get_llvm_builder();
        let target_data: &TargetData = context.get_target_data();

        let span: Span = self.get_symbol_span();
        let alignment: u32 = target_data.get_preferred_alignment(&new_value.get_type());

        context.mark_dbg_location(self.get_symbol_span());

        if let Self::Local { ptr, metadata, .. } = self {
            let instruction = llvm_builder
                .build_store(*ptr, new_value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to store a value in memory!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                atomic_volatile: metadata.volatile,
                atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
            };

            atomic_operations::set_atomic_behavior_store_instruction(
                context,
                instruction,
                atomic_config,
                span,
            );

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return;
        }

        if let Self::Static { ptr, metadata, .. } = self {
            let instruction = llvm_builder
                .build_store(*ptr, new_value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to store a value in memory!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                atomic_volatile: metadata.volatile,
                atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
            };

            atomic_operations::set_atomic_behavior_store_instruction(
                context,
                instruction,
                atomic_config,
                span,
            );

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return;
        }

        if let Self::AllocatedParameter { ptr, .. } = self {
            let store = llvm_builder
                .build_store(*ptr, new_value)
                .unwrap_or_else(|_| {
                    abort::abort_codegen(
                        context,
                        "Failed to store a value in memory!",
                        span,
                        PathBuf::from(file!()),
                        line!(),
                    );
                });

            store.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            return;
        }

        abort::abort_codegen(
            context,
            "Failed to store a value in memory!",
            self.get_symbol_span(),
            PathBuf::from(file!()),
            line!(),
        );
    }
}

impl<'ctx> SymbolAllocated<'ctx> {
    pub fn determinate_atomic_configuration(&self) -> Option<LLVMAtomicModificators> {
        match self {
            Self::Local { metadata, .. } => {
                let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                    atomic_volatile: metadata.volatile,
                    atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
                };

                Some(atomic_config)
            }

            Self::Static { metadata, .. } => {
                let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
                    atomic_volatile: metadata.volatile,
                    atomic_ord: metadata.atomic_ord.map(|ord| ord.to_llvm()),
                };

                Some(atomic_config)
            }

            _ => None,
        }
    }
}

impl<'ctx> SymbolAllocated<'ctx> {
    #[inline]
    pub fn get_symbol_span(&self) -> Span {
        match self {
            Self::Local { span, .. } => *span,
            Self::Constant { span, .. } => *span,
            Self::Static { span, .. } => *span,
            Self::Parameter { span, .. } => *span,
            Self::AllocatedParameter { span, .. } => *span,
            Self::LowLevelInstruction { span, .. } => *span,
            Self::Function { span, .. } => *span,
        }
    }

    #[inline]
    pub fn get_symbol_type(&self, context: &mut LLVMCodeGenContext<'_, '_>) -> &'ctx Type {
        match self {
            Self::Local { kind, .. } => kind,
            Self::Constant { kind, .. } => kind,
            Self::Static { kind, .. } => kind,
            Self::Parameter { kind, .. } => kind,
            Self::AllocatedParameter { kind, .. } => kind,
            Self::LowLevelInstruction { kind, .. } => kind,

            _ => {
                abort::abort_codegen(
                    context,
                    "Failed to get a type from an allocated symbol!",
                    self.get_symbol_span(),
                    PathBuf::from(file!()),
                    line!(),
                );
            }
        }
    }

    #[inline]
    pub fn get_ptr_value(&self) -> PointerValue<'ctx> {
        match self {
            Self::Function { ptr, .. } => *ptr,
            Self::Local { ptr, .. } => *ptr,
            Self::Constant { ptr, .. } => *ptr,
            Self::Static { ptr, .. } => *ptr,
            Self::AllocatedParameter { ptr, .. } => *ptr,
            Self::Parameter { value, .. } => value.into_pointer_value(),
            Self::LowLevelInstruction { value, .. } => value.into_pointer_value(),
        }
    }

    #[inline]
    pub fn get_symbol_value(
        &self,
        context: &mut LLVMCodeGenContext<'_, '_>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            Self::Local { ptr, .. } => (*ptr).into(),
            Self::Function { ptr, .. } => (*ptr).into(),
            Self::AllocatedParameter { ptr, .. } => (*ptr).into(),
            Self::Constant { value, .. } => *value,
            Self::Static { value, .. } => value.unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to get a value from static reference!",
                    self.get_symbol_span(),
                    PathBuf::from(file!()),
                    line!(),
                );
            }),
            Self::Parameter { value, .. } => *value,
            Self::LowLevelInstruction { value, .. } => *value,
        }
    }
}

pub fn store<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr: PointerValue<'ctx>,
    new_value: BasicValueEnum<'ctx>,
    atomic_config: Option<LLVMAtomicModificators>,
    span: Span,
) {
    let llvm_builder: &Builder = context.get_llvm_builder();
    let target_data: &TargetData = context.get_target_data();

    let alignment: u32 = target_data.get_preferred_alignment(&new_value.get_type());

    let store: InstructionValue<'_> =
        llvm_builder
            .build_store(ptr, new_value)
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to store a value in memory!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

    if let Some(atomic_config) = atomic_config {
        atomic_operations::set_atomic_behavior_store_instruction(
            context,
            store,
            atomic_config,
            span,
        );
    }

    store.set_alignment(alignment).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to set type alignment!",
            span,
            PathBuf::from(file!()),
            line!(),
        );
    });
}

pub fn load<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr: PointerValue<'ctx>,
    ptr_type: &Type,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, ptr_type);

    let alignment: u32 = context
        .get_target_data()
        .get_preferred_alignment(&llvm_type);

    let loaded_value: BasicValueEnum<'_> = llvm_builder
        .build_load(llvm_type, ptr, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to load a value from memory!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    context.mark_dbg_location(span);

    let instruction: InstructionValue<'_> =
        loaded_value.as_instruction_value().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to get instruction value!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    instruction.set_alignment(alignment).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to set type alignment!",
            span,
            PathBuf::from(file!()),
            line!(),
        );
    });

    loaded_value
}

pub fn load_pointer<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr: PointerValue<'ctx>,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();
    let llvm_context: &Context = context.get_llvm_context();

    let llvm_type: BasicTypeEnum = llvm_context.ptr_type(AddressSpace::default()).into();

    let alignment: u32 = context
        .get_target_data()
        .get_preferred_alignment(&llvm_type);

    let loaded_value: BasicValueEnum<'_> = llvm_builder
        .build_load(llvm_type, ptr, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to load a value from memory!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    context.mark_dbg_location(span);

    let instruction: InstructionValue<'_> =
        loaded_value.as_instruction_value().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to get instruction value!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    instruction.set_alignment(alignment).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to set type alignment!",
            span,
            PathBuf::from(file!()),
            line!(),
        );
    });

    loaded_value
}

pub fn dereference<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr: PointerValue<'ctx>,
    ptr_type: &Type,
    metadata: LLVMDereferenceMetadata,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();

    let llvm_type: BasicTypeEnum = typegeneration::generate_dereference_type(context, ptr_type);

    let alignment: u32 = context
        .get_target_data()
        .get_preferred_alignment(&llvm_type);

    let loaded_value: BasicValueEnum<'_> = llvm_builder
        .build_load(llvm_type, ptr, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to deference a pointer!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    context.mark_dbg_location(span);

    let instruction: InstructionValue<'_> =
        loaded_value.as_instruction_value().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to get instruction value!",
                span,
                PathBuf::from(file!()),
                line!(),
            )
        });

    let atomic_config: LLVMAtomicModificators = LLVMAtomicModificators {
        atomic_volatile: metadata.volatile,
        atomic_ord: metadata.atomic_ord.map(|atomic_ord| atomic_ord.to_llvm()),
    };

    atomic_operations::set_atomic_behavior_load_instruction(
        context,
        instruction,
        atomic_config,
        span,
    );

    instruction.set_alignment(alignment).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to set type alignment!",
            span,
            PathBuf::from(file!()),
            line!(),
        );
    });

    loaded_value
}

pub fn allocate_in<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    site: LLVMAllocationSite,
    kind: &Type,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();
    let llvm_builder: &Builder = context.get_llvm_builder();

    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, kind);

    let alignment: u32 = context
        .get_target_data()
        .get_preferred_alignment(&llvm_type);

    match site {
        LLVMAllocationSite::Stack => {
            let ptr: PointerValue<'_> =
                llvm_builder
                    .build_alloca(llvm_type, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to allocate in the stack!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    });

            context.mark_dbg_location(span);

            let instruction: InstructionValue<'_> = ptr.as_instruction().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to get instruction value!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                )
            });

            instruction.set_alignment(alignment).unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to set type alignment!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

            ptr
        }
        LLVMAllocationSite::Heap => {
            let ptr: PointerValue<'_> =
                llvm_builder
                    .build_malloc(llvm_type, "")
                    .unwrap_or_else(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to allocate in the heap!",
                            span,
                            PathBuf::from(file!()),
                            line!(),
                        )
                    });

            context.mark_dbg_location(span);

            ptr
        }
        LLVMAllocationSite::Static => llvm_module
            .add_global(llvm_type, Some(AddressSpace::default()), "")
            .as_pointer_value(),
    }
}

pub fn gep_struct_anon<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    mut ptr_value: PointerValue<'ctx>,
    ptr_type: &Type,
    index: u32,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    let ptr_type: BasicTypeEnum<'_> =
        typegeneration::generate_pointer_arithmetic_type(context, ptr_type);

    ptr_value = self::address_space_to_normal(context, ptr_value, span);

    let new_ptr_value = llvm_builder
        .build_struct_gep(ptr_type, ptr_value, index, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get the field pointer!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        });

    context.mark_dbg_location(span);

    new_ptr_value
}

pub fn gep_anon<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    mut ptr_value: PointerValue<'ctx>,
    ptr_type: &Type,
    indexes: &[IntValue<'ctx>],
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    let ptr_type: BasicTypeEnum<'_> =
        typegeneration::generate_pointer_arithmetic_type(context, ptr_type);

    ptr_value = self::address_space_to_normal(context, ptr_value, span);

    let new_ptr_value: PointerValue<'_> = unsafe {
        llvm_builder
            .build_in_bounds_gep(ptr_type, ptr_value, indexes, "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to get the field pointer!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            })
    };

    context.mark_dbg_location(span);

    new_ptr_value
}

pub fn auto_deference_a_nested_pointer<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr_value: PointerValue<'ctx>,
    ptr_type: &Type,
    nested_ptr_count: usize,
    span: Span,
) -> BasicValueEnum<'ctx> {
    let llvm_builder: &Builder = context.get_llvm_builder();
    let llvm_type: BasicTypeEnum = typegeneration::generate_type(context, ptr_type);

    let alignment: u32 = context
        .get_target_data()
        .get_preferred_alignment(&llvm_type);

    let first_load: BasicValueEnum = llvm_builder
        .build_load(llvm_type, ptr_value, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to load a value from memory!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        });

    let instruction: InstructionValue<'_> =
        first_load.as_instruction_value().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to transform a loaded value into an instruction value!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        });

    instruction.set_alignment(alignment).unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to set alignment on load instruction!",
            span,
            PathBuf::from(file!()),
            line!(),
        );
    });

    let mut last_load: BasicValueEnum = first_load;

    for _ in (0..nested_ptr_count).skip(1) {
        let load: BasicValueEnum = llvm_builder
            .build_load(llvm_type, last_load.into_pointer_value(), "")
            .unwrap_or_else(|_| {
                abort::abort_codegen(
                    context,
                    "Failed to load a value from memory!",
                    span,
                    PathBuf::from(file!()),
                    line!(),
                );
            });

        let instruction: InstructionValue<'_> = load.as_instruction_value().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to transform a loaded value into an instruction value!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        });

        instruction.set_alignment(alignment).unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to set alignment on load instruction!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        });

        last_load = load;
    }

    context.mark_dbg_location(span);

    last_load
}

#[inline]
pub fn get_llvm_address_space(ty: &Type) -> Option<AddressSpace> {
    if let Some(address_space) = ty.get_address_space() {
        return Some(AddressSpace::from(address_space));
    }

    None
}

#[inline]
pub fn constant_address_space_to_normal<'ctx>(
    llvm_context: &'ctx Context,
    ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    ptr.const_address_space_cast(llvm_context.ptr_type(AddressSpace::default()))
}

#[inline]
pub fn address_space_to_normal<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    ptr: PointerValue<'ctx>,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();
    let llvm_context: &Context = context.get_llvm_context();

    context.mark_dbg_location(span);

    llvm_builder
        .build_address_space_cast(ptr, llvm_context.ptr_type(AddressSpace::default()), "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile a address space cast!",
                span,
                PathBuf::from(file!()),
                line!(),
            );
        })
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolAttributes {
    align: Option<u64>,
}

impl SymbolAttributes {
    #[inline]
    pub fn new() -> Self {
        Self { align: None }
    }
}

impl SymbolAttributes {
    #[inline]
    pub fn set_explicit_memory_alignment(&mut self, value: u64) {
        self.align = Some(value)
    }
}

impl SymbolAttributes {
    #[inline]
    pub fn get_explicit_memory_alignment(&self) -> Option<u64> {
        self.align
    }
}

impl SymbolAttributes {
    #[inline]
    pub fn has_explicit_memory_alignment(&self) -> bool {
        self.align.is_some()
    }
}

pub fn into_symbol_attributes(llvm_attributes: &LLVMAttributes) -> SymbolAttributes {
    let mut attributes: SymbolAttributes = SymbolAttributes::new();

    {
        for attribute in llvm_attributes.iter() {
            if let LLVMAttribute::Align(value, ..) = *attribute {
                attributes.set_explicit_memory_alignment(value);
            }
        }
    }

    attributes
}
