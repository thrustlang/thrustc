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
use inkwell::IntPredicate;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::values::BasicValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_ast::Ast;
use thrustc_ast::traits::AstCodeBlockEntensions;
use thrustc_entities::Function;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributeComparator;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;
use thrustc_llvm_callconventions::LLVMCallConvention;
use thrustc_span::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::abort;
use crate::attributebuilder::AttributeBuilder;
use crate::attributebuilder::LLVMAttributeApplicant;
use crate::block;
use crate::codegen::LLVMCodegen;
use crate::context::LLVMCodeGenContext;
use crate::traits::LLVMFunctionExtensions;
use crate::typegeneration;
use crate::types::LLVMDBGFunction;
use crate::types::LLVMFunction;
use crate::utils;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

pub fn compile_top<'ctx>(context: &mut LLVMCodeGenContext<'_, 'ctx>, function: Function<'ctx>) {
    let llvm_module: &Module = context.get_llvm_module();

    let name: &str = function.0;
    let ascii_name: &str = function.1;

    let return_type: &Type = function.2;

    let parameters: &[Ast<'ctx>] = function.3;
    let parameters_types: &[Type] = function.4;
    let attributes: LLVMAttributes = thrustc_llvm_attributes::into_llvm_attributes(function.6);
    let span: Span = function.7;

    let ignore_args: bool = attributes.has_ignore_attribute();
    let is_public: bool = attributes.has_public_attribute();

    let call_convention: u32 = if let Some(LLVMAttribute::Convention(convention, ..)) =
        attributes.get_attr(LLVMAttributeComparator::Convention)
    {
        convention as u32
    } else {
        LLVMCallConvention::Standard as u32
    };

    let canonical_name: String = if let Some(LLVMAttribute::Extern(extern_name, ..)) =
        attributes.get_attr(LLVMAttributeComparator::Extern)
    {
        extern_name.to_string()
    } else if is_public {
        ascii_name.to_string()
    } else {
        format!(
            "__fn_{}_{}",
            utils::generate_string(context, utils::LONG_RANGE_OBFUSCATION),
            ascii_name
        )
    };

    let generated_function_type: (
        FunctionType<'_>,
        Option<thrustc_llvm_abi::LLVMABIConfiguration>,
    ) = typegeneration::compile_as_function_type(context, return_type, parameters, ignore_args);

    let function_type: FunctionType<'_> = generated_function_type.0;
    let function_abi_config: Option<thrustc_llvm_abi::LLVMABIConfiguration> =
        generated_function_type.1;

    let llvm_function: FunctionValue =
        llvm_module.add_function(&canonical_name, function_type, None);

    AttributeBuilder::new(attributes, LLVMAttributeApplicant::Function(llvm_function))
        .add_function_attributes(context);

    let prototype: LLVMFunction = (
        llvm_function,
        return_type,
        parameters_types,
        call_convention,
        function_abi_config,
        span,
    );

    context.set_current_function(prototype.clone());
    context.new_function(name, prototype);
}

pub fn compile_down<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, function: Function<'ctx>) {
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let function_name: &str = function.0;
    let function_type: &Type = function.2;
    let function_parameters: &[Ast<'ctx>] = function.3;
    let function_body: Option<&Ast> = function.5;

    let proto: LLVMFunction = codegen
        .get_context()
        .get_table()
        .get_function(function_name);

    let value: FunctionValue<'_> = proto.get_value();
    let return_type: &Type = proto.get_return_type();
    let parameters_types: Vec<Type> = proto.get_parameters_types().to_vec();
    let span: Span = proto.get_span();

    let llvm_function: FunctionValue = proto.0;
    let llvm_function_block: BasicBlock = block::append_block(codegen.get_context(), llvm_function);

    llvm_builder.position_at_end(llvm_function_block);

    codegen.get_mut_context().set_current_function(proto);

    if codegen
        .get_context()
        .get_compiler_options()
        .get_llvm_backend()
        .needs_stack_protector()
    {
        let stack_protector_ptr_value: PointerValue<'_> =
            self::emit_stack_protector_prologue(codegen.get_mut_context(), span);

        codegen
            .get_mut_context()
            .set_function_stackguard_protector_pointer(stack_protector_ptr_value);
    }

    {
        for parameter in function_parameters
            .iter()
            .map(|node| thrustc_entities::function_parameter_from_ast(node))
        {
            let name: &str = parameter.0;
            let ascii_name: &str = parameter.1;

            let kind: &Type = parameter.2;
            let position: u32 = parameter.3;

            let span: Span = parameter.4;

            if let Some(value) = llvm_function.get_nth_param(position) {
                codegen
                    .get_mut_context()
                    .new_parameter(name, ascii_name, kind, value, span);
            }
        }
    }

    if let Some(function_body) = function_body {
        {
            let dbg_prototype: LLVMDBGFunction = (
                function_name.to_owned(),
                value,
                return_type,
                parameters_types,
                true,
                true,
                span,
            );

            codegen
                .get_mut_context()
                .start_function_debug_data(&dbg_prototype);
        }

        {
            codegen.codegen_block(function_body);

            codegen.get_mut_context().finish_function_debug_data();

            if function_type.is_void_type() && !function_body.has_terminator() {
                let _ = llvm_builder.build_return(None);
            }
        }
    }

    codegen.get_mut_context().unset_current_function();
    codegen
        .get_mut_context()
        .unset_function_stackguard_protector_pointer();
}

pub fn emit_stack_protector_prologue<'ctx>(
    context: &mut LLVMCodeGenContext<'_, 'ctx>,
    span: Span,
) -> PointerValue<'ctx> {
    let llvm_module: &Module<'_> = context.get_llvm_module();
    let llvm_context: &Context = context.get_llvm_context();
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    let stackguard_intrinsic: FunctionValue<'_> = llvm_module.add_function(
        "llvm.stackguard",
        llvm_context
            .ptr_type(AddressSpace::default())
            .fn_type(&[], false),
        None,
    );

    let stackprotector_intrinsic: FunctionValue<'_> = llvm_module.add_function(
        "llvm.stackprotector",
        llvm_context.void_type().fn_type(
            &[
                llvm_context.ptr_type(AddressSpace::default()).into(),
                llvm_context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        ),
        None,
    );

    llvm_module.add_function(
        "__stack_chk_fail",
        llvm_context.void_type().fn_type(&[], false),
        None,
    );

    let stackguardslot_ptr: PointerValue<'_> = llvm_builder
        .build_alloca(llvm_context.ptr_type(AddressSpace::default()), "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile stackguardslot pointer!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    if let Some(instr) = stackguardslot_ptr.as_instruction_value() {
        let target_data: &TargetData = context.get_target_data();

        let _ = instr.set_alignment(
            target_data.get_preferred_alignment(&llvm_context.ptr_type(AddressSpace::default())),
        );
    }

    let stackguard: PointerValue<'_> = llvm_builder
        .build_call(stackguard_intrinsic, &[], "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get stackguard pointer!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .try_as_basic_value()
        .left()
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to get stackguard pointer!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .into_pointer_value();

    llvm_builder
        .build_call(
            stackprotector_intrinsic,
            &[stackguard.into(), stackguardslot_ptr.into()],
            "",
        )
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile the stackprotector call!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    stackguardslot_ptr
}

pub fn emit_stack_protector_epilogue<'ctx>(context: &mut LLVMCodeGenContext<'_, 'ctx>, span: Span) {
    let llvm_module: &Module<'_> = context.get_llvm_module();
    let llvm_context: &Context = context.get_llvm_context();
    let llvm_builder: &Builder<'_> = context.get_llvm_builder();

    let current_function: FunctionValue<'_> = context.get_current_function(span).get_value();

    let Some(stack_protector_pointer) = context.get_function_stack_protector_pointer() else {
        abort::abort_codegen(
            context,
            "Failed to get the stored stack guard!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    };

    let stored_guard: PointerValue<'_> = llvm_builder
        .build_load(
            llvm_context.ptr_type(AddressSpace::default()),
            *stack_protector_pointer,
            "",
        )
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get the last stack guard!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .into_pointer_value();

    if let Some(instr) = stored_guard.as_instruction_value() {
        let target_data: &TargetData = context.get_target_data();

        let _ = instr.set_alignment(
            target_data.get_preferred_alignment(&llvm_context.ptr_type(AddressSpace::default())),
        );
    }

    let current_guard: PointerValue<'_> = llvm_builder
        .build_call(
            llvm_module
                .get_function("llvm.stackguard")
                .unwrap_or_else(|| {
                    llvm_module.add_function(
                        "llvm.stackguard",
                        llvm_context
                            .ptr_type(AddressSpace::default())
                            .fn_type(&[], false),
                        None,
                    )
                }),
            &[],
            "",
        )
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to get the current stack guard!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .try_as_basic_value()
        .left()
        .unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to get the current stack guard!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        })
        .into_pointer_value();

    let failbranch: BasicBlock<'_> = block::append_block(context, current_function);
    let sucessbranch: BasicBlock<'_> = block::append_block(context, current_function);

    let comparison: IntValue<'_> = llvm_builder
        .build_int_compare(IntPredicate::EQ, stored_guard, current_guard, "")
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile a comparison between stored stack guard and current stack guard!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder
        .build_conditional_branch(comparison, sucessbranch, failbranch)
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to compile conditional comparison!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.position_at_end(failbranch);

    llvm_builder
        .build_call(
            llvm_module
                .get_function("__stack_chk_fail")
                .unwrap_or_else(|| {
                    llvm_module.add_function(
                        "__stack_chk_fail",
                        llvm_context.void_type().fn_type(&[], false),
                        None,
                    )
                }),
            &[],
            "",
        )
        .unwrap_or_else(|_| {
            abort::abort_codegen(
                context,
                "Failed to call '__stack_chk_fail'!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

    llvm_builder.build_unreachable().unwrap_or_else(|_| {
        abort::abort_codegen(
            context,
            "Failed to compile unreacheable instruction!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    llvm_builder.position_at_end(sucessbranch);
}
