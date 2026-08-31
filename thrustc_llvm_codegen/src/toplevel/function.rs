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
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use thrustc_ast::Ast;
use thrustc_ast::traits::AstCodeBlockEntensions;
use thrustc_entities::Function;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributeComparator;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;
use thrustc_llvm_call_conventions::LLVMCallConvention;
use thrustc_llvm_system_v_abi::SystemVABIFunctionParameterConfiguration;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::abort;
use crate::attribute_builder::AttributeBuilder;
use crate::attribute_builder::LLVMAttributeApplicant;
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
    let llvm_context: &Context = context.get_llvm_context();

    let has_abi: bool = context.has_abi();

    let name: &str = function.0;
    let ascii_name: &str = &function.1.replace("\0", "");

    let return_type: &Type = function.2;

    let parameters: &[Ast<'ctx>] = function.3;
    let parameters_types: &[Type] = function.4;
    let attributes: LLVMAttributes = thrustc_llvm_attributes::into_llvm_attributes(function.6);
    let span: Span = function.7;

    let ignore_args: bool = attributes.has_ignore_attribute();
    let is_public: bool = attributes.has_public_attribute();

    let call_convention: u32 = if let Some(LLVMAttribute::Convention(call_convention, ..)) =
        attributes.get_attr(LLVMAttributeComparator::Convention)
    {
        call_convention as u32
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
    ) = typegeneration::compile_as_function_type(context, return_type, parameters, ignore_args, CompilerFunctionVariant::PureFunction);

    let function_type: FunctionType<'_> = generated_function_type.0;
    let function_abi_configuration: Option<thrustc_llvm_abi::LLVMABIConfiguration> =
        generated_function_type.1;

    let llvm_function: FunctionValue =
        llvm_module.add_function(&canonical_name, function_type, None);

    let has_abi_configuration: bool = function_abi_configuration.is_some();

    if has_abi {
        let abi: &thrustc_llvm_abi_representation::LLVMABIRepresentation<'_> = context.get_abi().unwrap_or_else(|| {
            abort::abort_codegen(
                context,
                "Failed to compile as a function, expected an ABI!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        });

        if has_abi_configuration {
            let configuration: &thrustc_llvm_abi::LLVMABIConfiguration<'_> = function_abi_configuration.as_ref().unwrap_or_else(|| {
                abort::abort_codegen(
                    context,
                    "Failed to compile the function call, expected an ABI type configuration!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });

            let codegen_location: thrustc_llvm_abi::LLVMABICodeGenLocation = context.get_codegen_location().to_abi_representation();

            let lowered_parameters_conventions: bool = thrustc_llvm_abi::lower_parameter_conventions(llvm_context, abi, llvm_function, configuration, codegen_location);
    
            if !lowered_parameters_conventions {
                abort::abort_codegen(
                    context,
                    "Failed to lower the function parameters to a complaint ABI status!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            }

            let lowered_return_type_conventions: bool = thrustc_llvm_abi::lower_terminator_conventions(llvm_context, abi, configuration, llvm_function,  codegen_location);

            if !lowered_return_type_conventions {
                abort::abort_codegen(
                    context,
                    "Failed to lower the function return type to a complaint ABI status!",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            }

        }

    }

    let applicant: LLVMAttributeApplicant<'_>= LLVMAttributeApplicant::Function {
        value: llvm_function,
        span
    };

    AttributeBuilder::add_function_attributes(context, &attributes, applicant);

    let prototype: LLVMFunction = (
        llvm_function,
        return_type,
        parameters_types,
        call_convention,
        function_abi_configuration,
        attributes,
        ignore_args,
        span,
    );

    context.set_current_function(prototype.clone());
    context.add_function(name, prototype);
}

pub fn compile_body<'ctx>(codegen: &mut LLVMCodegen<'_, 'ctx>, function: Function<'ctx>) {
    let llvm_context: &Context = codegen.get_context().get_llvm_context();
    let llvm_builder: &Builder = codegen.get_context().get_llvm_builder();

    let has_abi: bool = codegen.get_context().has_abi();

    let function_name: &str = function.0;
    let function_type: &Type = function.2;
    let function_parameters: &[Ast<'ctx>] = function.3;
    let function_body: Option<&Ast> = function.5;

    let prototype: LLVMFunction<'ctx> = codegen
        .get_context()
        .get_table()
        .get_function(function_name);

    let function_value: FunctionValue = prototype.get_value();
    let return_type: &Type = prototype.get_return_type();
    let parameters_types: Vec<Type> = prototype.get_parameters_types().to_vec();

    let is_variadic: bool = prototype.is_variadic();

    let abi_configuration: Option<thrustc_llvm_abi::LLVMABIConfiguration> =
        prototype.get_abi_configuration().cloned();

    let span: Span = prototype.get_span();
    
    codegen.get_mut_context().set_current_function(prototype.clone());

    let llvm_function_block: BasicBlock =
        block::append_block(codegen.get_context(), function_value);

    llvm_builder.position_at_end(llvm_function_block);

    llvm_builder.unset_current_debug_location();

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

    if let Some(function_body) = function_body {
        
        if is_variadic {
            codegen
                .get_mut_context()
                .get_mut_variatic_context()
                .emit_va_start(span);
        }
        

        {
            // the abi doens't lower varitic functions.
            if has_abi && !is_variadic {                
                let abi: &thrustc_llvm_abi_representation::LLVMABIRepresentation<'_> =
                    codegen.get_context().get_abi().unwrap_or_else(|| {
                        abort::abort_codegen(
                            codegen.get_mut_context(),
                            "Failed to get the ABI of the current function!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });


                let configuration: &thrustc_llvm_abi::LLVMABIConfiguration = &abi_configuration
                    .unwrap_or_else(|| {
                        abort::abort_codegen(
                            codegen.get_mut_context(),
                            "Failed to get the ABI configuration of the current function!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                let codegen_location: thrustc_llvm_abi::LLVMABICodeGenLocation  = codegen.get_context().get_codegen_location().to_abi_representation();

                let lowered_abi_parameters: Option<Vec<thrustc_llvm_abi::LLVMABIFunctionLoweredParameter>> =
                    thrustc_llvm_abi::lower_function_parameters(
                        llvm_builder,
                        llvm_context,
                        abi,
                        function_value,
                        configuration,
                        codegen_location
                    );

                if let Some(lowered_parameters)= lowered_abi_parameters {
                    {
                        for lowered_parameter in lowered_parameters {
                            let name: &str = lowered_parameter.get_name();
                            let ascii_name: String = lowered_parameter.get_ascii_name().replace("\0", "");
                            let ty: &Type = lowered_parameter.get_type();
                            let value: BasicValueEnum = lowered_parameter.get_value();
                            let configuration: &thrustc_llvm_abi::LLVMABIConfiguration =
                                lowered_parameter.get_abi_configuration();
    
                            if let thrustc_llvm_abi::LLVMABIConfiguration::SystemVFunctionParameterConfiguration(
                                    configuration,
                                ) = configuration {
                                match configuration {
                                    SystemVABIFunctionParameterConfiguration::Normal => {
                                        codegen.get_mut_context().add_parameter(
                                            name,
                                            ascii_name,
                                            ty,
                                            value,
                                            span,
                                        );
                                    }
    
                                    SystemVABIFunctionParameterConfiguration::FromMemory => {
                                        codegen.get_mut_context().add_allocated_parameter(
                                            name,
                                            ty,
                                            value.into_pointer_value(),
                                            span,
                                        );
                                      
                                    }
                                    
                                }
                            } else {
                                abort::abort_codegen(
                                    codegen.get_mut_context(),
                                    "Unsupported ABI configuration for function parameters!",
                                    span,
                                    std::path::PathBuf::from(file!()),
                                    line!(),
                                )
                            }
    
                        }
                    }
                } else {
                    for parameter in function_parameters.iter().map(|node| thrustc_entities::function_parameter_from_ast(node))
                    {
                        let name: &str = parameter.0;
                        let ascii_name: String = parameter.1.replace("\0", "");

                        let kind: &Type = parameter.2;
                        let position: u32 = parameter.3;

                        let span: Span = parameter.4;

                        if let Some(parameter_value) = function_value.get_nth_param(position) {
                            codegen.get_mut_context().add_parameter(
                                name,
                                ascii_name,
                                kind,
                                parameter_value,
                                span,
                            );
                        }
                    }
                }
            } else {
                for parameter in function_parameters
                    .iter()
                    .map(|node| thrustc_entities::function_parameter_from_ast(node))
                {
                    let name: &str = parameter.0;
                    let ascii_name: String = parameter.1.replace("\0", "");

                    let kind: &Type = parameter.2;
                    let position: u32 = parameter.3;

                    let span: Span = parameter.4;

                    if let Some(parameter_value) = function_value.get_nth_param(position) {
                        codegen.get_mut_context().add_parameter(
                            name,
                            ascii_name,
                            kind,
                            parameter_value,
                            span,
                        );
                    }
                }
              
            }
        }

        {
            let dbg_prototype: LLVMDBGFunction = (
                function_name.to_owned(),
                function_value,
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

            for parameter in function_parameters
                .iter()
                .rev()
                .map(|node| thrustc_entities::function_parameter_from_ast(node))
            {
                let name: &str = parameter.0;
                let kind: &Type = parameter.2;
                let position: u32 = parameter.3;
                let span: Span = parameter.4;

                let Some(parameter_value) = function_value.get_nth_param(position) else {
                    continue;
                };

                let llvm_type: BasicTypeEnum =
                    typegeneration::generate_type(codegen.get_mut_context(), kind);

                codegen.get_mut_context().emit_parameter_debug(
                    name,
                    position.saturating_add(1),
                    kind,
                    llvm_type,
                    parameter_value,
                    span,
                );
            }

            codegen.get_mut_context().finish_function_debug_data();

            if function_type.is_void_type() && !function_body.has_terminator() {
                if codegen.get_context().get_variatic_context().has_current_va_list() {
                    codegen
                        .get_mut_context()
                        .get_mut_variatic_context()
                        .emit_va_end(span);
                }

                llvm_builder.build_return(None).unwrap_or_else(|_| {
                    abort::abort_codegen(
                        codegen.get_mut_context(),
                        "Failed to compile a empty function terminator!",
                        span,
                        std::path::PathBuf::from(file!()),
                        line!(),
                    )
                });
            }
        }
    }

    codegen.get_mut_context().unset_current_function();
    
    codegen
        .get_mut_context()
        .unset_function_stackguard_protector_pointer();

    codegen
        .get_mut_context()
        .get_mut_variatic_context()
        .unset_current_va_list();
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


#[derive(Debug, Clone, Copy)]
pub enum CompilerFunctionVariant {
    CompilerIntrinsic,
    AssemblerFunction,
    PureFunction,
    
}

impl CompilerFunctionVariant {
    #[inline]
    pub fn is_compiler_intrinsic(&self) -> bool {
        matches!(self, CompilerFunctionVariant::CompilerIntrinsic)
    }

    #[inline]
    pub fn is_assembler_function(&self) -> bool {
        matches!(self, CompilerFunctionVariant::AssemblerFunction)
    }

    #[inline]
    pub fn is_pure_function(&self) -> bool {
        matches!(self, CompilerFunctionVariant::PureFunction)
    }
}