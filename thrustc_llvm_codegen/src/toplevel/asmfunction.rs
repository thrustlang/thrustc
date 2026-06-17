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

use thrustc_ast::Ast;
use thrustc_entities::AssemblerFunction;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributeComparator;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;
use thrustc_llvm_callconventions::LLVMCallConvention;
use thrustc_span::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::abort;
use crate::attribute_builder::AttributeBuilder;
use crate::attribute_builder::LLVMAttributeApplicant;
use crate::block;
use crate::context::LLVMCodeGenContext;
use crate::toplevel::function::CompilerFunctionVariant;
use crate::typegeneration;
use crate::types::LLVMFunction;
use crate::utils;

use inkwell::InlineAsmDialect;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;

pub fn compile<'ctx>(context: &mut LLVMCodeGenContext<'_, 'ctx>, asm_fn: AssemblerFunction<'ctx>) {
    let llvm_module: &Module = context.get_llvm_module();
    let llvm_context: &Context = context.get_llvm_context();
    let llvm_builder: &Builder = context.get_llvm_builder();

    let name: &str = asm_fn.0;
    let ascii_name: &str = asm_fn.1;

    let assembler: String = asm_fn.2.to_string();
    let constraints: String = asm_fn.3.to_string();
    let return_type: &Type = asm_fn.4;
    let parameters: &[Ast] = asm_fn.5;
    let parameters_types: &[Type] = asm_fn.6;
    let attributes: LLVMAttributes = thrustc_llvm_attributes::into_llvm_attributes(asm_fn.7);

    let span: Span = asm_fn.8;

    let sideeffects: bool = attributes.has_asmsideffects_attribute();
    let align_stack: bool = attributes.has_asmalignstack_attribute();
    let can_throw: bool = attributes.has_asmthrow_attribute();
    let is_public: bool = attributes.has_public_attribute();

    let call_convention: u32 = if let Some(LLVMAttribute::Convention(call_convention, ..)) =
        attributes.get_attr(LLVMAttributeComparator::Convention)
    {
        call_convention as u32
    } else {
        LLVMCallConvention::Standard as u32
    };

    let syntax: InlineAsmDialect = match attributes.get_attr(LLVMAttributeComparator::AsmSyntax) {
        Some(LLVMAttribute::AsmSyntax(syntax), ..) => match syntax {
            "Intel" => InlineAsmDialect::Intel,
            "AT&T" => InlineAsmDialect::ATT,

            _ => InlineAsmDialect::Intel,
        },
        _ => InlineAsmDialect::Intel,
    };

    let llvm_function_name: String = if is_public {
        format!("__asm_fn_{}", ascii_name)
    } else {
        format!(
            "__asm_fn_{}_{}",
            utils::generate_string(context, utils::LONG_RANGE_OBFUSCATION),
            ascii_name
        )
    };

    let generated_function_type: (
        FunctionType<'_>,
        Option<thrustc_llvm_abi::LLVMABIConfiguration>,
    ) = typegeneration::compile_as_function_type(
        context,
        return_type,
        parameters,
        false,
        CompilerFunctionVariant::AssemblerFunction,
    );

    let function_type: FunctionType<'_> = generated_function_type.0;
    let function_abi_config: Option<thrustc_llvm_abi::LLVMABIConfiguration> =
        generated_function_type.1;

    let function_ptr: PointerValue = llvm_context.create_inline_asm(
        function_type,
        assembler,
        constraints,
        sideeffects,
        align_stack,
        Some(syntax),
        can_throw,
    );

    let asm_function: FunctionValue =
        llvm_module.add_function(&llvm_function_name, function_type, None);

    let applicant: LLVMAttributeApplicant<'_> = LLVMAttributeApplicant::AsmFunction {
        value: asm_function,
        span,
    };

    AttributeBuilder::add_function_attributes(context, &attributes, applicant);

    let last_block: BasicBlock = context.get_last_builder_block(span);
    let function_block: BasicBlock = block::append_block(context, asm_function);

    llvm_builder.position_at_end(function_block);

    let args: Vec<BasicMetadataValueEnum> = asm_function
        .get_param_iter()
        .map(|param| param.into())
        .collect();

    if let Ok(asm_fn_call) =
        llvm_builder.build_indirect_call(function_type, function_ptr, &args, "")
    {
        match (
            return_type.is_void_type(),
            asm_fn_call.try_as_basic_value().left(),
        ) {
            (false, Some(return_value)) => {
                llvm_builder
                    .build_return(Some(&return_value))
                    .map_err(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile assembly function!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .ok();
            }
            _ => {
                llvm_builder
                    .build_return(None)
                    .map_err(|_| {
                        abort::abort_codegen(
                            context,
                            "Failed to compile assembly function!",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        );
                    })
                    .ok();
            }
        }
    } else {
        abort::abort_codegen(
            context,
            "Failed to compile indirect call for assembly function!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        );
    }

    llvm_builder.position_at_end(last_block);

    let prototype: LLVMFunction = (
        asm_function,
        return_type,
        parameters_types,
        call_convention,
        function_abi_config,
        attributes,
        false,
        span,
    );

    context.add_function(name, prototype);
}
