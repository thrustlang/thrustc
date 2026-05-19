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
use thrustc_entities::Intrinsic;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributeComparator;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;
use thrustc_llvm_callconventions::LLVMCallConvention;
use thrustc_span::Span;
use thrustc_typesystem::Type;

use crate::attributebuilder::AttributeBuilder;
use crate::attributebuilder::LLVMAttributeApplicant;
use crate::context::LLVMCodeGenContext;
use crate::typegeneration;
use crate::types::LLVMFunction;

use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

pub fn compile<'ctx>(context: &mut LLVMCodeGenContext<'_, 'ctx>, intrinsic: Intrinsic<'ctx>) {
    let llvm_module: &Module = context.get_llvm_module();

    let name: &str = intrinsic.0;
    let external_name: &str = intrinsic.1;
    let return_type: &Type = intrinsic.2;
    let parameters: &[Ast<'ctx>] = intrinsic.3;
    let parameters_types: &[Type] = intrinsic.4;
    let attributes: LLVMAttributes = thrustc_llvm_attributes::into_llvm_attributes(intrinsic.5);
    let span: Span = intrinsic.6;

    let ignore_args: bool = attributes.has_ignore_attribute();

    let convention: u32 = if let Some(LLVMAttribute::Convention(convention, ..)) =
        attributes.get_attr(LLVMAttributeComparator::Convention)
    {
        convention as u32
    } else {
        LLVMCallConvention::Standard as u32
    };

    let generated_function_type: (
        FunctionType<'_>,
        Option<thrustc_llvm_abi::LLVMABIConfiguration>,
    ) = typegeneration::compile_as_function_type(context, return_type, parameters, ignore_args);

    let function_type: FunctionType<'_> = generated_function_type.0;
    let function_abi_config: Option<thrustc_llvm_abi::LLVMABIConfiguration> =
        generated_function_type.1;

    let llvm_function: FunctionValue = llvm_module.add_function(external_name, function_type, None);

    AttributeBuilder::new(attributes, LLVMAttributeApplicant::Function(llvm_function))
        .add_function_attributes(context);

    let prototype: LLVMFunction = (
        llvm_function,
        return_type,
        parameters_types,
        convention,
        function_abi_config,
        span,
    );

    context.new_function(name, prototype);
}
