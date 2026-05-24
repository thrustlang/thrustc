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

use inkwell::values::FunctionValue;

use thrustc_llvm_abi::LLVMABIConfiguration;
use thrustc_span::Span;
use thrustc_typesystem::Type;

pub trait AstLLVMGetType {
    fn get_type_for_llvm(&self) -> &Type;
}

pub trait LLVMFunctionExtensions<'ctx> {
    fn get_value(&self) -> FunctionValue<'ctx>;
    fn get_return_type(&self) -> &'ctx Type;
    fn get_call_convention(&self) -> u32;
    fn get_param_count(&self) -> usize;
    fn get_parameters_types(&self) -> &[Type];
    fn get_abi_configuration(&self) -> Option<&LLVMABIConfiguration<'ctx>>;
    fn is_variadic(&self) -> bool;
    fn get_span(&self) -> Span;
}

pub trait LLVMDBGFunctionExtensions<'ctx> {
    fn get_value(&self) -> FunctionValue<'ctx>;
    fn get_name(&self) -> &str;
    fn get_return_type(&self) -> &'ctx Type;
    fn get_parameters_types(&self) -> Vec<Type>;
    fn get_span(&self) -> Span;

    fn is_definition(&self) -> bool;
    fn is_local(&self) -> bool;
}
