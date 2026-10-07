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

use thrustc_compile_time::{BuiltinArgument, BuiltinValue};
use thrustc_errors::CompilationIssue;
use thrustc_typesystem::Type;

use crate::context::BuiltinContext;

#[derive(Debug, Clone)]
pub enum BuiltinParameter {
    Value(Type),
    Type,
}

#[derive(Debug, Clone)]
pub struct BuiltinFunctionSignature {
    return_type: Type,
    parameters: Vec<BuiltinParameter>,
}

impl BuiltinFunctionSignature {
    #[inline]
    pub fn new(return_type: Type, parameters: Vec<BuiltinParameter>) -> Self {
        Self {
            return_type,
            parameters,
        }
    }
}

pub trait CompileTimeBuiltinFunction: std::fmt::Debug {
    fn name(&self) -> &'static str;
    fn signature(&self) -> BuiltinFunctionSignature;
    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        ctx: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue>;
}

impl BuiltinFunctionSignature {
    #[inline]
    pub fn get_return_type(&self) -> &Type {
        &self.return_type
    }

    #[inline]
    pub fn get_parameters(&self) -> &Vec<BuiltinParameter> {
        &self.parameters
    }

    #[inline]
    pub fn get_parameter_count(&self) -> usize {
        self.parameters.len()
    }
}

impl BuiltinFunctionSignature {
    #[inline]
    pub fn get_mut_return_type(&mut self) -> &mut Type {
        &mut self.return_type
    }

    #[inline]
    pub fn get_mut_parameters(&mut self) -> &mut Vec<BuiltinParameter> {
        &mut self.parameters
    }
}

impl BuiltinFunctionSignature {
    #[inline]
    pub fn set_return_type(&mut self, return_type: Type) {
        self.return_type = return_type;
    }

    #[inline]
    pub fn set_parameters(&mut self, parameters: Vec<BuiltinParameter>) {
        self.parameters = parameters;
    }
}

impl BuiltinFunctionSignature {
    #[inline]
    pub fn is_parameter_a_type(&self, index: usize) -> bool {
        self.parameters
            .get(index)
            .is_some_and(|parameter| matches!(parameter, BuiltinParameter::Type))
    }
}
