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

use thrustc_errors::CompilationIssue;
use thrustc_typesystem::Type;

use crate::context::BuiltinContext;
use crate::value::{BuiltinArgument, BuiltinValue};

#[derive(Debug, Clone)]
pub enum BuiltinParameter {
    Value(Type),
    Type,
}

#[derive(Debug, Clone)]
pub struct BuiltinFunctionSignature {
    pub return_type: Type,
    pub parameters: Vec<BuiltinParameter>,
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
    pub fn get_parameter_count(&self) -> usize {
        self.parameters.len()
    }

    pub fn is_parameter_a_type(&self, index: usize) -> bool {
        self.parameters
            .get(index)
            .is_some_and(|parameter| matches!(parameter, BuiltinParameter::Type))
    }
}
