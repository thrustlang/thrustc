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

use ahash::AHashMap as HashMap;

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_errors::CompilationIssueCode;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_layout::TargetInfo;

use crate::builtin_type::BuiltinTypeInfo;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct BuiltinRegistry {
    functions: HashMap<&'static str, Box<dyn CompileTimeBuiltinFunction>>,
    types: HashMap<&'static str, BuiltinTypeInfo>,
    target_info: TargetInfo,
}

impl BuiltinRegistry {
    pub fn new(target_info: TargetInfo) -> Self {
        Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            target_info,
        }
    }

    pub fn register_function(&mut self, function: impl CompileTimeBuiltinFunction + 'static) {
        self.functions.insert(function.name(), Box::new(function));
    }

    pub fn register_type(&mut self, info: BuiltinTypeInfo) {
        self.types.insert(info.name, info);
    }

    pub fn get_function(&self, name: &str) -> Option<&dyn CompileTimeBuiltinFunction> {
        self.functions.get(name).map(|function| function.as_ref())
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name).map(|info| &info.ty)
    }

    pub fn evaluate<'builtin>(
        &mut self,
        name: &str,
        args: &[BuiltinArgument],
        span: Span,
        options: &CompilerOptions,
        file: &CompilationUnit,
    ) -> Result<Ast<'builtin>, CompilationIssue> {
        let signature: BuiltinFunctionSignature = {
            let function = self.functions.get(name).ok_or_else(|| {
                CompilationIssue::Error(
                    CompilationIssueCode::E0003,
                    format!("Unknown compiler builtin '{}'.", name),
                    "Compiler builtin doesn't exist on the compiler.".into(),
                    None,
                    span,
                )
            })?;

            function.signature()
        };

        let value: BuiltinValue = {
            let function = self.functions.get(name).expect("function must exist");
            let mut context: BuiltinContext = BuiltinContext {
                target_info: &mut self.target_info,
                options,
                file,
            };

            function.evaluate(args, &mut context)?
        };

        Ok(value.to_ast(signature.return_type, span))
    }
}
