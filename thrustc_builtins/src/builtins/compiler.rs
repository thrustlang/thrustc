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

use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_typesystem::Type;

use crate::builtins::location::cstring_type;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use thrustc_compile_time::{BuiltinArgument, BuiltinValue};

#[derive(Debug)]
pub struct CompilerVersion;

impl CompileTimeBuiltinFunction for CompilerVersion {
    #[inline]
    fn name(&self) -> &'static str {
        "compilerVersion"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: self::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        Ok(BuiltinValue::CString(
            env!("CARGO_PKG_VERSION").as_bytes().to_vec(),
        ))
    }
}

#[derive(Debug)]
pub struct DebugBuild;

impl CompileTimeBuiltinFunction for DebugBuild {
    #[inline]
    fn name(&self) -> &'static str {
        "debugBuild"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::Bool {
                span: Span::nothing(),
            },
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        Ok(BuiltinValue::Bool(context.options.omit_default_optimizations()))
    }
}
