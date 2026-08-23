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
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

use crate::builtins::location::cstring_type;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::BuiltinParameter;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct StringLength;

impl CompileTimeBuiltinFunction for StringLength {
    #[inline]
    fn name(&self) -> &'static str {
        "stringLength"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::USize {
                span: Span::nothing(),
            },
            parameters: vec![BuiltinParameter::Value(self::cstring_type())],
        }
    }

    fn evaluate(
        &self,
        args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let bytes: &Vec<u8> = match &args[0] {
            BuiltinArgument::Value {
                value: BuiltinValue::CString(bytes),
                ..
            } => bytes,
            _ => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0019,
                    "The 'stringLength' compiler builtin expects a constant string argument."
                        .into(),
                    "You should pass a constant string, like stringLength(\"hello\").".into(),
                    None,
                    Span::nothing(),
                ));
            }
        };

        Ok(BuiltinValue::Integer(bytes.len() as u64))
    }
}