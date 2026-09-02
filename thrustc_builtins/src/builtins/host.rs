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

use std::time::{SystemTime, UNIX_EPOCH};

use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_typesystem::Type;

use crate::builtins::location;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct HostOsName;

impl CompileTimeBuiltinFunction for HostOsName {
    #[inline]
    fn name(&self) -> &'static str {
        "hostOsName"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: location::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        Ok(BuiltinValue::CString(
            std::env::consts::OS.as_bytes().to_vec(),
        ))
    }
}

#[derive(Debug)]
pub struct HostArch;

impl CompileTimeBuiltinFunction for HostArch {
    #[inline]
    fn name(&self) -> &'static str {
        "hostArch"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: location::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        Ok(BuiltinValue::CString(
            std::env::consts::ARCH.as_bytes().to_vec(),
        ))
    }
}

#[derive(Debug)]
pub struct HostEndian;

impl CompileTimeBuiltinFunction for HostEndian {
    #[inline]
    fn name(&self) -> &'static str {
        "hostEndian"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: location::cstring_type(),
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let endian: &str = self::host_endian();

        Ok(BuiltinValue::CString(endian.as_bytes().to_vec()))
    }
}

#[cfg(target_endian = "big")]
fn host_endian() -> &'static str {
    "big"
}

#[cfg(target_endian = "little")]
fn host_endian() -> &'static str {
    "little"
}

#[derive(Debug)]
pub struct CurrentTimestamp;

impl CompileTimeBuiltinFunction for CurrentTimestamp {
    #[inline]
    fn name(&self) -> &'static str {
        "currentTimestamp"
    }

    #[inline]
    fn signature(&self) -> BuiltinFunctionSignature {
        BuiltinFunctionSignature {
            return_type: Type::USize {
                span: Span::nothing(),
            },
            parameters: Vec::new(),
        }
    }

    fn evaluate(
        &self,
        _args: &[BuiltinArgument],
        _context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        let now: u64 = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs())
            .unwrap_or(0);

        Ok(BuiltinValue::Integer(now))
    }
}
