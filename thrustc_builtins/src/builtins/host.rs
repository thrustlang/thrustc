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
use thrustc_compile_time::{BuiltinArgument, BuiltinValue};

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

#[derive(Debug)]
pub struct ProcessorCount;

impl CompileTimeBuiltinFunction for ProcessorCount {
    #[inline]
    fn name(&self) -> &'static str {
        "processorCount"
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
        let count: usize = std::thread::available_parallelism()
            .map(|value| value.get())
            .unwrap_or(1);

        Ok(BuiltinValue::Integer(count as u64))
    }
}

#[derive(Debug)]
pub struct PageSize;

impl CompileTimeBuiltinFunction for PageSize {
    #[inline]
    fn name(&self) -> &'static str {
        "pageSize"
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
        let size: u64 = self::sysconf_positive(libc::_SC_PAGESIZE).unwrap_or(4096);

        Ok(BuiltinValue::Integer(size))
    }
}

#[derive(Debug)]
pub struct CpuCacheLineSize;

impl CompileTimeBuiltinFunction for CpuCacheLineSize {
    #[inline]
    fn name(&self) -> &'static str {
        "cpuCacheLineSize"
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
        let size: u64 = self::cache_line_size();

        Ok(BuiltinValue::Integer(size))
    }
}

#[derive(Debug)]
pub struct HostName;

impl CompileTimeBuiltinFunction for HostName {
    #[inline]
    fn name(&self) -> &'static str {
        "hostName"
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
        let mut buffer: [u8; 256] = [0; 256];

        let result: libc::c_int = unsafe {
            libc::gethostname(buffer.as_mut_ptr().cast(), buffer.len())
        };

        if result != 0 {
            return Ok(BuiltinValue::CString(Vec::new()));
        }

        let len: usize = buffer.iter().position(|byte| *byte == 0).unwrap_or(buffer.len());

        Ok(BuiltinValue::CString(buffer[..len].to_vec()))
    }
}

#[inline]
fn sysconf_positive(name: libc::c_int) -> Option<u64> {
    let value: libc::c_long = unsafe { libc::sysconf(name) };

    if value > 0 {
        Some(value as u64)
    } else {
        None
    }
}

#[cfg(target_os = "linux")]
#[inline]
fn cache_line_size() -> u64 {
    self::sysconf_positive(libc::_SC_LEVEL1_DCACHE_LINESIZE).unwrap_or(64)
}

#[cfg(not(target_os = "linux"))]
#[inline]
fn cache_line_size() -> u64 {
    64
}
