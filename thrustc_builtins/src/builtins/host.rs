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
use thrustc_errors::CompilationIssueCode;
use thrustc_typesystem::Type;

use crate::builtins::location;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

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
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        self::host_name(context.call_span)
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
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        self::sysconf_value(
            libc::_SC_NPROCESSORS_ONLN,
            context.call_span,
            "processorCount",
        )
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
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        self::sysconf_value(libc::_SC_PAGESIZE, context.call_span, "pageSize")
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
        context: &mut BuiltinContext<'_>,
    ) -> Result<BuiltinValue, CompilationIssue> {
        self::sysconf_value(
            libc::_SC_LEVEL1_DCACHE_LINESIZE,
            context.call_span,
            "cpuCacheLineSize",
        )
    }
}

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

fn unsupported(span: Span, name: &str) -> CompilationIssue {
    CompilationIssue::Error(
        CompilationIssueCode::E0048,
        format!("The '{}' builtin is not supported on this platform.", name),
        "This builtin is only available on POSIX systems.".into(),
        None,
        span,
    )
}

#[cfg(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
    target_os = "solaris",
    target_os = "illumos"
))]
fn host_name(span: Span) -> Result<BuiltinValue, CompilationIssue> {
    let mut buffer: Vec<u8> = vec![0u8; 256];

    let result: i32 =
        unsafe { libc::gethostname(buffer.as_mut_ptr() as *mut libc::c_char, buffer.len()) };

    if result != 0 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0048,
            "Failed to query the host name.".into(),
            "The 'hostName' builtin could not be resolved.".into(),
            None,
            span,
        ));
    }

    if let Some(null_index) = buffer.iter().position(|&b| b == 0) {
        buffer.truncate(null_index);
    }

    Ok(BuiltinValue::CString(buffer))
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
    target_os = "solaris",
    target_os = "illumos"
)))]
fn host_name(span: Span) -> Result<BuiltinValue, CompilationIssue> {
    Err(unsupported(span, "hostName"))
}

#[cfg(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
    target_os = "solaris",
    target_os = "illumos"
))]
fn sysconf_value(
    name: libc::c_int,
    span: Span,
    builtin_name: &str,
) -> Result<BuiltinValue, CompilationIssue> {
    let value: libc::c_long = unsafe { libc::sysconf(name) };

    if value < 0 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0048,
            format!("Failed to query '{}'.", builtin_name),
            "The builtin could not be resolved on this system.".into(),
            None,
            span,
        ));
    }

    Ok(BuiltinValue::Integer(value as u64))
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "dragonfly",
    target_os = "solaris",
    target_os = "illumos"
)))]
fn sysconf_value(
    _name: libc::c_int,
    span: Span,
    builtin_name: &str,
) -> Result<BuiltinValue, CompilationIssue> {
    Err(unsupported(span, builtin_name))
}
