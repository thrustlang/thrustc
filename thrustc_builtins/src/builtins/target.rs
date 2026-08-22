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
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_typesystem::Type;

use crate::builtins::location;
use crate::context::BuiltinContext;
use crate::traits::BuiltinFunctionSignature;
use crate::traits::CompileTimeBuiltinFunction;
use crate::value::BuiltinArgument;
use crate::value::BuiltinValue;

#[derive(Debug)]
pub struct TargetOS;

impl CompileTimeBuiltinFunction for TargetOS {
    #[inline]
    fn name(&self) -> &'static str {
        "targetOS"
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
        let os: &str = self::target_triple(context).get_os();

        Ok(BuiltinValue::CString(os.as_bytes().to_vec()))
    }
}

#[derive(Debug)]
pub struct TargetArch;

impl CompileTimeBuiltinFunction for TargetArch {
    #[inline]
    fn name(&self) -> &'static str {
        "targetArch"
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
        let arch: &str = self::target_triple(context).get_arch();

        Ok(BuiltinValue::CString(arch.as_bytes().to_vec()))
    }
}

#[derive(Debug)]
pub struct TargetVendor;

impl CompileTimeBuiltinFunction for TargetVendor {
    #[inline]
    fn name(&self) -> &'static str {
        "targetVendor"
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
        let vendor: &str = self::target_triple(context).get_vendor();

        Ok(BuiltinValue::CString(vendor.as_bytes().to_vec()))
    }
}

#[derive(Debug)]
pub struct TargetAbi;

impl CompileTimeBuiltinFunction for TargetAbi {
    #[inline]
    fn name(&self) -> &'static str {
        "targetAbi"
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
        let abi: &str = self::target_triple(context).get_abi();

        Ok(BuiltinValue::CString(abi.as_bytes().to_vec()))
    }
}

#[derive(Debug)]
pub struct TargetTriple;

impl CompileTimeBuiltinFunction for TargetTriple {
    #[inline]
    fn name(&self) -> &'static str {
        "targetTriple"
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
        let normalized: String = self::target_triple(context).get_normalized();

        Ok(BuiltinValue::CString(normalized.into_bytes()))
    }
}

#[derive(Debug)]
pub struct IsLinux;

impl CompileTimeBuiltinFunction for IsLinux {
    #[inline]
    fn name(&self) -> &'static str {
        "isLinux"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_linux_based(),
        ))
    }
}

#[derive(Debug)]
pub struct IsWindows;

impl CompileTimeBuiltinFunction for IsWindows {
    #[inline]
    fn name(&self) -> &'static str {
        "isWindows"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_windows_based(),
        ))
    }
}

#[derive(Debug)]
pub struct IsDarwin;

impl CompileTimeBuiltinFunction for IsDarwin {
    #[inline]
    fn name(&self) -> &'static str {
        "isDarwin"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_darwin_os(),
        ))
    }
}

#[derive(Debug)]
pub struct IsApple;

impl CompileTimeBuiltinFunction for IsApple {
    #[inline]
    fn name(&self) -> &'static str {
        "isApple"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_apple_based(),
        ))
    }
}

#[derive(Debug)]
pub struct IsAix;

impl CompileTimeBuiltinFunction for IsAix {
    #[inline]
    fn name(&self) -> &'static str {
        "isAix"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_os_aix(),
        ))
    }
}

#[derive(Debug)]
pub struct Is64Bit;

impl CompileTimeBuiltinFunction for Is64Bit {
    #[inline]
    fn name(&self) -> &'static str {
        "is64Bit"
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
        Ok(BuiltinValue::Bool(self::target_triple(context).is_64_bit()))
    }
}

#[derive(Debug)]
pub struct Is32Bit;

impl CompileTimeBuiltinFunction for Is32Bit {
    #[inline]
    fn name(&self) -> &'static str {
        "is32Bit"
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
        Ok(BuiltinValue::Bool(self::target_triple(context).is_32_bit()))
    }
}

#[derive(Debug)]
pub struct IsBigEndian;

impl CompileTimeBuiltinFunction for IsBigEndian {
    #[inline]
    fn name(&self) -> &'static str {
        "isBigEndian"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_big_endian(),
        ))
    }
}

#[derive(Debug)]
pub struct IsLittleEndian;

impl CompileTimeBuiltinFunction for IsLittleEndian {
    #[inline]
    fn name(&self) -> &'static str {
        "isLittleEndian"
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
        Ok(BuiltinValue::Bool(
            !self::target_triple(context).is_big_endian(),
        ))
    }
}

#[derive(Debug)]
pub struct IsX86;

impl CompileTimeBuiltinFunction for IsX86 {
    #[inline]
    fn name(&self) -> &'static str {
        "isX86"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_x86_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsX8664;

impl CompileTimeBuiltinFunction for IsX8664 {
    #[inline]
    fn name(&self) -> &'static str {
        "isX8664"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_x86_64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsArm;

impl CompileTimeBuiltinFunction for IsArm {
    #[inline]
    fn name(&self) -> &'static str {
        "isArm"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_arm_family(),
        ))
    }
}

#[derive(Debug)]
pub struct IsAarch64;

impl CompileTimeBuiltinFunction for IsAarch64 {
    #[inline]
    fn name(&self) -> &'static str {
        "isAarch64"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_aarch64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsRiscv64;

impl CompileTimeBuiltinFunction for IsRiscv64 {
    #[inline]
    fn name(&self) -> &'static str {
        "isRiscv64"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_riscv64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsPpc;

impl CompileTimeBuiltinFunction for IsPpc {
    #[inline]
    fn name(&self) -> &'static str {
        "isPpc"
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
        Ok(BuiltinValue::Bool(self::target_triple(context).is_ppc_arch()))
    }
}

#[derive(Debug)]
pub struct IsPpc64;

impl CompileTimeBuiltinFunction for IsPpc64 {
    #[inline]
    fn name(&self) -> &'static str {
        "isPpc64"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_ppc64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsMips64;

impl CompileTimeBuiltinFunction for IsMips64 {
    #[inline]
    fn name(&self) -> &'static str {
        "isMips64"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_mips64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsSystemz;

impl CompileTimeBuiltinFunction for IsSystemz {
    #[inline]
    fn name(&self) -> &'static str {
        "isSystemz"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_systemz_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsLoongarch64;

impl CompileTimeBuiltinFunction for IsLoongarch64 {
    #[inline]
    fn name(&self) -> &'static str {
        "isLoongarch64"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_loongarch64_arch(),
        ))
    }
}

#[derive(Debug)]
pub struct IsWasm;

impl CompileTimeBuiltinFunction for IsWasm {
    #[inline]
    fn name(&self) -> &'static str {
        "isWasm"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).get_arch().contains("wasm"),
        ))
    }
}

#[derive(Debug)]
pub struct IsElf;

impl CompileTimeBuiltinFunction for IsElf {
    #[inline]
    fn name(&self) -> &'static str {
        "isElf"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_object_format_elf(),
        ))
    }
}

#[derive(Debug)]
pub struct IsMachO;

impl CompileTimeBuiltinFunction for IsMachO {
    #[inline]
    fn name(&self) -> &'static str {
        "isMachO"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_object_format_mach_o(),
        ))
    }
}

#[derive(Debug)]
pub struct IsCoff;

impl CompileTimeBuiltinFunction for IsCoff {
    #[inline]
    fn name(&self) -> &'static str {
        "isCoff"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).is_coff_object_format(),
        ))
    }
}

#[derive(Debug)]
pub struct HasPosixThreads;

impl CompileTimeBuiltinFunction for HasPosixThreads {
    #[inline]
    fn name(&self) -> &'static str {
        "hasPosixThreads"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).has_posix_thread_model(),
        ))
    }
}

#[derive(Debug)]
pub struct HasSysvAbi;

impl CompileTimeBuiltinFunction for HasSysvAbi {
    #[inline]
    fn name(&self) -> &'static str {
        "hasSysvAbi"
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
        Ok(BuiltinValue::Bool(
            self::target_triple(context).has_sysv_abi(),
        ))
    }
}

fn target_triple<'a>(context: &'a mut BuiltinContext<'_>) -> &'a LLVMTargetTriple {
    context.target_info.get_triple()
}