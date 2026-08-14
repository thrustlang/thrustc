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


use crate::LLVMCallConvention;

impl std::fmt::Display for LLVMCallConvention {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LLVMCallConvention::Standard => write!(f, "C"),
            LLVMCallConvention::Fast => write!(f, "fast"),
            LLVMCallConvention::Cold => write!(f, "cold"),
            LLVMCallConvention::GHC => write!(f, "Haskell"),
            LLVMCallConvention::PreserveAll => write!(f, "strongReg"),
            LLVMCallConvention::PreserveMost => write!(f, "weakReg"),
            LLVMCallConvention::Tail => write!(f, "tail"),
            LLVMCallConvention::Swift => write!(f, "Swift"),
            LLVMCallConvention::HiPE => write!(f, "Erlang"),
            LLVMCallConvention::GraalVM => write!(f, "GraalVM"),
            LLVMCallConvention::Win64 => write!(f, "Win64"),
            LLVMCallConvention::X86_StdCall => write!(f, "X86_StdCall"),
            LLVMCallConvention::X86_FastCall => write!(f, "X86_FastCall"),
            LLVMCallConvention::X86_ThisCall => write!(f, "X86_ThisCall"),
            LLVMCallConvention::X86_VectorCall => write!(f, "X86_VectorCall"),
            LLVMCallConvention::X86_RegCall => write!(f, "X86_RegCall"),
            LLVMCallConvention::X86_64_SysV => write!(f, "X86_64_SysV"),
            LLVMCallConvention::ARM_APCS => write!(f, "ARM_APCS"),
            LLVMCallConvention::ARM_AAPCS => write!(f, "ARM_AAPCS"),
            LLVMCallConvention::ARM_AAPCS_VFP => write!(f, "ARM_AAPCS_VFP"),
            LLVMCallConvention::AArch64_VectorCall => write!(f, "AArch64_VectorCall"),
            LLVMCallConvention::AArch64_SVE_VectorCall => write!(f, "AArch64_SVE_VectorCall"),
            LLVMCallConvention::SwiftTail => write!(f, "SwiftTail"),
            LLVMCallConvention::PreserveNone => write!(f, "PreserveNone"),
            LLVMCallConvention::AnyReg => write!(f, "AnyReg"),
            LLVMCallConvention::PTX_Kernel => write!(f, "PTX_Kernel"),
            LLVMCallConvention::PTX_Device => write!(f, "PTX_Device"),
            LLVMCallConvention::AMDGPU_KERNEL => write!(f, "AMDGPU_KERNEL"),
            LLVMCallConvention::AMDGPU_Gfx => write!(f, "AMDGPU_Gfx"),
            LLVMCallConvention::RISCV_VectorCall => write!(f, "RISCV_VectorCall"),
            LLVMCallConvention::CXX_FAST_TLS => write!(f, "CXX_FAST_TLS"),
            LLVMCallConvention::CFGuard_Check => write!(f, "CFGuard_Check"),
            LLVMCallConvention::MSP430_INTR => write!(f, "MSP430_INTR"),
            LLVMCallConvention::X86_INTR => write!(f, "X86_INTR"),
            LLVMCallConvention::AVR_INTR => write!(f, "AVR_INTR"),
            LLVMCallConvention::AVR_SIGNAL => write!(f, "AVR_SIGNAL"),
            LLVMCallConvention::AVR_BUILTIN => write!(f, "AVR_BUILTIN"),
            LLVMCallConvention::AMDGPU_VS => write!(f, "AMDGPU_VS"),
            LLVMCallConvention::AMDGPU_GS => write!(f, "AMDGPU_GS"),
            LLVMCallConvention::AMDGPU_PS => write!(f, "AMDGPU_PS"),
            LLVMCallConvention::AMDGPU_CS => write!(f, "AMDGPU_CS"),
            LLVMCallConvention::AMDGPU_HS => write!(f, "AMDGPU_HS"),
            LLVMCallConvention::MSP430_BUILTIN => write!(f, "MSP430_BUILTIN"),
            LLVMCallConvention::AMDGPU_LS => write!(f, "AMDGPU_LS"),
            LLVMCallConvention::AMDGPU_ES => write!(f, "AMDGPU_ES"),
            LLVMCallConvention::WASM_EmscriptenInvoke => write!(f, "WASM_EmscriptenInvoke"),
            LLVMCallConvention::M68k_INTR => write!(f, "M68k_INTR"),
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0 => {
                write!(f, "AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0")
            }
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2 => {
                write!(f, "AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2")
            }
            LLVMCallConvention::AMDGPU_CS_Chain => write!(f, "AMDGPU_CS_Chain"),
            LLVMCallConvention::AMDGPU_CS_ChainPreserve => write!(f, "AMDGPU_CS_ChainPreserve"),
            LLVMCallConvention::M68k_RTD => write!(f, "M68k_RTD"),
            LLVMCallConvention::ARM64EC_Thunk_X64 => write!(f, "ARM64EC_Thunk_X64"),
            LLVMCallConvention::ARM64EC_Thunk_Native => write!(f, "ARM64EC_Thunk_Native"),
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1 => {
                write!(f, "AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1")
            }
            LLVMCallConvention::RISCV_VLSCall_32 => write!(f, "RISCV_VLSCall_32"),
            LLVMCallConvention::RISCV_VLSCall_64 => write!(f, "RISCV_VLSCall_64"),
            LLVMCallConvention::RISCV_VLSCall_128 => write!(f, "RISCV_VLSCall_128"),
            LLVMCallConvention::RISCV_VLSCall_256 => write!(f, "RISCV_VLSCall_256"),
            LLVMCallConvention::RISCV_VLSCall_512 => write!(f, "RISCV_VLSCall_512"),
            LLVMCallConvention::RISCV_VLSCall_1024 => write!(f, "RISCV_VLSCall_1024"),
            LLVMCallConvention::RISCV_VLSCall_2048 => write!(f, "RISCV_VLSCall_2048"),
            LLVMCallConvention::RISCV_VLSCall_4096 => write!(f, "RISCV_VLSCall_4096"),
            LLVMCallConvention::RISCV_VLSCall_8192 => write!(f, "RISCV_VLSCall_8192"),
            LLVMCallConvention::RISCV_VLSCall_16384 => write!(f, "RISCV_VLSCall_16384"),
            LLVMCallConvention::RISCV_VLSCall_32768 => write!(f, "RISCV_VLSCall_32768"),
            LLVMCallConvention::RISCV_VLSCall_65536 => write!(f, "RISCV_VLSCall_65536"),
            LLVMCallConvention::AMDGPU_Gfx_WholeWave => write!(f, "AMDGPU_Gfx_WholeWave"),
            LLVMCallConvention::CHERIoT_CompartmentCall => write!(f, "CHERIoT_CompartmentCall"),
            LLVMCallConvention::CHERIoT_CompartmentCallee => write!(f, "CHERIoT_CompartmentCallee"),
            LLVMCallConvention::CHERIoT_LibraryCall => write!(f, "CHERIoT_LibraryCall"),
            LLVMCallConvention::SPIR_FUNC => write!(f, "SPIR_FUNC"),
            LLVMCallConvention::SPIR_KERNEL => write!(f, "SPIR_KERNEL"),
            LLVMCallConvention::Intel_OCL_BI => write!(f, "Intel_OCL_BI"),
        }
    }
}
