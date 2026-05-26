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

#![allow(clippy::upper_case_acronyms, non_camel_case_types)]

use ahash::AHashMap as HashMap;
use lazy_static::lazy_static;

mod impls;

// Call conventions: https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/IR/CallingConv.h
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LLVMCallConvention {
    // " Standard " call conventions.
    Standard = 0,
    Fast = 8,
    Cold = 9,
    Tail = 18,

    // Glasgow Haskell Compiler (GHC)
    GHC = 10,

    // High-Performance Erlang Compiler (HiPE).
    HiPE = 11,

    // Dynamic register based calls
    AnyReg = 13,

    // Preserves most caller-saved registers, balancing performance and compatibility.
    PreserveMost = 14,

    // Preserves all caller-saved registers, ensuring maximum compatibility but with higher overhead.
    PreserveAll = 15,

    // Swift.
    Swift = 16,

    // Access functions
    CXX_FAST_TLS = 17,

    // Control Guard Check ICall function
    CFGuard_Check = 19,

    // Swift with tail call guarantee
    SwiftTail = 20,

    // Preserves no general registers
    PreserveNone = 21,

    // X86 stdcall
    X86_StdCall = 64,

    // X86 fastcall
    X86_FastCall = 65,

    // ARM APCS
    ARM_APCS = 66,

    // ARM AAPCS
    ARM_AAPCS = 67,

    // ARM AAPCS VFP
    ARM_AAPCS_VFP = 68,

    // MSP430 interrupt
    MSP430_INTR = 69,

    // X86 thiscall
    X86_ThisCall = 70,

    // PTX kernel
    PTX_Kernel = 71,

    // PTX device
    PTX_Device = 72,

    // SPIR function
    SPIR_FUNC = 75,

    // SPIR kernel
    SPIR_KERNEL = 76,

    // Intel OpenCL built-ins
    Intel_OCL_BI = 77,

    // x86-64 System V
    X86_64_SysV = 78,

    // Win64
    Win64 = 79,

    // X86 vector call
    X86_VectorCall = 80,

    // X86 interrupt
    X86_INTR = 83,

    // AVR interrupt
    AVR_INTR = 84,

    // AVR signal
    AVR_SIGNAL = 85,

    // AVR builtin
    AVR_BUILTIN = 86,

    // AMDGPU vertex shader
    AMDGPU_VS = 87,

    // AMDGPU geometry shader
    AMDGPU_GS = 88,

    // AMDGPU pixel shader
    AMDGPU_PS = 89,

    // AMDGPU compute shader
    AMDGPU_CS = 90,

    // AMDGPU kernel
    AMDGPU_KERNEL = 91,

    // X86 register call
    X86_RegCall = 92,

    // AMDGPU hull shader
    AMDGPU_HS = 93,

    // MSP430 builtin
    MSP430_BUILTIN = 94,

    // AMDGPU local shader
    AMDGPU_LS = 95,

    // AMDGPU export shader
    AMDGPU_ES = 96,

    // AArch64 vector call
    AArch64_VectorCall = 97,

    // AArch64 SVE vector call
    AArch64_SVE_VectorCall = 98,

    // WebAssembly Emscripten invoke
    WASM_EmscriptenInvoke = 99,

    // AMDGPU graphics
    AMDGPU_Gfx = 100,

    // M68k interrupt
    M68k_INTR = 101,

    // AArch64 SME ABI support routines preserve most from X0
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0 = 102,

    // AArch64 SME ABI support routines preserve most from X2
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2 = 103,

    // AMDGPU CS chain
    AMDGPU_CS_Chain = 104,

    // AMDGPU CS chain preserve
    AMDGPU_CS_ChainPreserve = 105,

    // M68k RTD
    M68k_RTD = 106,

    // GraalVM
    GraalVM = 107,

    // ARM64EC thunk x64
    ARM64EC_Thunk_X64 = 108,

    // ARM64EC thunk native
    ARM64EC_Thunk_Native = 109,

    // RISC-V vector call
    RISCV_VectorCall = 110,

    // AArch64 SME ABI support routines preserve most from X1
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1 = 111,

    // RISC-V VLS calls
    RISCV_VLSCall_32 = 112,
    RISCV_VLSCall_64 = 113,
    RISCV_VLSCall_128 = 114,
    RISCV_VLSCall_256 = 115,
    RISCV_VLSCall_512 = 116,
    RISCV_VLSCall_1024 = 117,
    RISCV_VLSCall_2048 = 118,
    RISCV_VLSCall_4096 = 119,
    RISCV_VLSCall_8192 = 120,
    RISCV_VLSCall_16384 = 121,
    RISCV_VLSCall_32768 = 122,
    RISCV_VLSCall_65536 = 123,

    // AMDGPU graphics whole wave
    AMDGPU_Gfx_WholeWave = 124,

    // CHERIoT compartment call
    CHERIoT_CompartmentCall = 125,

    // CHERIoT compartment callee
    CHERIoT_CompartmentCallee = 126,

    // CHERIoT library call
    CHERIoT_LibraryCall = 127,
}

impl LLVMCallConvention {
    #[inline]
    pub fn is_specific_target_conv(&self) -> bool {
        matches!(
            self,
            LLVMCallConvention::AMDGPU_CS
                | LLVMCallConvention::AMDGPU_CS_Chain
                | LLVMCallConvention::AMDGPU_CS_ChainPreserve
                | LLVMCallConvention::AMDGPU_ES
                | LLVMCallConvention::AMDGPU_GS
                | LLVMCallConvention::AMDGPU_Gfx
                | LLVMCallConvention::AMDGPU_Gfx_WholeWave
                | LLVMCallConvention::AMDGPU_HS
                | LLVMCallConvention::AMDGPU_KERNEL
                | LLVMCallConvention::AMDGPU_LS
                | LLVMCallConvention::AMDGPU_PS
                | LLVMCallConvention::AMDGPU_VS
                | LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0
                | LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1
                | LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2
                | LLVMCallConvention::AArch64_SVE_VectorCall
                | LLVMCallConvention::AArch64_VectorCall
                | LLVMCallConvention::RISCV_VLSCall_1024
                | LLVMCallConvention::RISCV_VLSCall_128
                | LLVMCallConvention::RISCV_VLSCall_16384
                | LLVMCallConvention::RISCV_VLSCall_2048
                | LLVMCallConvention::RISCV_VLSCall_256
                | LLVMCallConvention::RISCV_VLSCall_32
                | LLVMCallConvention::RISCV_VLSCall_32768
                | LLVMCallConvention::RISCV_VLSCall_4096
                | LLVMCallConvention::RISCV_VLSCall_512
                | LLVMCallConvention::RISCV_VLSCall_64
                | LLVMCallConvention::RISCV_VLSCall_65536
                | LLVMCallConvention::RISCV_VLSCall_8192
                | LLVMCallConvention::RISCV_VectorCall
                | LLVMCallConvention::ARM_AAPCS
                | LLVMCallConvention::ARM_AAPCS_VFP
                | LLVMCallConvention::ARM_APCS
                | LLVMCallConvention::ARM64EC_Thunk_Native
                | LLVMCallConvention::ARM64EC_Thunk_X64
                | LLVMCallConvention::X86_StdCall
                | LLVMCallConvention::X86_FastCall
                | LLVMCallConvention::X86_ThisCall
                | LLVMCallConvention::X86_64_SysV
                | LLVMCallConvention::X86_INTR
                | LLVMCallConvention::X86_VectorCall
                | LLVMCallConvention::X86_RegCall
                | LLVMCallConvention::PTX_Kernel
                | LLVMCallConvention::PTX_Device
        )
    }
}

lazy_static! {
    pub static ref CALL_CONVENTIONS: HashMap<&'static [u8], LLVMCallConvention> = {
        let mut call_conventions: HashMap<&'static [u8], LLVMCallConvention> =
            HashMap::with_capacity(80);

        call_conventions.insert(b"C", LLVMCallConvention::Standard);
        call_conventions.insert(b"fast", LLVMCallConvention::Fast);
        call_conventions.insert(b"tail", LLVMCallConvention::Tail);
        call_conventions.insert(b"cold", LLVMCallConvention::Cold);
        call_conventions.insert(b"weakReg", LLVMCallConvention::PreserveMost);
        call_conventions.insert(b"strongReg", LLVMCallConvention::PreserveAll);
        call_conventions.insert(b"Swift", LLVMCallConvention::Swift);
        call_conventions.insert(b"Haskell", LLVMCallConvention::GHC);
        call_conventions.insert(b"Erlang", LLVMCallConvention::HiPE);
        call_conventions.insert(b"GraalVM", LLVMCallConvention::GraalVM);
        call_conventions.insert(b"Win64", LLVMCallConvention::Win64);
        call_conventions.insert(b"X86StdCall", LLVMCallConvention::X86_StdCall);
        call_conventions.insert(b"X86FastCall", LLVMCallConvention::X86_FastCall);
        call_conventions.insert(b"X86ThisCall", LLVMCallConvention::X86_ThisCall);
        call_conventions.insert(b"X86VectorCall", LLVMCallConvention::X86_VectorCall);
        call_conventions.insert(b"X86RegCall", LLVMCallConvention::X86_RegCall);
        call_conventions.insert(b"X86_64_SysV", LLVMCallConvention::X86_64_SysV);
        call_conventions.insert(b"ARMAPCS", LLVMCallConvention::ARM_APCS);
        call_conventions.insert(b"ARMAAPCS", LLVMCallConvention::ARM_AAPCS);
        call_conventions.insert(b"ARM_AAPCS_VFP", LLVMCallConvention::ARM_AAPCS_VFP);
        call_conventions.insert(b"AArch64VectorCall", LLVMCallConvention::AArch64_VectorCall);
        call_conventions.insert(
            b"AArch64SVEVectorCall",
            LLVMCallConvention::AArch64_SVE_VectorCall,
        );
        call_conventions.insert(b"SwiftTail", LLVMCallConvention::SwiftTail);
        call_conventions.insert(b"PreserveNone", LLVMCallConvention::PreserveNone);
        call_conventions.insert(b"AnyReg", LLVMCallConvention::AnyReg);
        call_conventions.insert(b"PTXKernel", LLVMCallConvention::PTX_Kernel);
        call_conventions.insert(b"PTXDevice", LLVMCallConvention::PTX_Device);
        call_conventions.insert(b"AMDGPUKernel", LLVMCallConvention::AMDGPU_KERNEL);
        call_conventions.insert(b"AMDGPUGfx", LLVMCallConvention::AMDGPU_Gfx);
        call_conventions.insert(b"RISCVVectorCall", LLVMCallConvention::RISCV_VectorCall);
        call_conventions.insert(b"CPPFastTLS", LLVMCallConvention::CXX_FAST_TLS);
        call_conventions.insert(b"CFGuardCheck", LLVMCallConvention::CFGuard_Check);
        call_conventions.insert(b"MSP430_INTR", LLVMCallConvention::MSP430_INTR);
        call_conventions.insert(b"SPIRFunc", LLVMCallConvention::SPIR_FUNC);
        call_conventions.insert(b"SPIRKernel", LLVMCallConvention::SPIR_KERNEL);
        call_conventions.insert(b"Intel_OCL_BI", LLVMCallConvention::Intel_OCL_BI);
        call_conventions.insert(b"X86_INTR", LLVMCallConvention::X86_INTR);
        call_conventions.insert(b"AVR_INTR", LLVMCallConvention::AVR_INTR);
        call_conventions.insert(b"AVR_SIGNAL", LLVMCallConvention::AVR_SIGNAL);
        call_conventions.insert(b"AVR_BUILTIN", LLVMCallConvention::AVR_BUILTIN);
        call_conventions.insert(b"AMDGPU_VS", LLVMCallConvention::AMDGPU_VS);
        call_conventions.insert(b"AMDGPU_GS", LLVMCallConvention::AMDGPU_GS);
        call_conventions.insert(b"AMDGPU_PS", LLVMCallConvention::AMDGPU_PS);
        call_conventions.insert(b"AMDGPU_CS", LLVMCallConvention::AMDGPU_CS);
        call_conventions.insert(b"AMDGPU_HS", LLVMCallConvention::AMDGPU_HS);
        call_conventions.insert(b"MSP430_BUILTIN", LLVMCallConvention::MSP430_BUILTIN);
        call_conventions.insert(b"AMDGPU_LS", LLVMCallConvention::AMDGPU_LS);
        call_conventions.insert(b"AMDGPU_ES", LLVMCallConvention::AMDGPU_ES);
        call_conventions.insert(b"WebAssembly", LLVMCallConvention::WASM_EmscriptenInvoke);
        call_conventions.insert(b"M68k_INTR", LLVMCallConvention::M68k_INTR);
        call_conventions.insert(
            b"AArch64SMEABISupportRoutinesPreserveMostFromX0",
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0,
        );
        call_conventions.insert(
            b"AArch64SMEABISupportRoutinesPreserveMostFromX2",
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2,
        );
        call_conventions.insert(b"AMDGPUCSChain", LLVMCallConvention::AMDGPU_CS_Chain);
        call_conventions.insert(
            b"AMDGPUCSChainPreserve",
            LLVMCallConvention::AMDGPU_CS_ChainPreserve,
        );
        call_conventions.insert(b"M68k_RTD", LLVMCallConvention::M68k_RTD);
        call_conventions.insert(b"ARM64ECThunkX64", LLVMCallConvention::ARM64EC_Thunk_X64);
        call_conventions.insert(
            b"ARM64ECThunkNative",
            LLVMCallConvention::ARM64EC_Thunk_Native,
        );
        call_conventions.insert(
            b"AArch64SMEABISupportRoutinesPreserveMostFrom_X1",
            LLVMCallConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X1,
        );
        call_conventions.insert(b"RISCV_VLSCall_32", LLVMCallConvention::RISCV_VLSCall_32);
        call_conventions.insert(b"RISCV_VLSCall_64", LLVMCallConvention::RISCV_VLSCall_64);
        call_conventions.insert(b"RISCV_VLSCall_128", LLVMCallConvention::RISCV_VLSCall_128);
        call_conventions.insert(b"RISCV_VLSCall_256", LLVMCallConvention::RISCV_VLSCall_256);
        call_conventions.insert(b"RISCV_VLSCall_512", LLVMCallConvention::RISCV_VLSCall_512);
        call_conventions.insert(
            b"RISCV_VLSCall_1024",
            LLVMCallConvention::RISCV_VLSCall_1024,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_2048",
            LLVMCallConvention::RISCV_VLSCall_2048,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_4096",
            LLVMCallConvention::RISCV_VLSCall_4096,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_8192",
            LLVMCallConvention::RISCV_VLSCall_8192,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_16384",
            LLVMCallConvention::RISCV_VLSCall_16384,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_32768",
            LLVMCallConvention::RISCV_VLSCall_32768,
        );
        call_conventions.insert(
            b"RISCV_VLSCall_65536",
            LLVMCallConvention::RISCV_VLSCall_65536,
        );
        call_conventions.insert(
            b"AMDGPU_Gfx_WholeWave",
            LLVMCallConvention::AMDGPU_Gfx_WholeWave,
        );
        call_conventions.insert(
            b"CHERIoT_CompartmentCall",
            LLVMCallConvention::CHERIoT_CompartmentCall,
        );
        call_conventions.insert(
            b"CHERIoTCompartmentCallee",
            LLVMCallConvention::CHERIoT_CompartmentCallee,
        );
        call_conventions.insert(
            b"CHERIoTLibraryCall",
            LLVMCallConvention::CHERIoT_LibraryCall,
        );

        call_conventions
    };
}

#[inline]
pub fn get_call_convention(name: &[u8]) -> LLVMCallConvention {
    CALL_CONVENTIONS
        .get(name)
        .copied()
        .unwrap_or(LLVMCallConvention::Standard)
}
