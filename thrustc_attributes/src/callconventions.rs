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

pub const CALL_CONVENTIONS_AVAILABLE: &[&str; 74] = &[
    "C",
    "fast",
    "tail",
    "cold",
    "weakReg",
    "strongReg",
    "Swift",
    "Haskell",
    "Erlang",
    "GraalVM",
    "Win64",
    "X86StdCall",
    "X86FastCall",
    "X86ThisCall",
    "X86VectorCall",
    "X86RegCall",
    "X86_64_SysV",
    "ARMAPCS",
    "ARMAAPCS",
    "ARM_AAPCS_VFP",
    "AArch64VectorCall",
    "AArch64SVEVectorCall",
    "SwiftTail",
    "PreserveNone",
    "AnyReg",
    "PTXKernel",
    "PTXDevice",
    "AMDGPUKernel",
    "AMDGPUGfx",
    "RISCVVectorCall",
    "CPPFastTLS",
    "CFGuardCheck",
    "MSP430_INTR",
    "SPIRFunc",
    "SPIRKernel",
    "Intel_OCL_BI",
    "X86_INTR",
    "AVR_INTR",
    "AVR_SIGNAL",
    "AVR_BUILTIN",
    "AMDGPU_VS",
    "AMDGPU_GS",
    "AMDGPU_PS",
    "AMDGPU_CS",
    "AMDGPU_HS",
    "MSP430_BUILTIN",
    "AMDGPU_LS",
    "AMDGPU_ES",
    "WebAssembly",
    "M68k_INTR",
    "AArch64SMEABISupportRoutinesPreserveMostFromX0",
    "AArch64SMEABISupportRoutinesPreserveMostFromX2",
    "AMDGPUCSChain",
    "AMDGPUCSChainPreserve",
    "M68k_RTD",
    "ARM64ECThunkX64",
    "ARM64ECThunkNative",
    "AArch64SMEABISupportRoutinesPreserveMostFrom_X1",
    "RISCV_VLSCall_32",
    "RISCV_VLSCall_64",
    "RISCV_VLSCall_128",
    "RISCV_VLSCall_256",
    "RISCV_VLSCall_512",
    "RISCV_VLSCall_1024",
    "RISCV_VLSCall_2048",
    "RISCV_VLSCall_4096",
    "RISCV_VLSCall_8192",
    "RISCV_VLSCall_16384",
    "RISCV_VLSCall_32768",
    "RISCV_VLSCall_65536",
    "AMDGPU_Gfx_WholeWave",
    "CHERIoT_CompartmentCall",
    "CHERIoTCompartmentCallee",
    "CHERIoTLibraryCall",
];
