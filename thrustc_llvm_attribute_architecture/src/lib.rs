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

use inkwell::intrinsics::Intrinsic;
use inkwell::llvm_sys::core::{LLVMGetCalledValue, LLVMIsAFunction, LLVMIsAInlineAsm};
use inkwell::module::Module;
use inkwell::values::{AsValueRef, CallSiteValue, FunctionValue};

use thrustc_llvm_call_conventions::LLVMCallConvention;
use thrustc_llvm_target_triple::{LLVMARMABI, LLVMTargetTriple};

#[derive(Debug)]
pub struct LLVMArchitectureAttribute;

impl LLVMArchitectureAttribute {
    pub fn apply(module: &Module, target_triple: &LLVMTargetTriple) {
        let arm_abi: Option<LLVMARMABI> = target_triple.get_arm_abi();

        let Some(arm_abi) = arm_abi else {
            return;
        };

        let architecture_call_convention: u32 = match arm_abi {
            LLVMARMABI::APCS => LLVMCallConvention::ARM_APCS as u32,
            LLVMARMABI::AAPCS => LLVMCallConvention::ARM_AAPCS as u32,
            LLVMARMABI::AAPCSVFP => LLVMCallConvention::ARM_AAPCS_VFP as u32,
        };

        for function in module.get_functions() {
            let function_name = function.get_name();
            let function_name_lossy = function_name.to_string_lossy();
            let intrinsic: Option<Intrinsic> = Intrinsic::find(function_name_lossy.as_ref());

            let call_convention: u32 = function.get_call_conventions();

            if intrinsic.is_none() && call_convention == LLVMCallConvention::Standard as u32 {
                function.set_call_conventions(architecture_call_convention);
            }
        }

        for function in module.get_functions() {
            let basic_blocks = function.get_basic_block_iter();

            for basic_block in basic_blocks {
                let instructions: inkwell::basic_block::InstructionIter<'_> =
                    basic_block.get_instructions();

                for instruction in instructions {
                    let callsite: CallSiteValue = match CallSiteValue::try_from(instruction) {
                        Ok(callsite) => callsite,
                        Err(_) => continue,
                    };

                    let called_value: *mut inkwell::llvm_sys::LLVMValue =
                        unsafe { LLVMGetCalledValue(callsite.as_value_ref()) };
                    let is_inline_assembler: bool =
                        !unsafe { LLVMIsAInlineAsm(called_value) }.is_null();

                    if is_inline_assembler {
                        continue;
                    }

                    let is_direct_call: bool = !unsafe { LLVMIsAFunction(called_value) }.is_null();

                    if is_direct_call {
                        let called_function: Option<FunctionValue> =
                            unsafe { FunctionValue::new(called_value) };

                        if let Some(called_function) = called_function {
                            let function_name = called_function.get_name();
                            let function_name_lossy = function_name.to_string_lossy();
                            let intrinsic: Option<Intrinsic> =
                                Intrinsic::find(function_name_lossy.as_ref());

                            if intrinsic.is_some() {
                                continue;
                            }

                            let call_convention: u32 = called_function.get_call_conventions();

                            callsite.set_call_convention(call_convention);
                        }
                    } else {
                        let call_convention: u32 = callsite.get_call_convention();

                        if call_convention == LLVMCallConvention::Standard as u32 {
                            callsite.set_call_convention(architecture_call_convention);
                        }
                    }
                }
            }
        }
    }
}
