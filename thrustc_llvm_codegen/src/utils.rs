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

use std::ffi::CString;

use ahash::AHashSet as HashSet;
use inkwell::values::{
    AsValueRef, CallSiteValue, FunctionValue, InstructionOpcode, InstructionValue,
};

use crate::context::LLVMCodeGenContext;

pub const SHORT_RANGE_OBFUSCATION: std::ops::RangeInclusive<usize> = 5..=12;
pub const LONG_RANGE_OBFUSCATION: std::ops::RangeInclusive<usize> = 10..=30;

#[inline]
#[must_use]
pub fn generate_string(
    context: &LLVMCodeGenContext<'_, '_>,
    range: std::ops::RangeInclusive<usize>,
) -> String {
    if !context.get_file_options().obfuscate_ir() {
        String::new()
    } else {
        let length: usize = fastrand::usize(range);
        let mut random_string: String = String::with_capacity(length);

        for _ in 0..length {
            let n: u8 = fastrand::u8(0..52);

            let c: char = match n {
                0..=25 => (b'A' + n) as char,
                26..=51 => (b'a' + (n - 26)) as char,
                _ => '_',
            };

            random_string.push(c);
        }

        random_string
    }
}

pub fn clean_llvm_name(name: &std::ffi::CStr) -> std::borrow::Cow<'_, str> {
    let s: std::borrow::Cow<'_, str> = name.to_string_lossy();

    if let Some(dot_pos) = s.rfind('.') {
        let suffix = &s[dot_pos + 1..];
        if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()) {
            return std::borrow::Cow::Owned(s[..dot_pos].to_string());
        }
    }

    s
}

pub fn get_functions_by_ordered_calls<'ctx>(
    functions: Vec<FunctionValue<'ctx>>,
) -> Vec<FunctionValue<'ctx>> {
    let mut ordered: Vec<FunctionValue> = Vec::with_capacity(functions.len());
    let mut visited: HashSet<CString> = HashSet::with_capacity(functions.len());

    let allowed: HashSet<CString> = functions.iter().map(|f| f.get_name().to_owned()).collect();

    for function in &functions {
        dfs_post_order(function, &allowed, &mut visited, &mut ordered);
    }

    ordered
}

fn dfs_post_order<'ctx>(
    function: &FunctionValue<'ctx>,
    allowed: &HashSet<CString>,
    visited: &mut HashSet<CString>,
    ordered: &mut Vec<FunctionValue<'ctx>>,
) {
    let name: CString = function.get_name().to_owned();

    if visited.contains(&name) {
        return;
    }

    visited.insert(name.clone());

    for basic_block in function.get_basic_blocks() {
        let mut instr: Option<InstructionValue<'_>> = basic_block.get_first_instruction();

        while let Some(current_instr) = instr {
            if current_instr.get_opcode() == InstructionOpcode::Call {
                let callsite: CallSiteValue<'_> =
                    unsafe { CallSiteValue::new(current_instr.as_value_ref()) };
                let called_fn: FunctionValue<'_> = callsite.get_called_fn_value();

                let called_name: CString = called_fn.get_name().to_owned();

                if allowed.contains(&called_name) {
                    dfs_post_order(&called_fn, allowed, visited, ordered);
                }
            }

            instr = current_instr.get_next_instruction();
        }
    }

    ordered.push(*function);
}
