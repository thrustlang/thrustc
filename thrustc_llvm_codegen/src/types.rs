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

use ahash::{AHashMap as HashMap, AHashSet as HashSet};
use inkwell::values::{FunctionValue, PointerValue};
use thrustc_llvm_abi::LLVMABIConfiguration;
use thrustc_span::Span;
use thrustc_typesystem::Type;

use crate::memory::SymbolAllocated;

pub type LLVMGlobalConstants<'ctx> = HashMap<&'ctx str, SymbolAllocated<'ctx>>;
pub type LLVMLocalConstants<'ctx> = Vec<HashMap<&'ctx str, SymbolAllocated<'ctx>>>;

pub type LLVMGlobalStatics<'ctx> = HashMap<&'ctx str, SymbolAllocated<'ctx>>;
pub type LLVMLocalStatics<'ctx> = Vec<HashMap<&'ctx str, SymbolAllocated<'ctx>>>;

pub type LLVMDBGFunction<'ctx> = (
    String,
    FunctionValue<'ctx>,
    &'ctx Type,
    Vec<Type>,
    bool,
    bool,
    Span,
);

pub type LLVMFunction<'ctx> = (
    FunctionValue<'ctx>,
    &'ctx Type,
    &'ctx [Type],
    u32,
    Option<LLVMABIConfiguration<'ctx>>,
    bool,
    Span,
);
pub type LLVMFunctions<'ctx> = HashMap<&'ctx str, LLVMFunction<'ctx>>;

pub type LLVMInstructions<'ctx> = Vec<HashMap<&'ctx str, SymbolAllocated<'ctx>>>;

pub type LLVMAllocatedParameters<'ctx> = HashMap<&'ctx str, SymbolAllocated<'ctx>>;
pub type LLVMFunctionsParameters<'ctx> = HashMap<&'ctx str, SymbolAllocated<'ctx>>;

pub type LLVMCtors<'ctx> = HashSet<(PointerValue<'ctx>, u32)>;
pub type LLVMDtors<'ctx> = HashSet<(PointerValue<'ctx>, u32)>;
pub type LLVMStackProtectorPointer<'ctx> = PointerValue<'ctx>;
