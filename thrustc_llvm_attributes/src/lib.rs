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

use ahash::{HashMap, HashMapExt};
use inkwell::module::Linkage;

use thrustc_attributes::{ThrustAttribute, ThrustAttributes};
use thrustc_llvm_call_conventions::LLVMCallConvention;
use thrustc_typesystem::Type;

pub mod impls;
pub mod traits;

pub type LLVMAttributes<'ctx> = Vec<LLVMAttribute<'ctx>>;

#[derive(Debug, Clone)]
pub enum LLVMAttribute<'ctx> {
    // Function Attributes
    Extern(&'ctx str),
    Convention(LLVMCallConvention),
    Linkage(Linkage),
    Public,
    Ignore,
    Hot,
    NoInline,
    InlineHint,
    MinSize,
    AlwaysInline,
    SafeStack,
    StrongStack,
    WeakStack,
    PreciseFloats,
    NoUnwind,
    OptFuzzing,
    Align(u64),
    Pure,
    Thunk,
    Promote(&'ctx HashMap<Type, Type>),

    // LLVM Structure Modificator
    Packed,

    // Memory Management
    Stack,
    Heap,

    // Assembler Attributes
    AsmThrow,
    AsmSyntax(&'ctx str),
    AsmAlignStack,
    AsmSideEffects,

    Constructor,
    Destructor,

    // Nvidia Cuda
    Cuda,
}

impl LLVMAttribute<'_> {
    #[inline]
    pub fn is_extern_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Extern(..))
    }

    #[inline]
    pub fn is_hot_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Hot)
    }

    #[inline]
    pub fn is_ignore_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Ignore)
    }

    #[inline]
    pub fn is_public_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Public)
    }

    #[inline]
    pub fn is_noinline_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::NoInline)
    }

    #[inline]
    pub fn is_inline_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::InlineHint)
    }

    #[inline]
    pub fn is_alwaysinline_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::AlwaysInline)
    }

    #[inline]
    pub fn is_minsize_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::MinSize)
    }

    #[inline]
    pub fn is_heap_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Heap)
    }

    #[inline]
    pub fn is_asmsideeffects_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::AsmSideEffects)
    }

    #[inline]
    pub fn is_asmthrow_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::AsmThrow)
    }

    #[inline]
    pub fn is_asmalingstack_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::AsmAlignStack)
    }

    #[inline]
    pub fn is_asmsyntax_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::AsmSyntax(..))
    }

    #[inline]
    pub fn is_packed_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Packed)
    }

    #[inline]
    pub fn is_linkage_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Linkage(..))
    }

    #[inline]
    pub fn is_constructor_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Constructor)
    }

    #[inline]
    pub fn is_destructor_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Destructor)
    }

    #[inline]
    pub fn is_cuda_attribute(&self) -> bool {
        matches!(self, LLVMAttribute::Cuda)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum LLVMAttributeComparator {
    Extern,
    Convention,
    Public,
    Ignore,
    Hot,
    NoInline,
    InlineHint,
    MinSize,
    AlwaysInline,
    SafeStack,
    StrongStack,
    WeakStack,
    PreciseFloats,
    NoUnwind,
    OptFuzzing,
    Align,
    Linkage,
    Pure,
    Thunk,
    Promote,

    Packed,

    Stack,
    Heap,

    AsmThrow,
    AsmSyntax,
    AsmAlignStack,
    AsmSideEffects,

    Constructor,
    Destructor,

    Cuda,
}

#[inline]
pub fn into_llvm_attribute(attribute: &ThrustAttribute) -> LLVMAttribute<'_> {
    match attribute {
        ThrustAttribute::Extern(external_name, ..) => LLVMAttribute::Extern(external_name),
        ThrustAttribute::Linkage(linkage, ..) => LLVMAttribute::Linkage(linkage.get_llvm_linkage()),
        ThrustAttribute::Convention(name, ..) => LLVMAttribute::Convention(
            thrustc_llvm_call_conventions::get_call_convention(name.as_bytes()),
        ),
        ThrustAttribute::Public(..) => LLVMAttribute::Public,
        ThrustAttribute::Ignore(..) => LLVMAttribute::Ignore,
        ThrustAttribute::Hot(..) => LLVMAttribute::Hot,
        ThrustAttribute::NoInline(..) => LLVMAttribute::NoInline,
        ThrustAttribute::InlineHint(..) => LLVMAttribute::InlineHint,
        ThrustAttribute::MinSize(..) => LLVMAttribute::MinSize,
        ThrustAttribute::AlwaysInline(..) => LLVMAttribute::AlwaysInline,
        ThrustAttribute::SafeStack(..) => LLVMAttribute::SafeStack,
        ThrustAttribute::StrongStack(..) => LLVMAttribute::StrongStack,
        ThrustAttribute::WeakStack(..) => LLVMAttribute::WeakStack,
        ThrustAttribute::PreciseFloats(..) => LLVMAttribute::PreciseFloats,
        ThrustAttribute::AsmThrow(..) => LLVMAttribute::AsmThrow,
        ThrustAttribute::AsmSyntax(syntax, ..) => LLVMAttribute::AsmSyntax(syntax),
        ThrustAttribute::AsmSideEffects(..) => LLVMAttribute::AsmSideEffects,
        ThrustAttribute::AsmAlignStack(..) => LLVMAttribute::AsmAlignStack,
        ThrustAttribute::Stack(..) => LLVMAttribute::Stack,
        ThrustAttribute::Heap(..) => LLVMAttribute::Heap,
        ThrustAttribute::Packed(..) => LLVMAttribute::Packed,
        ThrustAttribute::NoUnwind(..) => LLVMAttribute::NoUnwind,
        ThrustAttribute::OptFuzzing(..) => LLVMAttribute::OptFuzzing,
        ThrustAttribute::Align(align, ..) => LLVMAttribute::Align(*align),
        ThrustAttribute::Pure(..) => LLVMAttribute::Pure,
        ThrustAttribute::Thunk(..) => LLVMAttribute::Thunk,
        ThrustAttribute::Constructor(..) => LLVMAttribute::Constructor,
        ThrustAttribute::Destructor(..) => LLVMAttribute::Destructor,
        ThrustAttribute::Promote(promote, ..) => LLVMAttribute::Promote(promote),
        ThrustAttribute::Cuda(..) => LLVMAttribute::Cuda,
    }
}

pub fn into_llvm_attributes(thrust_attributes: &ThrustAttributes) -> Vec<LLVMAttribute<'_>> {
    let mut llvm_attributes: Vec<LLVMAttribute<'_>> = Vec::with_capacity(thrust_attributes.len());

    for attribute in thrust_attributes.iter() {
        llvm_attributes.push(self::into_llvm_attribute(attribute));
    }

    llvm_attributes
}

pub fn interpret_as_callconvention(llvm_attribute: &LLVMAttribute) -> Option<u32> {
    let mut attributes_call_conventions: HashMap<&'static str, LLVMCallConvention> = HashMap::new();

    attributes_call_conventions.insert("cuda", LLVMCallConvention::PTX_Kernel);

    if llvm_attribute.is_cuda_attribute() {
        if let Some(cuda_call_conv) = attributes_call_conventions.get("cuda") {
            return Some((*cuda_call_conv) as u32);
        }
    }

    None
}
