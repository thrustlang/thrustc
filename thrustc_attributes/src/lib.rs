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

use ahash::HashMap;
use serde::Serialize;
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    linkage::ThrustLinkage,
    traits::{ThrustAttributeComparatorExtensions, ThrustAttributesExtensions},
};

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

pub mod assembler;
pub mod callconventions;
mod impls;
pub mod linkage;
pub mod traits;

pub type ThrustAttributes = Vec<ThrustAttribute>;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum ThrustAttribute {
    Extern(String, Span),
    Convention(String, Span),
    Linkage(ThrustLinkage, String, Span),
    Public(Span),
    Ignore(Span),
    Hot(Span),
    NoInline(Span),
    InlineHint(Span),
    MinSize(Span),
    AlwaysInline(Span),
    SafeStack(Span),
    StrongStack(Span),
    WeakStack(Span),
    PreciseFloats(Span),
    NoUnwind(Span),
    NoReturn(Span),
    OptFuzzing(Span),
    Align(u64, Span),
    Pure(Span),
    Thunk(Span),

    // LLVM Structure Modificator
    Packed(Span),

    // Memory Management
    Stack(Span),
    Heap(Span),

    AsmThrow(Span),
    AsmSyntax(String, Span),
    AsmAlignStack(Span),
    AsmSideEffects(Span),

    //Ctors & Dtors
    Constructor(Span),
    Destructor(Span),

    // Nvidia Cuda
    Cuda(Span),

    // Va Args type auto promotion,
    Promote(HashMap<Type, Type>, Span),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ThrustAttributeComparator {
    Extern,
    Convention,
    Linkage,
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
    NoReturn,
    OptFuzzing,
    Align,
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

impl ThrustAttribute {
    #[inline]
    pub fn is_extern_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Extern(..))
    }

    #[inline]
    pub fn is_hot_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Hot(..))
    }

    #[inline]
    pub fn is_ignore_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Ignore(..))
    }

    #[inline]
    pub fn is_public_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Public(..))
    }

    #[inline]
    pub fn is_noinline_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::NoInline(..))
    }

    #[inline]
    pub fn is_inline_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::InlineHint(..))
    }

    #[inline]
    pub fn is_alwaysinline_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::AlwaysInline(..))
    }

    #[inline]
    pub fn is_minsize_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::MinSize(..))
    }

    #[inline]
    pub fn is_heap_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Heap(..))
    }

    #[inline]
    pub fn is_asmsideeffects_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::AsmSideEffects(..))
    }

    #[inline]
    pub fn is_asmthrow_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::AsmThrow(..))
    }

    #[inline]
    pub fn is_asmalingstack_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::AsmAlignStack(..))
    }

    #[inline]
    pub fn is_asmsyntax_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::AsmSyntax(..))
    }

    #[inline]
    pub fn is_packed(&self) -> bool {
        matches!(self, ThrustAttribute::Packed(..))
    }

    #[inline]
    pub fn is_linkage_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Linkage(..))
    }

    #[inline]
    pub fn is_conv_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Convention(..))
    }

    #[inline]
    pub fn is_constructor_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Constructor(..))
    }

    #[inline]
    pub fn is_destructor_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Destructor(..))
    }

    #[inline]
    pub fn is_align_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Align(..))
    }

    #[inline]
    pub fn is_cuda_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::Cuda(..))
    }

    #[inline]
    pub fn is_noreturn_attribute(&self) -> bool {
        matches!(self, ThrustAttribute::NoReturn(..))
    }
}

impl ThrustAttribute {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            ThrustAttribute::Extern(_, span) => *span,
            ThrustAttribute::Convention(_, span) => *span,
            ThrustAttribute::Linkage(.., span) => *span,
            ThrustAttribute::Public(span) => *span,
            ThrustAttribute::Ignore(span) => *span,
            ThrustAttribute::Hot(span) => *span,
            ThrustAttribute::NoInline(span) => *span,
            ThrustAttribute::InlineHint(span) => *span,
            ThrustAttribute::MinSize(span) => *span,
            ThrustAttribute::AlwaysInline(span) => *span,
            ThrustAttribute::SafeStack(span) => *span,
            ThrustAttribute::StrongStack(span) => *span,
            ThrustAttribute::WeakStack(span) => *span,
            ThrustAttribute::PreciseFloats(span) => *span,
            ThrustAttribute::AsmThrow(span) => *span,
            ThrustAttribute::AsmSyntax(_, span) => *span,
            ThrustAttribute::AsmSideEffects(span) => *span,
            ThrustAttribute::AsmAlignStack(span) => *span,
            ThrustAttribute::Stack(span) => *span,
            ThrustAttribute::Heap(span) => *span,
            ThrustAttribute::Packed(span) => *span,
            ThrustAttribute::NoUnwind(span) => *span,
            ThrustAttribute::NoReturn(span) => *span,
            ThrustAttribute::OptFuzzing(span) => *span,
            ThrustAttribute::Align(_, span) => *span,
            ThrustAttribute::Pure(span) => *span,
            ThrustAttribute::Thunk(span) => *span,
            ThrustAttribute::Constructor(span) => *span,
            ThrustAttribute::Destructor(span) => *span,
            ThrustAttribute::Cuda(span) => *span,
            ThrustAttribute::Promote(_, span) => *span,
        }
    }
}

#[must_use]
pub fn as_attribute(token_type: TokenType, span: Span) -> Option<ThrustAttribute> {
    match token_type {
        TokenType::Ignore => Some(ThrustAttribute::Ignore(span)),
        TokenType::MinSize => Some(ThrustAttribute::MinSize(span)),
        TokenType::NoInline => Some(ThrustAttribute::NoInline(span)),
        TokenType::AlwaysInline => Some(ThrustAttribute::AlwaysInline(span)),
        TokenType::InlineHint => Some(ThrustAttribute::InlineHint(span)),
        TokenType::Hot => Some(ThrustAttribute::Hot(span)),
        TokenType::SafeStack => Some(ThrustAttribute::SafeStack(span)),
        TokenType::WeakStack => Some(ThrustAttribute::WeakStack(span)),
        TokenType::StrongStack => Some(ThrustAttribute::StrongStack(span)),
        TokenType::PreciseFloats => Some(ThrustAttribute::PreciseFloats(span)),
        TokenType::Heap => Some(ThrustAttribute::Heap(span)),
        TokenType::AsmThrow => Some(ThrustAttribute::AsmThrow(span)),
        TokenType::AsmSideEffects => Some(ThrustAttribute::AsmSideEffects(span)),
        TokenType::AsmAlignStack => Some(ThrustAttribute::AsmAlignStack(span)),
        TokenType::Packed => Some(ThrustAttribute::Packed(span)),
        TokenType::NoUnwind => Some(ThrustAttribute::NoUnwind(span)),
        TokenType::NoReturn => Some(ThrustAttribute::NoReturn(span)),
        TokenType::OptFuzzing => Some(ThrustAttribute::OptFuzzing(span)),
        TokenType::Pure => Some(ThrustAttribute::Pure(span)),
        TokenType::Thunk => Some(ThrustAttribute::Thunk(span)),
        TokenType::Constructor => Some(ThrustAttribute::Constructor(span)),
        TokenType::Destructor => Some(ThrustAttribute::Destructor(span)),
        TokenType::Cuda => Some(ThrustAttribute::Cuda(span)),

        _ => None,
    }
}

impl ThrustAttributesExtensions for ThrustAttributes {
    fn has_align_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_align_attribute())
    }

    #[inline]
    fn has_linkage_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_linkage_attribute())
    }

    #[inline]
    fn has_extern_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_extern_attribute())
    }

    #[inline]
    fn has_ignore_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_ignore_attribute())
    }

    #[inline]
    fn has_heap_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_heap_attribute())
    }

    #[inline]
    fn has_public_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_public_attribute())
    }

    #[inline]
    fn has_hot_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_hot_attribute())
    }

    #[inline]
    fn has_inline_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_inline_attribute())
    }

    #[inline]
    fn has_minsize_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_minsize_attribute())
    }

    #[inline]
    fn has_inlinealways_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_alwaysinline_attribute())
    }

    #[inline]
    fn has_noinline_attr(&self) -> bool {
        self.iter().any(|attr| attr.is_noinline_attribute())
    }

    #[inline]
    fn has_asmalignstack_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_asmalingstack_attribute())
    }

    #[inline]
    fn has_asmsideffects_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_asmsideeffects_attribute())
    }

    #[inline]
    fn has_asmthrow_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_asmthrow_attribute())
    }

    #[inline]
    fn has_asmsyntax_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_asmsyntax_attribute())
    }

    #[inline]
    fn has_convention_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_conv_attribute())
    }

    #[inline]
    fn has_constructor_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_constructor_attribute())
    }

    #[inline]
    fn has_destructor_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_destructor_attribute())
    }

    #[inline]
    fn has_cuda_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_cuda_attribute())
    }

    #[inline]
    fn has_noreturn_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_noreturn_attribute())
    }

    #[inline]
    fn match_attr(&self, cmp: ThrustAttributeComparator) -> Option<Span> {
        if let Some(attr_found) = self.iter().find(|attr| attr.as_attr_cmp() == cmp) {
            return Some(attr_found.get_span());
        }

        None
    }

    #[inline]
    fn get_attr(&self, cmp: ThrustAttributeComparator) -> Option<ThrustAttribute> {
        if let Some(attr_found) = self.iter().find(|attr| attr.as_attr_cmp() == cmp) {
            return Some(attr_found.clone());
        }

        None
    }
}

impl ThrustAttributeComparatorExtensions for ThrustAttribute {
    #[inline]
    fn as_attr_cmp(&self) -> ThrustAttributeComparator {
        match self {
            ThrustAttribute::Extern(..) => ThrustAttributeComparator::Extern,
            ThrustAttribute::Convention(..) => ThrustAttributeComparator::Convention,
            ThrustAttribute::Linkage(..) => ThrustAttributeComparator::Linkage,
            ThrustAttribute::Stack(..) => ThrustAttributeComparator::Stack,
            ThrustAttribute::Heap(..) => ThrustAttributeComparator::Heap,
            ThrustAttribute::Public(..) => ThrustAttributeComparator::Public,
            ThrustAttribute::Ignore(..) => ThrustAttributeComparator::Ignore,
            ThrustAttribute::Hot(..) => ThrustAttributeComparator::Hot,
            ThrustAttribute::NoInline(..) => ThrustAttributeComparator::NoInline,
            ThrustAttribute::InlineHint(..) => ThrustAttributeComparator::InlineHint,
            ThrustAttribute::MinSize(..) => ThrustAttributeComparator::MinSize,
            ThrustAttribute::AlwaysInline(..) => ThrustAttributeComparator::AlwaysInline,
            ThrustAttribute::SafeStack(_) => ThrustAttributeComparator::SafeStack,
            ThrustAttribute::StrongStack(..) => ThrustAttributeComparator::StrongStack,
            ThrustAttribute::WeakStack(..) => ThrustAttributeComparator::WeakStack,
            ThrustAttribute::PreciseFloats(..) => ThrustAttributeComparator::PreciseFloats,
            ThrustAttribute::AsmAlignStack(..) => ThrustAttributeComparator::AsmAlignStack,
            ThrustAttribute::AsmSyntax(..) => ThrustAttributeComparator::AsmSyntax,
            ThrustAttribute::AsmThrow(..) => ThrustAttributeComparator::AsmThrow,
            ThrustAttribute::AsmSideEffects(..) => ThrustAttributeComparator::AsmSideEffects,
            ThrustAttribute::Packed(..) => ThrustAttributeComparator::Packed,
            ThrustAttribute::NoUnwind(..) => ThrustAttributeComparator::NoUnwind,
            ThrustAttribute::NoReturn(..) => ThrustAttributeComparator::NoReturn,
            ThrustAttribute::OptFuzzing(..) => ThrustAttributeComparator::OptFuzzing,
            ThrustAttribute::Align(..) => ThrustAttributeComparator::Align,
            ThrustAttribute::Pure(..) => ThrustAttributeComparator::Pure,
            ThrustAttribute::Thunk(..) => ThrustAttributeComparator::Thunk,
            ThrustAttribute::Constructor(..) => ThrustAttributeComparator::Constructor,
            ThrustAttribute::Destructor(..) => ThrustAttributeComparator::Destructor,
            ThrustAttribute::Promote(..) => ThrustAttributeComparator::Promote,
            ThrustAttribute::Cuda(..) => ThrustAttributeComparator::Cuda,
        }
    }
}
