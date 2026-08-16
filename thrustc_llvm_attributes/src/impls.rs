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

use inkwell::module::Linkage;

use crate::{
    LLVMAttribute, LLVMAttributeComparator, LLVMAttributes,
    traits::{LLVMAttributeComparatorExtensions, LLVMAttributesExtensions},
};

impl LLVMAttributesExtensions for LLVMAttributes<'_> {
    #[inline]
    fn has_extern_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_extern_attribute())
    }

    #[inline]
    fn has_linkage_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_linkage_attribute())
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
    fn has_cuda_attribute(&self) -> bool {
        self.iter().any(|attr| attr.is_cuda_attribute())
    }

    #[inline]
    fn get_attr(&self, cmp: LLVMAttributeComparator) -> Option<LLVMAttribute<'_>> {
        if let Some(attr_found) = self.iter().find(|attr| attr.as_llvm_attribute_cmp() == cmp) {
            return Some(attr_found.clone());
        }

        None
    }
}

impl LLVMAttributeComparatorExtensions for LLVMAttribute<'_> {
    fn as_llvm_attribute_cmp(&self) -> LLVMAttributeComparator {
        match self {
            LLVMAttribute::Extern(..) => LLVMAttributeComparator::Extern,
            LLVMAttribute::Linkage(..) => LLVMAttributeComparator::Linkage,
            LLVMAttribute::Convention(..) => LLVMAttributeComparator::Convention,
            LLVMAttribute::Stack => LLVMAttributeComparator::Stack,
            LLVMAttribute::Heap => LLVMAttributeComparator::Heap,
            LLVMAttribute::Public => LLVMAttributeComparator::Public,
            LLVMAttribute::Ignore => LLVMAttributeComparator::Ignore,
            LLVMAttribute::Hot => LLVMAttributeComparator::Hot,
            LLVMAttribute::NoInline => LLVMAttributeComparator::NoInline,
            LLVMAttribute::InlineHint => LLVMAttributeComparator::InlineHint,
            LLVMAttribute::MinSize => LLVMAttributeComparator::MinSize,
            LLVMAttribute::AlwaysInline => LLVMAttributeComparator::AlwaysInline,
            LLVMAttribute::SafeStack => LLVMAttributeComparator::SafeStack,
            LLVMAttribute::StrongStack => LLVMAttributeComparator::StrongStack,
            LLVMAttribute::WeakStack => LLVMAttributeComparator::WeakStack,
            LLVMAttribute::PreciseFloats => LLVMAttributeComparator::PreciseFloats,
            LLVMAttribute::AsmAlignStack => LLVMAttributeComparator::AsmAlignStack,
            LLVMAttribute::AsmSyntax(..) => LLVMAttributeComparator::AsmSyntax,
            LLVMAttribute::AsmThrow => LLVMAttributeComparator::AsmThrow,
            LLVMAttribute::AsmSideEffects => LLVMAttributeComparator::AsmSideEffects,
            LLVMAttribute::Packed => LLVMAttributeComparator::Packed,
            LLVMAttribute::NoUnwind => LLVMAttributeComparator::NoUnwind,
            LLVMAttribute::NoReturn => LLVMAttributeComparator::NoReturn,
            LLVMAttribute::OptFuzzing => LLVMAttributeComparator::OptFuzzing,
            LLVMAttribute::Align(..) => LLVMAttributeComparator::Align,
            LLVMAttribute::Pure => LLVMAttributeComparator::Pure,
            LLVMAttribute::Thunk => LLVMAttributeComparator::Thunk,
            LLVMAttribute::Constructor => LLVMAttributeComparator::Constructor,
            LLVMAttribute::Destructor => LLVMAttributeComparator::Destructor,
            LLVMAttribute::Cuda => LLVMAttributeComparator::Cuda,
            LLVMAttribute::Promote(..) => LLVMAttributeComparator::Promote,
        }
    }
}

impl std::fmt::Display for LLVMAttribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LLVMAttribute::AlwaysInline => write!(f, "@alwaysInline"),
            LLVMAttribute::NoInline => write!(f, "@noInline"),
            LLVMAttribute::InlineHint => write!(f, "@inline"),
            LLVMAttribute::Extern(name, ..) => write!(f, "@extern({})", name),
            LLVMAttribute::Linkage(linkage, ..) => {
                let display: &str = match *linkage {
                    Linkage::Appending => "Appending",
                    Linkage::Common => "Common",
                    Linkage::AvailableExternally => "AvailableExternally",
                    Linkage::External => "External",
                    Linkage::ExternalWeak => "ExternalWeak",
                    Linkage::Internal => "Internal",
                    Linkage::LinkOnceAny => "LinkOnceAny",
                    Linkage::LinkOnceODR => "LinkOnceODR",
                    Linkage::LinkOnceODRAutoHide => "LinkOnceODRAutoHide",
                    Linkage::Private => "Private",
                    Linkage::WeakAny => "WeakAny",
                    Linkage::WeakODR => "WeakODR",
                    Linkage::DLLExport => "DLLExport",
                    Linkage::DLLImport => "DLLImport",
                    Linkage::Ghost => "Ghost",
                    Linkage::LinkerPrivate => "LinkerPrivate",
                    Linkage::LinkerPrivateWeak => "LinkerPrivateWeak",
                };

                write!(f, "@linkage(\"{}\")", display)
            }
            LLVMAttribute::Convention(convention, ..) => {
                write!(f, "@convention(\"{}\")", convention)
            }
            LLVMAttribute::Stack => write!(f, "@stack"),
            LLVMAttribute::Heap => write!(f, "@heap"),
            LLVMAttribute::Public => write!(f, "@public"),
            LLVMAttribute::StrongStack => write!(f, "@strongStack"),
            LLVMAttribute::WeakStack => write!(f, "@weakStack"),
            LLVMAttribute::SafeStack => write!(f, "@safeStack"),
            LLVMAttribute::PreciseFloats => write!(f, "@preciseFloatingPoint"),
            LLVMAttribute::MinSize => write!(f, "@minSize"),
            LLVMAttribute::Hot => write!(f, "@hot"),
            LLVMAttribute::Ignore => write!(f, "@ignore"),
            LLVMAttribute::NoUnwind => write!(f, "@noUnwind"),
            LLVMAttribute::NoReturn => write!(f, "@noReturn"),
            LLVMAttribute::AsmThrow => write!(f, "@asmThrow"),
            LLVMAttribute::AsmSyntax(..) => write!(f, "@asmSyntax"),
            LLVMAttribute::AsmSideEffects => write!(f, "@asmEffects"),
            LLVMAttribute::AsmAlignStack => write!(f, "@asmAlignStack"),
            LLVMAttribute::Packed => write!(f, "@packed"),
            LLVMAttribute::OptFuzzing => write!(f, "@optFuzzing"),
            LLVMAttribute::Align(align) => write!(f, "@align({})", align),
            LLVMAttribute::Pure => write!(f, "@pure"),
            LLVMAttribute::Thunk => write!(f, "@thunk"),
            LLVMAttribute::Constructor => write!(f, "@constructor"),
            LLVMAttribute::Destructor => write!(f, "@destructor"),
            LLVMAttribute::Cuda => write!(f, "@cuda"),
            LLVMAttribute::Promote(promote) => {
                let type_displayed: Vec<String> = promote
                    .iter()
                    .map(|(from, target)| format!("{} -> {}", from, target))
                    .collect();

                write!(f, "@promote({})", type_displayed.join(", "))
            }
        }
    }
}
