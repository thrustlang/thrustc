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

use thrustc_span::Span;

use crate::{ThrustAttribute, ThrustAttributeComparator};

pub trait ThrustAttributesExtensions {
    fn has_extern_attribute(&self) -> bool;
    fn has_linkage_attribute(&self) -> bool;
    fn has_ignore_attribute(&self) -> bool;
    fn has_public_attribute(&self) -> bool;
    fn has_hot_attr(&self) -> bool;
    fn has_inline_attr(&self) -> bool;
    fn has_noinline_attr(&self) -> bool;
    fn has_minsize_attr(&self) -> bool;
    fn has_inlinealways_attr(&self) -> bool;
    fn has_align_attribute(&self) -> bool;

    fn has_heap_attr(&self) -> bool;

    fn has_asmalignstack_attribute(&self) -> bool;
    fn has_asmthrow_attribute(&self) -> bool;
    fn has_asmsideffects_attribute(&self) -> bool;
    fn has_constructor_attribute(&self) -> bool;
    fn has_destructor_attribute(&self) -> bool;
    fn has_asmsyntax_attribute(&self) -> bool;
    fn has_convention_attribute(&self) -> bool;

    fn has_cuda_attribute(&self) -> bool;

    fn match_attr(&self, cmp: ThrustAttributeComparator) -> Option<Span>;
    fn get_attr(&self, cmp: ThrustAttributeComparator) -> Option<ThrustAttribute>;

    //fn as_llvm_attributes(&self) -> LLVMAttributes<'_>;
}

pub trait ThrustAttributeComparatorExtensions {
    fn as_attr_cmp(&self) -> ThrustAttributeComparator;
}
