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

use inkwell::AtomicOrdering;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy, Serialize)]
pub enum ThrustAtomicOrdering {
    AtomicNone,
    AtomicFree,
    AtomicRelax,
    AtomicGrab,
    AtomicDrop,
    AtomicSync,
    AtomicStrict,
}

impl ThrustAtomicOrdering {
    #[inline]
    pub fn to_llvm(self) -> AtomicOrdering {
        match self {
            ThrustAtomicOrdering::AtomicNone => AtomicOrdering::NotAtomic,
            ThrustAtomicOrdering::AtomicFree => AtomicOrdering::Unordered,
            ThrustAtomicOrdering::AtomicRelax => AtomicOrdering::Monotonic,
            ThrustAtomicOrdering::AtomicGrab => AtomicOrdering::Acquire,
            ThrustAtomicOrdering::AtomicDrop => AtomicOrdering::Release,
            ThrustAtomicOrdering::AtomicSync => AtomicOrdering::AcquireRelease,
            ThrustAtomicOrdering::AtomicStrict => AtomicOrdering::SequentiallyConsistent,
        }
    }
}
