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

mod impls;
pub mod traits;

use serde::Serialize;
use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

pub type Modificators = Vec<Modificator>;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub enum Modificator {
    ThreadMode(ThrustThreadMode),
    AtomicOrdering(ThrustAtomicOrdering),
    Volatile,
    LazyThread,

    None,
}

impl Modificator {
    pub fn is_thread_mode(&self) -> bool {
        matches!(self, Modificator::ThreadMode(..))
    }

    pub fn is_atomic_ordering(&self) -> bool {
        matches!(self, Modificator::AtomicOrdering(..))
    }

    pub fn is_volatile(&self) -> bool {
        matches!(self, Modificator::Volatile)
    }

    pub fn is_lazy_thread(&self) -> bool {
        matches!(self, Modificator::LazyThread)
    }
}
