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

use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};

use crate::{Modificator, Modificators, traits::ModificatorsExtensions};

impl ModificatorsExtensions for Modificators {
    #[inline]
    fn get_atomic_ordering_modificator(&self) -> Option<ThrustAtomicOrdering> {
        self.iter().find_map(|m| match m {
            Modificator::AtomicOrdering(atomic_ord) => Some(*atomic_ord),
            _ => None,
        })
    }

    #[inline]
    fn get_thread_mode_modificator(&self) -> Option<ThrustThreadMode> {
        self.iter().find_map(|m| match m {
            Modificator::ThreadMode(thread_mode) => Some(*thread_mode),
            _ => None,
        })
    }

    #[inline]
    fn has_lazythread_modificator(&self) -> bool {
        self.iter().any(|m| m.is_lazy_thread())
    }

    #[inline]
    fn has_volatile_modificator(&self) -> bool {
        self.iter().any(|m| m.is_volatile())
    }
}
