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

#[derive(Debug, Clone, Copy)]
pub struct LLVMAtomicModificators {
    atomic_volatile: bool,
    atomic_ord: Option<AtomicOrdering>,
}

impl LLVMAtomicModificators {
    #[inline]
    pub fn new(atomic_volatile: bool, atomic_ord: Option<AtomicOrdering>) -> Self {
        Self {
            atomic_volatile,
            atomic_ord,
        }
    }
}

impl LLVMAtomicModificators {
    #[inline]
    pub fn get_atomic_volatile(&self) -> bool {
        self.atomic_volatile
    }

    #[inline]
    pub fn get_atomic_ord(&self) -> Option<AtomicOrdering> {
        self.atomic_ord
    }
}
