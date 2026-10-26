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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Options {
    max_expansion_depth: usize,
    max_rescan_passes: usize,
}

impl Options {
    #[inline]
    pub fn new() -> Self {
        Self {
            max_expansion_depth: 64,
            max_rescan_passes: 128,
        }
    }
}

impl Options {
    #[inline]
    pub fn get_max_expansion_depth(&self) -> usize {
        self.max_expansion_depth
    }

    #[inline]
    pub fn get_max_rescan_passes(&self) -> usize {
        self.max_rescan_passes
    }
}

impl Options {
    #[inline]
    pub fn set_max_expansion_depth(&mut self, max_expansion_depth: usize) {
        self.max_expansion_depth = max_expansion_depth;
    }

    #[inline]
    pub fn set_max_rescan_passes(&mut self, max_rescan_passes: usize) {
        self.max_rescan_passes = max_rescan_passes;
    }
}
