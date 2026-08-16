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

#[derive(Debug)]
pub struct AnalyzerContext {
    global_assembler: bool,
    node_depth: u32,
}

impl AnalyzerContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            global_assembler: false,
            node_depth: 0,
        }
    }
}

impl AnalyzerContext {
    #[inline]
    pub fn set_has_global_assembler(&mut self) {
        self.global_assembler = true;
    }

    #[inline]
    pub fn enter_node(&mut self) {
        self.node_depth = self.node_depth.saturating_add(1);
    }

    #[inline]
    pub fn leave_node(&mut self) {
        self.node_depth = self.node_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_node_depth(&mut self) {
        self.node_depth = 0;
    }
}

impl AnalyzerContext {
    #[inline]
    pub fn has_global_assembler(&self) -> bool {
        self.global_assembler
    }

    #[inline]
    pub fn get_node_depth(&self) -> u32 {
        self.node_depth
    }
}
