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

#[derive(Debug, Clone, Copy)]
pub struct ScoperContext {
    loop_depth: u32,
    inside_function: bool,
    node_depth: u32,
}

impl ScoperContext {
    #[inline]
    pub fn new() -> Self {
        ScoperContext {
            loop_depth: 0,
            inside_function: false,
            node_depth: 0,
        }
    }
}

impl ScoperContext {
    #[inline]
    pub fn enter_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_add(1);
    }

    #[inline]
    pub fn leave_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_sub(1);
    }

    #[inline]
    pub fn enter_function(&mut self) {
        self.inside_function = true;
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

    #[inline]
    pub fn leave_function(&mut self) {
        self.inside_function = false;
    }
}

impl ScoperContext {
    #[inline]
    pub fn is_inside_loop(&self) -> bool {
        self.loop_depth > 0
    }

    #[inline]
    pub fn is_inside_function(&self) -> bool {
        self.inside_function
    }

    #[inline]
    pub fn get_node_depth(&self) -> u32 {
        self.node_depth
    }
}
