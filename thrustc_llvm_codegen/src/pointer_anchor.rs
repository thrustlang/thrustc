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


use inkwell::values::PointerValue;

#[derive(Debug, Clone, Copy)]
pub struct PointerAnchor<'ctx> {
    pub pointer: PointerValue<'ctx>,
    pub triggered: bool,
}

impl<'ctx> PointerAnchor<'ctx> {
    #[inline]
    pub fn new(pointer: PointerValue<'ctx>, triggered: bool) -> PointerAnchor<'ctx> {
        Self { pointer, triggered }
    }
}

impl<'ctx> PointerAnchor<'ctx> {
    #[inline]
    pub fn get_pointer(&self) -> PointerValue<'ctx> {
        self.pointer
    }

    #[inline]
    pub fn is_triggered(&self) -> bool {
        self.triggered
    }
}
