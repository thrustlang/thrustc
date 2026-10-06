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
pub enum Location {
    LValue,
    RValue,
    CallArg,
    AddressOf,
}

impl Location {
    #[inline]
    pub fn is_direct(&self) -> bool {
        matches!(self, Location::LValue)
    }

    #[inline]
    pub fn is_load(&self) -> bool {
        matches!(self, Location::RValue | Location::CallArg)
    }

    #[inline]
    pub fn is_address_of(&self) -> bool {
        matches!(self, Location::AddressOf)
    }
}

impl Default for Location {
    #[inline]
    fn default() -> Self {
        Location::RValue
    }
}
