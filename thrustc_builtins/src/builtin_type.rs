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

use thrustc_typesystem::Type;

#[derive(Debug, Clone)]
pub struct BuiltinTypeInfo {
    pub name: &'static str,
    pub ty: Type,
}

impl BuiltinTypeInfo {
    #[inline]
    pub fn new(name: &'static str, ty: Type) -> Self {
        Self { name, ty }
    }
}
