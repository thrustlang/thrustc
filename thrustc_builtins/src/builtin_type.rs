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
    name: &'static str,
    ty: Type,
}

impl BuiltinTypeInfo {
    #[inline]
    pub fn new(name: &'static str, ty: Type) -> Self {
        Self { name, ty }
    }
}

impl BuiltinTypeInfo {
    #[inline]
    pub fn get_name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub fn get_ty(&self) -> &Type {
        &self.ty
    }
}

impl BuiltinTypeInfo {
    #[inline]
    pub fn get_mut_name(&mut self) -> &mut &'static str {
        &mut self.name
    }

    #[inline]
    pub fn get_mut_ty(&mut self) -> &mut Type {
        &mut self.ty
    }
}

impl BuiltinTypeInfo {
    #[inline]
    pub fn set_name(&mut self, name: &'static str) {
        self.name = name;
    }

    #[inline]
    pub fn set_ty(&mut self, ty: Type) {
        self.ty = ty;
    }
}
