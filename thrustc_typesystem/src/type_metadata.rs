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

use crate::{Type, type_modificators::StructureTypeModificator};

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ArrayTypeMetadata {
    infered_size_of_type: Option<std::boxed::Box<Type>>,
    address_space: Option<u16>,
}

impl ArrayTypeMetadata {
    pub fn new(
        infered_size_of_type: Option<std::boxed::Box<Type>>,
        address_space: Option<u16>,
    ) -> Self {
        Self {
            infered_size_of_type,
            address_space,
        }
    }
}

impl ArrayTypeMetadata {
    #[inline]
    pub fn get_infered_size_of_type(&self) -> Option<&Type> {
        self.infered_size_of_type.as_deref()
    }

    #[inline]
    pub fn get_address_space(&self) -> Option<u16> {
        self.address_space
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FixedArrayTypeMetadata {
    address_space: Option<u16>,
}

impl FixedArrayTypeMetadata {
    pub fn new(address_space: Option<u16>) -> Self {
        Self { address_space }
    }
}

impl FixedArrayTypeMetadata {
    #[inline]
    pub fn get_address_space(&self) -> Option<u16> {
        self.address_space
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash, Default)]
pub struct StructTypeMetadata {
    type_modificator: StructureTypeModificator,
}

impl StructTypeMetadata {
    pub fn new(type_modificator: StructureTypeModificator) -> Self {
        Self { type_modificator }
    }
}

impl StructTypeMetadata {
    #[inline]
    pub fn get_struct_type_modificator(&self) -> &StructureTypeModificator {
        &self.type_modificator
    }
}

impl std::fmt::Display for StructTypeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_modificator)
    }
}
