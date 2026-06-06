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

use crate::{
    Type,
    traits::{DereferenceExtensions, TypeExtensions},
};

impl DereferenceExtensions for Type {
    #[inline]
    fn dereference(&self) -> Type {
        if let Type::Ptr {
            subtype: Some(any), ..
        } = self
        {
            return (**any).clone();
        }

        self.clone()
    }

    #[inline]
    fn dereference_until_value(&self) -> Type {
        if let Type::Ptr {
            subtype: Some(any), ..
        } = self
        {
            return any.dereference_until_value();
        }

        if self.is_value() {
            return self.clone();
        }

        self.clone()
    }
}
