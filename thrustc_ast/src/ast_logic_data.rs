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

use thrustc_span::Span;
use thrustc_typesystem::{Type, type_modificators::StructureTypeModificator};

use crate::Ast;

pub type StructureData<'ctx> = (
    &'ctx str,
    Vec<(&'ctx str, Type, u32, Span)>,
    StructureTypeModificator,
    Span,
);

pub type StructureDataFields<'ctx> = Vec<(&'ctx str, Type, u32, Span)>;
pub type StructDataField<'ctx> = (usize, &'ctx (&'ctx str, Type, u32, Span));

pub type EnumData<'ctx> = Vec<(&'ctx str, Type, Ast<'ctx>)>;
pub type EnumDataField<'ctx> = (&'ctx str, Type, Ast<'ctx>);

pub type ConstructorData<'ctx> = Vec<(&'ctx str, Ast<'ctx>, Type, u32)>;

pub type PropertyData = Vec<(Type, (Type, u32))>;
pub type PropertyDataField = (Type, (Type, u32));
pub type PropertyDataBaseField = (Type, u32);
