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


use ahash::AHashMap as HashMap;
use thrustc_attributes::ThrustAttributes;
use thrustc_span::Span;
use thrustc_typesystem::Type;

pub type TypeCheckerLocal<'symbol> = (&'symbol Type, Span);
pub type TypeCheckerLocals<'symbol> = Vec<HashMap<&'symbol str, TypeCheckerLocal<'symbol>>>;

pub type TypeCheckerIntrinsic<'symbol> =
    (&'symbol Type, &'symbol [Type], &'symbol ThrustAttributes);
pub type TypeCheckerIntrinsics<'symbol> = HashMap<&'symbol str, TypeCheckerIntrinsic<'symbol>>;

pub type TypeCheckerAssemblerFunction<'symbol> =
    (&'symbol Type, &'symbol [Type], &'symbol ThrustAttributes);
pub type TypeCheckerAssemblerFunctions<'symbol> =
    HashMap<&'symbol str, TypeCheckerAssemblerFunction<'symbol>>;

pub type TypeCheckerFunction<'symbol> = (&'symbol Type, &'symbol [Type], &'symbol ThrustAttributes);
pub type TypeCheckerFunctions<'symbol> = HashMap<&'symbol str, TypeCheckerFunction<'symbol>>;
