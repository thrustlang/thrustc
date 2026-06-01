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
use thrustc_span::Span;

pub type LinterStaticInfo = (Span, bool, bool);
pub type LinterGlobalStatics<'linter> = HashMap<&'linter str, LinterStaticInfo>;
pub type LinterLocalStatics<'linter> = Vec<HashMap<&'linter str, LinterStaticInfo>>;

pub type LinterConstantInfo = (Span, bool);
pub type LinterGlobalConstants<'linter> = HashMap<&'linter str, LinterConstantInfo>;
pub type LinterLocalConstants<'linter> = Vec<HashMap<&'linter str, LinterConstantInfo>>;

pub type LinterLLIInfo<'symbol> = (Span, bool);
pub type LinterLLIs<'symbol> = Vec<HashMap<&'symbol str, LinterLLIInfo<'symbol>>>;

pub type LinterAssemblerFunctionInfo<'linter> = (Span, bool);
pub type LinterAssemblerFunctions<'linter> =
    HashMap<&'linter str, LinterAssemblerFunctionInfo<'linter>>;

pub type LinterFunctionInfo<'linter> = (Span, bool);
pub type LinterFunctions<'linter> = HashMap<&'linter str, LinterFunctionInfo<'linter>>;

pub type LinterIntrinsicInfo<'linter> = (Span, bool);
pub type LinterIntrinsics<'linter> = HashMap<&'linter str, LinterIntrinsicInfo<'linter>>;

pub type LinterLocalInfo = (Span, bool, bool);
pub type LinterLocals<'linter> = Vec<HashMap<&'linter str, LinterLocalInfo>>;

pub type LinterEnumFieldInfo = (Span, bool);

pub type LinterEnumsFieldsInfo<'linter> = (HashMap<&'linter str, LinterEnumFieldInfo>, Span, bool);
pub type LinterEnums<'linter> = HashMap<&'linter str, LinterEnumsFieldsInfo<'linter>>;

pub type LinterStructFieldInfo = (Span, bool);
pub type LinterStructFieldsInfo<'linter> =
    (HashMap<&'linter str, LinterStructFieldInfo>, Span, bool);
pub type LinterStructs<'linter> = HashMap<&'linter str, LinterStructFieldsInfo<'linter>>;

pub type LinterFunctionParameterInfo = (Span, bool, bool);
pub type LinterFunctionParameters<'linter> = HashMap<&'linter str, LinterFunctionParameterInfo>;
