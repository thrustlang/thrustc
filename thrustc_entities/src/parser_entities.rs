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

use thrustc_ast::{
    Ast,
    ast_logic_data::EnumData,
    ast_metadata::{FunctionParameterMetadata, LocalMetadata, StaticMetadata},
};
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};

use ahash::AHashMap as HashMap;

pub type Struct<'parser> = (
    &'parser str,
    Vec<(&'parser str, Type, u32, Span)>,
    ThrustAttributes,
    StructTypeMetadata,
    Span,
);

pub type Function<'parser> = (
    Type,
    FunctionParametersTypes,
    FunctionParameterNames<'parser>,
    bool,
);
pub type AssemblerFunction<'parser> = (Type, AssemblerFunctionParametersTypes, bool);
pub type Intrinsic<'parser> = (Type, IntrinsicParametersTypes, bool);

#[derive(Debug, Clone)]
pub struct FunctionParametersTypes(pub Vec<Type>);

#[derive(Debug, Clone)]
pub struct FunctionParameterNames<'parser>(pub Vec<&'parser str>);

#[derive(Debug, Clone)]
pub struct AssemblerFunctionParametersTypes(pub Vec<Type>);

#[derive(Debug, Clone)]
pub struct IntrinsicParametersTypes(pub Vec<Type>);

pub type FoundSymbolId<'parser> = (
    Option<(&'parser str, usize)>,
    Option<&'parser str>,
    Option<(&'parser str, usize)>,
    Option<(&'parser str, usize)>,
    Option<(&'parser str, usize)>,
    Option<(&'parser str, usize)>,
    Option<&'parser str>,
    Option<&'parser str>,
    Option<(&'parser str, usize)>,
    Option<(&'parser str, usize)>,
    Option<&'parser str>,
);

pub type CustomTypeSymbol<'ctx> = (Type, ThrustAttributes);
pub type EnumSymbol<'ctx> = (EnumData<'ctx>, ThrustAttributes);
pub type StaticSymbol<'parser> = (Type, StaticMetadata, ThrustAttributes);
pub type ConstantSymbol<'parser> = (Type, ThrustAttributes, Option<Ast<'parser>>);

pub type LLISymbol<'parser> = (Type, Span);
pub type LocalSymbol<'parser> = (Type, LocalMetadata, Span);
pub type ParameterSymbol<'parser> = (Type, FunctionParameterMetadata, Span);

pub type GlobalCustomTypes<'parser> = HashMap<&'parser str, CustomTypeSymbol<'parser>>;
pub type LocalCustomTypes<'parser> = Vec<HashMap<&'parser str, CustomTypeSymbol<'parser>>>;

pub type GlobalStructs<'parser> = HashMap<&'parser str, Struct<'parser>>;
pub type LocalStructs<'parser> = Vec<HashMap<&'parser str, Struct<'parser>>>;

pub type LocalStatics<'parser> = Vec<HashMap<&'parser str, StaticSymbol<'parser>>>;
pub type GlobalStatics<'parser> = HashMap<&'parser str, StaticSymbol<'parser>>;

pub type LocalConstants<'parser> = Vec<HashMap<&'parser str, ConstantSymbol<'parser>>>;
pub type GlobalConstants<'parser> = HashMap<&'parser str, ConstantSymbol<'parser>>;

pub type GlobalEnums<'parser> = HashMap<&'parser str, EnumSymbol<'parser>>;
pub type LocalEnums<'parser> = Vec<HashMap<&'parser str, EnumSymbol<'parser>>>;

pub type Parameters<'parser> = HashMap<&'parser str, ParameterSymbol<'parser>>;

pub type Intrinsics<'parser> = HashMap<&'parser str, Intrinsic<'parser>>;

pub type Functions<'parser> = HashMap<&'parser str, Function<'parser>>;
pub type AssemblerFunctions<'parser> = HashMap<&'parser str, AssemblerFunction<'parser>>;

pub type LLIs<'parser> = Vec<HashMap<&'parser str, LLISymbol<'parser>>>;
pub type Locals<'parser> = Vec<HashMap<&'parser str, LocalSymbol<'parser>>>;
