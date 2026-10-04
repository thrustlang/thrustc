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

use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;

#[derive(Debug)]
pub struct CImportedFunction {
    name: String,
    external_name: String,
    return_type: Type,
    parameter_types: Vec<Type>,
    parameter_names: Vec<String>,
    variadic: bool,
}

#[derive(Debug)]
pub struct CImportedStruct {
    name: String,
    fields: Vec<(String, Type)>,
}

#[derive(Debug)]
pub struct CImportedEnum {
    name: String,
    underlying_type: Type,
    fields: Vec<(String, u64)>,
}

#[derive(Debug)]
pub struct CImportedTypedef {
    name: String,
    ty: Type,
}

#[derive(Debug)]
pub struct CImportedConstant {
    name: String,
    kind: Type,
    value: BuiltinValue,
}

impl CImportedFunction {
    #[inline]
    pub fn new(
        name: String,
        external_name: String,
        return_type: Type,
        parameter_types: Vec<Type>,
        parameter_names: Vec<String>,
        variadic: bool,
    ) -> Self {
        Self {
            name,
            external_name,
            return_type,
            parameter_types,
            parameter_names,
            variadic,
        }
    }
}

impl CImportedStruct {
    #[inline]
    pub fn new(name: String, fields: Vec<(String, Type)>) -> Self {
        Self { name, fields }
    }
}

impl CImportedEnum {
    #[inline]
    pub fn new(name: String, underlying_type: Type, fields: Vec<(String, u64)>) -> Self {
        Self {
            name,
            underlying_type,
            fields,
        }
    }
}

impl CImportedTypedef {
    #[inline]
    pub fn new(name: String, ty: Type) -> Self {
        Self { name, ty }
    }
}

impl CImportedConstant {
    #[inline]
    pub fn new(name: String, kind: Type, value: BuiltinValue) -> Self {
        Self { name, kind, value }
    }
}

impl CImportedFunction {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn external_name(&self) -> &str {
        &self.external_name
    }

    #[inline]
    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    #[inline]
    pub fn parameter_types(&self) -> &[Type] {
        &self.parameter_types
    }

    #[inline]
    pub fn parameter_names(&self) -> &[String] {
        &self.parameter_names
    }

    #[inline]
    pub fn variadic(&self) -> bool {
        self.variadic
    }
}

impl CImportedStruct {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn fields(&self) -> &[(String, Type)] {
        &self.fields
    }
}

impl CImportedEnum {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn underlying_type(&self) -> &Type {
        &self.underlying_type
    }

    #[inline]
    pub fn fields(&self) -> &[(String, u64)] {
        &self.fields
    }
}

impl CImportedTypedef {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl CImportedConstant {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn kind(&self) -> &Type {
        &self.kind
    }

    #[inline]
    pub fn value(&self) -> &BuiltinValue {
        &self.value
    }
}
