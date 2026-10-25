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

use thrustc_c_transpiler_macro::macro_token::MacroToken;
use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;

pub use thrustc_c_transpiler_macro::thrust::{MacroAst, ThrustType, Variadic};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroType {
    Function,
    Statement,
    Type,
    Object,
}

#[derive(Debug, Clone)]
pub struct Constant {
    name: String,
    kind: Type,
    value: BuiltinValue,
}

#[derive(Debug, Clone)]
pub struct Macro {
    name: String,
    parameters: Vec<String>,
    variadic: Option<Variadic>,
    body: Vec<MacroToken>,
    ast: Option<MacroAst>,
    kind: MacroType,
}

#[derive(Debug, Clone)]
pub struct MacroExpansion {
    source: String,
    argument_ranges: Vec<(u32, u32)>,
}

#[derive(Debug, Clone)]
pub enum Entity {
    Constant(Constant),
    Macro(Macro),
}

impl MacroExpansion {
    #[inline]
    pub fn new(source: String, argument_ranges: Vec<(u32, u32)>) -> Self {
        Self {
            source,
            argument_ranges,
        }
    }
}

impl MacroExpansion {
    #[inline]
    pub fn get_source(&self) -> &str {
        &self.source
    }

    #[inline]
    pub fn get_argument_ranges(&self) -> &[(u32, u32)] {
        &self.argument_ranges
    }

    #[inline]
    pub fn is_argument_range(&self, start: u32, end: u32) -> bool {
        self.argument_ranges
            .iter()
            .any(|(range_start, range_end)| start < *range_end && end > *range_start)
    }
}

impl Constant {
    #[inline]
    pub fn new(name: String, kind: Type, value: BuiltinValue) -> Self {
        Self { name, kind, value }
    }
}

impl Macro {
    #[inline]
    pub fn new(
        name: String,
        parameters: Vec<String>,
        variadic: Option<Variadic>,
        body: Vec<MacroToken>,
        ast: Option<MacroAst>,
        kind: MacroType,
    ) -> Self {
        Self {
            name,
            parameters,
            variadic,
            body,
            ast,
            kind,
        }
    }
}

impl Constant {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_kind(&self) -> &Type {
        &self.kind
    }

    #[inline]
    pub fn get_value(&self) -> &BuiltinValue {
        &self.value
    }
}

impl Macro {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_parameters(&self) -> &[String] {
        &self.parameters
    }

    #[inline]
    pub fn get_variadic(&self) -> Option<&Variadic> {
        self.variadic.as_ref()
    }

    #[inline]
    pub fn get_body(&self) -> &[MacroToken] {
        &self.body
    }

    #[inline]
    pub fn get_ast(&self) -> Option<&MacroAst> {
        self.ast.as_ref()
    }

    #[inline]
    pub fn get_kind(&self) -> MacroType {
        self.kind
    }
}

impl Entity {
    #[inline]
    pub fn get_name(&self) -> &str {
        match self {
            Entity::Constant(constant) => constant.get_name(),
            Entity::Macro(macro_) => macro_.get_name(),
        }
    }
}
