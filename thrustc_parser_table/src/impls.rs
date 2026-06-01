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

use std::path::PathBuf;

use thrustc_ast::{
    ast_logic_data::{ConstructorData, EnumData, StructureData},
    ast_metadata::{FunctionParameterMetadata, LocalMetadata, StaticMetadata},
};
use thrustc_entities::parser_entities::{
    AssemblerFunction, ConstantSymbol, EnumSymbol, FoundSymbolId, Function, Intrinsic, LLISymbol,
    LocalSymbol, ParameterSymbol, StaticSymbol, Struct,
};
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type, traits::TypeStructExtensions, type_modificators::StructureTypeModificator,
};

use crate::traits::{
    ConstantSymbolExtensions, ConstructorExtensions, EnumExtensions, FoundSymbolEitherExtensions,
    FoundSymbolExtensions, FunctionAssemblerExtensions, FunctionExtensions,
    FunctionParameterSymbolExtensions, IntrinsicExtensions, LLISymbolExtensions,
    LocalSymbolExtensions, StaticSymbolExtensions, StructSymbolExtensions,
};

impl<'parser> FoundSymbolEitherExtensions<'parser> for FoundSymbolId<'parser> {
    fn expected_struct(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some(struct_id) = self.0 {
            return Ok(struct_id);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected struct reference"),
            String::from("Expected struct but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_function(&self, span: Span) -> Result<&'parser str, CompilationIssue> {
        if let Some(name) = self.1 {
            return Ok(name);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected function reference"),
            String::from("Expected function but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_enum(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some(name) = self.2 {
            return Ok(name);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected enum reference"),
            String::from("Expected enum but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_static(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some(static_id) = self.3 {
            return Ok(static_id);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected static reference"),
            String::from("Expected static but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_constant(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some(const_id) = self.4 {
            return Ok(const_id);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected constant reference"),
            String::from("Expected constant but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_custom_type(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some(type_id) = self.5 {
            return Ok(type_id);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected custom type reference"),
            String::from("Expected custom type but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_parameter(&self, span: Span) -> Result<&'parser str, CompilationIssue> {
        if let Some(name) = self.6 {
            return Ok(name);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected parameter reference"),
            String::from("Expected parameter but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_asm_function(&self, span: Span) -> Result<&'parser str, CompilationIssue> {
        if let Some(name) = self.7 {
            return Ok(name);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected assembler function reference"),
            String::from("Expected assembler function but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_lli(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some((name, scope_idx)) = self.8 {
            return Ok((name, scope_idx));
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected low level instruction reference"),
            String::from("Expected LLI but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_local(&self, span: Span) -> Result<(&'parser str, usize), CompilationIssue> {
        if let Some((name, scope_idx)) = self.9 {
            return Ok((name, scope_idx));
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected local reference"),
            String::from("Expected local but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }

    fn expected_intrinsic(&self, span: Span) -> Result<&'parser str, CompilationIssue> {
        if let Some(name) = self.10 {
            return Ok(name);
        }

        Err(CompilationIssue::FrontEndBug(
            String::from("Expected intrinsic reference"),
            String::from("Expected intrinsic but found something else."),
            span,
            CompilationPosition::Parser,
            PathBuf::from(file!()),
            line!(),
        ))
    }
}

impl FoundSymbolExtensions for FoundSymbolId<'_> {
    fn is_structure(&self) -> bool {
        self.0.is_some()
    }
    fn is_function(&self) -> bool {
        self.1.is_some()
    }

    fn is_static(&self) -> bool {
        self.3.is_some()
    }

    fn is_constant(&self) -> bool {
        self.4.is_some()
    }

    fn is_custom_type(&self) -> bool {
        self.5.is_some()
    }

    fn is_parameter(&self) -> bool {
        self.6.is_some()
    }

    fn is_function_asm(&self) -> bool {
        self.7.is_some()
    }

    fn is_lli(&self) -> bool {
        self.8.is_some()
    }

    fn is_local(&self) -> bool {
        self.9.is_some()
    }

    fn is_intrinsic(&self) -> bool {
        self.10.is_some()
    }
}

impl<'parser> StructSymbolExtensions<'parser> for Struct<'parser> {
    fn contains_field(&self, name: &str) -> bool {
        self.1.iter().any(|field| field.0 == name)
    }

    fn get_modificator(&self) -> StructureTypeModificator {
        self.3
    }

    fn get_field_type(&self, name: &str) -> Option<Type> {
        if let Some(field) = self.1.iter().find(|field| field.0 == name) {
            let field_type: Type = field.1.clone();
            Some(field_type)
        } else {
            None
        }
    }

    fn get_data(&self) -> StructureData<'parser> {
        (self.0, self.1.clone(), self.3, self.4)
    }
}

impl LocalSymbolExtensions for LocalSymbol<'_> {
    fn get_metadata(&self) -> LocalMetadata {
        self.1
    }

    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl StaticSymbolExtensions for StaticSymbol<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }

    fn get_metadata(&self) -> StaticMetadata {
        self.1
    }
}

impl ConstantSymbolExtensions for ConstantSymbol<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl FunctionParameterSymbolExtensions for ParameterSymbol<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }

    fn get_metadata(&self) -> FunctionParameterMetadata {
        self.1
    }
}

impl FunctionAssemblerExtensions for AssemblerFunction<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl IntrinsicExtensions for Intrinsic<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl FunctionExtensions for Function<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl LLISymbolExtensions for LLISymbol<'_> {
    fn get_type(&self) -> Type {
        self.0.clone()
    }
}

impl<'parser> EnumExtensions<'parser> for EnumSymbol<'parser> {
    fn get_fields(&self) -> EnumData<'parser> {
        self.0.clone()
    }
}

impl ConstructorExtensions for ConstructorData<'_> {
    #[inline]
    fn get_type(&self, name: &str, modificator: StructureTypeModificator, span: Span) -> Type {
        let types: Vec<Type> = self.iter().map(|field| field.2.clone()).collect();
        Type::create_struct_type(name.to_string(), types.as_slice(), modificator, span)
    }
}
