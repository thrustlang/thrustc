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

use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};

use crate::ast_logic_data::{EnumDataField, PropertyDataField, StructureDataFields};

pub trait AstBuiltinsExtensions {
    fn is_avalaible_at_compile_time(&self) -> bool;
}

pub trait AstGetType {
    fn get_any_type(&self) -> &Type;
    fn get_value_type(&self) -> Result<&Type, CompilationIssue>;
}

pub trait AstCodeLocation {
    fn get_span(&self) -> Span;
}

pub trait AstStatementExtensions {
    fn is_statement_keyword(&self) -> bool;
}

pub trait AstDeclarationExtensions {
    fn is_declaration_keyword(&self) -> bool;
}

pub trait AstExpressionExtensions {
    fn is_expression(&self) -> bool;

    fn is_binary_operation(&self) -> bool;
    fn is_unary_operation(&self) -> bool;
    fn is_unary_before_operation(&self) -> bool;

    fn get_binary_operator(&self) -> Option<TokenType>;
}

pub trait AstAttributeExtensions {
    fn get_attributes(&self) -> Option<&ThrustAttributes>;
}

pub trait AstStandardExtensions {
    fn is_reference(&self) -> bool;
    fn is_unreacheable_keyword(&self) -> bool;
    fn is_import_keyword(&self) -> bool;
    fn is_function_keyword(&self) -> bool;
    fn is_intrinsic_keyword(&self) -> bool;
    fn is_asm_function(&self) -> bool;
    fn is_global_asm_keyword(&self) -> bool;
    fn is_struct_keyword(&self) -> bool;
    fn is_enum_keyword(&self) -> bool;
    fn is_cstring(&self) -> bool;
    fn is_cnstring(&self) -> bool;
    fn is_constant_keyword(&self) -> bool;
    fn is_static_keyword(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_terminator_keyword(&self) -> bool;
    fn is_type_keyword(&self) -> bool;
    fn is_break_keyword(&self) -> bool;
    fn is_breakall_keyword(&self) -> bool;
    fn is_continue_keyword(&self) -> bool;
    fn is_continueall_keyword(&self) -> bool;
    fn is_conditional_keyword(&self) -> bool;
    fn is_function_parameter(&self) -> bool;
    fn is_defer_keyword(&self) -> bool;
    fn is_unstable_feature(&self) -> bool;
    fn is_invalid_ast_node(&self) -> bool;
}

pub trait AstLiteralExtensions {
    fn is_totaly_literal_value(&self) -> bool;
    fn is_literal_value(&self) -> bool;
    fn is_literal_ptr_value(&self) -> bool;
}

pub trait AstCodeBlockEntensions {
    fn is_empty_code_block(&self) -> bool;
    fn has_terminator(&self) -> bool;
}

pub trait AstMemoryExtensions {
    fn is_memory_assigned_value(&self) -> Result<bool, CompilationIssue>;
    fn is_memory_assigned_reference(&self) -> bool;
}

pub trait AstConstantExtensions {
    fn is_constant_value(&self) -> bool;
}

pub trait AstStructureDataExtensions<'ast> {
    fn new(name: &'ast str, metadata: StructTypeMetadata, span: Span) -> Self;
    fn get_struct_fields(&self) -> &StructureDataFields<'_>;
}

pub trait AstPropertyDataExtensions {
    fn get_first_property(&self) -> Option<&PropertyDataField>;
}

pub trait AstPropertyDataFieldExtensions {
    fn get_base_type(&self) -> Type;
    fn get_property_type(&self) -> Type;
    fn get_index(&self) -> u32;
}

pub trait AstEnumFieldsDataExtensions<'a> {
    fn get_enum_field(&self, name: &'a str) -> Option<EnumDataField<'a>>;
}

pub trait AstStructFieldsDataExtensions {
    fn get_struct_type(&self) -> Type;
    fn get_struct_metadata(&self) -> StructTypeMetadata;
}

pub trait AstConstructorDataExtensions {
    fn get_struct_type(&self, name: &str, metadata: StructTypeMetadata, span: Span) -> Type;
}
