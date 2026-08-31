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
use thrustc_typesystem::{
    Type,
    traits::{TypePointerExtensions, TypeStructExtensions},
    type_metadata::StructTypeMetadata,
};

use crate::{
    Ast,
    ast_logic_data::{
        ConstructorData, EnumData, EnumDataField, PropertyData, PropertyDataField, StructureData,
    },
    traits::{
        AstAttributeExtensions, AstCodeBlockEntensions, AstConstructorDataExtensions,
        AstDeclarationExtensions, AstEnumFieldsDataExtensions, AstExpressionExtensions, AstGetType,
        AstMemoryExtensions, AstPropertyDataExtensions, AstPropertyDataFieldExtensions,
        AstStandardExtensions, AstStatementExtensions, AstStructFieldsDataExtensions,
        AstStructureDataExtensions,
    },
};

mod ast_constant;
mod ast_literal_values;

impl AstStandardExtensions for Ast<'_> {
    #[inline]
    fn is_reference(&self) -> bool {
        matches!(self, Ast::Reference { .. })
    }

    #[inline]
    fn is_function_keyword(&self) -> bool {
        matches!(self, Ast::Function { .. })
    }

    #[inline]
    fn is_intrinsic_keyword(&self) -> bool {
        matches!(self, Ast::CompilerIntrinsic { .. })
    }

    #[inline]
    fn is_asm_function(&self) -> bool {
        matches!(self, Ast::AssemblerFunction { .. })
    }

    #[inline]
    fn is_struct_keyword(&self) -> bool {
        matches!(self, Ast::Struct { .. })
    }

    #[inline]
    fn is_enum_keyword(&self) -> bool {
        matches!(self, Ast::Enum { .. })
    }

    #[inline]
    fn is_cstring(&self) -> bool {
        matches!(self, Ast::CString { .. })
    }

    #[inline]
    fn is_cnstring(&self) -> bool {
        matches!(self, Ast::CNString { .. })
    }

    #[inline]
    fn is_constant_keyword(&self) -> bool {
        matches!(self, Ast::Const { .. })
    }

    #[inline]
    fn is_static_keyword(&self) -> bool {
        matches!(self, Ast::Static { .. })
    }

    #[inline]
    fn is_integer(&self) -> bool {
        matches!(self, Ast::Integer { .. })
    }

    #[inline]
    fn is_terminator_keyword(&self) -> bool {
        matches!(self, Ast::Return { .. })
    }

    #[inline]
    fn is_unreacheable_keyword(&self) -> bool {
        matches!(self, Ast::Unreachable { .. })
    }

    #[inline]
    fn is_break_keyword(&self) -> bool {
        matches!(self, Ast::Break { .. })
    }

    #[inline]
    fn is_breakall_keyword(&self) -> bool {
        matches!(self, Ast::BreakAll { .. })
    }

    #[inline]
    fn is_continue_keyword(&self) -> bool {
        matches!(self, Ast::Continue { .. })
    }

    #[inline]
    fn is_continueall_keyword(&self) -> bool {
        matches!(self, Ast::ContinueAll { .. })
    }

    #[inline]
    fn is_type_keyword(&self) -> bool {
        matches!(self, Ast::CustomType { .. })
    }

    #[inline]
    fn is_global_asm_keyword(&self) -> bool {
        matches!(self, Ast::GlobalAssembler { .. })
    }

    #[inline]
    fn is_import_keyword(&self) -> bool {
        matches!(self, Ast::Import { .. })
    }

    fn is_function_parameter(&self) -> bool {
        matches!(
            self,
            Ast::FunctionParameter { .. }
                | Ast::AssemblerFunctionParameter { .. }
                | Ast::CompilerIntrinsicParameter { .. }
        )
    }

    #[inline]
    fn is_conditional_keyword(&self) -> bool {
        matches!(self, Ast::If { .. } | Ast::Elif { .. } | Ast::Else { .. })
    }

    #[inline]
    fn is_defer_keyword(&self) -> bool {
        matches!(self, Ast::Defer { .. })
    }

    #[inline]
    fn is_unstable_feature(&self) -> bool {
        matches!(
            self,
            Ast::AssemblerFunction { .. }
                | Ast::AssemblerFunctionParameter { .. }
                | Ast::AsmValue { .. }
        )
    }

    #[inline]
    fn is_invalid_ast_node(&self) -> bool {
        matches!(self, Ast::Invalid { .. })
    }
}

impl AstAttributeExtensions for Ast<'_> {
    #[inline]
    fn get_attributes(&self) -> Option<&ThrustAttributes> {
        match self {
            // Primitive Types & Literals
            Ast::CString { .. } => None,
            Ast::CNString { .. } => None,
            Ast::Char { .. } => None,
            Ast::Boolean { .. } => None,
            Ast::Integer { .. } => None,
            Ast::Float { .. } => None,
            Ast::NullPtr { .. } => None,

            // Global Assembler
            Ast::GlobalAssembler { .. } => None,

            // Arrays & Indexing
            Ast::FixedArray { .. } => None,
            Ast::Array { .. } => None,
            Ast::Index { .. } => None,

            // Embedded
            Ast::Embedded { .. } => None,

            // Composite Types
            Ast::Struct { attributes, .. } => Some(attributes),
            Ast::Constructor { .. } => None,
            Ast::Property { .. } => None,

            // Conditional
            Ast::If { .. } => None,
            Ast::Elif { .. } => None,
            Ast::Else { .. } => None,

            // Loops
            Ast::For { .. } => None,
            Ast::While { .. } => None,
            Ast::Loop { .. } => None,

            // Loop Control Flow
            Ast::Continue { .. } => None,
            Ast::Break { .. } => None,
            Ast::ContinueAll { .. } => None,
            Ast::BreakAll { .. } => None,

            // Code Block & Scope
            Ast::Block { .. } => None,
            Ast::Defer { .. } => None,

            // Custom Type
            Ast::CustomType { .. } => None,

            // Enums
            Ast::Enum { attributes, .. } => Some(attributes),
            Ast::EnumValue { .. } => None,

            // Functions
            Ast::CompilerIntrinsic { attributes, .. } => Some(attributes),
            Ast::CompilerIntrinsicParameter { .. } => None,
            Ast::AssemblerFunction { attributes, .. } => Some(attributes),
            Ast::AssemblerFunctionParameter { .. } => None,
            Ast::Function { attributes, .. } => Some(attributes),
            Ast::FunctionParameter { .. } => None,
            Ast::Return { .. } => None,

            // Static & Constants & Locals
            Ast::Static { attributes, .. } => Some(attributes),
            Ast::Const { attributes, .. } => Some(attributes),
            Ast::Var { attributes, .. } => Some(attributes),

            // Reference & Mutation
            Ast::Reference { .. } => None,
            Ast::Mutation { .. } => None,

            // Memory Operations
            Ast::Address { .. } => None,
            Ast::Write { .. } => None,
            Ast::Load { .. } => None,
            Ast::Deref { .. } => None,

            // Casts
            Ast::As { .. } => None,

            // Expressions
            Ast::GetLocation { .. } => None,
            Ast::ModuleExpression { .. } => None,
            Ast::Call { .. } => None,
            Ast::IndirectCall { .. } => None,
            Ast::AsmValue { attributes, .. } => Some(attributes),
            Ast::BinaryOp { .. } => None,
            Ast::UnaryOp { .. } => None,
            Ast::Group { .. } => None,

            // Builtins
            Ast::Builtin { .. } => None,

            // Module Imports
            Ast::Import { .. } => None,
            Ast::ImportC { .. } => None,

            // Unreachable & Invalid
            Ast::Unreachable { .. } => None,
            Ast::Invalid { .. } => None,
        }
    }
}

impl AstStatementExtensions for Ast<'_> {
    fn is_statement_keyword(&self) -> bool {
        matches!(
            self,
            Ast::Block { .. }
                | Ast::If { .. }
                | Ast::Else { .. }
                | Ast::Elif { .. }
                | Ast::While { .. }
                | Ast::For { .. }
                | Ast::Loop { .. }
                | Ast::Return { .. }
                | Ast::Break { .. }
                | Ast::BreakAll { .. }
                | Ast::Continue { .. }
                | Ast::ContinueAll { .. }
                | Ast::Var { .. }
                | Ast::Struct { .. }
                | Ast::Const { .. }
                | Ast::Static { .. }
                | Ast::Defer { .. }
                | Ast::Mutation { .. }
        )
    }
}

impl AstDeclarationExtensions for Ast<'_> {
    fn is_declaration_keyword(&self) -> bool {
        matches!(
            self,
            Ast::CustomType { .. }
                | Ast::Struct { .. }
                | Ast::Const { .. }
                | Ast::Static { .. }
                | Ast::Enum { .. }
                | Ast::Function { .. }
                | Ast::CompilerIntrinsic { .. }
                | Ast::AssemblerFunction { .. }
                | Ast::GlobalAssembler { .. }
                | Ast::Import { .. }
                | Ast::Embedded { .. }
        )
    }
}

impl AstExpressionExtensions for Ast<'_> {
    #[inline]
    fn is_expression(&self) -> bool {
        !self.is_declaration_keyword() && !self.is_statement_keyword()
    }

    #[inline]
    fn is_binary_operation(&self) -> bool {
        matches!(self, Ast::BinaryOp { .. })
    }

    #[inline]
    fn is_unary_operation(&self) -> bool {
        matches!(self, Ast::UnaryOp { .. })
    }

    #[inline]
    fn is_unary_before_operation(&self) -> bool {
        matches!(self, Ast::UnaryOp { before: true, .. })
    }

    #[inline]
    fn get_binary_operator(&self) -> Option<TokenType> {
        if let Ast::BinaryOp { operator, .. } = self {
            return Some(*operator);
        }

        None
    }
}

impl AstCodeBlockEntensions for Ast<'_> {
    #[inline]
    fn is_empty_code_block(&self) -> bool {
        let Ast::Block { nodes, .. } = self else {
            return false;
        };

        nodes.is_empty()
    }

    #[inline]
    fn has_terminator(&self) -> bool {
        let Ast::Block { nodes, .. } = self else {
            return false;
        };

        {
            for node in nodes.iter() {
                if node.is_terminator_keyword() || node.is_unreacheable_keyword() {
                    return true;
                }

                if node.has_terminator() {
                    return true;
                }

                if let Ast::If {
                    then_branch,
                    else_if_branch,
                    else_branch,
                    ..
                } = node
                {
                    let if_branch_returns: bool = then_branch.has_terminator();

                    let all_elif_return: bool = else_if_branch.iter().all(|elif_node| {
                        if let Ast::Elif { block, .. } = elif_node {
                            block.has_terminator()
                        } else {
                            false
                        }
                    });

                    let else_branch_returns: bool = else_branch.as_ref().is_some_and(|otherwise| {
                        if let Ast::Else { block, .. } = &**otherwise {
                            block.has_terminator()
                        } else {
                            false
                        }
                    });

                    let if_else_returns: bool =
                        if_branch_returns && else_branch_returns && else_if_branch.is_empty();
                    let full_returns: bool =
                        if_branch_returns && all_elif_return && else_branch_returns;

                    if if_else_returns || full_returns {
                        return true;
                    }
                }
            }
        }

        false
    }
}

impl AstMemoryExtensions for Ast<'_> {
    #[inline]
    fn is_memory_assigned_value(&self) -> Result<bool, CompilationIssue> {
        match self {
            Ast::Reference { metadata, kind, .. } => {
                Ok(metadata.is_allocated() || kind.is_ptr_like_type())
            }

            Ast::Property { metadata, kind, .. } => {
                Ok(metadata.is_allocated() || metadata.is_deref() || kind.is_ptr_like_type())
            }

            Ast::Index { metadata, kind, .. } => {
                Ok(metadata.is_allocated() || kind.is_ptr_like_type())
            }

            _ => {
                let value_ty: &Type = self.get_value_type()?;
                let is_ptr_ty: bool = value_ty.is_ptr_like_type();

                Ok(is_ptr_ty)
            }
        }
    }

    #[inline]
    fn is_memory_assigned_reference(&self) -> bool {
        if let Ast::Reference { metadata, .. } = self {
            return metadata.is_allocated();
        }

        false
    }
}

impl AstPropertyDataExtensions for PropertyData {
    #[inline]
    fn get_first_property(&self) -> Option<&crate::ast_logic_data::PropertyDataField> {
        self.first()
    }
}

impl AstPropertyDataFieldExtensions for PropertyDataField {
    #[inline]
    fn get_base_type(&self) -> Type {
        self.0.clone()
    }

    #[inline]
    fn get_property_type(&self) -> Type {
        self.1.0.clone()
    }

    #[inline]
    fn get_index(&self) -> u32 {
        self.1.1
    }
}

impl AstConstructorDataExtensions for ConstructorData<'_> {
    #[inline]
    fn get_struct_type(&self, name: &str, metadata: StructTypeMetadata, span: Span) -> Type {
        let types: Vec<Type> = self.iter().map(|field| field.2.clone()).collect();
        Type::create_struct_type(name.to_string(), types.as_slice(), metadata, span)
    }
}

impl<'a> AstStructureDataExtensions<'a> for StructureData<'a> {
    #[inline]
    fn new(name: &'a str, metadata: StructTypeMetadata, span: thrustc_code_location::Span) -> Self {
        (name, Vec::with_capacity(u8::MAX as usize), metadata, span)
    }

    #[inline]
    fn get_struct_fields(&self) -> &crate::ast_logic_data::StructureDataFields<'_> {
        &self.1
    }
}

impl AstStructFieldsDataExtensions for StructureData<'_> {
    #[inline]
    fn get_struct_type(&self) -> Type {
        let types: Vec<Type> = self.1.iter().map(|field| field.1.clone()).collect();

        let name: String = self.0.to_string();
        let span: Span = self.3;

        let metadata: StructTypeMetadata = self.get_struct_metadata();

        Type::create_struct_type(name, types.as_slice(), metadata, span)
    }

    #[inline]
    fn get_struct_metadata(&self) -> StructTypeMetadata {
        self.2
    }
}

impl<'a> AstEnumFieldsDataExtensions<'a> for EnumData<'a> {
    #[inline]
    fn get_enum_field(&self, name: &str) -> Option<EnumDataField<'a>> {
        self.iter().find(|enum_field| enum_field.0 == name).cloned()
    }
}

impl std::fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:#?}", self)
    }
}
