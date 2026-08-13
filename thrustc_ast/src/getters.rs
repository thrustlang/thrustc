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

use thrustc_ast_external::{ExternalSignature, ExternalSymbol};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::{
    Ast,
    traits::{AstCodeLocation, AstGetType},
};

impl AstGetType for Ast<'_> {
    fn get_any_type(&self) -> &Type {
        match self {
            // Primitive Types & Literals
            Ast::Integer { kind, .. } => kind,
            Ast::Float { kind, .. } => kind,
            Ast::Boolean { kind, .. } => kind,
            Ast::Char { kind, .. } => kind,
            Ast::CString { kind, .. } => kind,
            Ast::CNString { kind, .. } => kind,
            Ast::NullPtr { kind, .. } => kind,

            // Custom Type
            Ast::CustomType { kind, .. } => kind,

            // Embedded
            Ast::Embedded { kind, .. } => kind,

            // Static
            Ast::Static { kind, .. } => kind,

            // Variables & mutation
            Ast::Var { kind, .. } => kind,
            Ast::Mutation { kind, .. } => kind,

            // Reference
            Ast::Reference { kind, .. } => kind,
            Ast::GetLocation { kind, .. } => kind,

            // Memory operations
            Ast::Deref { kind, .. } => kind,

            // LLI
            Ast::Address { kind, .. } => kind,
            Ast::Load { kind, .. } => kind,
            Ast::Write {
                write_type: kind, ..
            } => kind,

            // Function-Related Operations
            Ast::FunctionParameter { kind, .. } => kind,
            Ast::AssemblerFunctionParameter { kind, .. } => kind,
            Ast::Return { kind, .. } => kind,
            Ast::Function { return_type, .. } => return_type,
            Ast::AssemblerFunction { return_type, .. } => return_type,

            // Expressions & Operators
            Ast::ModuleExpression { data, .. } => {
                let ExternalSymbol { signature, .. } = data;

                match signature {
                    ExternalSignature::Constant { kind, .. } => kind,
                    ExternalSignature::CustomType { kind, .. } => kind,
                    ExternalSignature::Function { kind, .. } => kind,
                    ExternalSignature::Struct { kind, .. } => kind,
                    ExternalSignature::Static { kind, .. } => kind,
                    ExternalSignature::Unavailable { kind, .. } => kind,
                }
            }
            Ast::Call { kind, .. } => kind,
            Ast::BinaryOp { kind, .. } => kind,
            Ast::UnaryOp { kind, .. } => kind,
            Ast::Group { kind, .. } => kind,
            Ast::Index { kind, .. } => kind,
            Ast::AsmValue { kind, .. } => kind,

            // Builtins
            Ast::Builtin { kind, .. } => kind,

            // Composite Types
            Ast::Constructor { kind, .. } => kind,
            Ast::Property { kind, .. } => kind,
            Ast::EnumValue { kind, .. } => kind,
            Ast::FixedArray { kind, .. } => kind,
            Ast::Array { kind, .. } => kind,
            Ast::Struct { kind, .. } => kind,
            Ast::Enum { kind, .. } => kind,

            // Type Conversions
            Ast::As { cast, .. } => cast,

            // Constants
            Ast::Const { kind, .. } => kind,

            // Intrinsic
            Ast::CompilerIntrinsic { return_type, .. } => return_type,
            Ast::CompilerIntrinsicParameter { kind, .. } => kind,

            // Indirect Call
            Ast::IndirectCall { kind, .. } => kind,

            // Control flow
            Ast::If { kind, .. } => kind,
            Ast::Elif { kind, .. } => kind,
            Ast::Else { kind, .. } => kind,
            Ast::For { kind, .. } => kind,
            Ast::Loop { kind, .. } => kind,
            Ast::Break { kind, .. } => kind,
            Ast::BreakAll { kind, .. } => kind,
            Ast::Continue { kind, .. } => kind,
            Ast::ContinueAll { kind, .. } => kind,
            Ast::Block { kind, .. } => kind,
            Ast::Defer { kind, .. } => kind,

            // Module imports
            Ast::Import { kind, .. } => kind,
            Ast::ImportC { kind, .. } => kind,

            // Others
            Ast::Unreachable { kind, .. } => kind,
            Ast::GlobalAssembler { kind, .. } => kind,

            // Invalid
            Ast::Invalid { kind, .. } => kind,

            // While doesn't have kind field
            Ast::While { kind, .. } => kind,
        }
    }

    fn get_value_type(&self) -> Result<&Type, CompilationIssue> {
        match self {
            // Primitive values
            Ast::Integer { kind, .. } => Ok(kind),
            Ast::Float { kind, .. } => Ok(kind),
            Ast::Boolean { kind, .. } => Ok(kind),
            Ast::Char { kind, .. } => Ok(kind),
            Ast::CString { kind, .. } => Ok(kind),
            Ast::CNString { kind, .. } => Ok(kind),
            Ast::NullPtr { kind, .. } => Ok(kind),

            // Variables and references
            Ast::Var { kind, .. } => Ok(kind),
            Ast::Mutation { kind, .. } => Ok(kind),
            Ast::Reference { kind, .. } => Ok(kind),
            Ast::GetLocation { kind, .. } => Ok(kind),
            Ast::FunctionParameter { kind, .. } => Ok(kind),
            Ast::AssemblerFunctionParameter { kind, .. } => Ok(kind),

            // LLI
            Ast::Load { kind, .. } => Ok(kind),
            Ast::Address { kind, .. } => Ok(kind),

            // Memory operations
            Ast::Deref { kind, .. } => Ok(kind),

            // Composite types
            Ast::FixedArray { kind, .. } => Ok(kind),
            Ast::Array { kind, .. } => Ok(kind),
            Ast::Constructor { kind, .. } => Ok(kind),
            Ast::Property { kind, .. } => Ok(kind),
            Ast::EnumValue { kind, .. } => Ok(kind),

            // Expressions
            Ast::ModuleExpression { data, .. } => {
                let ExternalSymbol { signature, .. } = data;

                match signature {
                    ExternalSignature::Constant { kind, .. } => Ok(kind),
                    ExternalSignature::CustomType { kind, .. } => Ok(kind),
                    ExternalSignature::Function { kind, .. } => Ok(kind),
                    ExternalSignature::Struct { kind, .. } => Ok(kind),
                    ExternalSignature::Static { kind, .. } => Ok(kind),
                    ExternalSignature::Unavailable { kind, .. } => Ok(kind),
                }
            }
            Ast::Call { kind, .. } => Ok(kind),
            Ast::BinaryOp { kind, .. } => Ok(kind),
            Ast::UnaryOp { kind, .. } => Ok(kind),
            Ast::Group { kind, .. } => Ok(kind),
            Ast::Index { kind, .. } => Ok(kind),

            // Type operations
            Ast::As { cast: kind, .. } => Ok(kind),

            // Builtins
            Ast::Builtin { kind, .. } => Ok(kind),

            // ASM Code Block
            Ast::AsmValue { kind, .. } => Ok(kind),

            // Intrinsic
            Ast::CompilerIntrinsic {
                return_type: kind, ..
            } => Ok(kind),
            Ast::CompilerIntrinsicParameter { kind, .. } => Ok(kind),

            // Invalid
            Ast::Invalid { kind, .. } => Ok(kind),

            _ => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected a value, not anything else.".into(),
                "It should be an expression!".into(),
                None,
                self.get_span(),
            )),
        }
    }
}

impl AstCodeLocation for Ast<'_> {
    fn get_span(&self) -> Span {
        match self {
            // Primitive values and literals
            Ast::Integer { span, .. } => *span,
            Ast::Float { span, .. } => *span,
            Ast::Boolean { span, .. } => *span,
            Ast::Char { span, .. } => *span,
            Ast::CString { span, .. } => *span,
            Ast::CNString { span, .. } => *span,
            Ast::NullPtr { span, .. } => *span,

            // Embedded
            Ast::Embedded { span, .. } => *span,

            // Custom Type
            Ast::CustomType { span, .. } => *span,

            // Static
            Ast::Static { span, .. } => *span,

            // Variables and declarations
            Ast::Var { span, .. } => *span,
            Ast::Const { span, .. } => *span,
            Ast::FunctionParameter { span, .. } => *span,
            Ast::AssemblerFunctionParameter { span, .. } => *span,

            // Mutation
            Ast::Mutation { span, .. } => *span,

            // References variants
            Ast::Reference { span, .. } => *span,
            Ast::GetLocation { span, .. } => *span,

            // LLI
            Ast::Address { span, .. } => *span,
            Ast::Load { span, .. } => *span,
            Ast::Write { span, .. } => *span,

            // Memory operations
            Ast::Deref { span, .. } => *span,

            // Composite types
            Ast::FixedArray { span, .. } => *span,
            Ast::Array { span, .. } => *span,

            Ast::Struct { span, .. } => *span,
            Ast::Enum { span, .. } => *span,
            Ast::EnumValue { span, .. } => *span,
            Ast::Constructor { span, .. } => *span,
            Ast::Property { span, .. } => *span,

            // Expressions and operators
            Ast::ModuleExpression { span, .. } => *span,
            Ast::Call { span, .. } => *span,
            Ast::BinaryOp { span, .. } => *span,
            Ast::UnaryOp { span, .. } => *span,
            Ast::Group { span, .. } => *span,
            Ast::Index { span, .. } => *span,

            // Type conversions
            Ast::As { span, .. } => *span,

            // Builtins
            Ast::Builtin { span, .. } => *span,

            // Control flow
            Ast::If { span, .. } => *span,
            Ast::Elif { span, .. } => *span,
            Ast::Else { span, .. } => *span,
            Ast::While { span, .. } => *span,
            Ast::For { span, .. } => *span,
            Ast::Loop { span, .. } => *span,
            Ast::Break { span, .. } => *span,
            Ast::BreakAll { span, .. } => *span,
            Ast::Continue { span, .. } => *span,
            Ast::ContinueAll { span, .. } => *span,
            Ast::Block { span, .. } => *span,
            Ast::Defer { span, .. } => *span,

            // Functions
            Ast::Function { span, .. } => *span,
            Ast::AssemblerFunction { span, .. } => *span,
            Ast::Return { span, .. } => *span,

            // Low-level and special operations
            Ast::AsmValue { span, .. } => *span,

            // Global Assembler
            Ast::GlobalAssembler { span, .. } => *span,

            // Intrinsic
            Ast::CompilerIntrinsic { span, .. } => *span,
            Ast::CompilerIntrinsicParameter { span, .. } => *span,

            // Module Import
            Ast::Import { span, .. } => *span,
            // C Import
            Ast::ImportC { span, .. } => *span,

            // Indirect Call
            Ast::IndirectCall { span, .. } => *span,

            // Unreachable marker
            Ast::Unreachable { span, .. } => *span,

            // Invalid
            Ast::Invalid { span, .. } => *span,
        }
    }
}
