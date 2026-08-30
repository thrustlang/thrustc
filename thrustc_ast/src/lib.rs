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

use std::sync::atomic::AtomicU64;

use serde::Serialize;
use thrustc_ast_external::ExternalSymbol;
use thrustc_ast_modificators::Modificators;
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    ast_builtins::AstBuiltin,
    ast_logic_data::{ConstructorData, EnumData, PropertyData, StructureData},
    ast_metadata::{
        CastingMetadata, ConstantMetadata, DereferenceMetadata, FunctionParameterMetadata,
        IndexMetadata, LoadMetadata, LocalMetadata, PropertyMetadata, ReferenceMetadata,
        StaticMetadata,
    },
};

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

pub mod ast_builtins;
pub mod ast_logic_data;
pub mod ast_metadata;
mod getters;
mod impls;
pub mod traits;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum Ast<'ast> {
    CString {
        bytes: std::vec::Vec<u8>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    CNString {
        bytes: std::vec::Vec<u8>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    Char {
        kind: Type,
        byte: u64,
        span: Span,
        id: NodeId,
    },

    Boolean {
        kind: Type,
        value: u64,
        span: Span,
        id: NodeId,
    },

    Integer {
        kind: Type,
        value: u64,
        span: Span,
        id: NodeId,
    },

    Float {
        kind: Type,
        value: f64,
        span: Span,
        id: NodeId,
    },

    NullPtr {
        span: Span,
        kind: Type,
    },

    // Global Assembler
    GlobalAssembler {
        asm: String,
        span: Span,
        kind: Type,
        id: NodeId,
    },

    // Fixed Array
    FixedArray {
        items: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Array
    Array {
        items: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    Index {
        source: std::boxed::Box<Ast<'ast>>,
        index: std::boxed::Box<Ast<'ast>>,
        metadata: IndexMetadata,
        kind: Type,
        span: Span,
    },

    //
    Embedded {
        name: &'ast str,
        path: std::path::PathBuf,
        literal: &'ast str,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Structures
    Struct {
        name: &'ast str,
        data: StructureData<'ast>,
        kind: Type,
        span: Span,
        attributes: ThrustAttributes,
        id: NodeId,
    },

    Constructor {
        name: &'ast str,
        data: ConstructorData<'ast>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    Property {
        source: std::boxed::Box<Ast<'ast>>,
        data: PropertyData,
        metadata: PropertyMetadata,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    If {
        condition: std::boxed::Box<Ast<'ast>>,
        then_branch: std::boxed::Box<Ast<'ast>>,
        else_if_branch: std::vec::Vec<Ast<'ast>>,
        else_branch: Option<std::boxed::Box<Ast<'ast>>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    Elif {
        condition: std::boxed::Box<Ast<'ast>>,
        block: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    Else {
        block: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Loops
    For {
        local: std::boxed::Box<Ast<'ast>>,
        condition: std::boxed::Box<Ast<'ast>>,
        actions: std::boxed::Box<Ast<'ast>>,
        block: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    While {
        variable: Option<std::boxed::Box<Ast<'ast>>>,
        condition: std::boxed::Box<Ast<'ast>>,
        block: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    Loop {
        block: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Loop control flow
    Continue {
        kind: Type,
        span: Span,
        id: NodeId,
    },
    Break {
        kind: Type,
        span: Span,
        id: NodeId,
    },
    ContinueAll {
        kind: Type,
        span: Span,
        id: NodeId,
    },
    BreakAll {
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Code block
    Block {
        nodes: std::vec::Vec<Ast<'ast>>,
        post: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Scope Post Execution
    Defer {
        node: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Custom Type
    CustomType {
        name: String,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Enums
    Enum {
        name: &'ast str,
        data: EnumData<'ast>,
        attributes: ThrustAttributes,
        kind: Type,
        span: Span,
        id: NodeId,
    },
    EnumValue {
        name: String,
        value: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Functions
    CompilerIntrinsic {
        name: &'ast str,
        external_name: &'ast str,
        parameters: std::vec::Vec<Ast<'ast>>,
        parameters_types: std::vec::Vec<Type>,
        return_type: Type,
        attributes: ThrustAttributes,
        span: Span,
        id: NodeId,
    },
    CompilerIntrinsicParameter {
        kind: Type,
        span: Span,
        id: NodeId,
    },
    AssemblerFunction {
        name: &'ast str,
        ascii_name: &'ast str,
        parameters: std::vec::Vec<Ast<'ast>>,
        parameters_types: std::vec::Vec<Type>,
        assembler: String,
        constraints: String,
        return_type: Type,
        attributes: ThrustAttributes,
        span: Span,
        id: NodeId,
    },
    AssemblerFunctionParameter {
        name: &'ast str,
        kind: Type,
        position: u32,
        span: Span,
        id: NodeId,
    },
    Function {
        name: String,
        ascii_name: String,
        original_name: Option<String>,
        parameters: std::vec::Vec<Ast<'ast>>,
        parameter_types: std::vec::Vec<Type>,
        body: Option<std::boxed::Box<Ast<'ast>>>,
        return_type: Type,
        attributes: ThrustAttributes,
        span: Span,
        id: NodeId,
    },
    FunctionParameter {
        name: String,
        ascii_name: String,
        kind: Type,
        position: u32,
        metadata: FunctionParameterMetadata,
        span: Span,
        id: NodeId,
    },
    Return {
        expression: Option<std::boxed::Box<Ast<'ast>>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Static
    Static {
        name: &'ast str,
        ascii_name: &'ast str,
        kind: Type,
        value: Option<std::boxed::Box<Ast<'ast>>>,
        attributes: ThrustAttributes,
        modificators: Modificators,
        metadata: StaticMetadata,
        span: Span,
        id: NodeId,
    },

    // Constants
    Const {
        name: &'ast str,
        ascii_name: &'ast str,
        kind: Type,
        value: std::boxed::Box<Ast<'ast>>,
        attributes: ThrustAttributes,
        modificators: Modificators,
        metadata: ConstantMetadata,
        span: Span,
        id: NodeId,
    },

    // Locals variables
    Var {
        name: &'ast str,
        ascii_name: &'ast str,
        kind: Type,
        value: Option<std::boxed::Box<Ast<'ast>>>,
        attributes: ThrustAttributes,
        modificators: Modificators,
        metadata: LocalMetadata,
        span: Span,
        id: NodeId,
    },

    // Reference
    Reference {
        name: &'ast str,
        kind: Type,
        metadata: ReferenceMetadata,
        span: Span,
        id: NodeId,
    },

    // Mutation
    Mutation {
        source: std::boxed::Box<Ast<'ast>>,
        value: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    Address {
        source: std::boxed::Box<Ast<'ast>>,
        indexes: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    Write {
        source: std::boxed::Box<Ast<'ast>>,
        write_value: std::boxed::Box<Ast<'ast>>,
        write_type: Type,
        span: Span,
        id: NodeId,
    },
    Load {
        source: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        modificators: Modificators,
        metadata: LoadMetadata,
        span: Span,
        id: NodeId,
    },

    // Pointer Manipulation
    Deref {
        value: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        modificators: Modificators,
        metadata: DereferenceMetadata,
        span: Span,
        id: NodeId,
    },

    // Casts
    As {
        from: std::boxed::Box<Ast<'ast>>,
        cast: Type,
        metadata: CastingMetadata,
        span: Span,
        id: NodeId,
    },

    // Expressions
    GetLocation {
        expr: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    ModuleExpression {
        data: ExternalSymbol,
        values: ModuleExpressionValues<'ast>,
        span: Span,
        id: NodeId,
    },

    Call {
        name: String,
        args: std::vec::Vec<Ast<'ast>>,
        generic_args: std::vec::Vec<Type>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    IndirectCall {
        function: std::boxed::Box<Ast<'ast>>,
        function_type: Type,
        args: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    AsmValue {
        assembler: String,
        constraints: String,
        args: std::vec::Vec<Ast<'ast>>,
        kind: Type,
        attributes: ThrustAttributes,
        span: Span,
        id: NodeId,
    },

    BinaryOp {
        left: std::boxed::Box<Ast<'ast>>,
        operator: TokenType,
        right: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    UnaryOp {
        operator: TokenType,
        kind: Type,
        node: std::boxed::Box<Ast<'ast>>,
        before: bool,
        span: Span,
        id: NodeId,
    },

    Group {
        node: std::boxed::Box<Ast<'ast>>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Builtins
    Builtin {
        builtin: AstBuiltin<'ast>,
        kind: Type,
        span: Span,
        id: NodeId,
    },

    // Module Import
    Import {
        span: Span,
        kind: Type,
        id: NodeId,
    },
    // C Import
    ImportC {
        span: Span,
        kind: Type,
        id: NodeId,
    },

    // Unreachable
    Unreachable {
        span: Span,
        kind: Type,
        id: NodeId,
    },

    // Invalid
    Invalid {
        kind: Type,
        span: Span,
        id: NodeId,
    },
}

impl<'ast> Ast<'ast> {
    #[inline]
    pub fn new_float(kind: Type, value: f64, span: Span) -> Ast<'ast> {
        Ast::Float {
            kind,
            value,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_integer(kind: Type, value: u64, span: Span) -> Ast<'ast> {
        Ast::Integer {
            kind,
            value,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_boolean(kind: Type, value: u64, span: Span) -> Ast<'ast> {
        Ast::Boolean {
            kind,
            value,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_char(kind: Type, byte: u64, span: Span) -> Ast<'ast> {
        Ast::Char {
            kind,
            byte,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_cstring(bytes: Vec<u8>, kind: Type, span: Span) -> Ast<'ast> {
        Ast::CString {
            bytes,
            kind,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_cnstring(bytes: Vec<u8>, kind: Type, span: Span) -> Ast<'ast> {
        Ast::CNString {
            bytes,
            kind,
            span,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn new_nullptr(span: Span) -> Ast<'ast> {
        Ast::NullPtr {
            span,
            kind: Type::Ptr {
                subtype: None,
                address_space: None,
                span,
            },
        }
    }

    #[inline]
    pub fn new_unreacheable(kind: Type, span: Span) -> Ast<'ast> {
        Ast::Unreachable {
            span,
            kind,
            id: NodeId::new(),
        }
    }

    #[inline]
    pub fn invalid_ast(span: Span) -> Ast<'ast> {
        Ast::Invalid {
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        }
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum ModuleExpressionValues<'ast> {
    Call {
        arguments: Vec<Ast<'ast>>,
        span: Span,
    },
    Reference {
        name: String,
        span: Span,
    },
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct NodeId {
    pub discriminat: u64,
}

impl NodeId {
    pub fn new() -> Self {
        Self {
            discriminat: self::get_unique_discriminat(),
        }
    }
}

static NODE_DISCRIMINANT_COUNTER: AtomicU64 = AtomicU64::new(0);

fn get_unique_discriminat() -> u64 {
    if NODE_DISCRIMINANT_COUNTER.load(std::sync::atomic::Ordering::Relaxed) >= u64::MAX - 1 {
        thrustc_logging::print_critical_error(
            thrustc_logging::LoggingType::Panic,
            &format!("Current AST NodeId exceeds {}!.", u64::MAX),
        )
    }

    NODE_DISCRIMINANT_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}
