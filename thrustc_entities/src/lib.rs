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
    ast_metadata::{ConstantMetadata, FunctionParameterMetadata, LocalMetadata, StaticMetadata},
};
use thrustc_attributes::ThrustAttributes;
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

pub mod analyzer_entities;
pub mod linter_entities;
pub mod parser_entities;
pub mod typechecker_entities;

pub type BinaryOperation<'entity> = (
    &'entity Ast<'entity>,
    &'entity TokenType,
    &'entity Ast<'entity>,
    Span,
);
pub type UnaryOperation<'entity> = (&'entity TokenType, &'entity Type, &'entity Ast<'entity>);

pub type GlobalStatic<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    Option<&'entity Ast<'entity>>,
    &'entity ThrustAttributes,
    StaticMetadata,
    Span,
);

pub type LocalStatic<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    Option<&'entity Ast<'entity>>,
    &'entity ThrustAttributes,
    StaticMetadata,
    Span,
);

pub type GlobalConstant<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    &'entity Ast<'entity>,
    &'entity ThrustAttributes,
    ConstantMetadata,
    Span,
);

pub type LocalConstant<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    &'entity Ast<'entity>,
    &'entity ThrustAttributes,
    ConstantMetadata,
    Span,
);

pub type FunctionParameter<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    u32,
    Span,
    FunctionParameterMetadata,
);

pub type LocalVariable<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    Option<&'entity Ast<'entity>>,
    &'entity ThrustAttributes,
    LocalMetadata,
    Span,
);

pub type Function<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    &'entity [Ast<'entity>],
    &'entity [Type],
    Option<&'entity Ast<'entity>>,
    &'entity ThrustAttributes,
    Span,
);

pub type AssemblerFunction<'entity> = (
    &'entity str,
    &'entity str,
    &'entity str,
    &'entity str,
    &'entity Type,
    &'entity [Ast<'entity>],
    &'entity [Type],
    &'entity ThrustAttributes,
    Span,
);

pub type Intrinsic<'entity> = (
    &'entity str,
    &'entity str,
    &'entity Type,
    &'entity [Ast<'entity>],
    &'entity [Type],
    &'entity ThrustAttributes,
    Span,
);

pub fn assembler_function_from_ast<'entity>(ast: &'entity Ast) -> AssemblerFunction<'entity> {
    if let Ast::AssemblerFunction {
        name,
        ascii_name,
        assembler,
        constraints,
        parameters_types,
        parameters,
        return_type,
        attributes,
        span,
        ..
    } = ast
    {
        return (
            name,
            ascii_name,
            assembler,
            constraints,
            return_type,
            parameters,
            parameters_types,
            attributes,
            *span,
        );
    }

    unreachable!()
}

pub fn intrinsic_from_ast<'entity>(ast: &'entity Ast) -> Intrinsic<'entity> {
    if let Ast::Intrinsic {
        name,
        external_name,
        parameters,
        parameters_types,
        return_type,
        attributes,
        span,
        ..
    } = ast
    {
        return (
            name,
            external_name,
            return_type,
            parameters,
            parameters_types,
            attributes,
            *span,
        );
    }

    unreachable!()
}

pub fn function_from_ast<'entity>(ast: &'entity Ast) -> Function<'entity> {
    if let Ast::Function {
        name,
        ascii_name,
        parameters,
        parameter_types,
        body,
        return_type,
        attributes,
        span,
        ..
    } = ast
    {
        return (
            name,
            ascii_name,
            return_type,
            parameters,
            parameter_types,
            body.as_deref(),
            attributes,
            *span,
        );
    }

    unreachable!()
}

pub fn global_static_from_ast<'entity>(ast: &'entity Ast) -> GlobalStatic<'entity> {
    if let Ast::Static {
        name,
        ascii_name,
        kind,
        value,
        attributes,
        metadata,
        span,
        ..
    } = ast
    {
        return (
            name,
            ascii_name,
            kind,
            value.as_deref(),
            attributes,
            *metadata,
            *span,
        );
    }

    unreachable!()
}

pub fn local_static_from_ast<'entity>(ast: &'entity Ast) -> LocalStatic<'entity> {
    if let Ast::Static {
        name,
        ascii_name,
        kind,
        value,
        metadata,
        attributes,
        span,
        ..
    } = ast
    {
        return (
            name,
            ascii_name,
            kind,
            value.as_deref(),
            attributes,
            *metadata,
            *span,
        );
    }

    unreachable!()
}

pub fn global_constant_from_ast<'entity>(ast: &'entity Ast) -> GlobalConstant<'entity> {
    if let Ast::Const {
        name,
        ascii_name,
        kind,
        value,
        attributes,
        metadata,
        span,
        ..
    } = ast
    {
        return (
            name, ascii_name, kind, &**value, attributes, *metadata, *span,
        );
    }

    unreachable!()
}

pub fn local_constant_from_ast<'entity>(ast: &'entity Ast) -> LocalConstant<'entity> {
    if let Ast::Const {
        name,
        ascii_name,
        kind,
        value,
        metadata,
        span,
        attributes,
        ..
    } = ast
    {
        return (
            name, ascii_name, kind, &**value, attributes, *metadata, *span,
        );
    }

    unreachable!()
}

pub fn local_variable_from_ast<'entity>(ast: &'entity Ast) -> LocalVariable<'entity> {
    if let Ast::Var {
        name,
        ascii_name,
        kind,
        value,
        attributes,
        metadata,
        span,
        ..
    } = ast
    {
        return (
            name,
            ascii_name,
            kind,
            value.as_deref(),
            attributes,
            *metadata,
            *span,
        );
    }

    unreachable!()
}

#[inline]
pub fn function_parameter_from_ast<'entity>(ast: &'entity Ast) -> FunctionParameter<'entity> {
    if let Ast::FunctionParameter {
        name,
        ascii_name,
        kind,
        position,
        metadata,
        span,
        ..
    } = ast
    {
        return (name, ascii_name, kind, *position, *span, *metadata);
    }

    unreachable!()
}
