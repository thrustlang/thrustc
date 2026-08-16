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
    Ast, NodeId,
    ast_metadata::{ReferenceMetadata, ReferenceType, StaticMetadata},
};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::FunctionParametersTypes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};
use thrustc_parser_external_table::ExternalSymbolTable;
use thrustc_preprocessor::signatures::{Signature, Variant};

use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions};

fn resolve_signature<'parser>(
    ctx: &ParserContext<'parser>,
    access: &[String],
    symbol: &str,
    variant: Variant,
) -> Option<&'parser Signature> {
    let table: ExternalSymbolTable<'parser> = ExternalSymbolTable::new(ctx.get_modules());

    table.search_signature(access, symbol, variant)
}

pub fn build_qualified_expression<'parser>(
    ctx: &mut ParserContext<'parser>,
    access: &[String],
    symbol: &'parser str,
    span: Span,
) -> Result<thrustc_ast::Ast<'parser>, CompilationIssue> {
    if ctx.check(TokenType::LParen) {
        let Some(Signature::Function {
            kind,
            parameters,
            attributes,
            ..
        }) = self::resolve_signature(ctx, access, symbol, Variant::Function)
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}::{}' not found.", access.join("::"), symbol),
                "The module does not export a function with that name.".into(),
                None,
                span,
            ));
        };

        ctx.consume(
            TokenType::LParen,
            CompilationIssueCode::E0001,
            "Expected '('.".into(),
        )?;

        let mut args: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

        loop {
            if ctx.check(TokenType::RParen) {
                break;
            }

            let expr: Ast = expressions::parse_expr(ctx)?;
            args.push(expr);

            if ctx.check(TokenType::RParen) {
                break;
            }

            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        ctx.consume(
            TokenType::RParen,
            CompilationIssueCode::E0001,
            "Expected ')'.".into(),
        )?;

        let return_type: Type = kind.clone();

        let parameter_types: Vec<Type> = parameters.iter().map(|(ty, _)| ty.clone()).collect();

        let has_ignore: bool = attributes.has_ignore_attribute();

        let _ = ctx.get_mut_symbols().new_function(
            symbol,
            (
                return_type.clone(),
                FunctionParametersTypes(parameter_types.clone()),
                has_ignore,
            ),
        );

        self::synthesize_function(
            ctx,
            symbol,
            return_type.clone(),
            parameter_types,
            attributes.clone(),
            span,
        );

        return Ok(Ast::Call {
            name: symbol,
            args,
            kind: return_type,
            span,
            id: NodeId::new(),
        });
    }

    if let Some(Signature::Constant {
        kind,
        attributes,
        modificators,
        ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::Constant)
    {
        let _ = ctx
            .get_mut_symbols()
            .new_global_constant(symbol, (kind.clone(), attributes.clone()));

        self::synthesize_global(
            ctx,
            symbol,
            kind.clone(),
            attributes,
            true,
            modificators,
            span,
        );

        return Ok(Ast::Reference {
            name: symbol,
            kind: kind.clone(),
            span,
            metadata: ReferenceMetadata::new(true, false, ReferenceType::Constant, false),
            id: NodeId::new(),
        });
    }

    if let Some(Signature::Static {
        kind,
        is_mutable,
        attributes,
        modificators,
        ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::Static)
    {
        let metadata: thrustc_ast::ast_metadata::StaticMetadata =
            self::build_static_metadata(*is_mutable, attributes, modificators);

        let _ = ctx
            .get_mut_symbols()
            .new_global_static(symbol, (kind.clone(), metadata, attributes.clone()));

        self::synthesize_global(
            ctx,
            symbol,
            kind.clone(),
            attributes,
            *is_mutable,
            modificators,
            span,
        );

        return Ok(Ast::Reference {
            name: symbol,
            kind: kind.clone(),
            span,
            metadata: ReferenceMetadata::new(true, false, ReferenceType::Static, false),
            id: NodeId::new(),
        });
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0028,
        format!("'{}::{}' not found.", access.join("::"), symbol),
        "The module does not export a symbol with that name.".into(),
        None,
        span,
    ))
}

pub fn resolve_qualified_type<'parser>(
    ctx: &ParserContext<'parser>,
    access: &[String],
    symbol: &str,
) -> Option<thrustc_typesystem::Type> {
    if let Some(Signature::Struct { kind, .. }) =
        self::resolve_signature(ctx, access, symbol, Variant::Struct)
    {
        return Some(kind.clone());
    }

    if let Some(Signature::CustomType { kind, .. }) =
        self::resolve_signature(ctx, access, symbol, Variant::CustomType)
    {
        return Some(kind.clone());
    }

    None
}

fn synthesize_function<'parser>(
    ctx: &mut ParserContext<'parser>,
    symbol: &'parser str,
    return_type: thrustc_typesystem::Type,
    parameter_types: Vec<thrustc_typesystem::Type>,
    attributes: ThrustAttributes,
    span: Span,
) {
    let declaration: Ast = Ast::Function {
        name: symbol,
        ascii_name: symbol,
        parameters: Vec::with_capacity(u8::MAX as usize),
        parameter_types,
        body: None,
        return_type,
        attributes,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

fn synthesize_global<'parser>(
    ctx: &mut ParserContext<'parser>,
    symbol: &'parser str,
    kind: thrustc_typesystem::Type,
    attributes: &ThrustAttributes,
    is_mutable: bool,
    modificators: &Modificators,
    span: Span,
) {
    let metadata: StaticMetadata =
        self::build_static_metadata(is_mutable, attributes, modificators);

    let declaration: Ast = Ast::Static {
        name: symbol,
        ascii_name: symbol,
        kind,
        value: None,
        attributes: attributes.clone(),
        modificators: modificators.clone(),
        metadata,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

fn build_static_metadata(
    is_mutable: bool,
    attributes: &ThrustAttributes,
    modificators: &Modificators,
) -> StaticMetadata {
    let thread_local: bool = modificators.has_lazythread_modificator();
    let volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();
    let thread_mode: Option<ThrustThreadMode> = modificators.get_thread_mode_modificator();
    let external: bool = attributes.has_extern_attribute();

    StaticMetadata::new(
        true,
        is_mutable,
        true,
        thread_local,
        volatile,
        external,
        atomic_ord,
        thread_mode,
    )
}
