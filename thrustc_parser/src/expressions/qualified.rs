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

#![allow(clippy::too_many_arguments)]

use thrustc_ast::{
    Ast, NodeId,
    ast_metadata::{ReferenceMetadata, ReferenceType},
    traits::AstGetType,
};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_external_table::ExternalSymbolTable;
use thrustc_parser_table::GenericFunctionEntry;
use thrustc_preprocessor::signatures::{Signature, Variant};

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;
use thrustc_typesystem::traits::VoidTypeExtensions;

use crate::{ParserContext, expressions, typegeneration};

pub fn build_qualified_expression<'parser>(
    ctx: &mut ParserContext<'parser>,
    access: &[String],
    symbol: &'parser str,
    span: Span,
) -> Result<thrustc_ast::Ast<'parser>, CompilationIssue> {
    let qualified_symbol: String =
        thrustc_import_synthesis::synthesis::qualified_symbol_name(access, symbol);

    let origin: Option<std::path::PathBuf> = ExternalSymbolTable::new(ctx.get_modules())
        .resolve(access)
        .map(|module| module.get_path().to_path_buf());

    if ctx.match_token(TokenType::FatArrow)? {
        let Some(Signature::Enum { fields, .. }) =
            thrustc_import_synthesis::synthesis::resolve_signature(
                &ctx.import_context(),
                access,
                symbol,
                Variant::Enum,
            )
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}::{}' not found.", access.join("::"), symbol),
                "The module does not export an enum with that name.".into(),
                None,
                span,
            ));
        };

        let field_tk: &Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected enum field name.".into(),
        )?;
        let field_name: &str = field_tk.get_lexeme();
        let field_span: Span = field_tk.get_span();

        let Some((_, field_type, Some(value), _)) = fields
            .iter()
            .find(|(candidate, ..)| candidate == field_name)
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                "Unknown field.".into(),
                "You should make sure that it exist in the enum definition.".into(),
                None,
                field_span,
            ));
        };

        return Ok(Ast::EnumValue {
            name: format!("{}::{}.{}", access.join("::"), symbol, field_name),
            value: Box::new(value.to_ast(field_type.clone(), field_span)),
            kind: field_type.clone(),
            span,
            id: NodeId::new(),
        });
    }

    if ctx.check(TokenType::LParen) || ctx.check(TokenType::LBracket) {
        if let Some(Signature::CompilerIntrinsic {
            kind,
            external_name,
            parameters,
            attributes,
            ..
        }) = thrustc_import_synthesis::synthesis::resolve_signature(
            &ctx.import_context(),
            access,
            symbol,
            Variant::CompilerIntrinsic,
        ) {
            ctx.consume(
                TokenType::LParen,
                CompilationIssueCode::E0001,
                "Expected '('.".into(),
            )?;

            let arguments: expressions::call::ParsedCallArguments =
                expressions::call::parse_call_arguments(ctx)?;

            if !arguments.named.is_empty() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0044,
                    "Named arguments are not supported for compiler intrinsics.".into(),
                    "You should use only positional arguments.".into(),
                    None,
                    span,
                ));
            }

            let parameter_types: Vec<Type> =
                parameters.iter().map(|(_, ty, _)| ty.clone()).collect();

            if !ctx.get_symbols().has_compiler_intrinsic(&qualified_symbol) {
                let _ = ctx.get_mut_symbols().new_compiler_intrinsic(
                    qualified_symbol.clone(),
                    (
                        kind.clone(),
                        thrustc_entities::parser_entities::IntrinsicParametersTypes(
                            parameter_types.clone(),
                        ),
                        attributes.has_ignore_attribute(),
                    ),
                );

                if let Some(path) = origin.as_ref() {
                    ctx.get_mut_symbols()
                        .record_import_origin(qualified_symbol.clone(), path.clone());
                }

                thrustc_import_synthesis::synthesis::synthesize_compiler_intrinsic(
                    &mut ctx.import_context(),
                    &qualified_symbol,
                    external_name,
                    kind.clone(),
                    parameter_types.clone(),
                    attributes.clone(),
                    span,
                );
            }

            return Ok(Ast::Call {
                name: qualified_symbol.clone(),
                args: arguments.positional,
                generic_args: Vec::with_capacity(0),
                kind: kind.clone(),
                span,
                id: NodeId::new(),
            });
        }

        let Some(Signature::Function {
            kind,
            demangling_name,
            parameters,
            attributes,
            type_params,
            ..
        }) = thrustc_import_synthesis::synthesis::resolve_signature(
            &ctx.import_context(),
            access,
            symbol,
            Variant::Function,
        )
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}::{}' not found.", access.join("::"), symbol),
                "The module does not export a function with that name.".into(),
                None,
                span,
            ));
        };

        if let Some(type_params) = type_params {
            return self::build_qualified_generic_call(
                ctx,
                access,
                symbol,
                span,
                type_params,
                kind.clone(),
                parameters,
                attributes.clone(),
            );
        }

        ctx.consume(
            TokenType::LParen,
            CompilationIssueCode::E0001,
            "Expected '('.".into(),
        )?;

        let arguments: expressions::call::ParsedCallArguments =
            expressions::call::parse_call_arguments(ctx)?;

        let return_type: Type = kind.clone();

        let parameter_types: Vec<Type> = parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
        let parameter_names: Vec<String> =
            parameters.iter().map(|(name, ..)| name.clone()).collect();

        let has_ignore: bool = attributes.has_ignore_attribute();

        let parameter_names_refs: Vec<&str> = parameter_names.iter().map(String::as_str).collect();

        let args: Vec<Ast> = match expressions::call::reorder_call_arguments(
            symbol,
            span,
            arguments,
            &parameter_names_refs,
            has_ignore,
        ) {
            Ok(args) => args,
            Err(error) => {
                ctx.add_error_report(error);
                return Ok(Ast::invalid_ast(span));
            }
        };

        if !thrustc_import_synthesis::synthesis::has_any_synthetized_function(
            &ctx.import_context(),
            &qualified_symbol,
        ) {
            thrustc_import_synthesis::synthesis::synthesize_function(
                &mut ctx.import_context(),
                &qualified_symbol,
                symbol,
                return_type.clone(),
                parameter_types,
                parameter_names,
                attributes.clone(),
                demangling_name.clone(),
                span,
            );
        }

        return Ok(Ast::Call {
            name: qualified_symbol,
            args,
            generic_args: Vec::with_capacity(0),
            kind: return_type,
            span,
            id: NodeId::new(),
        });
    }

    if let Some(Signature::Constant {
        kind,
        attributes,
        modificators,
        value,
        ..
    }) = thrustc_import_synthesis::synthesis::resolve_signature(
        &ctx.import_context(),
        access,
        symbol,
        Variant::Constant,
    ) {
        let qualified_symbol: String =
            thrustc_import_synthesis::synthesis::qualified_symbol_name(access, symbol);

        if !ctx.get_symbols().has_global_constant(&qualified_symbol) {
            let folded_value: Option<Ast> = value.as_ref().map(|v| v.to_ast(kind.clone(), span));

            let _ = ctx.get_mut_symbols().new_global_constant(
                qualified_symbol.clone(),
                (kind.clone(), attributes.clone(), folded_value.clone()),
            );

            if let Some(path) = origin.as_ref() {
                ctx.get_mut_symbols()
                    .record_import_origin(qualified_symbol.clone(), path.clone());
            }

            let Some(value_ast) = folded_value else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0028,
                    format!(
                        "'{}::{}' cannot be resolved to a compile-time value.",
                        access.join("::"),
                        symbol
                    ),
                    "The referenced constant cannot be resolved to a compile-time value; it must be resolvable to a literal."
                        .into(),
                    None,
                    span,
                ));
            };

            thrustc_import_synthesis::synthesis::synthesize_constant(
                &mut ctx.import_context(),
                &qualified_symbol,
                kind.clone(),
                &value_ast,
                attributes,
                modificators,
                span,
            );
        }

        let metadata: ReferenceMetadata =
            ReferenceMetadata::new(true, false, ReferenceType::Constant, false, None);

        return Ok(Ast::Reference {
            name: qualified_symbol,
            kind: kind.clone(),
            span,
            metadata,
            id: NodeId::new(),
        });
    }

    if let Some(Signature::Static {
        kind,
        is_mutable,
        attributes,
        modificators,
        ..
    }) = thrustc_import_synthesis::synthesis::resolve_signature(
        &ctx.import_context(),
        access,
        symbol,
        Variant::Static,
    ) {
        let qualified_symbol: String =
            thrustc_import_synthesis::synthesis::qualified_symbol_name(access, symbol);

        if !ctx.get_symbols().has_global_static(&qualified_symbol) {
            let metadata: thrustc_ast::ast_metadata::StaticMetadata =
                thrustc_import_synthesis::synthesis::build_static_metadata(
                    *is_mutable,
                    attributes,
                    modificators,
                );

            let _ = ctx.get_mut_symbols().new_global_static(
                qualified_symbol.clone(),
                (kind.clone(), metadata, attributes.clone()),
            );

            if let Some(path) = origin.as_ref() {
                ctx.get_mut_symbols()
                    .record_import_origin(qualified_symbol.clone(), path.clone());
            }

            thrustc_import_synthesis::synthesis::synthesize_global(
                &mut ctx.import_context(),
                &qualified_symbol,
                kind.clone(),
                attributes,
                *is_mutable,
                modificators,
                span,
            );
        }

        let metadata: ReferenceMetadata =
            ReferenceMetadata::new(true, false, ReferenceType::Static, false, None);

        return Ok(Ast::Reference {
            name: qualified_symbol,
            kind: kind.clone(),
            span,
            metadata,
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

fn build_qualified_generic_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    access: &[String],
    symbol: &'parser str,
    span: Span,
    type_params: &[String],
    kind: Type,
    parameters: &[(String, Type, Span)],
    attributes: ThrustAttributes,
) -> Result<thrustc_ast::Ast<'parser>, CompilationIssue> {
    let origin: Option<std::path::PathBuf> = ExternalSymbolTable::new(ctx.get_modules())
        .resolve(access)
        .map(|module| module.get_path().to_path_buf());

    let qualified_symbol: String =
        thrustc_import_synthesis::synthesis::qualified_symbol_name(access, symbol);

    let parameter_types: Vec<Type> = parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
    let parameter_names: Vec<String> = parameters.iter().map(|(name, ..)| name.clone()).collect();
    let has_ignore: bool = attributes.has_ignore_attribute();

    let mut generic_args: Vec<Type> = Vec::with_capacity(type_params.len());

    if ctx.match_token(TokenType::LBracket)? {
        loop {
            if ctx.check(TokenType::RBracket) {
                break;
            }

            let argument_type: Type = typegeneration::build_type(ctx, false)?;

            generic_args.push(argument_type);

            if ctx.check(TokenType::RBracket) {
                break;
            }

            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;
    }

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let arguments: expressions::call::ParsedCallArguments =
        expressions::call::parse_call_arguments(ctx)?;

    let parameter_names_refs: Vec<&str> = parameter_names.iter().map(String::as_str).collect();

    let args: Vec<Ast> = match expressions::call::reorder_call_arguments(
        symbol,
        span,
        arguments,
        &parameter_names_refs,
        has_ignore,
    ) {
        Ok(args) => args,
        Err(error) => {
            ctx.add_error_report(error);
            return Ok(Ast::invalid_ast(span));
        }
    };

    let argument_types: Vec<Type> = args
        .iter()
        .map(|argument| match argument.get_value_type() {
            Ok(ty) => ty.clone(),
            Err(_) => Type::Void { span },
        })
        .collect();

    let must_defer: bool = generic_args
        .iter()
        .chain(argument_types.iter())
        .any(|ty| ty.contains_an_unresolved_type());

    if must_defer {
        if !ctx.get_symbols().has_generic_function(&qualified_symbol) {
            ctx.get_mut_symbols().new_generic_function(
                qualified_symbol.clone(),
                GenericFunctionEntry {
                    name: symbol.to_string(),
                    type_params: type_params.to_vec(),
                    parameter_types: parameter_types.clone(),
                    parameter_names: parameter_names.clone(),
                    return_type: kind.clone(),
                    attributes: attributes.clone(),
                    has_local_template: false,
                    has_varargs: has_ignore,
                    span,
                },
            );

            if let Some(origin) = origin.as_ref() {
                ctx.get_mut_symbols()
                    .record_import_origin(qualified_symbol.clone(), origin.clone());
            }
        }

        return Ok(Ast::Call {
            name: qualified_symbol,
            args,
            generic_args,
            kind,
            span,
            id: NodeId::new(),
        });
    }

    let result: thrustc_generics::SolveResult = match thrustc_generics::solve(
        type_params,
        &generic_args,
        &parameter_types,
        &argument_types,
        &kind,
        has_ignore,
        span,
    ) {
        Ok(result) => result,
        Err(error) => {
            ctx.add_error_report(error);

            return Ok(Ast::invalid_ast(span));
        }
    };

    let origin_key: Option<String> = origin
        .as_ref()
        .map(|path| path.to_string_lossy().to_string());
    let key: String =
        thrustc_generics::instantiation_key(origin_key.as_deref(), symbol, &result.env);

    if !thrustc_import_synthesis::synthesis::has_any_synthetized_function(
        &ctx.import_context(),
        &key,
    ) {
        let concrete_parameter_types: Vec<Type> = parameter_types
            .iter()
            .map(|parameter| thrustc_generics::substitute(parameter, &result.env))
            .collect();

        let concrete_return_type: Type = thrustc_generics::substitute(&kind, &result.env);
        let demangling_name: String = origin
            .as_ref()
            .and_then(|path| path.file_stem())
            .map_or_else(
                || format!("{}.{key}", access.join("_")),
                |module| format!("{}.{}", module.to_string_lossy(), key),
            );

        thrustc_import_synthesis::synthesis::synthesize_function(
            &mut ctx.import_context(),
            &key,
            symbol,
            concrete_return_type.clone(),
            concrete_parameter_types,
            parameter_names.clone(),
            attributes.clone(),
            demangling_name,
            span,
        );
    }

    if let Some(origin) = origin {
        thrustc_generics::record_pending(origin, symbol.to_string(), result.env);
    }

    Ok(Ast::Call {
        name: key,
        args,
        generic_args: Vec::with_capacity(0),
        kind: result.return_type,
        span,
        id: NodeId::new(),
    })
}
