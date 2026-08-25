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

use thrustc_ast::traits::AstGetType;
use thrustc_ast::{
    Ast, NodeId, ast_logic_data::ConstructorData, traits::AstStructureDataExtensions,
};
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::{FoundSymbolId, Struct};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_preprocessor::signatures::{Signature, Variant};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};

use thrustc_parser_external_table::ExternalSymbolTable;
use thrustc_parser_table::traits::{
    ConstructorExtensions, FoundSymbolEitherExtensions, StructSymbolExtensions,
};

use crate::{ParserContext, expressions};

pub fn build_constructor<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::New,
        CompilationIssueCode::E0001,
        "Expected 'new' keyword.".into(),
    )?;

    let first_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected 'identifier' keyword.".into(),
    )?;

    let mut access: Vec<String> = Vec::with_capacity(u8::MAX as usize);
    let mut symbol: &str = first_tk.get_lexeme();
    let mut span: Span = first_tk.get_span();
    let mut qualified: bool = false;

    if ctx.check(TokenType::ColonColon) {
        qualified = true;
        access.push(first_tk.get_lexeme().to_string());

        loop {
            ctx.only_advance()?;

            let part_tk: &Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected identifier after the path separator.".into(),
            )?;

            if ctx.check(TokenType::ColonColon) {
                access.push(part_tk.get_lexeme().to_string());
                continue;
            }

            symbol = part_tk.get_lexeme();
            span = part_tk.get_span();
            break;
        }
    }

    let mut type_params: Option<Vec<String>> = None;
    let metadata: StructTypeMetadata;
    let mut fields: Vec<(String, Type)> = Vec::with_capacity(u8::MAX as usize);

    if qualified {
        let Some(Signature::Struct {
            kind,
            fields: signature_fields,
            type_params: signature_type_params,
            ..
        }) = crate::module_import::resolve_signature(ctx, &access, symbol, Variant::Struct)
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}::{}' not found.", access.join("::"), symbol),
                "The module does not export a structure with that name.".into(),
                None,
                span,
            ));
        };

        metadata = match &kind {
            Type::Struct { metadata, .. } => *metadata,

            _ => {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0028,
                    format!("'{}::{}' is not a structure.", access.join("::"), symbol),
                    "A structure type was expected.".into(),
                    None,
                    span,
                ));
            }
        };

        fields = signature_fields
            .iter()
            .map(|(name, ty, _)| (name.clone(), ty.clone()))
            .collect();

        type_params = signature_type_params.clone();

        let origin: Option<std::path::PathBuf> = ExternalSymbolTable::new(ctx.get_modules())
            .resolve(&access)
            .map(|module| module.get_path().to_path_buf());

        if type_params.is_none() {
            if ctx.get_symbols().has_global_struct(symbol) {
                crate::module_import::check_qualified_collision(
                    ctx,
                    symbol,
                    &access,
                    origin.as_ref(),
                    span,
                )?;
            } else {
                let mut data: Vec<(&str, Type, u32, Span)> = Vec::with_capacity(fields.len());
                let mut position: u32 = 0;

                for (field_name, field_type, field_span) in signature_fields.iter() {
                    data.push((
                        field_name.as_str(),
                        field_type.clone(),
                        position,
                        *field_span,
                    ));
                    position = position.saturating_add(1);
                }

                let _ = ctx.get_mut_symbols().new_global_struct(
                    symbol,
                    (
                        symbol,
                        data.clone(),
                        ThrustAttributes::new(),
                        metadata,
                        span,
                    ),
                );

                ctx.add_ast_node(Ast::Struct {
                    name: symbol,
                    data: (symbol, data.clone(), metadata, span),
                    kind: kind.clone(),
                    attributes: ThrustAttributes::new(),
                    span,
                    id: NodeId::new(),
                });

                if let Some(path) = origin.as_ref() {
                    ctx.get_mut_symbols()
                        .record_import_origin(symbol, path.clone());
                }
            }
        }
    } else if let Some(generic) = ctx.get_symbols().get_generic_struct(symbol).cloned() {
        type_params = Some(generic.type_params);
        metadata = generic.metadata;
        fields = generic
            .field_names
            .iter()
            .zip(generic.field_types.iter())
            .map(|(name, ty)| ((*name).to_string(), ty.clone()))
            .collect();
    } else {
        let reference: Result<FoundSymbolId, CompilationIssue> =
            ctx.get_symbols().get_symbols_id(symbol, span);

        let object: Struct = match reference {
            Ok(object) => {
                let structure_id: (&str, usize) = object.expected_struct(span)?;
                let id: &str = structure_id.0;
                let scope_idx: usize = structure_id.1;

                let reference: Result<Struct, CompilationIssue> =
                    ctx.get_symbols().get_struct_by_id(id, scope_idx, span);

                match reference {
                    Ok(object) => object,
                    Err(error) => {
                        ctx.add_error_report(error);

                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            Err(error) => {
                ctx.add_error_report(error);

                return Ok(Ast::invalid_ast(span));
            }
        };

        metadata = object.get_metadata();

        let struct_data: thrustc_ast::ast_logic_data::StructureData<'parser> = object.get_data();

        for (name, ty, ..) in struct_data.get_struct_fields().iter() {
            fields.push((name.to_string(), ty.clone()));
        }
    }

    let mut explicit_args: Vec<Type> =
        Vec::with_capacity(type_params.as_ref().map_or(0, |params| params.len()));

    if type_params.is_some() && ctx.check(TokenType::LBracket) {
        ctx.consume(
            TokenType::LBracket,
            CompilationIssueCode::E0001,
            "Expected '['.".into(),
        )?;

        loop {
            if ctx.check(TokenType::RBracket) {
                break;
            }

            let argument_type: Type = crate::typegeneration::build_type(ctx, false)?;

            explicit_args.push(argument_type);

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
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let mut data: ConstructorData = ConstructorData::with_capacity(u8::MAX as usize);
    let mut counter: usize = 0;

    let required: usize = fields.len();

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        if ctx.match_token(TokenType::Identifier)? {
            let field_tk: &Token = ctx.previous();
            let field_span: Span = field_tk.get_span();
            let field_name: &str = field_tk.get_lexeme();

            ctx.consume(
                TokenType::Colon,
                CompilationIssueCode::E0001,
                "Expected ':'.".into(),
            )?;

            let field_index: Option<usize> = fields
                .iter()
                .position(|(name, _)| name.as_str() == field_name);

            let Some(field_index) = field_index else {
                ctx.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Unknown field.".into(),
                    "You should make sure that it exist in the structure type".into(),
                    None,
                    field_span,
                ));

                continue;
            };

            if counter >= required {
                ctx.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0026,
                    format!("Expected '{}' fields, not '{}' fields.", required, counter),
                    "You should reorder it and fill it out.".into(),
                    None,
                    span,
                ));

                continue;
            }

            let expression: Ast = expressions::parse_expr(ctx)?;

            let target_type: Type = fields[field_index].1.clone();

            data.push((field_name, expression, target_type, counter as u32));

            counter += 1;

            if ctx.check(TokenType::RBrace) {
                break;
            }

            if ctx.match_token(TokenType::Comma)? {
                if ctx.check(TokenType::RBrace) {
                    break;
                }
            } else if ctx.check_to(TokenType::Identifier, 0) {
                ctx.consume(
                    TokenType::Comma,
                    CompilationIssueCode::E0001,
                    "Expected ','.".into(),
                )?;
            } else {
                ctx.consume(
                    TokenType::Identifier,
                    CompilationIssueCode::E0001,
                    "Expected identifier.".into(),
                )?;
            }
        } else {
            ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected identifier.".into(),
            )?;

            continue;
        }
    }

    let provided: usize = data.len();

    if provided != required {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0027,
            format!(
                "Expected '{}' arguments, but '{}' was gived.",
                required, provided
            ),
            "You fill it out.".into(),
            None,
            span,
        ));
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    if let Some(type_params) = type_params {
        if !explicit_args.is_empty() && explicit_args.len() != type_params.len() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "The generic structure does not receive that many type arguments.".into(),
                "You should provide one type per generic parameter.".into(),
                None,
                span,
            ));
        }

        let raw_field_types: Vec<Type> = data.iter().map(|entry| entry.2.clone()).collect();

        let argument_types: Vec<Type> = data
            .iter()
            .map(|entry| match entry.1.get_value_type() {
                Ok(ty) => ty.clone(),
                Err(_) => Type::Void { span },
            })
            .collect();

        let result: Result<thrustc_generics::SolveResult, CompilationIssue> =
            thrustc_generics::solve(
                &type_params,
                &explicit_args,
                &raw_field_types,
                &argument_types,
                &Type::Void { span },
                false,
                span,
            );

        match result {
            Ok(result) => {
                for entry in data.iter_mut() {
                    entry.2 = thrustc_generics::substitute(&entry.2, &result.env);
                }
            }
            Err(error) => {
                ctx.add_error_report(error);
                return Ok(Ast::invalid_ast(span));
            }
        }
    }

    let constructor_type: Type = data.get_type(symbol, metadata, span);

    let struct_constructor_ast: Ast<'_> = Ast::Constructor {
        name: symbol,
        data,
        kind: constructor_type,
        span,
        id: NodeId::new(),
    };

    Ok(struct_constructor_ast)
}
