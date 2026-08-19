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

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let object: Struct = if qualified {
        let Some(Signature::Struct { kind, fields, .. }) =
            crate::module_import::resolve_signature(ctx, &access, symbol, Variant::Struct)
        else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}::{}' not found.", access.join("::"), symbol),
                "The module does not export a structure with that name.".into(),
                None,
                span,
            ));
        };

        let metadata: StructTypeMetadata = match kind {
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

        let mut data: Vec<(&str, Type, u32, Span)> = Vec::with_capacity(fields.len());
        let mut position: u32 = 0;

        for (field_name, field_type, field_span) in fields.iter() {
            data.push((field_name, field_type.clone(), position, *field_span));
            position = position.saturating_add(1);
        }

        let origin: Option<std::path::PathBuf> = ExternalSymbolTable::new(ctx.get_modules())
            .resolve(&access)
            .map(|module| module.get_path().to_path_buf());

        if ctx.get_symbols().has_global_struct(symbol) {
            crate::module_import::check_qualified_collision(
                ctx,
                symbol,
                &access,
                origin.as_ref(),
                span,
            )?;
        } else {
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

            if let Some(path) = origin.as_ref() {
                ctx.get_mut_symbols().record_import_origin(symbol, path.clone());
            }
        }

        (symbol, data, ThrustAttributes::new(), metadata, span)
    } else {
        let reference: Result<FoundSymbolId, CompilationIssue> =
            ctx.get_symbols().get_symbols_id(symbol, span);

        match reference {
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
        }
    };

    let metadata: StructTypeMetadata = object.get_metadata();

    let mut data: ConstructorData = ConstructorData::with_capacity(u8::MAX as usize);
    let mut counter: usize = 0;

    let required: usize = object.get_data().get_struct_fields().len();

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

            if !object.contains_field(field_name) {
                ctx.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Unknown field.".into(),
                    "You should make sure that it exist in the structure type".into(),
                    None,
                    field_span,
                ));

                continue;
            }

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

            if let Some(target_type) = object.get_field_type(field_name) {
                data.push((field_name, expression, target_type, counter as u32));
            }

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
