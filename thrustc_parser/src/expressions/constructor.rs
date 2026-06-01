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
use thrustc_entities::parser_entities::{FoundSymbolId, Struct};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, type_modificators::StructureTypeModificator};

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

    let identifier_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected 'identifier' keyword.".into(),
    )?;

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let name: &str = identifier_tk.get_lexeme();
    let span: Span = identifier_tk.get_span();

    let reference: Result<FoundSymbolId, CompilationIssue> =
        ctx.get_symbols().get_symbols_id(name, span);

    match reference {
        Ok(object) => {
            let structure_id: (&str, usize) = object.expected_struct(span)?;
            let id: &str = structure_id.0;
            let scope_idx: usize = structure_id.1;

            let reference: Result<Struct, CompilationIssue> =
                ctx.get_symbols().get_struct_by_id(id, scope_idx, span);

            match reference {
                Ok(object) => {
                    let modificator: StructureTypeModificator = object.get_modificator();

                    let mut data: ConstructorData =
                        ConstructorData::with_capacity(u8::MAX as usize);
                    let mut count: usize = 0;

                    let required: usize = object.get_data().get_fields().len();

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
                                    "You should make sure that it exist in the structure type"
                                        .into(),
                                    None,
                                    field_span,
                                ));

                                continue;
                            }

                            if count >= required {
                                ctx.add_error_report(CompilationIssue::Error(
                                    CompilationIssueCode::E0026,
                                    format!(
                                        "Expected '{}' fields, not '{}' fields.",
                                        required, count
                                    ),
                                    "You should reorder it and fill it out.".into(),
                                    None,
                                    span,
                                ));

                                continue;
                            }

                            let expression: Ast = expressions::parse_expr(ctx)?;

                            if let Some(target_type) = object.get_field_type(field_name) {
                                data.push((field_name, expression, target_type, count as u32));
                            }

                            count += 1;

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

                    let constructor_type: Type = data.get_type(name, modificator, span);

                    Ok(Ast::Constructor {
                        name,
                        data,
                        kind: constructor_type,
                        span,
                        id: NodeId::new(),
                    })
                }
                Err(error) => {
                    ctx.add_error_report(error);

                    Ok(Ast::invalid_ast(span))
                }
            }
        }

        Err(error) => {
            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}
