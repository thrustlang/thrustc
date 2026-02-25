use thrustc_ast::{Ast, data::ConstructorData, traits::AstStructureDataExtensions};
use thrustc_entities::parser::{FoundSymbolId, Struct};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, modificators::StructureTypeModificator};

use crate::{
    ParserContext, expressions,
    traits::{ConstructorExtensions, FoundSymbolEitherExtensions, StructSymbolExtensions},
};

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
                                ctx.add_error(CompilationIssue::Error(
                                    CompilationIssueCode::E0001,
                                    "Expected existing field name.".into(),
                                    None,
                                    field_span,
                                ));

                                continue;
                            }

                            if count >= required {
                                ctx.add_error(CompilationIssue::Error(
                                    CompilationIssueCode::E0026,
                                    format!(
                                        "Expected '{}' fields, not '{}' fields.",
                                        required, count
                                    ),
                                    None,
                                    span,
                                ));

                                continue;
                            }

                            let expression: Ast = expressions::build_expr(ctx)?;

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
                    })
                }
                Err(error) => {
                    ctx.add_error(error);

                    Ok(Ast::invalid_ast(span))
                }
            }
        }

        Err(error) => {
            ctx.add_error(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}
