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

use thrustc_ast::{Ast, NodeId, ast_logic_data::EnumData, traits::AstEnumFieldsDataExtensions};
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::FoundSymbolId;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use thrustc_parser_table::traits::{EnumExtensions, FoundSymbolEitherExtensions};
use thrustc_preprocessor::signatures::{Signature, Variant};

use crate::ParserContext;

pub fn build_enum_value<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let field_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected enum name.".into(),
    )?;

    let field_span: Span = field_tk.get_span();

    let reference: Result<FoundSymbolId, CompilationIssue> =
        ctx.get_symbols().get_symbols_id(name, span);

    match reference {
        Ok(object) => {
            let enum_id: (&str, usize) = object.expected_enum(span)?;
            let id: &str = enum_id.0;
            let scope_idx: usize = enum_id.1;

            match ctx.get_symbols().get_enum_by_id(id, scope_idx, span) {
                Ok(enum_) => {
                    let data: EnumData = enum_.get_fields();
                    let field_name: &str = field_tk.get_lexeme();

                    match data.get_enum_field(field_name) {
                        Some(field) => {
                            let field_type: Type = field.1;
                            let field_value: Ast = field.2;

                            let canonical_name: String = format!("{}.{}", name, field_name);

                            Ok(Ast::EnumValue {
                                name: canonical_name,
                                value: field_value.into(),
                                kind: field_type,
                                span,
                                id: NodeId::new(),
                            })
                        }
                        None => {
                            ctx.add_error_report(CompilationIssue::Error(
                                CompilationIssueCode::E0028,
                                "Unknown field.".into(),
                                "You should make sure that it exist in the enum definition.".into(),
                                None,
                                field_span,
                            ));

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

        Err(error) => {
            if let Some((access, imported_symbol)) =
                crate::imported_scope::find_symbol(ctx, name, span)?
                && imported_symbol.variant == Variant::Enum
            {
                let _qualified_enum: String =
                    thrustc_import::synthesis::ensure_qualified_enum(
                        &mut ctx.import_context(),
                        &access,
                        &imported_symbol.name,
                        span,
                    )?;

                let Some(Signature::Enum { fields, .. }) =
                    thrustc_import::synthesis::resolve_signature(
                        &ctx.import_context(),
                        &access,
                        &imported_symbol.name,
                        Variant::Enum,
                    )
                    .cloned()
                else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0028,
                        format!(
                            "'{}::{}' was not resolved.",
                            access.join("::"),
                            imported_symbol.name
                        ),
                        "The imported enum is unavailable.".into(),
                        None,
                        span,
                    ));
                };

                let field_name: &str = field_tk.get_lexeme();

                return match fields
                    .iter()
                    .find(|(candidate, ..)| candidate == field_name)
                {
                    Some((_, field_type, Some(value), _)) => Ok(Ast::EnumValue {
                        name: format!(
                            "{}::{}::{}",
                            access.join("::"),
                            imported_symbol.name,
                            field_name
                        ),
                        value: Box::new(value.to_ast(field_type.clone(), field_span)),
                        kind: field_type.clone(),
                        span,
                        id: NodeId::new(),
                    }),
                    _ => Err(CompilationIssue::Error(
                        CompilationIssueCode::E0028,
                        "Unknown field.".into(),
                        "You should make sure that it exist in the enum definition.".into(),
                        None,
                        field_span,
                    )),
                };
            }

            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}
