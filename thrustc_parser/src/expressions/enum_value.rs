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
use thrustc_entities::parser_entities::FoundSymbolId;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use thrustc_parser_table::traits::{EnumExtensions, FoundSymbolEitherExtensions};

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

                    match data.get_field(field_name) {
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
                                "You should make sure that it existion in the enum definition."
                                    .into(),
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
            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}
