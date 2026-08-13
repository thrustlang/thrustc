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
    Ast, NodeId, ast_logic_data::StructureData, traits::AstStructFieldsDataExtensions,
};
use thrustc_attributes::ThrustAttributes;
use thrustc_entities::parser_entities::Struct;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type, type_metadata::StructTypeMetadata, type_modificators::StructureTypeModificator,
};

use thrustc_ast::traits::AstStructureDataExtensions;

use crate::{ParserContext, attributes, modificators, typegeneration};

pub fn build_structure<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Struct,
        CompilationIssueCode::E0001,
        "Expected 'struct' keyword.".into(),
    )?;

    let name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::LBrace])?;
    let modificator: StructureTypeModificator =
        modificators::build_structure_modificator(&attributes);

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let name: &str = name_tk.get_lexeme();
    let span: Span = name_tk.get_span();

    let metadata: StructTypeMetadata = StructTypeMetadata::new(modificator);

    let mut data: StructureData = StructureData::new(name, metadata, span);
    let mut field_position: u32 = 0;

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        if ctx.check(TokenType::Identifier) {
            let field_tk: &Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected identifier.".into(),
            )?;

            let field_name: &str = field_tk.get_lexeme();
            let field_span: Span = field_tk.get_span();

            ctx.consume(
                TokenType::Colon,
                CompilationIssueCode::E0001,
                "Expected ':'.".into(),
            )?;

            let field_type: Type = typegeneration::build_type(ctx, false)?;

            data.1
                .push((field_name, field_type, field_position, field_span));

            field_position = field_position.saturating_add(1);

            if ctx.check(TokenType::RBrace) {
                break;
            } else if ctx.match_token(TokenType::Comma)? {
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
        }
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    if parse_forward {
        let struct_: Struct = (name, data.1, attributes, metadata, span);

        ctx.get_mut_symbols().new_global_struct(name, struct_)?;

        Ok(Ast::new_nullptr(span))
    } else {
        let structure_type: Type = data.get_struct_type();

        let struct_: Ast<'_> = Ast::Struct {
            name,
            data,
            kind: structure_type,
            attributes,
            span,
            id: NodeId::new(),
        };

        Ok(struct_)
    }
}
