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

use thrustc_ast::{Ast, NodeId};
use thrustc_attributes::ThrustAttributes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_parser_table::GenericCustomTypeEntry;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, attributes, typegeneration};

pub fn build_custom_type<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Type,
        CompilationIssueCode::E0001,
        "Expected 'type' keyword.".into(),
    )?;

    let name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let name: &str = name_tk.get_lexeme();
    let span: Span = name_tk.get_span();

    let has_generics: bool = ctx.check(TokenType::LBracket);

    if has_generics {
        ctx.get_mut_symbols().begin_generic_scope();
    }

    let type_params: Vec<String> = crate::generics::parse_type_parameters(ctx)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::LBrace])?;

    ctx.consume(
        TokenType::Eq,
        CompilationIssueCode::E0001,
        "Expected '='.".into(),
    )?;

    let custom_type: Type = typegeneration::build_type(ctx, false)?;

    if has_generics {
        ctx.get_mut_symbols().end_generic_scope();
    }

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    let is_generic: bool = !type_params.is_empty();

    if parse_forward {
        if is_generic {
            ctx.get_mut_symbols().new_generic_custom_type(
                name,
                GenericCustomTypeEntry {
                    type_params,
                    kind: custom_type.clone(),
                },
            );
        } else {
            ctx.get_mut_symbols()
                .new_global_custom_type(name, (custom_type, attributes))?;
        }

        Ok(Ast::new_nullptr(span))
    } else {
        if is_generic {
            ctx.get_mut_symbols().new_generic_custom_type(
                name,
                GenericCustomTypeEntry {
                    type_params,
                    kind: custom_type.clone(),
                },
            );
        }

        Ok(Ast::CustomType {
            name: name.to_string(),
            kind: custom_type,
            span,
            id: NodeId::new(),
        })
    }
}
