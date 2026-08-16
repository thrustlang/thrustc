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

use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{self, attributes, modificators, typegeneration},
};

pub fn parse_static<'module_parser>(ctx: &mut ModuleParser<'module_parser>) -> Result<Symbol, ()> {
    ctx.consume(TokenType::Static)?;

    let is_mutable: bool = ctx.match_token(TokenType::Mut)?;

    let modificators: thrustc_ast_modificators::Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let name_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = name_tk.get_lexeme().to_string();
    let span: Span = name_tk.get_span();

    ctx.consume(TokenType::Colon)?;

    let r#type: Type = typegeneration::build_type(ctx)?;

    let mut attributes: ThrustAttributes =
        attributes::build_attributes(ctx, &[TokenType::Eq, TokenType::SemiColon])?;

    let added_public: bool = submodule_parsing::ensure_exposed(&mut attributes, &name, span, true);

    if added_public {
        ctx.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0030,
            format!(
                "The module symbol '{name}' lacks the '@public' attribute in its definition. It may fail at link time if referenced from another module."
            ),
            span,
        ));
    }

    if ctx.check(TokenType::Eq) {
        ctx.advance_until(TokenType::SemiColon)?;
    } else {
        ctx.consume(TokenType::SemiColon)?;
    }

    let symbol: Symbol = Symbol {
        name,
        signature: Signature::Static {
            kind: r#type.clone(),
            invalid_kind: Type::Void { span },
            is_mutable,
            attributes,
            modificators,
            span,
        },
        variant: Variant::Static,
    };

    Ok(symbol)
}
