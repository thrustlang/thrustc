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

pub fn parse_constant<'module_parser>(
    ctx: &mut ModuleParser<'module_parser>,
) -> Result<Symbol, ()> {
    ctx.consume(TokenType::Const)?;

    let modificators: thrustc_ast_modificators::Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let identifier_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = identifier_tk.get_lexeme().to_string();
    let span: Span = identifier_tk.get_span();

    ctx.consume(TokenType::Colon)?;

    let r#type: Type = typegeneration::build_type(ctx)?;

    let mut attributes: ThrustAttributes = attributes::build_attributes(ctx, &[TokenType::Eq])?;

    let added_public: bool = submodule_parsing::ensure_exposed(&mut attributes, &name, span, false);

    if added_public {
        ctx.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0030,
            format!(
                "The module symbol '{name}' lacks the '@public' attribute in its definition. It may fail at link time if referenced from another module."
            ),
            span,
        ));
    }

    ctx.consume(TokenType::Eq)?;

    ctx.advance_until(TokenType::SemiColon)?;

    let symbol: Symbol = Symbol {
        name,
        signature: Signature::Constant {
            kind: r#type,
            invalid_kind: Type::Void { span },
            span,
            attributes,
            modificators,
        },
        variant: Variant::Constant,
    };

    Ok(symbol)
}
