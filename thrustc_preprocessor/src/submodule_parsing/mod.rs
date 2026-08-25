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

pub mod attributes;
pub mod constant;
pub mod custom_type;
pub mod expressions;
pub mod function;
pub mod import;
pub mod modificators;
pub mod reinterpret;
pub mod r#static;
pub mod structure;
pub mod typegeneration;

use thrustc_attributes::{ThrustAttribute, ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::parser::ModuleParser;

pub fn parse_generic_parameters(ctx: &mut ModuleParser) -> Result<Option<Vec<String>>, ()> {
    if !ctx.check(TokenType::LBracket) {
        return Ok(None);
    }

    ctx.only_advance()?;

    let mut parameters: Vec<String> = Vec::with_capacity(4);

    while !ctx.check(TokenType::RBracket) {
        let parameter_tk: &Token = ctx.consume(TokenType::Identifier)?;
        let name: String = parameter_tk.get_lexeme().to_string();
        let span: Span = parameter_tk.get_span();

        parameters.push(name.clone());

        ctx.push_type_parameter(name, span);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(TokenType::Comma)?;
    }

    ctx.consume(TokenType::RBracket)?;

    Ok(Some(parameters))
}

pub fn ensure_exposed(
    attributes: &mut ThrustAttributes,
    name: &str,
    span: Span,
    needs_extern: bool,
) -> bool {
    let added_public: bool = !attributes.has_public_attribute();

    if added_public {
        attributes.push(ThrustAttribute::Public(span));
    }

    if needs_extern && !attributes.has_extern_attribute() {
        attributes.push(ThrustAttribute::Extern(name.to_string(), span));
    }

    added_public
}
