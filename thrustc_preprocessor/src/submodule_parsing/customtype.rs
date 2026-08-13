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
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{attributes, typegeneration},
};

pub fn parse_type<'module_parser>(ctx: &mut ModuleParser<'module_parser>) -> Result<Symbol, ()> {
    ctx.consume(TokenType::Type)?;

    let identifier_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = identifier_tk.get_lexeme().to_string();
    let span: Span = identifier_tk.get_span();

    let attributes: ThrustAttributes = attributes::build_attributes(ctx, &[TokenType::Eq])?;

    ctx.consume(TokenType::Eq)?;

    let r#type: Type = typegeneration::build_type(ctx)?;

    ctx.consume(TokenType::SemiColon)?;

    let symbol: Symbol = Symbol {
        name,
        signature: Signature::CustomType {
            kind: r#type,
            invalid_kind: Type::Void { span },
            attributes,
            span,
        },
        variant: Variant::CustomType,
    };

    Ok(symbol)
}
