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

use ahash::{HashMap, HashMapExt};
use thrustc_ast::Ast;
use thrustc_attributes::{ThrustAttribute, ThrustAttributes, linkage::ThrustLinkage};
use thrustc_span::Span;

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{
    TokenType,
    traits::{TokenTypeAttributesExtensions, TokenTypeExtensions},
};
use thrustc_typesystem::Type;

use crate::{
    parser::ModuleParser,
    submodule_parsing::{expressions, typegeneration},
};

pub fn build_attributes<'parser>(
    parser: &mut ModuleParser<'parser>,
    limits: &[TokenType],
) -> Result<ThrustAttributes, ()> {
    let mut attributes: ThrustAttributes = Vec::with_capacity(10);

    while !limits.contains(&parser.peek().get_type()) {
        let current_tk: &Token = parser.peek();
        let span: Span = current_tk.get_span();

        match current_tk.get_type() {
            TokenType::Extern => {
                parser.consume(TokenType::Extern)?;

                attributes.push(ThrustAttribute::Extern(
                    self::build_external_attribute(parser)?,
                    span,
                ));
            }

            TokenType::Convention => {
                parser.consume(TokenType::Convention)?;

                attributes.push(ThrustAttribute::Convention(
                    self::build_call_convention_attribute(parser)?,
                    span,
                ));
            }

            TokenType::Linkage => {
                parser.consume(TokenType::Linkage)?;

                let result: (ThrustLinkage, String) = self::build_linkage_attribute(parser)?;

                let linkage: ThrustLinkage = result.0;
                let id: String = result.1;

                attributes.push(ThrustAttribute::Linkage(linkage, id, span));
            }

            TokenType::Public => {
                attributes.push(ThrustAttribute::Public(span));
                parser.only_advance()?;
            }

            TokenType::AsmSyntax => {
                parser.consume(TokenType::AsmSyntax)?;

                attributes.push(ThrustAttribute::AsmSyntax(
                    self::build_assembler_syntax_attribute(parser)?,
                    span,
                ))
            }

            TokenType::Promote => {
                parser.consume(TokenType::Promote)?;

                attributes.push(ThrustAttribute::Promote(
                    self::build_promotion_type_attribute(parser)?,
                    span,
                ))
            }

            TokenType::Align => {
                parser.consume(TokenType::Align)?;

                attributes.push(ThrustAttribute::Align(
                    self::build_align_attribute(parser)?,
                    span,
                ))
            }

            tk_type if tk_type.is_attribute() => {
                if let Some(compiler_attribute) = thrustc_attributes::as_attribute(tk_type, span) {
                    attributes.push(compiler_attribute);
                    parser.only_advance()?;
                }
            }

            _ => {
                break;
            }
        }
    }

    Ok(attributes)
}

fn build_align_attribute<'parser>(parser: &mut ModuleParser<'parser>) -> Result<u64, ()> {
    parser.consume(TokenType::LParen)?;

    let expr: Ast<'_> = expressions::parse_expr(parser)?;

    parser.consume(TokenType::RParen)?;

    if let Ast::Integer { value, .. } = expr {
        Ok(value)
    } else {
        Err(())
    }
}

fn build_promotion_type_attribute<'parser>(
    parser: &mut ModuleParser<'parser>,
) -> Result<HashMap<Type, Type>, ()> {
    parser.consume(TokenType::LParen)?;

    let mut promote_types: HashMap<Type, Type> = HashMap::new();

    while !parser.check(TokenType::RParen) {
        if !parser.peek().kind.is_type() {
            return Err(());
        }

        let type_to_promote: Type = typegeneration::build_type(parser)?;

        parser.consume(TokenType::Arrow)?;

        if !parser.peek().kind.is_type() {
            return Err(());
        }

        let type_promoted: Type = typegeneration::build_type(parser)?;

        promote_types.insert(type_to_promote, type_promoted);

        if !parser.check(TokenType::RParen) {
            parser.consume(TokenType::Comma)?;

            continue;
        }
    }

    parser.consume(TokenType::RParen)?;

    Ok(promote_types)
}

fn build_linkage_attribute<'parser>(
    parser: &mut ModuleParser<'parser>,
) -> Result<(ThrustLinkage, String), ()> {
    parser.consume(TokenType::LParen)?;

    let linkage_tk: &Token = parser.consume_these(&[TokenType::CString, TokenType::CNString])?;

    let id: String = linkage_tk.get_ascii_lexeme().to_string();
    let linkage: ThrustLinkage = ThrustLinkage::get_linkage(&id);

    parser.consume(TokenType::RParen)?;

    Ok((linkage, id))
}

fn build_external_attribute<'parser>(parser: &mut ModuleParser<'parser>) -> Result<String, ()> {
    parser.consume(TokenType::LParen)?;

    let name: &Token = parser.consume_these(&[TokenType::CString, TokenType::CNString])?;
    let name: String = name.get_lexeme().to_string();

    parser.consume(TokenType::RParen)?;

    Ok(name)
}

fn build_assembler_syntax_attribute<'parser>(
    parser: &mut ModuleParser<'parser>,
) -> Result<String, ()> {
    parser.consume(TokenType::LParen)?;

    let syntax_tk: &Token = parser.consume_these(&[TokenType::CString, TokenType::CNString])?;
    let syntax: String = syntax_tk.get_lexeme().to_string();

    parser.consume(TokenType::RParen)?;

    Ok(syntax)
}

fn build_call_convention_attribute(parser: &mut ModuleParser) -> Result<String, ()> {
    parser.consume(TokenType::LParen)?;

    let convention_tk: &Token = parser.consume_these(&[TokenType::CString, TokenType::CNString])?;
    let name: String = convention_tk.get_lexeme().to_string();

    parser.consume(TokenType::RParen)?;

    Ok(name)
}
