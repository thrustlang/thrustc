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
use thrustc_typesystem::{
    Type,
    type_metadata::StructTypeMetadata,
    type_modificators::{
        GCCStructureTypeModificator, LLVMStructureTypeModificator, StructureTypeModificator,
    },
};

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{attributes, typegeneration},
};

pub fn parse_structure<'module_parser>(
    ctx: &mut ModuleParser<'module_parser>,
) -> Result<Symbol, ()> {
    ctx.consume(TokenType::Struct)?;

    let name_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = name_tk.get_lexeme().to_string();
    let span: Span = name_tk.get_span();

    let attributes: ThrustAttributes = attributes::build_attributes(ctx, &[TokenType::LBrace])?;

    ctx.consume(TokenType::LBrace)?;

    let mut field_types: Vec<Type> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.check(TokenType::RBrace) {
        ctx.consume(TokenType::Identifier)?;

        ctx.consume(TokenType::Colon)?;

        let field_type: Type = typegeneration::build_type(ctx)?;

        field_types.push(field_type);

        if ctx.check(TokenType::RBrace) {
            break;
        }

        ctx.consume(TokenType::Comma)?;
    }

    ctx.consume(TokenType::RBrace)?;

    if ctx.check(TokenType::SemiColon) {
        ctx.only_advance()?;
    }

    let is_packed: bool = attributes.iter().any(|attr| attr.is_packed());

    let structure_modificator: StructureTypeModificator = StructureTypeModificator::new(
        LLVMStructureTypeModificator::new(is_packed),
        GCCStructureTypeModificator::new(),
    );

    let metadata: StructTypeMetadata = StructTypeMetadata::new(structure_modificator);

    let structure_type: Type = Type::Struct {
        name: name.clone(),
        fields: field_types,
        metadata,
        span,
    };

    let symbol: Symbol = Symbol {
        name,
        signature: Signature::Struct {
            kind: structure_type,
            invalid_kind: Type::Void { span },
            span,
        },
        variant: Variant::Struct,
    };

    Ok(symbol)
}
