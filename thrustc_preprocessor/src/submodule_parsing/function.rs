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
    submodule_parsing::{attributes, typegeneration},
};

pub fn parse_function<'module_parser>(
    ctx: &mut ModuleParser<'module_parser>,
) -> Result<Symbol, ()> {
    ctx.consume(TokenType::Fn)?;

    let name_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = name_tk.get_lexeme().to_string();
    let span: Span = name_tk.get_span();

    let has_generics: bool = ctx.check(TokenType::LBracket);

    if has_generics {
        ctx.begin_generic_scope();
    }

    let type_params: Option<Vec<String>> = crate::submodule_parsing::parse_generic_parameters(ctx)?;

    ctx.consume(TokenType::LParen)?;

    let mut parameters: Vec<(String, Type, Span)> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.check(TokenType::RParen) {
        let param_name_tk: &Token = ctx.consume(TokenType::Identifier)?;
        let param_name: String = param_name_tk.get_lexeme().to_string();
        let param_span: Span = param_name_tk.get_span();

        ctx.consume(TokenType::Colon)?;

        let param_type: Type = typegeneration::build_type(ctx)?;

        parameters.push((param_name, param_type, param_span));

        if ctx.check(TokenType::RParen) {
            break;
        }

        ctx.consume(TokenType::Comma)?;
    }

    ctx.consume(TokenType::RParen)?;

    let return_type: Type = if ctx.check(TokenType::SemiColon) || ctx.check(TokenType::LBrace) {
        Type::Void { span }
    } else {
        typegeneration::build_type(ctx)?
    };

    if has_generics {
        ctx.end_generic_scope();
    }

    let mut attributes: ThrustAttributes =
        attributes::build_attributes(ctx, &[TokenType::SemiColon, TokenType::LBrace])?;

    let added_public: bool =
        crate::submodule_parsing::ensure_exposed(&mut attributes, &name, span, true);

    if added_public {
        ctx.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0030,
            format!(
                "The module symbol '{name}' lacks the '@public' attribute in its definition. It may fail at link time if referenced from another module."
            ),
            span,
        ));
    }

    if ctx.check(TokenType::LBrace) {
        self::skip_function_body(ctx)?;
    } else {
        ctx.consume(TokenType::SemiColon)?;
    }

    let symbol: Symbol = Symbol {
        name,
        signature: Signature::Function {
            kind: return_type.clone(),
            invalid_kind: Type::Void { span },
            type_params,
            parameters,
            attributes,
            span,
        },
        variant: Variant::Function,
    };

    Ok(symbol)
}

fn skip_function_body<'module_parser>(ctx: &mut ModuleParser<'module_parser>) -> Result<(), ()> {
    ctx.only_advance()?;

    let mut depth: usize = 1;

    while depth > 0 {
        if ctx.check(TokenType::RBrace) {
            depth = depth.saturating_sub(1);
        } else if ctx.check(TokenType::LBrace) {
            depth = depth.saturating_add(1);
        }

        ctx.only_advance()?;
    }

    Ok(())
}
