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

use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{attributes, expressions, typegeneration},
};

pub fn parse_enum<'module_parser>(ctx: &mut ModuleParser<'module_parser>) -> Result<Symbol, ()> {
    let enum_tk: &Token = ctx.consume(TokenType::Enum)?;
    let span: Span = enum_tk.get_span();

    let name_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = name_tk.get_lexeme().to_string();

    let mut attributes: ThrustAttributes = attributes::build_attributes(ctx, &[TokenType::LBrace])?;
    let public: bool = attributes.has_public_attribute();
    let added_public: bool = crate::submodule_parsing::ensure_exposed(&mut attributes, &name, span, false);

    if added_public {
        ctx.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0030,
            format!(
                "The module symbol '{name}' lacks the '@public' attribute in its definition. It may fail at link time if referenced from another module."
            ),
            span,
        ));
    }

    ctx.consume(TokenType::LBrace)?;

    let mut fields: Vec<(String, Type, Option<BuiltinValue>, Span)> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.check(TokenType::RBrace) {
        let field_tk: &Token = ctx.consume(TokenType::Identifier)?;
        let field_name: String = field_tk.get_lexeme().to_string();
        let field_span: Span = field_tk.get_span();

        ctx.consume(TokenType::Colon)?;
        let field_type: Type = typegeneration::build_type(ctx)?;
        ctx.consume(TokenType::Eq)?;

        let value: Option<BuiltinValue> = match expressions::parse_expr(ctx) {
            Ok(expression) => thrustc_compile_time::fold(&expression),
            Err(()) => None,
        };

        fields.push((field_name, field_type, value, field_span));

        ctx.consume(TokenType::SemiColon)?;
    }

    ctx.consume(TokenType::RBrace)?;

    Ok(Symbol {
        name,
        signature: Signature::Enum {
            invalid_kind: Type::Void { span },
            fields,
            attributes,
            span,
        },
        variant: Variant::Enum,
        public,
    })
}
