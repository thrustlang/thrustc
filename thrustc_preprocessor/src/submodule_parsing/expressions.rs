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

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{parser::ModuleParser, submodule_parsing::reinterpret};

pub fn parse_expr<'module_parser>(ctx: &mut ModuleParser) -> Result<Ast<'module_parser>, ()> {
    match ctx.peek().get_type() {
        TokenType::Integer => {
            let tk: &Token = ctx.advance()?;

            let integer: &str = tk.get_lexeme();
            let span: Span = tk.get_span();

            let parsed_integer: (Type, u64) = reinterpret::integer(integer, span)?;

            let integer_type: Type = parsed_integer.0;
            let integer_value: u64 = parsed_integer.1;

            Ok(Ast::new_integer(integer_type, integer_value, span))
        }

        TokenType::Identifier => {
            ctx.advance()?;

            Err(())
        }

        _ => Err(()),
    }
}
