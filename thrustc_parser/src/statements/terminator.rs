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

use thrustc_ast::{Ast, NodeId, traits::AstGetType};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions};

pub fn parse_return_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let return_tk: &Token = ctx.consume(
        TokenType::Return,
        CompilationIssueCode::E0001,
        "Expected 'return' keyword.".into(),
    )?;

    let span: Span = return_tk.get_span();

    if ctx.match_token(TokenType::SemiColon)? {
        return Ok(Ast::Return {
            expression: None,
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        });
    }

    let value: Ast = expressions::parse_expr(ctx)?;
    let kind: &Type = value.get_value_type()?;

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::Return {
        expression: Some(value.clone().into()),
        kind: kind.clone(),
        span,
        id: NodeId::new(),
    })
}
