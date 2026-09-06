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

use thrustc_ast::{Ast, NodeId};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, statements::code_block};

pub fn parse_post_executation_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let defer_tk: &Token = ctx.consume(
        TokenType::Defer,
        CompilationIssueCode::E0001,
        "Expected 'defer'.".into(),
    )?;

    let span: Span = defer_tk.get_span();

    let node: Ast<'_> = if ctx.check(TokenType::LBrace) {
        code_block::parse_code_block_stmt(ctx)?
    } else {
        expressions::parse_expression(ctx)?
    };

    Ok(Ast::Defer {
        node: node.into(),
        kind: Type::Void { span },
        span,
        id: NodeId::new(),
    })
}
