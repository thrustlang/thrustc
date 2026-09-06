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

use thrustc_ast::{
    Ast, NodeId,
    ast_metadata::CastingMetadata,
    traits::{AstConstantExtensions, AstMemoryExtensions},
};
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions::precedences, typegeneration};

pub fn cast_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let mut expression: Ast = precedences::indexation::index_precedence(ctx)?;

    if ctx.match_token(TokenType::As)? {
        let span: Span = ctx.previous().get_span();

        let cast: Type = typegeneration::build_type(ctx, false)?;

        let is_constant: bool = expression.is_constant_value();
        let is_allocated: bool = expression.is_memory_assigned_value()?;

        expression = Ast::As {
            from: expression.into(),
            cast,
            metadata: CastingMetadata::new(is_constant, is_allocated),
            span,
            id: NodeId::new(),
        };
    }

    ctx.leave_expression();

    Ok(expression)
}
