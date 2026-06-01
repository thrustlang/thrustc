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
    Ast,
    ast_metadata::IndexMetadata,
    traits::{AstCodeLocation, AstGetType},
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{IndexExtensions, TypeExtensions},
};

use crate::{ParserContext, expressions};

pub fn build_index<'parser>(
    ctx: &mut ParserContext<'parser>,
    source_expr: Ast<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let index_type: &Type = source_expr.get_value_type()?;
    let index_expr: Ast = expressions::parse_expr(ctx)?;

    let span: Span = index_expr.get_span();

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let index_type: Type = Type::Ptr {
        subtype: Some(index_type.calculate_index_type(1).clone().into()),
        address_space: index_type.get_address_space(),
        span,
    };

    let metadata: IndexMetadata = IndexMetadata::new(true);

    Ok(Ast::Index {
        source: source_expr.into(),
        index: index_expr.into(),
        metadata,
        kind: index_type,
        span,
    })
}

/*pub fn build_index_deref<'parser>(
    ctx: &mut ParserContext<'parser>,
    source: Ast<'parser>,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let index_type: &Type = source.get_value_type()?;
    let index: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let index_type: Type = Type::Ptr {
        subtype: Some(index_type.calculate_index_type(1).clone().into()),
        address_space: index_type.get_address_space(),
        span,
    };

    Ok(Ast::Index {
        source: source.into(),
        index: index.into(),
        kind: index_type,
        span,
    })
}*/
