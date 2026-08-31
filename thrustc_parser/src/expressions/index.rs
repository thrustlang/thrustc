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
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{IndexExtensions, TypeExtensions, TypePointerExtensions},
};

use crate::{ParserContext, expressions};

pub fn build_index<'parser>(
    ctx: &mut ParserContext<'parser>,
    source_expr: Ast<'parser>,
    deref: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    let index_type: &Type = source_expr.get_value_type()?;
    let index_expr: Ast = expressions::parse_expr(ctx)?;

    let span: Span = index_expr.get_span();

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let index_type: Type = if deref {
        index_type.calculate_index_type(1).clone()
    } else {
        let inner_index_type: Type = index_type.calculate_index_type(1).clone();

        if !inner_index_type.is_ptr_like_type() {
            Type::Ptr {
                subtype: Some(inner_index_type.into()),
                address_space: index_type.get_address_space(),
                span,
            }
        } else {
            index_type.calculate_index_type(1).clone()
        }
    };

    let metadata: IndexMetadata = IndexMetadata::new(true, deref);

    Ok(Ast::Index {
        source: source_expr.into(),
        index: index_expr.into(),
        metadata,
        kind: index_type,
        span,
    })
}
