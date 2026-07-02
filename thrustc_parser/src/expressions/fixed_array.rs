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
use thrustc_parser_context::traits::TypeContextExtensions;
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{TypeFixedArrayEntensions, TypeIsExtensions},
    type_metadata::FixedArrayTypeMetadata,
};

use crate::{ParserContext, expressions};

pub fn build_fixed_array<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Fixed,
        CompilationIssueCode::E0001,
        "Expected 'fixed' keyword.".into(),
    )?;

    let array_start_tk: &Token = ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    let span: Span = array_start_tk.get_span();

    let infered_type: Option<Type> = ctx.get_type_context().get_infered_type();
    let mut array_type: Type = Type::Void { span };

    let mut items: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let item: Ast = expressions::parse_expr(ctx)?;

        items.push(item);

        if ctx.check(TokenType::RBracket) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    if let Some(item) = items
        .iter()
        .try_fold(None::<&Ast>, |acc: Option<&Ast<'_>>, item| {
            let item_type: &Type = item.get_value_type()?;

            Ok(match acc {
                None => Some(item),
                Some(current) => {
                    let current_type: &Type = current.get_value_type()?;

                    if item_type.get_fixed_array_type_herarchy()
                        > current_type.get_fixed_array_type_herarchy()
                    {
                        Some(item)
                    } else {
                        Some(current)
                    }
                }
            })
        })?
    {
        let size: Result<u32, std::num::TryFromIntError> = u32::try_from(items.len());

        if size.is_err() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Array size is out of bounds.".into(),
                "You must reduce the size of it.".into(),
                None,
                span,
            ));
        }

        array_type = Type::FixedArray {
            base_type: item.get_value_type()?.clone().into(),
            size: size.unwrap_or_default(),
            metadata: FixedArrayTypeMetadata::new(None),
            span,
        };
    }

    {
        let is_unknown_array_type: bool = items.is_empty() && array_type.is_void_type();
        let has_top_infered_type: bool = infered_type
            .as_ref()
            .is_some_and(|ty| ty.is_fixed_array_type());

        if is_unknown_array_type && has_top_infered_type {
            if let Some(infered_type) = infered_type {
                array_type = infered_type;
            }
        }
    }

    Ok(Ast::FixedArray {
        items,
        kind: array_type,
        span,
        id: NodeId::new(),
    })
}
