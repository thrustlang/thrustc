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

use thrustc_ast::{Ast, NodeId, traits::{AstCodeLocation, AstGetType}};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_context::traits::TypeContextExtensions;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{ConstantTypeExtensions, TypeIsExtensions},
};

use crate::{ParserContext, expressions};

pub fn build_native_vector<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let native_tk: &Token = ctx.consume(
        TokenType::Native,
        CompilationIssueCode::E0001,
        "Expected 'native' keyword.".into(),
    )?;

    self::build_native_vector_after_name(ctx, native_tk.get_span())
}

pub fn build_native_vector_after_name<'parser>(
    ctx: &mut ParserContext<'parser>,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    let infered_type: Option<Type> = ctx.get_type_context().get_infered_type();

    let mut items: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let item: Ast = expressions::parse_expr(ctx)?;

        items.push(item);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(
            TokenType::Comma,
            CompilationIssueCode::E0001,
            "Expected ','.".into(),
        )?;
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let kind: Type = self::resolve_native_vector_type(ctx, &items, infered_type, span)?;

    Ok(Ast::NativeVector {
        items,
        kind,
        span,
        id: NodeId::new(),
    })
}

fn resolve_native_vector_type(
    ctx: &mut ParserContext<'_>,
    items: &[Ast<'_>],
    infered_type: Option<Type>,
    span: Span,
) -> Result<Type, CompilationIssue> {
    if let Some(Type::NativeVector {
        element_type,
        element_count,
        ..
    }) = infered_type
    {
        let non_constant_element_type: Type = element_type.remove_all_constant_type();

        if matches!(non_constant_element_type, Type::NativeVector { .. }) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                "Nested native vectors are not supported.".into(),
                "You should use an array/fixed array of NativeVector values, or a NativeVector of pointers.".into(),
                None,
                span,
            ));
        }

        if element_count as usize != items.len() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0020,
                format!(
                    "Expected '{}' native vector elements, got '{}'.",
                    element_count,
                    items.len()
                ),
                "You should pass the expected number of elements.".into(),
                None,
                span,
            ));
        }

        return Ok(Type::NativeVector {
            element_type,
            element_count,
            span,
        });
    }

    let Some(first) = items.first() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Cannot infer native vector type from an empty native vector.".into(),
            "You should provide at least one element or an expected NativeVector type.".into(),
            None,
            span,
        ));
    };

    let first_type: Type = first.get_value_type()?.clone();

    let non_constant_first_type: Type = first_type.remove_all_constant_type();

    if matches!(non_constant_first_type, Type::NativeVector { .. }) {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Nested native vectors are not supported.".into(),
            "You should use an array/fixed array of NativeVector values, or a NativeVector of pointers.".into(),
            None,
            span,
        ));
    }

    if !first_type.is_integer_type() && !first_type.is_float_type() && !first_type.is_bool_type() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0020,
            format!("Native vector element type '{}' is not supported.", first_type),
            "You should use integer, floating point, char or bool elements.".into(),
            None,
            span,
        ));
    }

    for item in items.iter().skip(1) {
        let item_type: &Type = item.get_value_type()?;

        if item_type != &first_type {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0020,
                format!("Expected '{}' type, got '{}' type.", first_type, item_type),
                "Native vector elements must have the same type.".into(),
                None,
                item.get_span(),
            ));
        }
    }

    let element_count: u32 = u32::try_from(items.len()).map_err(|_| {
        CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Native vector element count is too large.".into(),
            "The element count must fit in a unsigned 32-bit integer.".into(),
            None,
            span,
        )
    })?;

    if element_count == 0 {
        ctx.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Native vector element count must be greater than zero.".into(),
            "You should provide at least one element.".into(),
            None,
            span,
        ));
    }

    Ok(Type::NativeVector {
        element_type: first_type.into(),
        element_count,
        span,
    })
}
