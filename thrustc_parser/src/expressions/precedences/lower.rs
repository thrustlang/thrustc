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

#![allow(clippy::if_same_then_else)]

use thrustc_ast::{Ast, NodeId, traits::AstGetType};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_context::traits::PositionExtensions;
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{TokenType, traits::TokenTypeBuiltinExtensions};
use thrustc_typesystem::{
    Type,
    traits::TypeExtensions,
    type_metadata::{ArrayTypeMetadata, FixedArrayTypeMetadata},
};

use crate::{
    ParserContext, builtins,
    expressions::{
        self, array, asm, call, deref, enum_value, fixed_array, reference, struct_constructor,
    },
    reinterpret,
};

pub fn lower_precedence<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_expression()?;

    let primary_expr: Ast = match &ctx.peek().kind {
        TokenType::New => struct_constructor::build_constructor(ctx)?,

        TokenType::Fixed => fixed_array::build_fixed_array(ctx)?,
        TokenType::LBracket => array::build_array(ctx)?,
        TokenType::Deref => deref::build_dereference(ctx)?,

        TokenType::Asm => asm::build_asm_code_block(ctx)?,

        TokenType::LParen => {
            let lparen_tk: &Token = ctx.consume(
                TokenType::LParen,
                CompilationIssueCode::E0001,
                "Expected '('.".into(),
            )?;

            let span: Span = lparen_tk.get_span();

            let expr: Ast = expressions::parse_expr(ctx)?;
            let expr_type: &Type = expr.get_value_type()?;

            ctx.consume(
                TokenType::RParen,
                CompilationIssueCode::E0001,
                "Expected ')'.".into(),
            )?;

            Ast::Group {
                node: expr.clone().into(),
                kind: expr_type.clone(),
                span,
                id: NodeId::new(),
            }
        }

        TokenType::CString | TokenType::CNString => {
            let tk: &Token = ctx.advance()?;

            let tk_type: TokenType = tk.get_type();
            let content: &str = tk.get_lexeme();
            let span: Span = tk.get_span();

            let mut cstring_type: Type = Type::Const(
                Type::Array {
                    base_type: Type::Char { span }.into(),
                    infered_type: None,
                    metadata: ArrayTypeMetadata::new(None, None),
                    span,
                }
                .into(),
                span,
            );

            let at_variable_position: bool = ctx
                .get_control_context()
                .get_position()
                .is_variable_position();

            let at_static_position: bool = ctx
                .get_control_context()
                .get_position()
                .is_static_position();

            let at_constant_position: bool = ctx
                .get_control_context()
                .get_position()
                .is_constant_position();

            let at_expression_position: bool = ctx
                .get_control_context()
                .get_position()
                .is_expression_position();

            let source: &[u8] = content.as_bytes();

            let mut processed: Vec<u8> = Vec::with_capacity(source.len());
            let mut idx: usize = 0;

            while idx < source.len() {
                if let Some(byte) = source.get(idx) {
                    if *byte == b'\\' {
                        idx = idx.saturating_add(1);

                        match source.get(idx) {
                            Some(b'n') => processed.push(b'\n'),
                            Some(b't') => processed.push(b'\t'),
                            Some(b'r') => processed.push(b'\r'),
                            Some(b'\\') => processed.push(b'\\'),
                            Some(b'0') => processed.push(b'\0'),
                            Some(b'\'') => processed.push(b'\''),
                            Some(b'"') => processed.push(b'"'),

                            _ => (),
                        }

                        idx = idx.saturating_add(1);

                        continue;
                    }

                    if let Some(byte) = source.get(idx) {
                        processed.push(*byte);
                    }

                    idx = idx.saturating_add(1);
                }
            }

            let fixed_array_type: Type = Type::FixedArray {
                base_type: Type::Char { span }.into(),
                size: processed
                    .len()
                    .saturating_add(1)
                    .try_into()
                    .unwrap_or(u32::MAX),
                metadata: FixedArrayTypeMetadata::new(None),
                span,
            };

            {
                if at_variable_position {
                    cstring_type = Type::Array {
                        base_type: Type::Char { span }.into(),
                        infered_type: None,
                        metadata: ArrayTypeMetadata::new(Some(fixed_array_type.into()), None),
                        span,
                    };
                } else if at_static_position {
                    cstring_type = Type::Array {
                        base_type: Type::Char { span }.into(),
                        infered_type: None,
                        metadata: ArrayTypeMetadata::new(Some(fixed_array_type.into()), None),
                        span,
                    };
                } else if at_constant_position {
                    cstring_type = Type::Const(
                        Type::Array {
                            base_type: Type::Char { span }.into(),
                            infered_type: None,
                            metadata: ArrayTypeMetadata::new(Some(fixed_array_type.into()), None),
                            span,
                        }
                        .into(),
                        span,
                    );
                } else if at_expression_position {
                    cstring_type = Type::Const(
                        Type::Array {
                            base_type: Type::Char { span }.into(),
                            infered_type: None,
                            metadata: ArrayTypeMetadata::new(Some(fixed_array_type.into()), None),
                            span,
                        }
                        .into(),
                        span,
                    );
                }
            }

            if tk_type == TokenType::CString {
                Ast::new_cstring(processed, cstring_type, span)
            } else {
                Ast::new_cnstring(processed, cstring_type, span)
            }
        }

        TokenType::Char => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            Ast::new_char(Type::Char { span }, tk.get_lexeme_first_byte(), span)
        }

        TokenType::NullPtr => Ast::new_nullptr(ctx.advance()?.span),

        TokenType::Integer => {
            let tk: &Token = ctx.advance()?;

            let integer: &str = tk.get_lexeme();
            let span: Span = tk.get_span();

            let parsed_integer: (Type, u64) = reinterpret::integer(integer, span)?;

            let kind: Type = parsed_integer.0;
            let value: u64 = parsed_integer.1;

            Ast::new_integer(kind, value, span)
        }

        TokenType::Float => {
            let tk: &Token = ctx.advance()?;

            let float: &str = tk.get_lexeme();
            let span: Span = tk.get_span();

            let parsed_float: (Type, f64) = reinterpret::floating_point(float, span)?;

            let kind: Type = parsed_float.0;
            let value: f64 = parsed_float.1;

            Ast::new_float(kind, value, span)
        }

        TokenType::Identifier => {
            let tk: &Token = ctx.advance()?;

            let name: &str = tk.get_lexeme();
            let span: Span = tk.get_span();

            if ctx.match_token(TokenType::Arrow)? {
                enum_value::build_enum_value(ctx, name, span)?
            } else if ctx.match_token(TokenType::LParen)? {
                call::build_call(ctx, name, span)?
            } else if ctx.match_token(TokenType::ColonColon)? {
                todo!()
            } else {
                reference::build_reference(ctx, name, span)?
            }
        }

        TokenType::Reference => {
            let span: Span = ctx.advance()?.get_span();

            let expr: Ast = expressions::parse_expr(ctx)?;
            let expr_type: &Type = expr.get_value_type()?;

            Ast::GetLocation {
                expr: expr.clone().into(),
                kind: expr_type.get_type_ref(),
                span,
                id: NodeId::new(),
            }
        }

        TokenType::True => {
            let span: Span = ctx.advance()?.get_span();

            Ast::new_boolean(Type::Bool { span }, 1, span)
        }

        TokenType::False => {
            let span: Span = ctx.advance()?.get_span();

            Ast::new_boolean(Type::Bool { span }, 0, span)
        }

        TokenType::Unreachable => {
            let span: Span = ctx.advance()?.get_span();

            Ast::new_unreacheable(Type::Void { span }, span)
        }

        tk_type if tk_type.is_builtin() => builtins::build_compiler_builtin(ctx, *tk_type)?,

        _ => {
            let previous: &Token = ctx.advance()?;
            let span: Span = previous.get_span();

            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                format!(
                    "It is not recognized '{}' as an expression at this point.",
                    previous.get_lexeme()
                ),
                "You should remove it".into(),
                None,
                span,
            ));

            Ast::invalid_ast(span)
        }
    };

    ctx.leave_expression();

    Ok(primary_expr)
}
