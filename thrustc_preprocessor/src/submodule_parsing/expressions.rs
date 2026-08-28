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

use thrustc_ast::{Ast, NodeId, traits::AstCodeLocation};
use thrustc_builtins::{BuiltinArgument, BuiltinFunctionSignature, BuiltinValue};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{reinterpret, typegeneration},
};

pub fn parse_expr(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    self::parse_or(ctx)
}

fn parse_or(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_and(ctx)?;

    while ctx.check(TokenType::Or) {
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_and(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator: TokenType::Or,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_and(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_equality(ctx)?;

    while ctx.check(TokenType::And) {
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_equality(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator: TokenType::And,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_equality(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_relational(ctx)?;

    while ctx.check(TokenType::EqEq) || ctx.check(TokenType::BangEq) {
        let operator: TokenType = ctx.peek().get_type();
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_relational(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_relational(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_additive(ctx)?;

    while ctx.check(TokenType::Less)
        || ctx.check(TokenType::LessEq)
        || ctx.check(TokenType::Greater)
        || ctx.check(TokenType::GreaterEq)
    {
        let operator: TokenType = ctx.peek().get_type();
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_additive(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_additive(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_multiplicative(ctx)?;

    while ctx.check(TokenType::Plus) || ctx.check(TokenType::Minus) {
        let operator: TokenType = ctx.peek().get_type();
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_multiplicative(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_multiplicative(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_unary(ctx)?;

    while ctx.check(TokenType::Star) || ctx.check(TokenType::Slash) || ctx.check(TokenType::Arith) {
        let operator: TokenType = ctx.peek().get_type();
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let right: Ast<'static> = self::parse_unary(ctx)?;

        left = Ast::BinaryOp {
            left: left.into(),
            operator,
            right: right.into(),
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        };
    }

    Ok(left)
}

fn parse_unary(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    if ctx.check(TokenType::Bang)
        || ctx.check(TokenType::Not)
        || ctx.check(TokenType::Minus)
        || ctx.check(TokenType::Plus)
    {
        let operator: TokenType = ctx.peek().get_type();
        let span: Span = ctx.peek().get_span();

        ctx.only_advance()?;

        let node: Ast<'static> = self::parse_unary(ctx)?;

        return Ok(Ast::UnaryOp {
            operator,
            node: node.into(),
            before: true,
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        });
    }

    self::parse_primary(ctx)
}

fn parse_primary(ctx: &mut ModuleParser<'_>) -> Result<Ast<'static>, ()> {
    match ctx.peek().get_type() {
        TokenType::True => {
            let span: Span = ctx.advance()?.get_span();

            Ok(Ast::new_boolean(Type::Bool { span }, 1, span))
        }
        TokenType::False => {
            let span: Span = ctx.advance()?.get_span();

            Ok(Ast::new_boolean(Type::Bool { span }, 0, span))
        }
        TokenType::Integer => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            let (kind, value): (Type, u64) = reinterpret::integer(tk.get_lexeme(), span)?;

            Ok(Ast::new_integer(kind, value, span))
        }
        TokenType::Float => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            let (kind, value): (Type, f64) = reinterpret::floating_point(tk.get_lexeme(), span)?;

            Ok(Ast::new_float(kind, value, span))
        }
        TokenType::Char => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            Ok(Ast::new_char(
                Type::Char { span },
                tk.get_lexeme_first_byte(),
                span,
            ))
        }
        TokenType::NullPtr => {
            let span: Span = ctx.advance()?.get_span();

            Ok(Ast::new_nullptr(span))
        }
        TokenType::LParen => {
            ctx.only_advance()?;

            let expression: Ast<'static> = self::parse_expr(ctx)?;

            ctx.consume(TokenType::RParen)?;

            let span: Span = expression.get_span();

            Ok(Ast::Group {
                node: expression.into(),
                kind: Type::Void { span },
                span,
                id: NodeId::new(),
            })
        }
        TokenType::Identifier => {
            let (name, span): (String, Span) = {
                let tk: &Token = ctx.advance()?;

                (tk.get_lexeme().to_string(), tk.get_span())
            };

            if ctx.check(TokenType::ColonColon) {
                self::parse_qualified_constant_reference(ctx, &name, span)
            } else if ctx.check(TokenType::LParen) {
                self::parse_builtin_call(ctx, &name, span)
            } else {
                self::parse_constant_reference(ctx, &name, span)
            }
        }
        _ => Err(()),
    }
}

fn parse_builtin_call(
    ctx: &mut ModuleParser<'_>,
    name: &str,
    span: Span,
) -> Result<Ast<'static>, ()> {
    ctx.consume(TokenType::LParen)?;

    let signature: BuiltinFunctionSignature = ctx
        .get_builtins()
        .get_function(name)
        .ok_or(())?
        .signature();

    let expected_parameters: usize = signature.get_parameter_count();

    let mut args: Vec<BuiltinArgument> = Vec::with_capacity(expected_parameters);
    let mut index: usize = 0;

    while !ctx.check(TokenType::RParen) {
        let argument: BuiltinArgument =
            if index < expected_parameters && signature.is_parameter_a_type(index) {
                let ty: Type = typegeneration::build_type(ctx)?;
                let argument_span: Span = ctx.previous().get_span();

                BuiltinArgument::Type {
                    ty,
                    span: argument_span,
                }
            } else {
                let expr: Ast<'static> = self::parse_expr(ctx)?;
                let value: BuiltinValue = thrustc_builtins::value::fold(&expr).ok_or(())?;
                let argument_span: Span = expr.get_span();

                BuiltinArgument::Value {
                    value,
                    span: argument_span,
                }
            };

        args.push(argument);
        index = index.saturating_add(1);

        if ctx.check(TokenType::RParen) {
            break;
        }

        ctx.consume(TokenType::Comma)?;
    }

    ctx.consume(TokenType::RParen)?;

    if args.len() != expected_parameters {
        return Err(());
    }

    let value: BuiltinValue = ctx
        .get_builtins()
        .evaluate_function(
            name,
            &args,
            span,
            None,
            ctx.get_options(),
            ctx.get_file(),
        )
        .map_err(|_| ())?;

    Ok(value.to_ast(signature.return_type, span))
}

fn parse_constant_reference(
    ctx: &mut ModuleParser<'_>,
    name: &str,
    span: Span,
) -> Result<Ast<'static>, ()> {
    let symbol: &Symbol = ctx
        .get_module()
        .search_symbol(name.to_string(), Variant::Constant)
        .ok_or(())?;

    let Signature::Constant {
        kind,
        value: Some(value),
        ..
    } = &symbol.signature
    else {
        return Err(());
    };

    Ok(value.to_ast(kind.clone(), span))
}

fn parse_qualified_constant_reference(
    ctx: &mut ModuleParser<'_>,
    module_name: &str,
    span: Span,
) -> Result<Ast<'static>, ()> {
    ctx.consume(TokenType::ColonColon)?;

    let name_tk: &Token = ctx.consume(TokenType::Identifier)?;
    let name: String = name_tk.get_lexeme().to_string();

    let symbol: &Symbol = ctx
        .get_module()
        .find_submodule(vec![module_name.to_string()])
        .and_then(|module| module.search_symbol(name.clone(), Variant::Constant))
        .ok_or(())?;

    let Signature::Constant {
        kind,
        value: Some(value),
        ..
    } = &symbol.signature
    else {
        return Err(());
    };

    Ok(value.to_ast(kind.clone(), span))
}