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
use thrustc_ast::NodeId;
use thrustc_ast::traits::AstCodeLocation;
use thrustc_builtins::BuiltinArgument;
use thrustc_builtins::BuiltinValue;
use thrustc_code_location::Span;
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::context::PreprocessorContext;

pub fn evaluate_condition<'preprocessor>(
    parser: &mut PreprocessorContext<'preprocessor>,
) -> Result<bool, ()> {
    parser.advance()?;

    parser.consume(TokenType::LParen)?;

    let expression: Ast<'static> = self::parse_expression(parser)?;

    parser.consume(TokenType::RParen)?;

    match thrustc_builtins::value::fold(&expression) {
        Some(BuiltinValue::Bool(condition)) => Ok(condition),
        _ => Err(()),
    }
}

pub fn skip_import(parser: &mut PreprocessorContext) -> Result<(), ()> {
    parser.advance_until(TokenType::SemiColon)?;

    Ok(())
}

fn parse_expression(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    self::parse_or(parser)
}

fn parse_or(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_and(parser)?;

    while parser.check(TokenType::Or) {
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_and(parser)?;

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

fn parse_and(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_equality(parser)?;

    while parser.check(TokenType::And) {
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_equality(parser)?;

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

fn parse_equality(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_relational(parser)?;

    while parser.check(TokenType::EqEq) || parser.check(TokenType::BangEq) {
        let operator: TokenType = parser.peek().get_type();
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_relational(parser)?;

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

fn parse_relational(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_additive(parser)?;

    while parser.check(TokenType::Less)
        || parser.check(TokenType::LessEq)
        || parser.check(TokenType::Greater)
        || parser.check(TokenType::GreaterEq)
    {
        let operator: TokenType = parser.peek().get_type();
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_additive(parser)?;

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

fn parse_additive(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_multiplicative(parser)?;

    while parser.check(TokenType::Plus) || parser.check(TokenType::Minus) {
        let operator: TokenType = parser.peek().get_type();
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_multiplicative(parser)?;

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

fn parse_multiplicative(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let mut left: Ast<'static> = self::parse_unary(parser)?;

    while parser.check(TokenType::Star)
        || parser.check(TokenType::Slash)
        || parser.check(TokenType::Arith)
    {
        let operator: TokenType = parser.peek().get_type();
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let right: Ast<'static> = self::parse_unary(parser)?;

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

fn parse_unary(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    if parser.check(TokenType::Bang)
        || parser.check(TokenType::Not)
        || parser.check(TokenType::Minus)
        || parser.check(TokenType::Plus)
    {
        let operator: TokenType = parser.peek().get_type();
        let span: Span = parser.peek().get_span();

        parser.only_advance()?;

        let node: Ast<'static> = self::parse_unary(parser)?;

        return Ok(Ast::UnaryOp {
            operator,
            node: node.into(),
            before: true,
            kind: Type::Void { span },
            span,
            id: NodeId::new(),
        });
    }

    self::parse_primary(parser)
}

fn parse_primary(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    match parser.peek().get_type() {
        TokenType::True => {
            let span: Span = parser.advance()?.get_span();

            Ok(Ast::new_boolean(Type::Bool { span }, 1, span))
        }
        TokenType::False => {
            let span: Span = parser.advance()?.get_span();

            Ok(Ast::new_boolean(Type::Bool { span }, 0, span))
        }
        TokenType::Integer => {
            let span: Span = parser.advance()?.get_span();
            let lexeme: &str = parser.previous().get_lexeme();

            let value: u64 = lexeme.parse::<u64>().map_err(|_| ())?;

            Ok(Ast::new_integer(Type::U64 { span }, value, span))
        }
        TokenType::Identifier => self::parse_builtin_call(parser),
        TokenType::LParen => {
            parser.only_advance()?;

            let expression: Ast<'static> = self::parse_expression(parser)?;

            parser.consume(TokenType::RParen)?;

            let span: Span = expression.get_span();

            Ok(Ast::Group {
                node: expression.into(),
                kind: Type::Void { span },
                span,
                id: NodeId::new(),
            })
        }
        _ => Err(()),
    }
}

fn parse_builtin_call(parser: &mut PreprocessorContext) -> Result<Ast<'static>, ()> {
    let name_tk: &thrustc_token::Token = parser.advance()?;
    let name: String = name_tk.get_lexeme().to_string();
    let span: Span = name_tk.get_span();

    parser.consume(TokenType::LParen)?;

    let mut args: Vec<BuiltinArgument> = Vec::new();

    if !parser.check(TokenType::RParen) {
        loop {
            let argument: Ast<'static> = self::parse_expression(parser)?;

            let value: BuiltinValue = thrustc_builtins::value::fold(&argument).ok_or(())?;
            let argument_span: Span = argument.get_span();

            args.push(BuiltinArgument::Value {
                value,
                span: argument_span,
            });

            if parser.check(TokenType::RParen) {
                break;
            }

            parser.consume(TokenType::Comma)?;
        }
    }

    parser.consume(TokenType::RParen)?;

    let builtins: &thrustc_builtins::BuiltinRegistry = parser.get_builtins();

    let function = builtins.get_function(&name).ok_or(())?;
    let signature: thrustc_builtins::BuiltinFunctionSignature = function.signature();

    if signature.get_parameter_count() != args.len() {
        return Err(());
    }

    let value: BuiltinValue = builtins
        .evaluate_function(
            &name,
            &args,
            span,
            None,
            parser.get_options(),
            parser.get_compilation_unit(),
        )
        .map_err(|_| ())?;

    Ok(value.to_ast(signature.return_type, span))
}