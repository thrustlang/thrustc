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
    traits::{AstCodeLocation, AstGetType},
};
use thrustc_entities::parser_entities::{FoundSymbolId, Function, Intrinsic};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::FunctionReferenceExtensions};

use thrustc_parser_table::traits::{
    FoundSymbolEitherExtensions, FoundSymbolExtensions, FunctionAssemblerExtensions,
    FunctionExtensions, IntrinsicExtensions,
};

use crate::{ParserContext, expressions};

pub fn build_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let mut args: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let expr: Ast<'_> = expressions::parse_expr(ctx)?;

        args.push(expr);

        if ctx.check(TokenType::RParen) {
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
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    let reference: Result<FoundSymbolId, CompilationIssue> =
        ctx.get_symbols().get_symbols_id(name, span);

    match reference {
        Ok(object) => {
            let function_type: Type = if object.is_intrinsic() {
                let id: &str = object.expected_intrinsic(span)?;
                let intrinsic: Result<Intrinsic, CompilationIssue> =
                    ctx.get_symbols().get_intrinsic_by_id(span, id);

                match intrinsic {
                    Ok(intrinsic) => IntrinsicExtensions::get_type(&intrinsic),
                    Err(error) => {
                        ctx.add_error_report(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            } else if object.is_function_asm() {
                let id: &str = object.expected_asm_function(span)?;
                let asm_function: Result<
                    thrustc_entities::parser_entities::AssemblerFunction,
                    CompilationIssue,
                > = ctx.get_symbols().get_asm_function_by_id(span, id);

                match asm_function {
                    Ok(asm_function) => FunctionAssemblerExtensions::get_type(&asm_function),

                    Err(error) => {
                        ctx.add_error_report(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            } else {
                let id: &str = object.expected_function(span)?;
                let function: Result<Function, CompilationIssue> =
                    ctx.get_symbols().get_function_by_id(span, id);

                match function {
                    Ok(function) => FunctionExtensions::get_type(&function),
                    Err(error) => {
                        ctx.add_error_report(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            };

            Ok(Ast::Call {
                name,
                args,
                kind: function_type,
                span,
                id: NodeId::new(),
            })
        }

        Err(error) => {
            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}

pub fn build_anonymous_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    expr: Ast<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let span: Span = expr.get_span();

    let mut args: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let expr: Ast<'_> = expressions::parse_expr(ctx)?;

        args.push(expr);

        if ctx.check(TokenType::RParen) {
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
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    let expr_type: &Type = expr.get_value_type()?;
    let return_type: Type = expr_type.get_function_reference_return_type();

    Ok(Ast::IndirectCall {
        function: expr.clone().into(),
        function_type: expr_type.clone(),
        args,
        kind: return_type,
        span,
        id: NodeId::new(),
    })
}
