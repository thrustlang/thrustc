use thrustc_ast::{
    Ast,
    traits::{AstCodeLocation, AstGetType},
};
use thrustc_entities::parser::{FoundSymbolId, Function, Intrinsic};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::FunctionReferenceExtensions};

use crate::{
    ParserContext, expressions,
    traits::{FoundSymbolEitherExtensions, FoundSymbolExtensions},
};

pub fn build_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let mut args: Vec<Ast> = Vec::with_capacity(10);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let expr: Ast<'_> = expressions::build_expr(ctx)?;

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
                    Ok(intrinsic) => crate::traits::IntrinsicExtensions::get_type(&intrinsic),
                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            } else if object.is_function_asm() {
                let id: &str = object.expected_asm_function(span)?;
                let asm_function: Result<
                    thrustc_entities::parser::AssemblerFunction,
                    CompilationIssue,
                > = ctx.get_symbols().get_asm_function_by_id(span, id);

                match asm_function {
                    Ok(asm_function) => {
                        crate::traits::FunctionAssemblerExtensions::get_type(&asm_function)
                    }

                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            } else {
                let id: &str = object.expected_function(span)?;
                let function: Result<Function, CompilationIssue> =
                    ctx.get_symbols().get_function_by_id(span, id);

                match function {
                    Ok(function) => crate::traits::FunctionExtensions::get_type(&function),
                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            };

            Ok(Ast::Call {
                name,
                args,
                kind: function_type,
                span,
            })
        }

        Err(error) => {
            ctx.add_error(error);
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

    let mut args: Vec<Ast> = Vec::with_capacity(10);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let expr: Ast<'_> = expressions::build_expr(ctx)?;

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
    })
}
