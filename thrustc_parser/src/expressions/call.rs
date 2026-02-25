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

    let object: FoundSymbolId = ctx.get_symbols().get_symbols_id(name, span)?;

    let function_type: Type = if object.is_intrinsic() {
        let id: &str = object.expected_intrinsic(span)?;
        let intrinsic: Intrinsic = ctx.get_symbols().get_intrinsic_by_id(span, id)?;

        crate::traits::IntrinsicExtensions::get_type(&intrinsic)
    } else if object.is_function_asm() {
        let id: &str = object.expected_asm_function(span)?;
        let asm_function: thrustc_entities::parser::AssemblerFunction =
            ctx.get_symbols().get_asm_function_by_id(span, id)?;

        crate::traits::FunctionAssemblerExtensions::get_type(&asm_function)
    } else {
        let id: &str = object.expected_function(span)?;
        let function: Function = ctx.get_symbols().get_function_by_id(span, id)?;

        crate::traits::FunctionExtensions::get_type(&function)
    };

    Ok(Ast::Call {
        name,
        args,
        kind: function_type,
        span,
    })
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
