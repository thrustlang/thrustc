use thrustc_ast::Ast;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::ParserContext;

pub fn build_embedded<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Embedded,
        CompilationIssueCode::E0001,
        "Expected 'embedded' keyword.".into(),
    )?;

    let name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let lexeme: &str = name_tk.get_lexeme();
    let span: Span = name_tk.get_span();

    ctx.consume_these(
        &[TokenType::CNString, TokenType::CString],
        CompilationIssueCode::E0001,
        "Expected string literal.".into(),
    )?;

    ctx.consume(
        TokenType::SemiColon,
        CompilationIssueCode::E0001,
        "Expected ';'.".into(),
    )?;

    Ok(Ast::Embedded {
        name: lexeme,
        path: lexeme.into(),
        literal: lexeme,
        kind: Type::Array {
            base_type: Type::U8(span).into(),
            infered_type: None,
            span,
        },
        span,
    })
}
