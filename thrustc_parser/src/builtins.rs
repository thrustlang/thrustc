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

use thrustc_ast::{Ast, NodeId, ast_builtins::AstBuiltin};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, typegeneration};

pub fn build_compiler_builtin<'parser>(
    ctx: &mut ParserContext<'parser>,
    tk_type: TokenType,
) -> Result<Ast<'parser>, CompilationIssue> {
    match tk_type {
        TokenType::Halloc => self::build_halloc(ctx),
        TokenType::MemSet => self::build_memset(ctx),
        TokenType::MemMove => self::build_memmove(ctx),
        TokenType::MemCpy => self::build_memcpy(ctx),
        TokenType::AbiSizeOf => self::build_abi_size_of(ctx),
        TokenType::BitSizeOf => self::build_bit_size_of(ctx),
        TokenType::AbiAlignOf => self::build_abi_align_of(ctx),

        _ => {
            let token: &Token = ctx.advance()?;
            let lexeme: &str = token.get_lexeme();
            let span: Span = token.get_span();

            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0003,
                format!("Unknown compiler intrinsic '{}'.", lexeme),
                "Compiler intrinsic doesn't exist on the compiler.".into(),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }
    }
}

pub fn build_halloc<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let halloc_tk: &Token = ctx.consume(
        TokenType::Halloc,
        CompilationIssueCode::E0001,
        "Expected 'halloc' keyword.".into(),
    )?;

    let span: Span = halloc_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let of: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::Halloc {
            of: of.clone(),
            span,
        },
        kind: Type::Ptr {
            subtype: Some(of.into()),
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memcpy<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemCpy,
        CompilationIssueCode::E0001,
        String::from("Expected 'memcpy' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let span: Span = memcpy_tk.get_span();

    let source: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemCpy {
            src: source.into(),
            dst: destination.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memmove<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemMove,
        CompilationIssueCode::E0001,
        String::from("Expected 'memmove' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        String::from("Expected '('."),
    )?;

    let span: Span = memcpy_tk.get_span();

    let source: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        String::from("Expected ')'."),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemMove {
            src: source.into(),
            dst: destination.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memset<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemSet,
        CompilationIssueCode::E0001,
        String::from("Expected 'memset' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        String::from("Expected '('."),
    )?;

    let span: Span = memcpy_tk.get_span();

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let new_size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        String::from("Expected ')'."),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemSet {
            dst: destination.into(),
            new_size: new_size.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_bit_size_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::BitSizeOf,
        CompilationIssueCode::E0001,
        String::from("Expected 'bit_size_of' keyword."),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::BitSizeOf { ty, span },
        kind: Type::U64 { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_abi_size_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::AbiSizeOf,
        CompilationIssueCode::E0001,
        "Expected 'abi_size_of' keyword.".into(),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AbiSizeOf { ty, span },
        kind: Type::U64 { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_abi_align_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::AbiAlignOf,
        CompilationIssueCode::E0001,
        "Expected 'abi_align_of' keyword.".into(),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AbiAlignOf { ty, span },
        kind: Type::U32 { span },
        span,
        id: NodeId::new(),
    })
}
