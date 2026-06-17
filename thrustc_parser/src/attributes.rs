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

use ahash::{HashMap, HashMapExt};
use thrustc_ast::{Ast, traits::AstCodeLocation};
use thrustc_attributes::{ThrustAttribute, ThrustAttributes, linkage::ThrustLinkage};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{
    TokenType,
    traits::{TokenTypeAttributesExtensions, TokenTypeExtensions},
};
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, typegeneration};

pub fn build_compiler_attributes<'parser>(
    ctx: &mut ParserContext<'parser>,
    limits: &[TokenType],
) -> Result<ThrustAttributes, CompilationIssue> {
    let mut attributes: ThrustAttributes = Vec::with_capacity(u8::MAX as usize);

    while !limits.contains(&ctx.peek().get_type()) {
        let current_tk: &Token = ctx.peek();
        let span: Span = current_tk.get_span();

        match current_tk.get_type() {
            TokenType::Extern => {
                ctx.consume(
                    TokenType::Extern,
                    CompilationIssueCode::E0001,
                    "Expected '@extern' prologue for an attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::Extern(
                    self::build_external_attribute(ctx)?,
                    span,
                ));
            }

            TokenType::Convention => {
                ctx.consume(
                    TokenType::Convention,
                    CompilationIssueCode::E0001,
                    "Expected '@convention' prologue for an attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::Convention(
                    self::build_call_convention_attribute(ctx)?,
                    span,
                ));
            }

            TokenType::Linkage => {
                ctx.consume(
                    TokenType::Linkage,
                    CompilationIssueCode::E0001,
                    "Expected '@linkage' prologue for an attribute.".into(),
                )?;

                let result: (ThrustLinkage, String) = self::build_linkage_attribute(ctx)?;

                let linkage: ThrustLinkage = result.0;
                let id: String = result.1;

                attributes.push(ThrustAttribute::Linkage(linkage, id, span));
            }

            TokenType::Public => {
                ctx.consume(
                    TokenType::Public,
                    CompilationIssueCode::E0001,
                    "Expected '@public' attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::Public(span));
            }

            TokenType::AsmSyntax => {
                ctx.consume(
                    TokenType::AsmSyntax,
                    CompilationIssueCode::E0001,
                    "Expected '@asmSyntax' prologue for an attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::AsmSyntax(
                    self::build_assembler_syntax_attribute(ctx)?,
                    span,
                ))
            }

            TokenType::Align => {
                ctx.consume(
                    TokenType::Align,
                    CompilationIssueCode::E0001,
                    "Expected '@align' prologue for an attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::Align(
                    self::build_align_attribute(ctx)?,
                    span,
                ))
            }

            TokenType::Promote => {
                ctx.consume(
                    TokenType::Promote,
                    CompilationIssueCode::E0001,
                    "Expected '@promote' prologue for an attribute.".into(),
                )?;

                attributes.push(ThrustAttribute::Promote(
                    self::build_promotion_type_attribute(ctx)?,
                    span,
                ))
            }

            tk_type if tk_type.is_attribute() => {
                if let Some(compiler_attribute) = thrustc_attributes::as_attribute(tk_type, span) {
                    attributes.push(compiler_attribute);
                    ctx.only_advance()?;
                }
            }

            _ => {
                break;
            }
        }
    }

    Ok(attributes)
}

fn build_align_attribute<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<u64, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let expr: Ast<'_> = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    if let Ast::Integer { value, .. } = expr {
        Ok(value)
    } else {
        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            "Expected literal unsigned integer.".into(),
            "You should try to pass a unsigned integer type.".into(),
            None,
            expr.get_span(),
        ))
    }
}

fn build_promotion_type_attribute<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<HashMap<Type, Type>, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let mut promote_types: HashMap<Type, Type> = HashMap::new();

    while !ctx.check(TokenType::RParen) {
        if !ctx.peek().kind.is_type() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected a type to promote!".into(),
                "You should indicate the type to be promoted.".into(),
                None,
                ctx.peek().get_span(),
            ));
        }

        let type_to_promote: Type = typegeneration::build_type(ctx, false)?;

        ctx.consume(
            TokenType::Arrow,
            CompilationIssueCode::E0001,
            "Expected '->'.".into(),
        )?;

        if !ctx.peek().kind.is_type() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected a promotion type representation!".into(),
                "You should indicate the type promoted representation.".into(),
                None,
                ctx.peek().get_span(),
            ));
        }

        let type_promoted: Type = typegeneration::build_type(ctx, false)?;

        promote_types.insert(type_to_promote, type_promoted);

        if !ctx.check(TokenType::RParen) {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;

            continue;
        }
    }

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(promote_types)
}

fn build_linkage_attribute<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<(ThrustLinkage, String), CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let linkage_tk: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected a string literal.".into(),
    )?;

    let id: String = linkage_tk.get_ascii_lexeme().to_string();
    let linkage: ThrustLinkage = ThrustLinkage::get_linkage(&id);

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok((linkage, id))
}

fn build_external_attribute<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<String, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let name: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected a string literal.".into(),
    )?;

    let name: String = name.get_lexeme().to_string();

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(name)
}

fn build_assembler_syntax_attribute<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<String, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let syntax_tk: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected a string literal.".into(),
    )?;

    let syntax: String = syntax_tk.get_lexeme().to_string();

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(syntax)
}

fn build_call_convention_attribute(ctx: &mut ParserContext) -> Result<String, CompilationIssue> {
    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let convention_tk: &Token = ctx.consume_these(
        &[TokenType::CString, TokenType::CNString],
        CompilationIssueCode::E0001,
        "Expected a string literal.".into(),
    )?;

    let name: String = convention_tk.get_lexeme().to_string();

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(name)
}
