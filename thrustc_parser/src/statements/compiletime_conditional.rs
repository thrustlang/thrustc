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
use thrustc_ast::ast_builtins::AstBuiltin;
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, statements, statements::code_block};

pub fn build_compiletime_conditional<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let span: Span = ctx.peek().get_span();

    let first_condition: CompileTimeCondition<'parser> = self::evaluate_condition(ctx)?;

    let mut active: Option<Ast<'parser>> = None;

    match first_condition {
        CompileTimeCondition::Known(true) => active = Some(self::parse_branch(ctx)?),
        CompileTimeCondition::Known(false) => self::parse_branch_discarded(ctx)?,
        CompileTimeCondition::Deferred(condition) => {
            return self::parse_deferred_compiletime_conditional(ctx, condition, span);
        }
    }

    loop {
        if ctx.check(TokenType::ElifAttribute) {
            let condition: CompileTimeCondition<'parser> = self::evaluate_condition(ctx)?;

            match condition {
                CompileTimeCondition::Known(true) if active.is_none() => {
                    active = Some(self::parse_branch(ctx)?)
                }
                CompileTimeCondition::Known(_) => self::parse_branch_discarded(ctx)?,
                CompileTimeCondition::Deferred(condition) if active.is_none() => {
                    return self::parse_deferred_compiletime_conditional(ctx, condition, span);
                }
                CompileTimeCondition::Deferred(_) => self::parse_branch_discarded(ctx)?,
            }

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) && ctx.check_to(TokenType::If, 1) {
            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            let condition: CompileTimeCondition<'parser> = self::evaluate_condition(ctx)?;

            match condition {
                CompileTimeCondition::Known(true) if active.is_none() => {
                    active = Some(self::parse_branch(ctx)?)
                }
                CompileTimeCondition::Known(_) => self::parse_branch_discarded(ctx)?,
                CompileTimeCondition::Deferred(condition) if active.is_none() => {
                    return self::parse_deferred_compiletime_conditional(ctx, condition, span);
                }
                CompileTimeCondition::Deferred(_) => self::parse_branch_discarded(ctx)?,
            }

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) {
            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            if active.is_none() {
                active = Some(self::parse_branch(ctx)?);
            } else {
                self::parse_branch_discarded(ctx)?;
            }

            break;
        }

        break;
    }

    match active {
        Some(ast) => Ok(ast),
        None => {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0019,
                "The '@if' compile-time conditional has no active branch.".into(),
                "Every '@if'/'@elif' condition was false and there is no '@else' branch. Make a condition true or add an '@else' branch.".into(),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }
    }
}

fn parse_deferred_compiletime_conditional<'parser>(
    ctx: &mut ParserContext<'parser>,
    condition: Ast<'parser>,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let then_branch: Ast<'parser> = self::parse_branch(ctx)?;
    let mut else_if_branch: Vec<Ast<'parser>> = Vec::with_capacity(4);
    let mut else_branch: Option<std::boxed::Box<Ast<'parser>>> = None;

    loop {
        if ctx.check(TokenType::ElifAttribute) {
            let condition: Ast<'parser> = self::evaluate_condition(ctx)?.into_ast();
            let block: Ast<'parser> = self::parse_branch(ctx)?;

            else_if_branch.push(Ast::Elif {
                condition: std::boxed::Box::new(condition),
                block: std::boxed::Box::new(block),
                kind: Type::Void { span },
                span,
                id: thrustc_ast::NodeId::new(),
            });

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) && ctx.check_to(TokenType::If, 1) {
            let else_span: Span = ctx.peek().get_span();

            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            let condition: Ast<'parser> = self::evaluate_condition(ctx)?.into_ast();
            let block: Ast<'parser> = self::parse_branch(ctx)?;

            else_if_branch.push(Ast::Elif {
                condition: std::boxed::Box::new(condition),
                block: std::boxed::Box::new(block),
                kind: Type::Void { span: else_span },
                span: else_span,
                id: thrustc_ast::NodeId::new(),
            });

            continue;
        }

        if ctx.check(TokenType::ElseAttribute) {
            let else_span: Span = ctx.peek().get_span();

            ctx.consume(
                TokenType::ElseAttribute,
                CompilationIssueCode::E0001,
                "Expected '@else'.".into(),
            )?;

            let block: Ast<'parser> = self::parse_branch(ctx)?;

            else_branch = Some(std::boxed::Box::new(Ast::Else {
                block: std::boxed::Box::new(block),
                kind: Type::Void { span: else_span },
                span: else_span,
                id: thrustc_ast::NodeId::new(),
            }));

            break;
        }

        break;
    }

    Ok(Ast::CompileTimeIf {
        condition: std::boxed::Box::new(condition),
        then_branch: std::boxed::Box::new(then_branch),
        else_if_branch,
        else_branch,
        kind: Type::Void { span },
        span,
        id: thrustc_ast::NodeId::new(),
    })
}

fn parse_branch<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    if ctx.check(TokenType::LBrace) {
        code_block::parse_code_block_stmt(ctx)
    } else {
        statements::parse(ctx)
    }
}

fn parse_branch_discarded<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<(), CompilationIssue> {
    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let result: Result<(), CompilationIssue> = self::parse_branch(ctx).map(|_| ());

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    result
}

pub(crate) fn evaluate_condition<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<CompileTimeCondition<'parser>, CompilationIssue> {
    let if_tk: &Token = ctx.advance()?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let expression: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    match thrustc_compile_time::fold(&expression) {
        Some(BuiltinValue::Bool(condition)) => Ok(CompileTimeCondition::Known(condition)),
        _ if self::contains_deferred_builtin(&expression) => {
            Ok(CompileTimeCondition::Deferred(expression))
        }
        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "The compile-time condition must be a constant boolean.".into(),
            "You should use a constant expression, like @if(isLinux()) or @if(1 + 1 == 2).".into(),
            None,
            if_tk.get_span(),
        )),
    }
}

pub(crate) enum CompileTimeCondition<'parser> {
    Known(bool),
    Deferred(Ast<'parser>),
}

impl<'parser> CompileTimeCondition<'parser> {
    fn into_ast(self) -> Ast<'parser> {
        match self {
            Self::Known(value) => Ast::new_boolean(
                Type::Bool {
                    span: Span::nothing(),
                },
                value as u64,
                Span::nothing(),
            ),
            Self::Deferred(ast) => ast,
        }
    }
}

fn contains_deferred_builtin(node: &Ast<'_>) -> bool {
    match node {
        Ast::Builtin {
            builtin: AstBuiltin::DeferredCompileTime { .. },
            ..
        } => true,
        Ast::Group { node, .. } => self::contains_deferred_builtin(node),
        Ast::BinaryOp { left, right, .. } => {
            self::contains_deferred_builtin(left) || self::contains_deferred_builtin(right)
        }
        Ast::UnaryOp { node, .. } => self::contains_deferred_builtin(node),
        Ast::As { from, .. } => self::contains_deferred_builtin(from),
        _ => false,
    }
}
