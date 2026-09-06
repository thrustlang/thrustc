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

use thrustc_ast::ast_builtins::{AstBuiltin, DeferredBuiltinArgument};
use thrustc_ast::traits::AstCodeLocation;
use thrustc_ast::{Ast, NodeId};
use thrustc_builtins::BuiltinFunctionSignature;
use thrustc_code_location::Span;
use thrustc_compile_time::{BuiltinArgument, BuiltinValue};
use thrustc_errors::CompilationIssue;
use thrustc_errors::CompilationIssueCode;
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, typegeneration};

pub fn build_builtin_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let signature: BuiltinFunctionSignature = {
        let function = ctx.get_builtins().get_function(name).ok_or_else(|| {
            CompilationIssue::Error(
                CompilationIssueCode::E0003,
                format!("Unknown compiler builtin '{}'.", name),
                "Compiler builtin doesn't exist on the compiler.".into(),
                None,
                span,
            )
        })?;

        function.signature()
    };

    let expected_parameters: usize = signature.get_parameter_count();

    let mut args: Vec<BuiltinArgument> = Vec::with_capacity(expected_parameters);
    let mut deferred_arguments: Vec<DeferredBuiltinArgument<'parser>> =
        Vec::with_capacity(expected_parameters);
    let mut should_defer: bool = false;
    let mut index: usize = 0;

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let (argument, deferred_argument): (BuiltinArgument, DeferredBuiltinArgument<'parser>) =
            if index < expected_parameters && signature.is_parameter_a_type(index) {
                let ty: Type = typegeneration::build_type(ctx, true)?;
                let argument_span: Span = ctx.previous().get_span();

                let mut hints: std::collections::HashSet<String> =
                    std::collections::HashSet::with_capacity(u8::MAX as usize);
                thrustc_generics::collect_unresolved_type_hints(&ty, &mut hints);

                if !hints.is_empty() {
                    should_defer = true;
                }

                (
                    BuiltinArgument::Type {
                        ty: ty.clone(),
                        span: argument_span,
                    },
                    DeferredBuiltinArgument::Type {
                        ty,
                        span: argument_span,
                    },
                )
            } else {
                let expr: Ast<'_> = expressions::parse_expr(ctx)?;
                let argument_span: Span = expr.get_span();

                let argument: BuiltinArgument = match thrustc_compile_time::fold(&expr) {
                    Some(value) => BuiltinArgument::Value {
                        value,
                        span: argument_span,
                    },
                    None if self::contains_deferred_builtin(&expr) => {
                        should_defer = true;

                        BuiltinArgument::Value {
                            value: BuiltinValue::Void,
                            span: argument_span,
                        }
                    }
                    None => {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0006,
                            "The compiler builtin expects a constant argument.".into(),
                            "You should pass a constant value.".into(),
                            None,
                            argument_span,
                        ));
                    }
                };

                let deferred_argument = DeferredBuiltinArgument::Value {
                    expression: std::boxed::Box::new(expr),
                    span: argument_span,
                };

                (argument, deferred_argument)
            };

        args.push(argument);
        deferred_arguments.push(deferred_argument);

        index = index.saturating_add(1);

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

    if args.len() != expected_parameters {
        ctx.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!(
                "The '{}' builtin expects {} arguments.",
                name, expected_parameters
            ),
            format!("You passed {} arguments.", args.len()),
            None,
            span,
        ));

        return Ok(Ast::invalid_ast(span));
    }

    if should_defer {
        return Ok(Ast::Builtin {
            builtin: AstBuiltin::DeferredCompileTime {
                name,
                arguments: deferred_arguments,
                span,
            },
            kind: signature.return_type,
            span,
            id: NodeId::new(),
        });
    }

    ctx.evaluate_builtin(name, &args, span)
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
