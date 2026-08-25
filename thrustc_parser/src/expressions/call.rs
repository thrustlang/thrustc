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
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::{FoundSymbolId, Function, Intrinsic};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::FunctionReferenceExtensions};

use thrustc_parser_table::traits::{
    FoundSymbolEitherExtensions, FoundSymbolExtensions, FunctionAssemblerExtensions,
    FunctionExtensions, IntrinsicExtensions,
};

use crate::{ParserContext, expressions};

#[derive(Debug)]
pub struct ParsedCallArguments<'parser> {
    pub positional: Vec<Ast<'parser>>,
    pub named: Vec<(&'parser str, Span, Ast<'parser>)>,
}

pub fn parse_call_arguments<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<ParsedCallArguments<'parser>, CompilationIssue> {
    let mut positional: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut named: Vec<(&str, Span, Ast)> = Vec::with_capacity(u8::MAX as usize);

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        if ctx.check(TokenType::Identifier) && ctx.check_to(TokenType::Eq, 1) {
            let name_tk: &Token = ctx.consume(
                TokenType::Identifier,
                CompilationIssueCode::E0001,
                "Expected 'identifier'.".into(),
            )?;

            let name: &str = name_tk.get_lexeme();
            let name_span: Span = name_tk.get_span();

            ctx.consume(
                TokenType::Eq,
                CompilationIssueCode::E0001,
                "Expected '='.".into(),
            )?;

            let expr: Ast = expressions::parse_expr(ctx)?;

            named.push((name, name_span, expr));
        } else {
            if !named.is_empty() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0046,
                    "A positional argument cannot follow a named argument.".into(),
                    "You should place all positional arguments before the named ones.".into(),
                    None,
                    ctx.peek().get_span(),
                ));
            }

            let expr: Ast = expressions::parse_expr(ctx)?;

            positional.push(expr);
        }

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

    Ok(ParsedCallArguments { positional, named })
}

pub fn reorder_call_arguments<'parser>(
    name: &str,
    span: Span,
    arguments: ParsedCallArguments<'parser>,
    parameter_names: &[&str],
    var_args: bool,
) -> Result<Vec<Ast<'parser>>, CompilationIssue> {
    let positional: Vec<Ast> = arguments.positional;
    let named: Vec<(&str, Span, Ast)> = arguments.named;

    if named.is_empty() {
        return Ok(positional);
    }

    if var_args {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0044,
            format!(
                "Function '{}' does not support named arguments because it accepts a variable number of arguments.",
                name
            ),
            "You should use only positional arguments.".into(),
            None,
            span,
        ));
    }

    let positional_count: usize = positional.len();

    let mut named_indexed: Vec<(usize, Ast)> = Vec::with_capacity(named.len());
    let mut filled_by_named: Vec<bool> = vec![false; parameter_names.len()];

    for (parameter_name, parameter_span, expr) in named.into_iter() {
        let index: Option<usize> = parameter_names
            .iter()
            .position(|candidate| *candidate == parameter_name);

        let Some(index) = index else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0044,
                format!(
                    "Function '{}' has no parameter named '{}'.",
                    name, parameter_name
                ),
                "You should use one of the declared parameter names.".into(),
                None,
                parameter_span,
            ));
        };

        if index < positional_count || filled_by_named[index] {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0045,
                format!(
                    "Argument '{}' was already provided for function '{}'.",
                    parameter_name, name
                ),
                "You should remove the duplicated argument.".into(),
                None,
                parameter_span,
            ));
        }

        filled_by_named[index] = true;
        named_indexed.push((index, expr));
    }

    if positional_count + named_indexed.len() != parameter_names.len() {
        let mut combined: Vec<Ast> = positional;

        for (_, expr) in named_indexed.into_iter() {
            combined.push(expr);
        }

        return Ok(combined);
    }

    let mut slots: Vec<Option<Ast>> = vec![None; parameter_names.len()];

    for (index, expr) in positional.into_iter().enumerate() {
        slots[index] = Some(expr);
    }

    for (index, expr) in named_indexed.into_iter() {
        slots[index] = Some(expr);
    }

    let args: Vec<Ast> = slots.into_iter().map(|slot| slot.unwrap()).collect();

    Ok(args)
}

pub fn build_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let arguments: ParsedCallArguments = self::parse_call_arguments(ctx)?;

    let reference: Result<FoundSymbolId, CompilationIssue> =
        ctx.get_symbols().get_symbols_id(name, span);

    match reference {
        Ok(object) => {
            let function_type: Type = if object.is_intrinsic() {
                if !arguments.named.is_empty() {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0044,
                        "Named arguments are not supported for compiler intrinsics.".into(),
                        "You should use only positional arguments.".into(),
                        None,
                        span,
                    ));
                }

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
                if !arguments.named.is_empty() {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0044,
                        "Named arguments are not supported for assembler functions.".into(),
                        "You should use only positional arguments.".into(),
                        None,
                        span,
                    ));
                }

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
                    Ok(function) => {
                        let parameter_names: Vec<&str> =
                            FunctionExtensions::get_parameter_names(&function);

                        let args: Vec<Ast> = match self::reorder_call_arguments(
                            name,
                            span,
                            arguments,
                            &parameter_names,
                            function.3,
                        ) {
                            Ok(args) => args,
                            Err(error) => {
                                ctx.add_error_report(error);
                                return Ok(Ast::invalid_ast(span));
                            }
                        };

                        return Ok(Ast::Call {
                            name: name.to_string(),
                            args,
                            generic_args: Vec::with_capacity(0),
                            kind: FunctionExtensions::get_type(&function),
                            span,
                            id: NodeId::new(),
                        });
                    }
                    Err(error) => {
                        ctx.add_error_report(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            };

            let args: Vec<Ast> = arguments.positional;

            Ok(Ast::Call {
                name: name.to_string(),
                args,
                generic_args: Vec::with_capacity(0),
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

pub fn build_generic_call<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let Some(generic) = ctx.get_symbols().get_generic_function(name).cloned() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("'{}' not found.", name),
            "You should make sure that it exist at this scope.".into(),
            None,
            span,
        ));
    };

    let mut generic_args: Vec<Type> = Vec::with_capacity(generic.type_params.len());

    if ctx.match_token(TokenType::LBracket)? {
        loop {
            if ctx.check(TokenType::RBracket) {
                break;
            }

            let argument_type: Type = crate::typegeneration::build_type(ctx, false)?;

            generic_args.push(argument_type);

            if ctx.check(TokenType::RBracket) {
                break;
            }

            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;
    }

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let arguments: ParsedCallArguments = self::parse_call_arguments(ctx)?;

    let parameter_names: Vec<&str> =
        generic.parameter_names.iter().map(String::as_str).collect();

    let args: Vec<Ast> = self::reorder_call_arguments(
        name,
        span,
        arguments,
        &parameter_names,
        generic.has_varargs,
    )?;

    let argument_types: Vec<Type> = args
        .iter()
        .map(|argument| match argument.get_value_type() {
            Ok(ty) => ty.clone(),
            Err(_) => Type::Void { span },
        })
        .collect();

    let kind: Type = match thrustc_generics::solve(
        &generic.type_params,
        &generic_args,
        &generic.parameter_types,
        &argument_types,
        &generic.return_type,
        generic.has_varargs,
        span,
    ) {
        Ok(result) => result.return_type,
        Err(error) => {
            ctx.add_error_report(error);

            Type::Void { span }
        }
    };

    Ok(Ast::Call {
        name: name.to_string(),
        args,
        generic_args,
        kind,
        span,
        id: NodeId::new(),
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

    let arguments: ParsedCallArguments = self::parse_call_arguments(ctx)?;

    if !arguments.named.is_empty() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0044,
            "Named arguments are not supported in anonymous function calls.".into(),
            "You should use only positional arguments.".into(),
            None,
            span,
        ));
    }

    let expr_type: &Type = expr.get_value_type()?;
    let return_type: Type = expr_type.get_function_reference_return_type();

    Ok(Ast::IndirectCall {
        function: expr.clone().into(),
        function_type: expr_type.clone(),
        args: arguments.positional,
        kind: return_type,
        span,
        id: NodeId::new(),
    })
}
