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
    ast_metadata::{ReferenceMetadata, ReferenceType},
    traits::AstStandardExtensions,
};
use thrustc_attributes::{
    ThrustAttribute, ThrustAttributeComparator, traits::ThrustAttributesExtensions,
};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{TypeExtensions, TypeIsExtensions},
};

use crate::{ParserContext, statements};

pub fn parse_code_block_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_block()?;

    let block_tk: &Token = ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let span: Span = block_tk.get_span();

    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let mut nodes: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut post: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.match_token(TokenType::RBrace)? {
        let statement: Ast<'_> = statements::parse(ctx)?;

        if statement.is_defer_keyword() {
            post.push(statement);
        } else {
            if let Some(dealloc) = self::build_dealloc_defer(ctx, &statement)? {
                post.push(dealloc);
            }

            nodes.push(statement);
        }
    }

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    ctx.leave_block();

    Ok(Ast::Block {
        nodes,
        post,
        span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}

pub fn parse_code_block_without_start_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.enter_block()?;

    ctx.begin_scope();
    ctx.get_mut_symbols().begin_scope();

    let mut nodes: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut post: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);

    while !ctx.check(TokenType::RBrace) {
        let statement: Ast<'_> = statements::parse(ctx)?;

        if statement.is_defer_keyword() {
            post.push(statement);
        } else {
            if let Some(dealloc) = self::build_dealloc_defer(ctx, &statement)? {
                post.push(dealloc);
            }

            nodes.push(statement);
        }
    }

    let block_tk: &Token = ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    let span: Span = block_tk.get_span();

    ctx.get_mut_symbols().end_scope();
    ctx.end_scope();

    ctx.leave_block();

    Ok(Ast::Block {
        nodes,
        post,
        span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}

fn build_dealloc_defer<'parser>(
    ctx: &mut ParserContext<'parser>,
    statement: &Ast<'parser>,
) -> Result<Option<Ast<'parser>>, CompilationIssue> {
    let Ast::Var {
        name,
        kind,
        attributes,
        span,
        ..
    } = statement
    else {
        return Ok(None);
    };

    let Some(ThrustAttribute::Dealloc(deallocator, _)) =
        attributes.get_attr(ThrustAttributeComparator::Dealloc)
    else {
        return Ok(None);
    };

    let mut deallocator_name: Option<String> = None;
    let mut generic_args: Vec<Type> = Vec::with_capacity(4);

    if let Some(deallocator) = deallocator {
        if let Some(name) = deallocator.last() {
            deallocator_name = if deallocator.len() > 1 {
                let access: &[String] = &deallocator[..deallocator.len().saturating_sub(1)];

                Some(crate::module_import::qualified_symbol_name(access, name))
            } else {
                Some(name.clone())
            };
        }
    } else if let Some((name, args)) =
        crate::module_import::ensure_deallocator_for_type(ctx, kind, *span)?
    {
        deallocator_name = Some(name);
        generic_args = args;
    } else if let Some(name) = self::find_local_deallocator(ctx, kind) {
        deallocator_name = Some(name);
    }

    let Some(deallocator_name) = deallocator_name else {
        return Ok(None);
    };

    if generic_args.is_empty() {
        if let Some(entry) = ctx.get_symbols().get_generic_function(&deallocator_name).cloned() {
            let expected_arg: Type = Type::Ptr {
                subtype: Some(std::boxed::Box::new(kind.clone())),
                address_space: None,
                span: *span,
            };

            let argument_types: Vec<Type> = vec![expected_arg];

            if let Some(parameter_type) = entry.parameter_types.first() {
                if parameter_type != &argument_types[0] {
                    return Ok(None);
                }
            }

            if let Ok(result) = thrustc_generics::solve(
                &entry.type_params,
                &[],
                &entry.parameter_types,
                &argument_types,
                &entry.return_type,
                entry.has_varargs,
                *span,
            ) {
                generic_args = entry
                    .type_params
                    .iter()
                    .filter_map(|parameter| result.env.get(parameter).cloned())
                    .collect();
            }
        }
    }

    let reference: Ast = Ast::Reference {
        name,
        kind: kind.clone(),
        metadata: ReferenceMetadata::new(true, true, ReferenceType::Local, false),
        span: *span,
        id: NodeId::new(),
    };

    let argument: Ast = if kind.is_ptr_type() {
        reference
    } else {
        Ast::GetLocation {
            expr: reference.into(),
            kind: kind.get_type_ref(),
            span: *span,
            id: NodeId::new(),
        }
    };

    let call: Ast = Ast::Call {
        name: deallocator_name,
        args: vec![argument],
        generic_args,
        kind: Type::Void { span: *span },
        span: *span,
        id: NodeId::new(),
    };

    Ok(Some(Ast::Defer {
        node: call.into(),
        kind: Type::Void { span: *span },
        span: *span,
        id: NodeId::new(),
    }))
}

fn find_local_deallocator<'parser>(ctx: &ParserContext<'parser>, kind: &Type) -> Option<String> {
    for node in ctx.get_ast() {
        let Ast::Function {
            name,
            parameter_types,
            attributes,
            ..
        } = node
        else {
            continue;
        };

        if !attributes.has_deallocator_attribute() {
            continue;
        }

        if parameter_types.len() != 1 {
            continue;
        }

        let Type::Ptr {
            subtype: Some(subtype),
            ..
        } = &parameter_types[0]
        else {
            continue;
        };

        if subtype.as_ref() == kind {
            return Some(name.clone());
        }
    }

    None
}
