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

use arbitrary::Unstructured;
use thrustc_ast::Ast;
use thrustc_ast::NodeId;
use thrustc_ast::ast_metadata::{
    CastingMetadata, ConstantMetadata, DereferenceMetadata, FunctionParameterMetadata,
    LocalMetadata, ReferenceMetadata, ReferenceType, StaticMetadata,
};
use thrustc_ast::traits::{AstConstantExtensions, AstMemoryExtensions};
use thrustc_typesystem::traits::TypePointerExtensions;
use thrustc_typesystem::Type;

const MAX_DEPTH: usize = 8;
const MAX_STATEMENTS_PER_BLOCK: usize = 12;
const MAX_EXPR_DEPTH: usize = 4;

#[derive(Clone)]
struct ScopedVar<'ast> {
    name: &'ast str,
    kind: Type,
    reference_type: ReferenceType,
}

#[derive(Default)]
struct ScopeStack<'ast> {
    frames: Vec<Vec<ScopedVar<'ast>>>,
}

impl<'ast> ScopeStack<'ast> {
    fn push(&mut self) {
        self.frames.push(Vec::new());
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    fn declare(&mut self, name: &'ast str, kind: Type, reference_type: ReferenceType) {
        if let Some(frame) = self.frames.last_mut() {
            frame.push(ScopedVar {
                name,
                kind,
                reference_type,
            });
        }
    }

    fn visible(&self) -> Vec<ScopedVar<'ast>> {
        self.frames.iter().flatten().cloned().collect()
    }

    fn has_any(&self) -> bool {
        self.frames.iter().any(|f| !f.is_empty())
    }

    fn pick_mutable(&self, u: &mut Unstructured<'ast>) -> arbitrary::Result<Option<ScopedVar<'ast>>> {
        let mutables: Vec<ScopedVar<'ast>> = self
            .visible()
            .into_iter()
            .filter(|var| {
                matches!(var.reference_type, ReferenceType::Local)
                    || (matches!(var.reference_type, ReferenceType::Parameter)
                        && var.kind.is_ptr_like_type())
            })
            .collect();

        if mutables.is_empty() {
            return Ok(None);
        }

        let idx = u.int_in_range(0..=(mutables.len() - 1))?;

        Ok(Some(mutables[idx].clone()))
    }
}

fn reference_metadata_for(var: &ScopedVar<'_>, is_mutable: bool) -> ReferenceMetadata {
    match var.reference_type {
        ReferenceType::Parameter => ReferenceMetadata::new(
            var.kind.is_ptr_like_type(),
            is_mutable,
            ReferenceType::Parameter,
            false,
        ),
        ReferenceType::Static => {
            ReferenceMetadata::new(true, is_mutable, ReferenceType::Static, false)
        }
        ReferenceType::Constant => {
            ReferenceMetadata::new(true, false, ReferenceType::Constant, false)
        }
        _ => ReferenceMetadata::new(true, is_mutable, ReferenceType::Local, false),
    }
}

#[inline]
fn gen_name<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<&'ast str> {
    crate::names::gen_name(u)
}

pub fn gen_root<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
    let mut scope = ScopeStack::default();
    scope.push();

    gen_function(u, &mut scope, MAX_DEPTH)
}

fn gen_function<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_params = u.int_in_range(0..=4usize)?;
    let mut parameters = Vec::with_capacity(n_params);
    let mut parameter_types = Vec::with_capacity(n_params);

    for i in 0..n_params {
        let name = gen_name(u)?;
        let kind: Type = u.arbitrary()?;

        scope.declare(name, kind.clone(), ReferenceType::Parameter);
        parameter_types.push(kind.clone());

        parameters.push(Ast::FunctionParameter {
            name: name.to_string(),
            ascii_name: name.to_string(),
            kind: kind.clone(),
            position: i as u32,
            metadata: FunctionParameterMetadata::new(kind.is_ptr_like_type()),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let return_type: Type = u.arbitrary()?;

    let body = Some(Box::new(gen_function_body(
        u,
        scope,
        depth.saturating_sub(1).max(2),
        &return_type,
    )?));

    scope.pop();

    let name = gen_name(u)?;

    Ok(Ast::Function {
        name: name.to_string(),
        ascii_name: name.to_string(),
        parameters,
        parameter_types,
        body,
        return_type,
        attributes: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_function_body<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
    return_type: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts = u.int_in_range(1..=MAX_STATEMENTS_PER_BLOCK)?;
    let mut nodes = Vec::with_capacity(n_stmts + 1);
    for _ in 0..n_stmts {
        nodes.push(gen_stmt(u, scope, depth)?);
    }

    let is_void = matches!(return_type, Type::Void { .. });
    let should_return = !is_void || u.arbitrary()?;

    if should_return {
        let expression = if is_void {
            None
        } else {
            Some(Box::new(gen_expr(u, scope, MAX_EXPR_DEPTH)?))
        };
        nodes.push(Ast::Return {
            expression,
            kind: return_type.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let n_post = u.int_in_range(0..=2usize)?;
    let mut post = Vec::with_capacity(n_post);
    for _ in 0..n_post {
        post.push(gen_defer(u, scope, depth)?);
    }

    scope.pop();

    Ok(Ast::Block {
        nodes,
        post,
        kind: return_type.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_block<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts = u.int_in_range(1..=MAX_STATEMENTS_PER_BLOCK)?;
    let mut nodes = Vec::with_capacity(n_stmts);

    for _ in 0..n_stmts {
        nodes.push(gen_stmt(u, scope, depth)?);
    }

    let n_post = u.int_in_range(0..=2usize)?;
    let mut post = Vec::with_capacity(n_post);

    for _ in 0..n_post {
        post.push(gen_defer(u, scope, depth)?);
    }

    scope.pop();

    Ok(Ast::Block {
        nodes,
        post,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_stmt<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    if depth == 0 {
        return match u.int_in_range(0..=1)? {
            0 => Ok(Ast::Unreachable {
                span: u.arbitrary()?,
                kind: u.arbitrary()?,
                id: NodeId::new(),
            }),
            _ => Ok(Ast::Invalid {
                kind: u.arbitrary()?,
                span: u.arbitrary()?,
                id: NodeId::new(),
            }),
        };
    }

    let has_vars = scope.has_any();

    let choice = if has_vars {
        u.int_in_range(0..=19)?
    } else {
        u.int_in_range(0..=1)?
    };

    match choice {
        0 => gen_var(u, scope, depth),
        1 => gen_const(u, scope),
        2 | 3 => gen_if(u, scope, depth),
        4 | 5 => gen_for(u, scope, depth),
        6 | 7 => gen_while(u, scope, depth),
        8 | 9 => gen_loop(u, scope, depth),
        10 | 11 => gen_block(u, scope, depth - 1),
        12 => gen_static(u, scope),
        13 => gen_mutation(u, scope, depth),
        14 => gen_call_stmt(u, scope, depth),
        15 => gen_return(u, scope, depth),
        16 => gen_defer(u, scope, depth),
        17 => gen_loop_control(u),
        _ => gen_reference(u, scope),
    }
}

fn gen_var<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let name = gen_name(u)?;
    let kind: Type = u.arbitrary()?;

    let value = if u.arbitrary()? {
        Some(Box::new(gen_expr(u, scope, depth.saturating_sub(1))?))
    } else {
        None
    };

    scope.declare(name, kind.clone(), ReferenceType::Local);

    let is_unitialized: bool = value.is_none();

    Ok(Ast::Var {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: LocalMetadata::new(is_unitialized, true, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_const<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    let name = gen_name(u)?;
    let kind: Type = u.arbitrary()?;
    let value = Box::new(gen_expr(u, scope, MAX_EXPR_DEPTH)?);

    scope.declare(name, kind.clone(), ReferenceType::Constant);

    Ok(Ast::Const {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: ConstantMetadata::new(false, false, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_static<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    let name = gen_name(u)?;
    let kind: Type = u.arbitrary()?;

    let value = if u.arbitrary()? {
        Some(Box::new(gen_expr(u, scope, MAX_EXPR_DEPTH)?))
    } else {
        None
    };

    scope.declare(name, kind.clone(), ReferenceType::Static);

    let is_unitialized: bool = value.is_none();

    Ok(Ast::Static {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: StaticMetadata::new(
            false,
            true,
            is_unitialized,
            false,
            false,
            false,
            None,
            None,
        ),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_condition<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    if scope.has_any() {
        let visible = scope.visible();
        let idx = u.int_in_range(0..=(visible.len() - 1))?;
        let picked = visible[idx].clone();

        let left = Ast::Reference {
            name: picked.name,
            kind: picked.kind.clone(),
            metadata: reference_metadata_for(&picked, false),
            span: u.arbitrary()?,
            id: NodeId::new(),
        };
        let right = Ast::Integer {
            kind: picked.kind.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        };

        return Ok(Ast::BinaryOp {
            left: Box::new(left),
            operator: u.arbitrary()?,
            right: Box::new(right),
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    Ok(Ast::Boolean {
        kind: u.arbitrary()?,
        value: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_increment<'ast>(
    u: &mut Unstructured<'ast>,
    var: &ScopedVar<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    let name: &'ast str = var.name;
    let kind: Type = var.kind.clone();

    let current = Ast::Reference {
        name,
        kind: kind.clone(),
        metadata: reference_metadata_for(var, true),
        span: u.arbitrary()?,
        id: NodeId::new(),
    };
    let step = Ast::Integer {
        kind: kind.clone(),
        value: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    };
    let new_value = Ast::BinaryOp {
        left: Box::new(current),
        operator: u.arbitrary()?,
        right: Box::new(step),
        kind: kind.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    };
    let target = Ast::Reference {
        name,
        kind: kind.clone(),
        metadata: reference_metadata_for(var, true),
        span: u.arbitrary()?,
        id: NodeId::new(),
    };

    Ok(Ast::Mutation {
        source: Box::new(target),
        value: Box::new(new_value),
        kind,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_if<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let condition = Box::new(gen_condition(u, scope)?);
    let then_branch = Box::new(gen_block(u, scope, depth - 1)?);

    let n_elif = u.int_in_range(0..=2usize)?;
    let mut else_if_branch = Vec::with_capacity(n_elif);
    for _ in 0..n_elif {
        let elif_condition = Box::new(gen_condition(u, scope)?);
        let elif_block = Box::new(gen_block(u, scope, depth - 1)?);
        else_if_branch.push(Ast::Elif {
            condition: elif_condition,
            block: elif_block,
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let else_branch = if u.arbitrary()? {
        Some(Box::new(Ast::Else {
            block: Box::new(gen_block(u, scope, depth - 1)?),
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }))
    } else {
        None
    };

    Ok(Ast::If {
        condition,
        then_branch,
        else_if_branch,
        else_branch,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_for<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let loop_var_name = gen_name(u)?;
    let loop_var_kind: Type = u.arbitrary()?;

    let init_value = Box::new(Ast::Integer {
        kind: loop_var_kind.clone(),
        value: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let local = Box::new(Ast::Var {
        name: loop_var_name,
        ascii_name: loop_var_name,
        kind: loop_var_kind.clone(),
        value: Some(init_value),
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: LocalMetadata::new(false, true, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let loop_var = ScopedVar {
        name: loop_var_name,
        kind: loop_var_kind.clone(),
        reference_type: ReferenceType::Local,
    };

    scope.declare(loop_var_name, loop_var_kind.clone(), ReferenceType::Local);

    let condition = Box::new(Ast::BinaryOp {
        left: Box::new(Ast::Reference {
            name: loop_var_name,
            kind: loop_var_kind.clone(),
            metadata: reference_metadata_for(&loop_var, false),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        operator: u.arbitrary()?,
        right: Box::new(Ast::Integer {
            kind: loop_var_kind.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let actions = Box::new(gen_increment(u, &loop_var)?);
    let block = Box::new(gen_block(u, scope, depth.saturating_sub(1))?);

    scope.pop();

    Ok(Ast::For {
        local,
        condition,
        actions,
        block,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_while<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let variable = if u.arbitrary()? {
        Some(Box::new(gen_var(u, scope, depth.saturating_sub(1))?))
    } else {
        None
    };

    let condition = Box::new(gen_condition(u, scope)?);
    let block = Box::new(gen_block(u, scope, depth.saturating_sub(1))?);

    scope.pop();

    Ok(Ast::While {
        variable,
        condition,
        block,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_loop<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Loop {
        block: Box::new(gen_block(u, scope, depth.saturating_sub(1))?),
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_loop_control<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
    match u.int_in_range(0..=3)? {
        0 => Ok(Ast::Continue {
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        1 => Ok(Ast::Break {
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        2 => Ok(Ast::ContinueAll {
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        _ => Ok(Ast::BreakAll {
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
    }
}

fn gen_return<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let expression = if u.arbitrary()? {
        Some(Box::new(gen_expr(
            u,
            scope,
            depth.saturating_sub(1).min(MAX_EXPR_DEPTH),
        )?))
    } else {
        None
    };

    Ok(Ast::Return {
        expression,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_defer<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Defer {
        node: Box::new(gen_stmt(u, scope, depth.saturating_sub(1))?),
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_mutation<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let Some(picked) = scope.pick_mutable(u)? else {
        return gen_var(u, scope, depth);
    };

    let kind: Type = picked.kind.clone();

    Ok(Ast::Mutation {
        source: Box::new(Ast::Reference {
            name: picked.name,
            kind: kind.clone(),
            metadata: reference_metadata_for(&picked, true),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        value: Box::new(gen_expr(u, scope, depth.saturating_sub(1))?),
        kind,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_call_stmt<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let n_args = u.int_in_range(0..=3usize)?;
    let mut args = Vec::with_capacity(n_args);
    for _ in 0..n_args {
        args.push(gen_expr(
            u,
            scope,
            depth.saturating_sub(1).min(MAX_EXPR_DEPTH),
        )?);
    }

    Ok(Ast::Call {
        name: gen_name(u)?.to_string(),
        args,
        generic_args: Vec::with_capacity(0),
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_reference<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    let visible = scope.visible();
    debug_assert!(!visible.is_empty());
    let idx = u.int_in_range(0..=(visible.len() - 1))?;
    let picked = visible[idx].clone();

    Ok(Ast::Reference {
        name: picked.name,
        kind: picked.kind.clone(),
        metadata: reference_metadata_for(&picked, false),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_literal<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
    match u.int_in_range(0..=5)? {
        0 => Ok(Ast::Integer {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        1 => Ok(Ast::Float {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        2 => Ok(Ast::Boolean {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        3 => Ok(Ast::Char {
            kind: u.arbitrary()?,
            byte: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        4 => Ok(Ast::CString {
            bytes: u.arbitrary()?,
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        _ => Ok(Ast::NullPtr {
            span: u.arbitrary()?,
            kind: u.arbitrary()?,
        }),
    }
}

fn gen_expr<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    if depth == 0 {
        return if scope.has_any() && u.arbitrary()? {
            gen_reference(u, scope)
        } else {
            gen_literal(u)
        };
    }

    let has_vars = scope.has_any();
    let upper: u32 = if has_vars { 8 } else { 6 };

    match u.int_in_range(0..=upper)? {
        0 => gen_literal(u),
        1 if has_vars => gen_reference(u, scope),
        1 | 2 => Ok(Ast::BinaryOp {
            left: Box::new(gen_expr(u, scope, depth - 1)?),
            operator: u.arbitrary()?,
            right: Box::new(gen_expr(u, scope, depth - 1)?),
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        3 => Ok(Ast::UnaryOp {
            operator: u.arbitrary()?,
            kind: u.arbitrary()?,
            node: Box::new(gen_expr(u, scope, depth - 1)?),
            before: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        4 => Ok(Ast::Group {
            node: Box::new(gen_expr(u, scope, depth - 1)?),
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        5 => {
            let from = Box::new(gen_expr(u, scope, depth - 1)?);
            let is_constant: bool = from.is_constant_value();
            let is_allocated: bool = from
                .is_memory_assigned_value()
                .map_err(|_| arbitrary::Error::IncorrectFormat)?;

            Ok(Ast::As {
                from,
                cast: u.arbitrary()?,
                metadata: CastingMetadata::new(is_constant, is_allocated),
                span: u.arbitrary()?,
                id: NodeId::new(),
            })
        }
        6 if has_vars => Ok(Ast::Deref {
            value: Box::new(gen_reference(u, scope)?),
            kind: u.arbitrary()?,
            modificators: u.arbitrary()?,
            metadata: DereferenceMetadata::new(false, None),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        7 if has_vars => Ok(Ast::Load {
            source: Box::new(gen_reference(u, scope)?),
            kind: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        8 => gen_call_stmt(u, scope, depth),
        _ => gen_literal(u),
    }
}

fn gen_struct<'ast>(
    u: &mut Unstructured<'ast>,
    _scope: &mut ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Struct {
        name: gen_name(u)?,
        data: u.arbitrary()?,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        attributes: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_enum<'ast>(
    u: &mut Unstructured<'ast>,
    _scope: &mut ScopeStack<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Enum {
        name: gen_name(u)?,
        data: u.arbitrary()?,
        attributes: u.arbitrary()?,
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}
