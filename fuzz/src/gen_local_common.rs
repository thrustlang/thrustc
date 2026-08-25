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
use thrustc_ast::ast_metadata::{
    CastingMetadata, ConstantMetadata, DereferenceMetadata, FunctionParameterMetadata,
    LocalMetadata, ReferenceMetadata, ReferenceType, StaticMetadata,
};
use thrustc_ast::traits::{AstConstantExtensions, AstMemoryExtensions};
use thrustc_ast::Ast;
use thrustc_ast::NodeId;
use thrustc_token_type::TokenType;
use thrustc_typesystem::traits::{TypeIsExtensions, TypePointerExtensions};
use thrustc_typesystem::Type;

pub struct Config {
    pub max_depth: usize,
    pub max_statements_per_block: usize,
    pub max_expr_depth: usize,
    pub max_loop_nesting: usize,
    pub loop_bias: bool,
    pub allow_loops: bool,
    pub allow_defer: bool,
    pub allow_const_static: bool,
    pub allow_self_calls: bool,
    pub allow_extras: bool,
}

impl Config {
    pub fn general() -> Self {
        Self {
            max_depth: 5,
            max_statements_per_block: 12,
            max_expr_depth: 6,
            max_loop_nesting: 4,
            loop_bias: false,
            allow_loops: true,
            allow_defer: true,
            allow_const_static: true,
            allow_self_calls: true,
            allow_extras: false,
        }
    }

    pub fn loops() -> Self {
        Self {
            max_depth: 6,
            max_statements_per_block: 8,
            max_expr_depth: 3,
            max_loop_nesting: 4,
            loop_bias: true,
            allow_loops: true,
            allow_defer: false,
            allow_const_static: false,
            allow_self_calls: false,
            allow_extras: false,
        }
    }
}

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

    fn pick_any(&self, u: &mut Unstructured<'ast>) -> arbitrary::Result<Option<ScopedVar<'ast>>> {
        let visible = self.visible();

        if visible.is_empty() {
            return Ok(None);
        }

        let idx = u.int_in_range(0..=(visible.len() - 1))?;

        Ok(Some(visible[idx].clone()))
    }

    fn has_of_type(&self, target: &Type) -> bool {
        self.frames.iter().flatten().any(|var| var.kind == *target)
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

    fn has_ptr_of(&self, pointee: &Type) -> Option<ScopedVar<'ast>> {
        self.visible().into_iter().find(|var| match &var.kind {
            Type::Ptr {
                subtype: Some(inner),
                ..
            } => **inner == *pointee,
            _ => false,
        })
    }

    fn has_array_of(&self, base: &Type) -> Option<ScopedVar<'ast>> {
        self.visible().into_iter().find(|var| match &var.kind {
            Type::FixedArray {
                base_type: inner, ..
            } => **inner == *base,
            _ => false,
        })
    }
}

pub fn gen_root<'ast>(u: &mut Unstructured<'ast>, cfg: &Config) -> arbitrary::Result<Ast<'ast>> {
    let mut scope: ScopeStack<'_> = ScopeStack::default();

    scope.push();

    self::gen_function(u, &mut scope, cfg, cfg.max_depth)
}

fn gen_function<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_params: usize = u.int_in_range(0..=4usize)?;

    let mut parameters: Vec<Ast<'_>> = Vec::with_capacity(n_params);

    let mut parameter_types: Vec<Type> = Vec::with_capacity(n_params);

    for i in 0..n_params {
        let name = crate::names::gen_name(u)?;

        let kind: Type = self::gen_scalar_type(u)?;

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

    let return_type: Type = if u.arbitrary()? {
        self::gen_scalar_type(u)?
    } else {
        Type::Void {
            span: u.arbitrary()?,
        }
    };

    let name = crate::names::gen_name(u)?;

    let body: Option<Box<Ast<'_>>> = Some(Box::new(self::gen_function_body(
        u,
        scope,
        cfg,
        depth.saturating_sub(1).max(2),
        &name,
        &parameter_types,
        &return_type,
    )?));

    scope.pop();

    Ok(Ast::Function {
        name: name.to_string(),
        ascii_name: name.to_string(),
        parameters,
        parameter_types,
        body,
        return_type,
        attributes: Vec::new(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_function_body<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
    return_type: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts: usize = u.int_in_range(1..=cfg.max_statements_per_block)?;

    let mut nodes: Vec<Ast<'_>> = Vec::with_capacity(n_stmts + 1);

    for _ in 0..n_stmts {
        nodes.push(self::gen_stmt(
            u,
            scope,
            cfg,
            depth,
            0,
            fn_name,
            param_types,
        )?);
    }

    let is_void: bool = return_type.is_void_type();

    if !is_void {
        nodes.push(Ast::Return {
            expression: Some(Box::new(self::gen_expr_of_type(
                u,
                scope,
                cfg,
                cfg.max_expr_depth,
                return_type,
            )?)),
            kind: return_type.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    } else if u.arbitrary()? {
        nodes.push(Ast::Return {
            expression: None,
            kind: return_type.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let mut post: Vec<Ast<'_>> = Vec::new();

    if cfg.allow_defer && u.arbitrary()? {
        post.push(self::gen_defer(u, scope, cfg, depth, fn_name, param_types)?);
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
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    is_loop_body: bool,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts: usize = u.int_in_range(1..=cfg.max_statements_per_block)?;

    let mut nodes: Vec<Ast<'_>> = Vec::with_capacity(n_stmts + 1);

    for _ in 0..n_stmts {
        nodes.push(self::gen_stmt(
            u,
            scope,
            cfg,
            depth,
            loop_depth,
            fn_name,
            param_types,
        )?);
    }

    if is_loop_body && u.arbitrary()? {
        nodes.push(self::gen_loop_control(u)?);
    }

    scope.pop();

    Ok(Ast::Block {
        nodes,
        post: Vec::new(),
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_stmt<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    if depth == 0 {
        return if u.arbitrary()? {
            self::gen_var(u, scope, cfg, 1)
        } else {
            self::gen_expr_stmt(u, scope, cfg, 1)
        };
    }

    let can_nest_loop = cfg.allow_loops && depth > 1 && loop_depth < cfg.max_loop_nesting;

    let has_vars = scope.has_any();

    let mut options: Vec<u8> = Vec::with_capacity(16);

    options.push(0); // var
    options.push(1); // if
    if has_vars {
        options.push(2); // mutation
    }
    options.push(3); // nested block
    if can_nest_loop {
        let weight: u8 = if cfg.loop_bias { 4 } else { 1 };

        for _ in 0..weight {
            options.push(4); // while
            options.push(5); // for
            options.push(6); // loop
        }
    }
    if cfg.allow_const_static {
        options.push(7); // static
        options.push(8); // const
    }
    if cfg.allow_defer {
        options.push(9); // defer
    }
    if cfg.allow_self_calls {
        options.push(10); // self call
    }
    options.push(11); // bare expression statement

    match *u.choose(&options)? {
        0 => self::gen_var(u, scope, cfg, depth.saturating_sub(1)),
        1 => self::gen_if(
            u,
            scope,
            cfg,
            depth.saturating_sub(1),
            loop_depth,
            fn_name,
            param_types,
        ),
        2 => self::gen_mutation(u, scope, cfg, depth.saturating_sub(1)),
        3 => self::gen_block(
            u,
            scope,
            cfg,
            depth.saturating_sub(1),
            loop_depth,
            false,
            fn_name,
            param_types,
        ),
        4 => self::gen_while(
            u,
            scope,
            cfg,
            depth.saturating_sub(1),
            loop_depth,
            fn_name,
            param_types,
        ),
        5 => self::gen_for(
            u,
            scope,
            cfg,
            depth.saturating_sub(1),
            loop_depth,
            fn_name,
            param_types,
        ),
        6 => self::gen_loop(
            u,
            scope,
            cfg,
            depth.saturating_sub(1),
            loop_depth,
            fn_name,
            param_types,
        ),
        7 => self::gen_static(u, scope, cfg),
        8 => self::gen_const(u, scope, cfg),
        9 => self::gen_defer(u, scope, cfg, depth.saturating_sub(1), fn_name, param_types),
        10 => self::gen_self_call(u, scope, cfg, depth.saturating_sub(1), fn_name, param_types),
        _ => self::gen_expr_stmt(u, scope, cfg, depth.saturating_sub(1)),
    }
}

fn gen_var<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let name = crate::names::gen_name(u)?;

    let kind: Type = self::gen_decl_type(u, cfg)?;

    let value = if u.arbitrary()? {
        Some(Box::new(self::gen_expr_of_type(
            u, scope, cfg, depth, &kind,
        )?))
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
        attributes: Vec::new(),
        modificators: Vec::new(),
        metadata: LocalMetadata::new(is_unitialized, true, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_const<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
) -> arbitrary::Result<Ast<'ast>> {
    let name = crate::names::gen_name(u)?;

    let kind: Type = self::gen_decl_type(u, cfg)?;

    let value = Box::new(self::gen_const_expr_of_type(
        u,
        scope,
        cfg,
        cfg.max_expr_depth,
        &kind,
    )?);

    scope.declare(name, kind.clone(), ReferenceType::Constant);

    Ok(Ast::Const {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: Vec::new(),
        modificators: Vec::new(),
        metadata: ConstantMetadata::new(false, false, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_static<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
) -> arbitrary::Result<Ast<'ast>> {
    let name = crate::names::gen_name(u)?;

    let kind: Type = self::gen_decl_type(u, cfg)?;

    let value = if u.arbitrary()? {
        Some(Box::new(self::gen_const_expr_of_type(
            u,
            scope,
            cfg,
            cfg.max_expr_depth,
            &kind,
        )?))
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
        attributes: Vec::new(),
        modificators: Vec::new(),
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

fn gen_if<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    let bool_kind: Type = Type::Bool {
        span: u.arbitrary()?,
    };

    let condition = Box::new(self::gen_expr_of_type(
        u,
        scope,
        cfg,
        cfg.max_expr_depth,
        &bool_kind,
    )?);

    let then_branch = Box::new(self::gen_block(
        u,
        scope,
        cfg,
        depth,
        loop_depth,
        false,
        fn_name,
        param_types,
    )?);

    let n_elif = u.int_in_range(0..=2usize)?;

    let mut else_if_branch: Vec<Ast<'_>> = Vec::with_capacity(n_elif);

    for _ in 0..n_elif {
        let elif_bool_kind: Type = Type::Bool {
            span: u.arbitrary()?,
        };

        let elif_condition = Box::new(self::gen_expr_of_type(
            u,
            scope,
            cfg,
            cfg.max_expr_depth,
            &elif_bool_kind,
        )?);

        let elif_block = Box::new(self::gen_block(
            u,
            scope,
            cfg,
            depth,
            loop_depth,
            false,
            fn_name,
            param_types,
        )?);

        else_if_branch.push(Ast::Elif {
            condition: elif_condition,
            block: elif_block,
            kind: Type::Void {
                span: u.arbitrary()?,
            },
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let else_branch = if u.arbitrary()? {
        Some(Box::new(Ast::Else {
            block: Box::new(self::gen_block(
                u,
                scope,
                cfg,
                depth,
                loop_depth,
                false,
                fn_name,
                param_types,
            )?),
            kind: Type::Void {
                span: u.arbitrary()?,
            },
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
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_for<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let loop_var_name = crate::names::gen_name(u)?;

    let loop_var_kind: Type = self::gen_integer_type(u)?;

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
        attributes: Vec::new(),
        modificators: Vec::new(),
        metadata: LocalMetadata::new(false, true, false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let condition = Box::new(Ast::BinaryOp {
        left: Box::new(self::reference(loop_var_name, loop_var_kind.clone())),
        operator: self::relational_operator(u)?,
        right: Box::new(Ast::Integer {
            kind: loop_var_kind.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        kind: Type::Bool {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let actions = Box::new(Ast::BinaryOp {
        left: Box::new(self::reference(loop_var_name, loop_var_kind.clone())),
        operator: self::compound_integer_operator(u)?,
        right: Box::new(Ast::Integer {
            kind: loop_var_kind.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        kind: loop_var_kind.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    let block = Box::new(self::gen_block(
        u,
        scope,
        cfg,
        depth,
        loop_depth + 1,
        true,
        fn_name,
        param_types,
    )?);

    scope.pop();

    Ok(Ast::For {
        local,
        condition,
        actions,
        block,
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_while<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let variable_node: Option<Ast<'ast>> = if u.arbitrary()? {
        Some(self::gen_var(u, scope, cfg, depth.saturating_sub(1))?)
    } else {
        None
    };

    let bool_kind: Type = Type::Bool {
        span: u.arbitrary()?,
    };

    let condition = Box::new(self::gen_expr_of_type(
        u,
        scope,
        cfg,
        cfg.max_expr_depth,
        &bool_kind,
    )?);

    let mut block = self::gen_block(
        u,
        scope,
        cfg,
        depth,
        loop_depth + 1,
        true,
        fn_name,
        param_types,
    )?;

    if let Some(var_node) = variable_node {
        if let Ast::Block { nodes, .. } = &mut block {
            nodes.insert(0, var_node);
        }
    }

    scope.pop();

    Ok(Ast::While {
        variable: None,
        condition,
        block: Box::new(block),
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_loop<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    loop_depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Loop {
        block: Box::new(self::gen_block(
            u,
            scope,
            cfg,
            depth,
            loop_depth + 1,
            true,
            fn_name,
            param_types,
        )?),
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_loop_control<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
    match u.int_in_range(0..=3)? {
        0 => Ok(Ast::Continue {
            kind: Type::Void {
                span: u.arbitrary()?,
            },
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        1 => Ok(Ast::Break {
            kind: Type::Void {
                span: u.arbitrary()?,
            },
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        2 => Ok(Ast::ContinueAll {
            kind: Type::Void {
                span: u.arbitrary()?,
            },
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        _ => Ok(Ast::BreakAll {
            kind: Type::Void {
                span: u.arbitrary()?,
            },
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
    }
}

fn gen_defer<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Defer {
        node: Box::new(self::gen_stmt(
            u,
            scope,
            cfg,
            depth,
            0,
            fn_name,
            param_types,
        )?),
        kind: Type::Void {
            span: u.arbitrary()?,
        },
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_mutation<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let Some(picked) = scope.pick_mutable(u)? else {
        return self::gen_var(u, scope, cfg, depth);
    };

    let kind: Type = picked.kind.clone();

    Ok(Ast::Mutation {
        source: Box::new(Ast::Reference {
            name: picked.name,
            kind: kind.clone(),
            metadata: self::reference_metadata_for(&picked, true),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        value: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, &kind)?),
        kind: kind.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_self_call<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    fn_name: &'ast str,
    param_types: &[Type],
) -> arbitrary::Result<Ast<'ast>> {
    let mut args: Vec<Ast<'_>> = Vec::with_capacity(param_types.len());

    for param_type in param_types.iter() {
        args.push(self::gen_expr_of_type(u, scope, cfg, depth, param_type)?);
    }

    Ok(Ast::Call {
        name: fn_name.to_string(),
        args,
        generic_args: Vec::with_capacity(0),
        kind: self::gen_scalar_type(u)?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_expr_stmt<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let kind: Type = self::gen_scalar_type(u)?;

    if scope.has_of_type(&kind) && u.arbitrary()? {
        return Ok(self::gen_reference_of_type(u, scope, &kind)?);
    }

    self::gen_expr_of_type(u, scope, cfg, depth.max(1), &kind)
}

fn gen_expr_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    target: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    if depth == 0 {
        return self::gen_expr_leaf(u, target);
    }

    if target.is_ptr_type() {
        return Ok(Ast::NullPtr {
            span: u.arbitrary()?,
            kind: target.clone(),
        });
    }

    let has_matching_ref = scope.has_of_type(target);

    let has_ptr = scope.has_ptr_of(target).is_some();

    let upper: u32 = if has_matching_ref && has_ptr {
        8
    } else if has_matching_ref {
        7
    } else if has_ptr {
        6
    } else {
        5
    };

    match u.int_in_range(0..=upper)? {
        0 => self::gen_expr_leaf(u, target),
        1 if has_matching_ref => self::gen_reference_of_type(u, scope, target),
        2 => self::gen_binary_of_type(u, scope, cfg, depth - 1, target),
        3 => self::gen_unary_of_type(u, scope, cfg, depth - 1, target)
            .map(|opt| opt.unwrap_or_else(|| self::gen_expr_leaf(u, target).unwrap())),
        4 => self::gen_cast_of_type(u, scope, cfg, depth - 1, target),
        5 => Ok(Ast::Group {
            node: Box::new(self::gen_expr_of_type(u, scope, cfg, depth - 1, target)?),
            kind: target.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        6 if has_ptr => self::gen_deref_of_type(u, scope, target)
            .map(|opt| opt.unwrap_or_else(|| self::gen_expr_leaf(u, target).unwrap())),
        7 if has_matching_ref => self::gen_reference_of_type(u, scope, target),
        _ => self::gen_expr_leaf(u, target),
    }
}

fn gen_const_expr_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    target: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    if target.is_ptr_type() {
        return Ok(Ast::NullPtr {
            span: u.arbitrary()?,
            kind: target.clone(),
        });
    }

    if depth == 0 {
        return self::gen_const_leaf(u, target);
    }

    if target.is_bool_type() {
        let bool_kind: Type = Type::Bool {
            span: u.arbitrary()?,
        };

        if u.arbitrary()? {
            return Ok(Ast::Boolean {
                kind: target.clone(),
                value: u.arbitrary()?,
                span: u.arbitrary()?,
                id: NodeId::new(),
            });
        }

        let operator = self::logical_operator(u)?;

        return Ok(Ast::BinaryOp {
            left: Box::new(self::gen_const_expr_of_type(
                u,
                scope,
                cfg,
                depth.saturating_sub(1),
                &bool_kind,
            )?),
            operator,
            right: Box::new(self::gen_const_expr_of_type(
                u,
                scope,
                cfg,
                depth.saturating_sub(1),
                &bool_kind,
            )?),
            kind: target.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    if target.is_char_type() {
        return self::gen_const_leaf(u, target);
    }

    match u.int_in_range(0..=5)? {
        0 => self::gen_const_leaf(u, target),
        1 => {
            let operator: TokenType = if target.is_float_type() {
                self::floating_operator(u)?
            } else {
                self::const_integer_operator(u)?
            };

            Ok(Ast::BinaryOp {
                left: Box::new(self::gen_const_expr_of_type(
                    u,
                    scope,
                    cfg,
                    depth.saturating_sub(1),
                    target,
                )?),
                operator,
                right: Box::new(self::gen_const_expr_of_type(
                    u,
                    scope,
                    cfg,
                    depth.saturating_sub(1),
                    target,
                )?),
                kind: target.clone(),
                span: u.arbitrary()?,
                id: NodeId::new(),
            })
        }
        2 => {
            let operator: TokenType = if target.is_bool_type() {
                TokenType::Bang
            } else if target.is_integer_type() {
                if u.arbitrary()? {
                    TokenType::Minus
                } else {
                    TokenType::Not
                }
            } else {
                TokenType::Minus
            };

            Ok(Ast::UnaryOp {
                operator,
                kind: target.clone(),
                node: Box::new(self::gen_const_expr_of_type(
                    u,
                    scope,
                    cfg,
                    depth.saturating_sub(1),
                    target,
                )?),
                before: false,
                span: u.arbitrary()?,
                id: NodeId::new(),
            })
        }
        3 => Ok(Ast::Group {
            node: Box::new(self::gen_const_expr_of_type(
                u,
                scope,
                cfg,
                depth.saturating_sub(1),
                target,
            )?),
            kind: target.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        4 => {
            let source: Type = self::cast_source_type(u, target)?;

            Ok(Ast::As {
                from: Box::new(self::gen_const_expr_of_type(
                    u,
                    scope,
                    cfg,
                    depth.saturating_sub(1),
                    &source,
                )?),
                cast: target.clone(),
                metadata: CastingMetadata::new(true, false),
                span: u.arbitrary()?,
                id: NodeId::new(),
            })
        }
        _ => self::gen_const_leaf(u, target),
    }
}

fn gen_binary_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    target: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    if target.is_char_type() {
        return self::gen_expr_leaf(u, target);
    }

    if target.is_bool_type() {
        let bool_kind: Type = Type::Bool {
            span: u.arbitrary()?,
        };

        if u.arbitrary()? {
            let operator = self::logical_operator(u)?;

            return Ok(Ast::BinaryOp {
                left: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, &bool_kind)?),
                operator,
                right: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, &bool_kind)?),
                kind: target.clone(),
                span: u.arbitrary()?,
                id: NodeId::new(),
            });
        }

        let operand_type: Type = self::gen_integer_type(u)?;

        let operator = self::relational_operator(u)?;

        return Ok(Ast::BinaryOp {
            left: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, &operand_type)?),
            operator,
            right: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, &operand_type)?),
            kind: target.clone(),
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }

    let operator: TokenType = if target.is_float_type() {
        self::floating_operator(u)?
    } else {
        self::integer_operator(u)?
    };

    Ok(Ast::BinaryOp {
        left: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, target)?),
        operator,
        right: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, target)?),
        kind: target.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_unary_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    target: &Type,
) -> arbitrary::Result<Option<Ast<'ast>>> {
    let operator: TokenType = if target.is_char_type() {
        return Ok(None);
    } else if target.is_bool_type() {
        TokenType::Bang
    } else if target.is_integer_type() {
        if u.arbitrary()? {
            TokenType::Minus
        } else {
            TokenType::Not
        }
    } else if target.is_float_type() {
        TokenType::Minus
    } else {
        return Ok(None);
    };

    Ok(Some(Ast::UnaryOp {
        operator,
        kind: target.clone(),
        node: Box::new(self::gen_expr_of_type(u, scope, cfg, depth, target)?),
        before: false,
        span: u.arbitrary()?,
        id: NodeId::new(),
    }))
}

fn gen_cast_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    cfg: &Config,
    depth: usize,
    target: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    let source: Type = self::cast_source_type(u, target)?;

    let from = self::gen_expr_of_type(u, scope, cfg, depth, &source)?;

    let is_constant: bool = from.is_constant_value();
    let is_allocated: bool = from
        .is_memory_assigned_value()
        .map_err(|_| arbitrary::Error::IncorrectFormat)?;

    Ok(Ast::As {
        from: Box::new(from),
        cast: target.clone(),
        metadata: CastingMetadata::new(is_constant, is_allocated),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_deref_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &ScopeStack<'ast>,
    target: &Type,
) -> arbitrary::Result<Option<Ast<'ast>>> {
    let Some(ptr_var) = scope.has_ptr_of(target) else {
        return Ok(None);
    };

    Ok(Some(Ast::Deref {
        value: Box::new(Ast::Reference {
            name: ptr_var.name,
            kind: ptr_var.kind.clone(),
            metadata: self::reference_metadata_for(&ptr_var, true),
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        kind: target.clone(),
        modificators: Vec::new(),
        metadata: DereferenceMetadata::new(false, None),
        span: u.arbitrary()?,
        id: NodeId::new(),
    }))
}

fn gen_reference_of_type<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &ScopeStack<'ast>,
    target: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    let visible = scope.visible();

    let matches: Vec<_> = visible
        .into_iter()
        .filter(|var| var.kind == *target)
        .collect();

    let picked = if matches.is_empty() {
        return Err(arbitrary::Error::IncorrectFormat);
    } else {
        let idx = u.int_in_range(0..=(matches.len() - 1))?;

        matches[idx].clone()
    };

    Ok(Ast::Reference {
        name: picked.name,
        kind: picked.kind.clone(),
        metadata: self::reference_metadata_for(&picked, true),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn reference_metadata_for(var: &ScopedVar<'_>, is_mutable: bool) -> ReferenceMetadata {
    match var.reference_type {
        ReferenceType::Parameter => ReferenceMetadata::new(
            var.kind.is_ptr_like_type(),
            is_mutable,
            ReferenceType::Parameter,
            false,
        ),
        ReferenceType::Static => ReferenceMetadata::new(true, is_mutable, ReferenceType::Static, false),
        ReferenceType::Constant => {
            ReferenceMetadata::new(true, false, ReferenceType::Constant, false)
        }
        _ => ReferenceMetadata::new(true, is_mutable, ReferenceType::Local, false),
    }
}

fn gen_expr_leaf<'ast>(u: &mut Unstructured<'ast>, target: &Type) -> arbitrary::Result<Ast<'ast>> {
    if target.is_integer_type() {
        return Ok(Ast::Integer {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_float_type() {
        return Ok(Ast::Float {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_bool_type() {
        return Ok(Ast::Boolean {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_char_type() {
        return Ok(Ast::Char {
            kind: target.clone(),
            byte: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_ptr_type() {
        return Ok(Ast::NullPtr {
            span: u.arbitrary()?,
            kind: target.clone(),
        });
    }
    Err(arbitrary::Error::IncorrectFormat)
}

fn gen_const_leaf<'ast>(u: &mut Unstructured<'ast>, target: &Type) -> arbitrary::Result<Ast<'ast>> {
    if target.is_integer_type() {
        return Ok(Ast::Integer {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_float_type() {
        return Ok(Ast::Float {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_bool_type() {
        return Ok(Ast::Boolean {
            kind: target.clone(),
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    if target.is_char_type() {
        return Ok(Ast::Char {
            kind: target.clone(),
            byte: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        });
    }
    Err(arbitrary::Error::IncorrectFormat)
}

fn gen_scalar_type<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Type> {
    match u.int_in_range(0..=13)? {
        0 => Ok(Type::S8 {
            span: u.arbitrary()?,
        }),
        1 => Ok(Type::S16 {
            span: u.arbitrary()?,
        }),
        2 => Ok(Type::S32 {
            span: u.arbitrary()?,
        }),
        3 => Ok(Type::S64 {
            span: u.arbitrary()?,
        }),
        4 => Ok(Type::SSize {
            span: u.arbitrary()?,
        }),
        5 => Ok(Type::U8 {
            span: u.arbitrary()?,
        }),
        6 => Ok(Type::U16 {
            span: u.arbitrary()?,
        }),
        7 => Ok(Type::U32 {
            span: u.arbitrary()?,
        }),
        8 => Ok(Type::U64 {
            span: u.arbitrary()?,
        }),
        9 => Ok(Type::USize {
            span: u.arbitrary()?,
        }),
        10 => Ok(Type::F32 {
            span: u.arbitrary()?,
        }),
        11 => Ok(Type::F64 {
            span: u.arbitrary()?,
        }),
        12 => Ok(Type::Bool {
            span: u.arbitrary()?,
        }),
        _ => Ok(Type::Char {
            span: u.arbitrary()?,
        }),
    }
}

fn gen_integer_type<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Type> {
    match u.int_in_range(0..=9)? {
        0 => Ok(Type::S8 {
            span: u.arbitrary()?,
        }),
        1 => Ok(Type::S16 {
            span: u.arbitrary()?,
        }),
        2 => Ok(Type::S32 {
            span: u.arbitrary()?,
        }),
        3 => Ok(Type::S64 {
            span: u.arbitrary()?,
        }),
        4 => Ok(Type::SSize {
            span: u.arbitrary()?,
        }),
        5 => Ok(Type::U8 {
            span: u.arbitrary()?,
        }),
        6 => Ok(Type::U16 {
            span: u.arbitrary()?,
        }),
        7 => Ok(Type::U32 {
            span: u.arbitrary()?,
        }),
        8 => Ok(Type::U64 {
            span: u.arbitrary()?,
        }),
        _ => Ok(Type::USize {
            span: u.arbitrary()?,
        }),
    }
}

fn gen_decl_type<'ast>(u: &mut Unstructured<'ast>, cfg: &Config) -> arbitrary::Result<Type> {
    if cfg.allow_extras && u.arbitrary()? {
        return Ok(Type::Ptr {
            subtype: Some(Box::new(self::gen_scalar_type(u)?)),
            address_space: None,
            span: u.arbitrary()?,
        });
    }
    self::gen_scalar_type(u)
}

fn cast_source_type<'ast>(u: &mut Unstructured<'ast>, target: &Type) -> arbitrary::Result<Type> {
    if target.is_char_type() {
        let r: Type = match u.int_in_range(0..=1)? {
            0 => self::gen_integer_type(u),
            _ => Ok(Type::Char {
                span: u.arbitrary()?,
            }),
        }?;

        return Ok(r);
    }
    if target.is_integer_type() {
        return match u.int_in_range(0..=4)? {
            0 => self::gen_scalar_type(u),
            1 => match u.int_in_range(0..=1)? {
                0 => Ok(Type::F32 {
                    span: u.arbitrary()?,
                }),
                _ => Ok(Type::F64 {
                    span: u.arbitrary()?,
                }),
            },
            2 => Ok(Type::Char {
                span: u.arbitrary()?,
            }),
            3 => Ok(Type::Bool {
                span: u.arbitrary()?,
            }),
            _ => Ok(Type::Ptr {
                subtype: Some(Box::new(self::gen_scalar_type(u)?)),
                address_space: None,
                span: u.arbitrary()?,
            }),
        };
    }
    if target.is_float_type() {
        return match u.int_in_range(0..=2)? {
            0 => self::gen_integer_type(u),
            1 => match u.int_in_range(0..=1)? {
                0 => Ok(Type::F32 {
                    span: u.arbitrary()?,
                }),
                _ => Ok(Type::F64 {
                    span: u.arbitrary()?,
                }),
            },
            _ => Ok(Type::Char {
                span: u.arbitrary()?,
            }),
        };
    }
    if target.is_bool_type() {
        return self::gen_integer_type(u);
    }
    if target.is_ptr_type() {
        return match u.int_in_range(0..=1)? {
            0 => self::gen_integer_type(u),
            _ => Ok(Type::Ptr {
                subtype: Some(Box::new(self::gen_scalar_type(u)?)),
                address_space: None,
                span: u.arbitrary()?,
            }),
        };
    }
    Err(arbitrary::Error::IncorrectFormat)
}

fn reference<'ast>(name: &'ast str, kind: Type) -> Ast<'ast> {
    Ast::Reference {
        name,
        kind,
        metadata: ReferenceMetadata::new(true, true, ReferenceType::Local, false),
        span: thrustc_code_location::Span::nothing(),
        id: NodeId::new(),
    }
}

fn integer_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 19] = [
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Star,
        TokenType::Slash,
        TokenType::Arith,
        TokenType::Xor,
        TokenType::Bor,
        TokenType::BAnd,
        TokenType::LShift,
        TokenType::RShift,
        TokenType::PlusEq,
        TokenType::MinusEq,
        TokenType::StarEq,
        TokenType::SlashEq,
        TokenType::ArithEq,
        TokenType::BAndEq,
        TokenType::BorEq,
        TokenType::XorEq,
        TokenType::LShiftEq,
    ];
    Ok(*u.choose(&OPS)?)
}

fn const_integer_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 14] = [
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Star,
        TokenType::Xor,
        TokenType::Bor,
        TokenType::BAnd,
        TokenType::PlusEq,
        TokenType::MinusEq,
        TokenType::StarEq,
        TokenType::SlashEq,
        TokenType::ArithEq,
        TokenType::BAndEq,
        TokenType::BorEq,
        TokenType::XorEq,
    ];
    Ok(*u.choose(&OPS)?)
}

fn compound_integer_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 10] = [
        TokenType::PlusEq,
        TokenType::MinusEq,
        TokenType::StarEq,
        TokenType::SlashEq,
        TokenType::ArithEq,
        TokenType::BAndEq,
        TokenType::BorEq,
        TokenType::XorEq,
        TokenType::LShiftEq,
        TokenType::RShiftEq,
    ];
    Ok(*u.choose(&OPS)?)
}

fn floating_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 4] = [
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Star,
        TokenType::Slash,
    ];
    Ok(*u.choose(&OPS)?)
}

fn logical_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 2] = [TokenType::And, TokenType::Or];
    Ok(*u.choose(&OPS)?)
}

fn relational_operator<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<TokenType> {
    const OPS: [TokenType; 6] = [
        TokenType::EqEq,
        TokenType::BangEq,
        TokenType::Less,
        TokenType::LessEq,
        TokenType::Greater,
        TokenType::GreaterEq,
    ];
    Ok(*u.choose(&OPS)?)
}
