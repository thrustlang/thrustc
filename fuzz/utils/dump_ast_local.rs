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
use std::fs;
use std::path::PathBuf;
use thrustc_ast::Ast;
use thrustc_ast::NodeId;
use thrustc_typesystem::Type;

const MAX_DEPTH: usize = 5;
const MAX_STATEMENTS_PER_BLOCK: usize = 20;
const MAX_EXPR_DEPTH: usize = 10;

#[derive(Clone)]
struct ScopedVar<'ast> {
    name: &'ast str,
    kind: Type,
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

    fn declare(&mut self, name: &'ast str, kind: Type) {
        if let Some(frame) = self.frames.last_mut() {
            frame.push(ScopedVar { name, kind });
        }
    }

    fn visible(&self) -> Vec<ScopedVar<'ast>> {
        self.frames.iter().flatten().cloned().collect()
    }

    fn has_any(&self) -> bool {
        self.frames.iter().any(|f| !f.is_empty())
    }
}

#[inline]
fn gen_name<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<&'ast str> {
    u.arbitrary()
}

fn gen_root<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
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
        scope.declare(name, kind.clone());
        parameter_types.push(kind.clone());
        parameters.push(Ast::FunctionParameter {
            name,
            ascii_name: name,
            kind,
            position: i as u32,
            metadata: u.arbitrary()?,
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
        name,
        ascii_name: name,
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

    scope.declare(name, kind.clone());

    Ok(Ast::Var {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: u.arbitrary()?,
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

    scope.declare(name, kind.clone());

    Ok(Ast::Const {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: u.arbitrary()?,
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

    scope.declare(name, kind.clone());

    Ok(Ast::Static {
        name,
        ascii_name: name,
        kind,
        value,
        attributes: u.arbitrary()?,
        modificators: u.arbitrary()?,
        metadata: u.arbitrary()?,
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
            metadata: u.arbitrary()?,
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
    name: &'ast str,
    kind: Type,
) -> arbitrary::Result<Ast<'ast>> {
    let current = Ast::Reference {
        name,
        kind: kind.clone(),
        metadata: u.arbitrary()?,
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
        metadata: u.arbitrary()?,
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
        metadata: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    });

    scope.declare(loop_var_name, loop_var_kind.clone());

    let condition = Box::new(Ast::BinaryOp {
        left: Box::new(Ast::Reference {
            name: loop_var_name,
            kind: loop_var_kind.clone(),
            metadata: u.arbitrary()?,
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

    let actions = Box::new(gen_increment(u, loop_var_name, loop_var_kind.clone())?);
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
    if !scope.has_any() {
        return gen_var(u, scope, depth);
    }
    Ok(Ast::Mutation {
        source: Box::new(gen_reference(u, scope)?),
        value: Box::new(gen_expr(u, scope, depth.saturating_sub(1))?),
        kind: u.arbitrary()?,
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
        name: gen_name(u)?,
        args,
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
        kind: picked.kind,
        metadata: u.arbitrary()?,
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
        5 => Ok(Ast::As {
            from: Box::new(gen_expr(u, scope, depth - 1)?),
            cast: u.arbitrary()?,
            metadata: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        6 if has_vars => Ok(Ast::Deref {
            value: Box::new(gen_reference(u, scope)?),
            kind: u.arbitrary()?,
            modificators: u.arbitrary()?,
            metadata: u.arbitrary()?,
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

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("usage: dump_ast_local <crash-file>");
    let data = fs::read(&path).expect("could not read crash file");

    let mut unstructured = Unstructured::new(&data);
    match gen_root(&mut unstructured) {
        Ok(ast) => {
            let out_dir = PathBuf::from("fuzz/ast_dumps");
            fs::create_dir_all(&out_dir).unwrap();

            let name = PathBuf::from(&path)
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();
            let out_path = out_dir.join(format!("{name}.txt"));

            fs::write(&out_path, format!("{ast:#?}")).unwrap();
            println!("AST dumped successfully to: {}", out_path.display());
        }
        Err(e) => eprintln!("Arbitrary failed to reconstruct the AST with gen_root: {e}"),
    }
}
