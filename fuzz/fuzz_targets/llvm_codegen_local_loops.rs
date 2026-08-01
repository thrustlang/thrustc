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

#![no_main]

use arbitrary::Unstructured;
use either::Either;
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine, TargetTriple},
};
use libfuzzer_sys::{fuzz_target, Corpus};
use thrustc_ast::traits::AstStandardExtensions;
use thrustc_ast::Ast;
use thrustc_ast::NodeId;
use thrustc_backends::{
    llvm::{target::LLVMTarget, LLVMBackend},
    ThrustOptimization,
};
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_codegen::context::LLVMCodeGenContext;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic::SemanticAnalysis;
use thrustc_typesystem::type_layout::TargetInfo;
use thrustc_typesystem::Type;

const MAX_DEPTH: usize = 6;
const MAX_LOOP_NESTING: usize = 4;
const MAX_STATEMENTS_PER_BLOCK: usize = 8;
const MAX_EXPR_DEPTH: usize = 3;

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

    let n_params = u.int_in_range(0..=3usize)?;
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
        depth,
        0,
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
    loop_depth: usize,
    return_type: &Type,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts = u.int_in_range(1..=MAX_STATEMENTS_PER_BLOCK)?;
    let mut nodes = Vec::with_capacity(n_stmts + 1);
    for _ in 0..n_stmts {
        nodes.push(gen_stmt(u, scope, depth, loop_depth)?);
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

    scope.pop();

    Ok(Ast::Block {
        nodes,
        post: Vec::new(),
        kind: return_type.clone(),
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_block<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
    loop_depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let n_stmts = u.int_in_range(1..=MAX_STATEMENTS_PER_BLOCK)?;
    let mut nodes = Vec::with_capacity(n_stmts);

    for _ in 0..n_stmts {
        nodes.push(gen_stmt(u, scope, depth, loop_depth)?);
    }

    scope.pop();

    Ok(Ast::Block {
        nodes,
        post: Vec::new(),
        kind: u.arbitrary()?,
        span: u.arbitrary()?,
        id: NodeId::new(),
    })
}

fn gen_stmt<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
    loop_depth: usize,
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

    let can_nest_loop = depth > 1 && loop_depth < MAX_LOOP_NESTING;
    let can_control = loop_depth > 0;

    let upper: u32 = match (can_nest_loop, can_control) {
        (true, true) => 15,
        (true, false) => 12,
        (false, true) => 6,
        (false, false) => 4,
    };

    match u.int_in_range(0..=upper)? {
        0 => gen_var(u, scope, depth),
        1 => gen_mutation(u, scope, depth),
        2 | 3 | 4 => gen_if(u, scope, depth, loop_depth),
        5 | 6 | 7 if can_nest_loop => gen_for(u, scope, depth, loop_depth),
        8 | 9 | 10 if can_nest_loop => gen_while(u, scope, depth, loop_depth),
        11 | 12 if can_nest_loop => gen_loop(u, scope, depth, loop_depth),
        13 | 14 if can_control => gen_loop_control(u),
        15 if can_control => gen_loop_control(u),
        _ => gen_var(u, scope, depth),
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
        Some(Box::new(gen_expr(
            u,
            scope,
            depth.saturating_sub(1).min(MAX_EXPR_DEPTH),
        )?))
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
    loop_depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    let condition = Box::new(gen_condition(u, scope)?);
    let then_branch = Box::new(gen_block(u, scope, depth - 1, loop_depth)?);

    let n_elif = u.int_in_range(0..=2usize)?;
    let mut else_if_branch = Vec::with_capacity(n_elif);
    for _ in 0..n_elif {
        let elif_condition = Box::new(gen_condition(u, scope)?);
        let elif_block = Box::new(gen_block(u, scope, depth - 1, loop_depth)?);
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
            block: Box::new(gen_block(u, scope, depth - 1, loop_depth)?),
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
    loop_depth: usize,
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
    let block = Box::new(gen_block(u, scope, depth - 1, loop_depth + 1)?);

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
    loop_depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    scope.push();

    let variable = if u.arbitrary()? {
        Some(Box::new(gen_var(u, scope, depth.saturating_sub(1))?))
    } else {
        None
    };

    let condition = Box::new(gen_condition(u, scope)?);
    let block = Box::new(gen_block(u, scope, depth - 1, loop_depth + 1)?);

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
    loop_depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    Ok(Ast::Loop {
        block: Box::new(gen_block(u, scope, depth - 1, loop_depth + 1)?),
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

fn gen_mutation<'ast>(
    u: &mut Unstructured<'ast>,
    scope: &mut ScopeStack<'ast>,
    depth: usize,
) -> arbitrary::Result<Ast<'ast>> {
    if !scope.has_any() {
        return gen_var(u, scope, depth);
    }

    let visible = scope.visible();
    let idx = u.int_in_range(0..=(visible.len() - 1))?;
    let picked = visible[idx].clone();

    Ok(Ast::Mutation {
        source: Box::new(Ast::Reference {
            name: picked.name,
            kind: picked.kind.clone(),
            metadata: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        value: Box::new(gen_expr(
            u,
            scope,
            depth.saturating_sub(1).min(MAX_EXPR_DEPTH),
        )?),
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
    match u.int_in_range(0..=2)? {
        0 => Ok(Ast::Integer {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        1 => Ok(Ast::Boolean {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
        }),
        _ => Ok(Ast::Float {
            kind: u.arbitrary()?,
            value: u.arbitrary()?,
            span: u.arbitrary()?,
            id: NodeId::new(),
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
    let upper: u32 = if has_vars { 4 } else { 3 };

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
        _ => gen_literal(u),
    }
}

fuzz_target!(|data: &[u8]| -> Corpus {
    let stable_mode: bool = std::env::args().any(|arg| arg == "--stable");

    let mut unstructured = Unstructured::new(data);

    let Ok(ast) = gen_root(&mut unstructured) else {
        return Corpus::Reject;
    };

    if stable_mode && self::contains_unstable_ast(&ast) {
        return Corpus::Reject;
    }

    let options: CompilerOptions = CompilerOptions::new();

    let file = CompilationUnit::new(
        "codegen_loops.fuzz".into(),
        std::path::PathBuf::from(file!()),
        String::new(),
        "codegen_loops".into(),
    );

    let failed = SemanticAnalysis::new(std::slice::from_ref(&ast), &file, &options).execute(false);

    if let Either::Left(had_errors) = failed
        && !had_errors
    {
        Target::initialize_all(&InitializationConfig::default());

        let llvm_backend: LLVMBackend = LLVMBackend::new();

        let target: &LLVMTarget = llvm_backend.get_target();
        let llvm_triple: &TargetTriple = target.get_target_triple();

        let llvm_target_triple_formatted: String =
            llvm_triple.as_str().to_string_lossy().to_string();

        let llvm_target_triple: LLVMTargetTriple =
            LLVMTargetTriple::new(llvm_target_triple_formatted.clone());

        let llvm_cpu_name: &str = llvm_backend.get_target_cpu().get_cpu_name();
        let llvm_cpu_features: &str = llvm_backend.get_target_cpu().get_cpu_features();

        let compiler_optimization: ThrustOptimization = llvm_backend.get_optimization();
        let llvm_opt: inkwell::OptimizationLevel = compiler_optimization.to_llvm_opt();

        let target: Target = Target::from_triple(llvm_triple).unwrap_or_else(|_| {
            panic!("The compiler couldn't be configured correctly. The target is possibly unrecognizable. Try again another target or try to fix it.")
        });

        if !target.has_target_machine() {
            panic!(
                "The compiler couldn't be configured correctly. The specified target cannot be used for code generation. Try with another target."
            );
        }

        let target_machine: TargetMachine = target
            .create_target_machine(
                llvm_triple,
                llvm_cpu_name,
                llvm_cpu_features,
                llvm_opt,
                llvm_backend.get_reloc_mode(),
                llvm_backend.get_code_model(),
            )
            .unwrap_or_else(|| {
                panic!(
                    "The compiler couldn't be configured correctly. Possibly the target is not supported for code generation.",
                )
            });

        let target_data: TargetData = target_machine.get_target_data();
        let target_triple: TargetTriple = target_machine.get_triple();

        let target_info: TargetInfo =
            TargetInfo::new(LLVMTargetTriple::new(llvm_target_triple_formatted));

        let target_abi: Option<LLVMABIRepresentation> = thrustc_llvm_abi::get_abi(
            options.abi_configuration().specific(),
            &file,
            &options,
            &llvm_target_triple,
            &target_info,
            &target_data,
        );

        let llvm_context: Context = Context::create();
        let llvm_builder: Builder = llvm_context.create_builder();
        let llvm_module: Module = llvm_context.create_module(file.get_name());

        llvm_module.set_triple(llvm_triple);
        llvm_module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        let mut llvm_codegen_context: LLVMCodeGenContext = LLVMCodeGenContext::new(
            &llvm_module,
            &llvm_context,
            &llvm_builder,
            &target_data,
            &target_triple,
            &target_machine,
            target_abi.as_ref(),
            Diagnostician::new(&file, &options),
            &options,
            &file,
        );

        thrustc_llvm_codegen::LLVMCompiler::compile(
            &mut llvm_codegen_context,
            std::slice::from_ref(&ast),
        );

        if let Err(codegen_error) = llvm_module.verify() {
            panic!("LLVM CODEGEN ERROR: {}", codegen_error);
        }

        return Corpus::Keep;
    }

    Corpus::Reject
});

fn contains_unstable_ast(ast: &Ast) -> bool {
    ast.is_asm_function() || ast.is_global_asm_keyword()
}
