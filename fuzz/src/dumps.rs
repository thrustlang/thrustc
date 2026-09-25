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

use std::path::PathBuf;

use thrustc_directive::{FileDirectives, FileOptions};

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine, TargetTriple},
};
use thrustc_ast::ast_builtins::{AstBuiltin, DeferredBuiltinArgument};
use thrustc_ast::traits::AstStandardExtensions;
use thrustc_ast::{Ast, ModuleExpressionValues};
use thrustc_backends::{
    llvm::{target::LLVMTarget, LLVMBackend},
    ThrustOptimization,
};
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_codegen::context::LLVMCodeGenContext;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic_analysis::SemanticAnalysis;
use thrustc_typesystem::type_layout::TargetInfo;

pub const TARGETS: &[&str] = &[
    "llvm-codegen-top-level",
    "llvm-codegen-local-loops",
    "llvm-codegen-local",
    "lexer",
    "pipeline",
];

pub const GENERATORS: &[&str] = &[
    "pipeline",
    "llvm-codegen-top-level",
    "llvm-codegen-local",
    "llvm-codegen-local-loops",
];

pub const CODGEN_TARGETS: &[&str] = &[
    "llvm-codegen-top-level",
    "llvm-codegen-local",
    "llvm-codegen-local-loops",
];

pub const CRASH_MARKERS: &[&str] = &[
    "ERROR: libFuzzer: deadly signal",
    "ERROR: libFuzzer: out-of-memory",
    "ERROR: libFuzzer: timeout",
    "ERROR: AddressSanitizer",
    "SUMMARY: AddressSanitizer",
    "SUMMARY: libFuzzer",
    "UNREACHABLE executed",
    "panicked at",
    "SEGV on unknown address",
    "attempt to subtract with overflow",
    "attempt to add with overflow",
    "attempt to multiply with overflow",
    "index out of bounds",
];

pub fn classify(combined: &str) -> Option<&'static str> {
    CRASH_MARKERS.iter().copied().find(|m| combined.contains(m))
}

/// Returns true if the given AST node is an unstable feature, or if any of its
/// descendants is. The leaf detection (`is_unstable_feature`) lives in the
/// compiler's `AstStandardExtensions` trait; the tree walk lives here in the
/// fuzzer so the compiler itself stays untouched.
pub fn contains_unstable_ast(ast: &Ast) -> bool {
    if ast.is_unstable_feature() {
        return true;
    }

    contains_unstable_in_children(ast)
}

fn contains_unstable_in_children(ast: &Ast) -> bool {
    match ast {
        Ast::FixedArray { items, .. } | Ast::Array { items, .. } => {
            items.iter().any(contains_unstable_ast)
        }
        Ast::Index { source, index, .. } => {
            contains_unstable_ast(&**source) || contains_unstable_ast(&**index)
        }
        Ast::Property { source, .. } => contains_unstable_ast(&**source),
        Ast::If {
            condition,
            then_branch,
            else_if_branch,
            else_branch,
            ..
        } => {
            contains_unstable_ast(&**condition)
                || contains_unstable_ast(&**then_branch)
                || else_if_branch.iter().any(contains_unstable_ast)
                || else_branch
                    .as_ref()
                    .map_or(false, |node| contains_unstable_ast(&**node))
        }
        Ast::Elif { condition, block, .. } => {
            contains_unstable_ast(&**condition) || contains_unstable_ast(&**block)
        }
        Ast::Else { block, .. } => contains_unstable_ast(&**block),
        Ast::CompileTimeIf {
            condition,
            then_branch,
            else_if_branch,
            else_branch,
            ..
        } => {
            contains_unstable_ast(&**condition)
                || contains_unstable_ast(&**then_branch)
                || else_if_branch.iter().any(contains_unstable_ast)
                || else_branch
                    .as_ref()
                    .map_or(false, |node| contains_unstable_ast(&**node))
        }
        Ast::For {
            local,
            condition,
            actions,
            block,
            ..
        } => {
            contains_unstable_ast(&**local)
                || contains_unstable_ast(&**condition)
                || contains_unstable_ast(&**actions)
                || contains_unstable_ast(&**block)
        }
        Ast::While {
            variable,
            condition,
            block,
            ..
        } => {
            variable
                .as_ref()
                .map_or(false, |node| contains_unstable_ast(&**node))
                || contains_unstable_ast(&**condition)
                || contains_unstable_ast(&**block)
        }
        Ast::Loop { block, .. } => contains_unstable_ast(&**block),
        Ast::Block { nodes, post, .. } => {
            nodes.iter().any(contains_unstable_ast) || post.iter().any(contains_unstable_ast)
        }
        Ast::Defer { node, .. } => contains_unstable_ast(&**node),
        Ast::Enum { data, .. } => data
            .iter()
            .any(|(_, _, value)| contains_unstable_ast(value)),
        Ast::EnumValue { value, .. } => contains_unstable_ast(&**value),
        Ast::CompilerIntrinsic { parameters, .. } => {
            parameters.iter().any(contains_unstable_ast)
        }
        Ast::AssemblerFunction { parameters, .. } => {
            parameters.iter().any(contains_unstable_ast)
        }
        Ast::Function { parameters, body, .. } => {
            parameters.iter().any(contains_unstable_ast)
                || body
                    .as_ref()
                    .map_or(false, |node| contains_unstable_ast(&**node))
        }
        Ast::Return { expression, .. } => expression
            .as_ref()
            .map_or(false, |node| contains_unstable_ast(&**node)),
        Ast::Static { value, .. } => value
            .as_ref()
            .map_or(false, |node| contains_unstable_ast(&**node)),
        Ast::Const { value, .. } => contains_unstable_ast(&**value),
        Ast::Var { value, .. } => value
            .as_ref()
            .map_or(false, |node| contains_unstable_ast(&**node)),
        Ast::Mutation { source, value, .. } => {
            contains_unstable_ast(&**source) || contains_unstable_ast(&**value)
        }
        Ast::Address { source, indexes, .. } => {
            contains_unstable_ast(&**source) || indexes.iter().any(contains_unstable_ast)
        }
        Ast::Write { source, write_value, .. } => {
            contains_unstable_ast(&**source) || contains_unstable_ast(&**write_value)
        }
        Ast::Load { source, .. } => contains_unstable_ast(&**source),
        Ast::Deref { value, .. } => contains_unstable_ast(&**value),
        Ast::As { from, .. } => contains_unstable_ast(&**from),
        Ast::GetLocation { expr, .. } => contains_unstable_ast(&**expr),
        Ast::ModuleExpression { values, .. } => match values {
            ModuleExpressionValues::Call { arguments, .. } => {
                arguments.iter().any(contains_unstable_ast)
            }
            ModuleExpressionValues::Reference { .. } => false,
        },
        Ast::Call { args, .. } => args.iter().any(contains_unstable_ast),
        Ast::IndirectCall { function, args, .. } => {
            contains_unstable_ast(&**function) || args.iter().any(contains_unstable_ast)
        }
        Ast::AsmValue { args, .. } => args.iter().any(contains_unstable_ast),
        Ast::BinaryOp { left, right, .. } => {
            contains_unstable_ast(&**left) || contains_unstable_ast(&**right)
        }
        Ast::UnaryOp { node, .. } => contains_unstable_ast(&**node),
        Ast::Group { node, .. } => contains_unstable_ast(&**node),
        Ast::Builtin { builtin, .. } => contains_unstable_in_builtin(builtin),
        Ast::Constructor { data, .. } => data
            .iter()
            .any(|(_, value, _, _)| contains_unstable_ast(value)),
        Ast::CString { .. }
        | Ast::CNString { .. }
        | Ast::Char { .. }
        | Ast::Boolean { .. }
        | Ast::Integer { .. }
        | Ast::Float { .. }
        | Ast::NullPtr { .. }
        | Ast::GlobalAssembler { .. }
        | Ast::Embedded { .. }
        | Ast::Struct { .. }
        | Ast::CustomType { .. }
        | Ast::CompilerIntrinsicParameter { .. }
        | Ast::AssemblerFunctionParameter { .. }
        | Ast::FunctionParameter { .. }
        | Ast::Reference { .. }
        | Ast::Continue { .. }
        | Ast::Break { .. }
        | Ast::ContinueAll { .. }
        | Ast::BreakAll { .. }
        | Ast::Import { .. }
        | Ast::ImportC { .. }
        | Ast::Unreachable { .. }
        | Ast::Invalid { .. } => false,
    }
}

fn contains_unstable_in_builtin(builtin: &AstBuiltin) -> bool {
    match builtin {
        AstBuiltin::MemCpy { src, dst, size, .. }
        | AstBuiltin::MemMove { src, dst, size, .. } => {
            contains_unstable_ast(&**src)
                || contains_unstable_ast(&**dst)
                || contains_unstable_ast(&**size)
        }
        AstBuiltin::MemSet { dst, size, .. } => {
            contains_unstable_ast(&**dst) || contains_unstable_ast(&**size)
        }
        AstBuiltin::AtomicRMW { destination, value, .. } => {
            contains_unstable_ast(&**destination) || contains_unstable_ast(&**value)
        }
        AstBuiltin::AtomicCompareAndSwap {
            destination,
            expected,
            new_value,
            ..
        } => {
            contains_unstable_ast(&**destination)
                || contains_unstable_ast(&**expected)
                || contains_unstable_ast(&**new_value)
        }
        AstBuiltin::ArbitraryArgsCopy { source, .. } => contains_unstable_ast(&**source),
        AstBuiltin::ArbitraryArgsEnd { list, .. } => contains_unstable_ast(&**list),
        AstBuiltin::ArbitraryArgFrom { list, .. } => contains_unstable_ast(&**list),
        AstBuiltin::DeferredCompileTime { arguments, .. } => arguments.iter().any(|arg| match arg {
            DeferredBuiltinArgument::Value { expression, .. } => {
                contains_unstable_ast(&**expression)
            }
            _ => false,
        }),
        _ => false,
    }
}

pub fn reconstruct_ast<'a>(target: &str, data: &'a [u8]) -> Result<Ast<'a>, String> {
    let mut unstructured = Unstructured::new(data);

    let result = match target {
        "pipeline" => Ast::arbitrary(&mut unstructured),
        "llvm-codegen-top-level" => crate::llvm_codegen_top_level::gen_root(&mut unstructured),
        "llvm-codegen-local" => crate::llvm_codegen_local::gen_root(&mut unstructured),
        "llvm-codegen-local-loops" => crate::llvm_codegen_local_loops::gen_root(&mut unstructured),
        other => return Err(format!("unknown AST generator for target '{other}'")),
    };

    result.map_err(|e| format!("Arbitrary failed to reconstruct the AST: {e}"))
}

pub fn ast_dump(target: &str, data: &[u8]) -> Result<String, String> {
    let ast = reconstruct_ast(target, data)?;

    Ok(format!("{ast:#?}"))
}

pub fn emit_llvm_ir_core<'ast>(ast: &Ast<'ast>) -> Option<String> {
    let options: CompilerOptions = CompilerOptions::new();

    let file = CompilationUnit::new(
        "codegen.fuzz".into(),
        PathBuf::from(file!()),
        String::new(),
        "codegen".into(),
    );

    let directives = FileDirectives::default();
    let file_options = FileOptions::new(&options, &directives);

    let failed = SemanticAnalysis::new(std::slice::from_ref(ast), &file, &file_options).execute(false);

    let Either::Left(had_errors) = failed else {
        return None;
    };

    if had_errors {
        return None;
    }

    Target::initialize_all(&InitializationConfig::default());

    let llvm_backend: LLVMBackend = LLVMBackend::new();

    let target: &LLVMTarget = llvm_backend.get_target();
    let llvm_triple: &TargetTriple = target.get_target_triple();

    let llvm_target_triple_formatted: String = llvm_triple.as_str().to_string_lossy().to_string();

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
        &file_options,
        &file,
    );

    thrustc_llvm_codegen::LLVMCompiler::compile(
        &mut llvm_codegen_context,
        std::slice::from_ref(ast),
    );

    Some(llvm_module.print_to_string().to_string())
}

pub fn emit_llvm_ir<'ast>(ast: &Ast<'ast>) -> Result<Option<String>, String> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| emit_llvm_ir_core(ast)))
        .map_err(|payload| panic_message(&payload))
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "unknown panic (non-string payload)".to_string()
    }
}
