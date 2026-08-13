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

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine, TargetTriple},
};
use thrustc_ast::traits::AstStandardExtensions;
use thrustc_ast::Ast;
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

pub fn contains_unstable_ast(ast: &Ast) -> bool {
    ast.is_asm_function() || ast.is_global_asm_keyword()
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

    let failed = SemanticAnalysis::new(std::slice::from_ref(ast), &file, &options).execute(false);

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
