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
use either::Either;
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine, TargetTriple},
};
use std::fs;
use std::path::{Path, PathBuf};
use thrustc_ast::Ast;
use thrustc_ast::traits::AstStandardExtensions;
use thrustc_backends::{
    ThrustOptimization,
    llvm::{LLVMBackend, target::LLVMTarget},
};
use thrustc_diagnostician::Diagnostician;
use thrustc_llvm_abi_representation::LLVMABIRepresentation;
use thrustc_llvm_codegen::context::LLVMCodeGenContext;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic::SemanticAnalysis;
use thrustc_typesystem::type_layout::TargetInfo;

const VALID_GENERATORS: &[&str] = &["llvm-codegen-local", "llvm-codegen-local-loops"];
const DEFAULT_GENERATOR: &str = "llvm-codegen-local";

fn main() {
    let mut args = std::env::args().skip(1);

    let Some(crash_file) = args.next() else {
        eprintln!("usage: dump_llvm_ir <crash-file> [generator] [--stable]");
        eprintln!("valid generators: {}", VALID_GENERATORS.join(", "));
        std::process::exit(1);
    };

    let mut generator: String = DEFAULT_GENERATOR.to_string();
    let mut stable_mode: bool = false;

    for arg in args {
        match arg.as_str() {
            "--stable" => stable_mode = true,
            name if VALID_GENERATORS.contains(&name) => generator = name.to_string(),
            unknown => {
                eprintln!(
                    "Unknown argument '{unknown}'. Valid generators are: {}",
                    VALID_GENERATORS.join(", ")
                );
                std::process::exit(1);
            }
        }
    }

    let crash_file: PathBuf = PathBuf::from(crash_file);

    if !crash_file.exists() {
        eprintln!("Crash file not found: {}", crash_file.display());
        std::process::exit(1);
    }

    let data: Vec<u8> = fs::read(&crash_file).expect("could not read crash file");

    let mut unstructured: Unstructured<'_> = Unstructured::new(&data);

    let ast: Ast<'_> = match gen_root_for(&generator, &mut unstructured) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Arbitrary failed to reconstruct the AST: {e}");
            std::process::exit(1);
        }
    };

    if stable_mode && contains_unstable_ast(&ast) {
        eprintln!("AST contains unstable constructs; skipping under --stable.");
        std::process::exit(1);
    }

    emit_llvm_ir(&ast, &crash_file);
}

fn gen_root_for<'ast>(
    generator: &str,
    unstructured: &mut Unstructured<'ast>,
) -> arbitrary::Result<Ast<'ast>> {
    match generator {
        "llvm-codegen-local" => thrustc_fuzz::llvm_codegen_local::gen_root(unstructured),
        "llvm-codegen-local-loops" => thrustc_fuzz::llvm_codegen_local_loops::gen_root(unstructured),
        _ => unreachable!("validated generator"),
    }
}

fn emit_llvm_ir<'ast>(ast: &Ast<'ast>, crash_file: &Path) {
    let options: CompilerOptions = CompilerOptions::new();

    let file = CompilationUnit::new(
        "codegen.fuzz".into(),
        PathBuf::from(file!()),
        String::new(),
        "codegen".into(),
    );

    let failed = SemanticAnalysis::new(std::slice::from_ref(ast), &file, &options).execute(false);

    let Either::Left(had_errors) = failed else {
        eprintln!("Semantic analysis aborted due to parser/diagnostic errors.");
        return;
    };

    if had_errors {
        eprintln!("Semantic analysis reported errors; no LLVM IR was generated.");
        return;
    }

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
        std::slice::from_ref(ast),
    );

    let out_dir: PathBuf = PathBuf::from("fuzz/llvm_ir_dumps");
    fs::create_dir_all(&out_dir).unwrap();

    let name: String = crash_file
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let out_path: PathBuf = out_dir.join(format!("{name}.ll"));

    fs::write(&out_path, llvm_module.print_to_string().to_string()).unwrap();

    println!("LLVM IR dumped to: {}", out_path.display());
}

fn contains_unstable_ast(ast: &Ast) -> bool {
    ast.is_asm_function() || ast.is_global_asm_keyword()
}
