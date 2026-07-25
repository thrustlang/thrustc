#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine, TargetTriple},
};
use libfuzzer_sys::{Corpus, fuzz_target};
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

fuzz_target!(|data: &[u8]| -> Corpus {
    let stable_mode: bool = std::env::args().any(|arg| arg == "--stable");

    let mut unstructured = Unstructured::new(data);

    let Ok(ast) = Ast::arbitrary(&mut unstructured) else {
        return Corpus::Reject;
    };

    if stable_mode && self::contains_unstable_ast(&ast) {
        return Corpus::Reject;
    }

    let options: CompilerOptions = CompilerOptions::new();

    let file = CompilationUnit::new(
        "codegen.fuzz".into(),
        std::path::PathBuf::from(file!()),
        String::new(),
        "codegen".into(),
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
