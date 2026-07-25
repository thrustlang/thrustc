#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use libfuzzer_sys::{Corpus, fuzz_target};
use thrustc_ast::{Ast, traits::AstStandardExtensions};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic::SemanticAnalysis;

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
        "pipeline.fuzz".into(),
        std::path::PathBuf::from(file!()),
        String::new(),
        "pipeline".into(),
    );

    let failed = SemanticAnalysis::new(std::slice::from_ref(&ast), &file, &options).execute(false);

    if let Either::Left(had_errors) = failed
        && !had_errors
    {
        save_interesting_ast(&ast);

        return Corpus::Keep;
    }

    Corpus::Keep
});

fn save_interesting_ast(ast: &Ast) {
    static mut COUNTER: u32 = 0;

    let counter: u32 = unsafe {
        COUNTER = COUNTER.saturating_add(1);
        COUNTER
    };

    let filename: String = format!("valid_ast_{:04}.txt", counter);

    let content: String = format!(
        "=== Interesting AST #{}\n\
         Generated at: {}\n\
         Size of input data: {} bytes\n\n\
         {:#?}\n",
        counter,
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
        std::mem::size_of_val(ast),
        ast
    );

    let _ = std::fs::create_dir_all("fuzz_pipeline");

    let path: String = format!("fuzz_pipeline/{}", filename);

    if let Err(e) = std::fs::write(&path, content) {
        eprintln!("Failed to write interesting AST to {}: {}", path, e);
    } else {
        println!("✓ Saved interesting AST: {}", path);
    }
}

fn contains_unstable_ast(ast: &Ast) -> bool {
    ast.is_asm_function() || ast.is_global_asm_keyword()
}
