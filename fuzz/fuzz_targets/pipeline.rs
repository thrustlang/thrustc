#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use libfuzzer_sys::fuzz_target;
use thrustc_ast::Ast;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic::SemanticAnalysis;

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);

    if let Ok(ast) = Ast::arbitrary(&mut unstructured) {
        let options: CompilerOptions = CompilerOptions::new();

        let file = CompilationUnit::new(
            "pipeline.fuzz".into(),
            std::path::PathBuf::from(file!()),
            String::new(),
            "pipeline".into(),
        );

        let failed =
            SemanticAnalysis::new(std::slice::from_ref(&ast), &file, &options).analyze(false);

        if let Either::Left(had_errors) = failed
            && !had_errors
        {
            save_interesting_ast(&ast);
        }
    }
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
