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

use arbitrary::{Arbitrary, Unstructured};
use either::Either;
use libfuzzer_sys::{Corpus, fuzz_target};
use std::sync::atomic::{AtomicU32, Ordering};
use thrustc_ast::{Ast, traits::AstStandardExtensions};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic::SemanticAnalysis;

const MAX_SAVED_ASTS: u32 = 512;

static SAVED_ASTS: AtomicU32 = AtomicU32::new(0);

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

    let Either::Left(had_errors) = failed else {
        return Corpus::Reject;
    };

    if had_errors {
        return Corpus::Reject;
    }

    save_interesting_ast(&ast, data.len());

    Corpus::Keep
});

fn save_interesting_ast(ast: &Ast, input_size: usize) {
    let counter: u32 = SAVED_ASTS.fetch_add(1, Ordering::Relaxed);

    if counter >= MAX_SAVED_ASTS {
        return;
    }

    let filename: String = format!("valid_ast_{:04}.txt", counter);

    let content: String = format!(
        "=== Interesting AST #{}\n\
         Generated at: {}\n\
         Size of input data: {} bytes\n\n\
         {:#?}\n",
        counter,
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
        input_size,
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
