use std::fs;
use std::path::PathBuf;

use either::Either;
use thrustc_fuzz::dumps;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic_analysis::SemanticAnalysis;

struct XorShift64(u64);

impl XorShift64 {
    fn fill(&mut self, len: usize) -> Vec<u8> {
        let mut buffer = Vec::with_capacity(len);
        while buffer.len() < len {
            let mut x = self.0;
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            self.0 = x;
            buffer.extend_from_slice(&x.to_le_bytes());
        }
        buffer.truncate(len);
        buffer
    }
}

fn main() {
    let target = "llvm-codegen-local";
    let samples: usize = std::env::args().nth(1).unwrap_or("300".into()).parse().unwrap();
    let seed: u64 = std::env::args().nth(2).unwrap_or("4660".into()).parse().unwrap();

    let file = CompilationUnit::new(
        "debug.fuzz".into(),
        PathBuf::from(file!()),
        String::new(),
        "codegen".into(),
    );
    let options = CompilerOptions::new();
    let mut rng = XorShift64(seed);
    let want_idx: Option<usize> = std::env::args().nth(3).map(|s| s.parse().unwrap());

    for i in 0..samples {
        let data = rng.fill(8192);
        let ast = match dumps::reconstruct_ast(target, &data) {
            Ok(ast) => ast,
            Err(_) => continue,
        };

        let failed = SemanticAnalysis::new(std::slice::from_ref(&ast), &file, &options).execute(false);

        let Either::Left(had_errors) = failed else { continue };
        if !had_errors {
            continue;
        }

        if Some(i) == want_idx {
            let out = std::env::temp_dir().join("fail.bin");
            fs::write(&out, &data).unwrap();
            eprintln!("wrote {} for sample #{i}", out.display());
            return;
        }
        eprintln!("### sample #{i} had errors");
    }
}
