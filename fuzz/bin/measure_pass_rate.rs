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

use either::Either;
use thrustc_fuzz::dumps;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_semantic_analysis::SemanticAnalysis;

const DEFAULT_SAMPLES: usize = 200;
const DEFAULT_MAX_BYTES: usize = 8192;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let targets: Vec<&str> = if args
        .iter()
        .any(|a| a == "--all")
    {
        dumps::GENERATORS.to_vec()
    } else if let Some(target) = args.first().map(String::as_str).filter(|t| !t.starts_with("--")) {
        if !dumps::GENERATORS.contains(&target) {
            eprintln!(
                "unknown target '{target}'. Valid generators: {}",
                dumps::GENERATORS.join(", ")
            );
            std::process::exit(1);
        }
        vec![target]
    } else {
        vec!["llvm-codegen-local"]
    };

    let samples: usize = parse_usize_flag(&args, "--samples").unwrap_or(DEFAULT_SAMPLES);
    let max_bytes: usize = parse_usize_flag(&args, "--max-bytes").unwrap_or(DEFAULT_MAX_BYTES);
    let seed: u64 = parse_u64_flag(&args, "--seed").unwrap_or(0xDECAF_BAD);

    let file = CompilationUnit::new(
        "passrate.fuzz".into(),
        PathBuf::from(file!()),
        String::new(),
        "codegen".into(),
    );

    for target in targets {
        let pass = measure(target, samples, max_bytes, seed, &file);
        let pct = 100.0 * pass as f64 / samples as f64;
        println!(
            "{target}: {pass}/{samples} kept ({pct:.2}%)  [seed={seed:#x}]"
        );
    }
}

fn measure(
    target: &str,
    samples: usize,
    max_bytes: usize,
    seed: u64,
    file: &CompilationUnit,
) -> usize {
    let options = CompilerOptions::new();
    let mut rng = XorShift64::new(seed);
    let mut kept = 0usize;

    for _ in 0..samples {
        let data = rng.fill(max_bytes);

        let ast = match dumps::reconstruct_ast(target, &data) {
            Ok(ast) => ast,
            Err(_) => continue,
        };

        let failed =
            SemanticAnalysis::new(std::slice::from_ref(&ast), file, &options).execute(false);

        let Either::Left(had_errors) = failed else {
            continue;
        };

        if !had_errors {
            kept += 1;
        }
    }

    kept
}

fn parse_usize_flag(args: &[String], flag: &str) -> Option<usize> {
    let index = args.iter().position(|a| a == flag)?;
    args.get(index + 1)?.parse().ok()
}

fn parse_u64_flag(args: &[String], flag: &str) -> Option<u64> {
    let index = args.iter().position(|a| a == flag)?;
    args.get(index + 1)?.parse().ok()
}

/// Deterministic xorshift64 PRNG so the measurement is reproducible without
/// pulling in an extra dependency.
struct XorShift64 {
    state: u64,
}

impl XorShift64 {
    fn new(seed: u64) -> Self {
        Self {
            state: if seed == 0 { 0x9E37_79B9_7F4A_7C15 } else { seed },
        }
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.state;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.state = x;
        x
    }

    fn fill(&mut self, len: usize) -> Vec<u8> {
        let mut buffer = Vec::with_capacity(len);
        while buffer.len() < len {
            buffer.extend_from_slice(&self.next_u64().to_le_bytes());
        }
        buffer.truncate(len);
        buffer
    }
}
