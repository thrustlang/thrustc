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

use std::fs;
use std::path::{Path, PathBuf};

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

    let ast: thrustc_ast::Ast<'_> = match thrustc_fuzz::dumps::reconstruct_ast(&generator, &data) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    };

    if stable_mode && thrustc_fuzz::dumps::contains_unstable_ast(&ast) {
        eprintln!("AST contains unstable constructs; skipping under --stable.");
        std::process::exit(1);
    }

    emit_llvm_ir(&ast, &crash_file);
}

fn emit_llvm_ir(ast: &thrustc_ast::Ast, crash_file: &Path) {
    let Some(ir) = thrustc_fuzz::dumps::emit_llvm_ir_core(ast) else {
        eprintln!("Semantic analysis did not pass; no LLVM IR was generated.");
        return;
    };

    let out_dir: PathBuf = PathBuf::from("fuzz/llvm_ir_dumps");
    fs::create_dir_all(&out_dir).unwrap();

    let name: String = crash_file
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let out_path: PathBuf = out_dir.join(format!("{name}.ll"));

    fs::write(&out_path, ir).unwrap();

    println!("LLVM IR dumped to: {}", out_path.display());
}
