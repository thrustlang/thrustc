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

// SPDX-License-Identifier: Apache-2.0

use std::path::{Path, PathBuf};

use glob::Pattern;

use crate::common;

#[path = "logging.rs"]
pub mod logging;
#[path = "utils.rs"]
pub mod utils;

//================================================
// Searching
//================================================

/// Clang static libraries required to link to `libclang` 3.5 and later.
const CLANG_LIBRARIES: &[&str] = &[
    "clang",
    "clangAST",
    "clangAnalysis",
    "clangBasic",
    "clangDriver",
    "clangEdit",
    "clangFrontend",
    "clangIndex",
    "clangLex",
    "clangParse",
    "clangRewrite",
    "clangSema",
    "clangSerialization",
];

//================================================
// Linking
//================================================

/// Finds and links to `libclang` static libraries.
pub fn link() {
    let cep: common::CommandErrorPrinter = common::CommandErrorPrinter::default();

    let thrustlang_libclang_directory: PathBuf = self::utils::get_libclang_build_path().join("lib");

    if !thrustlang_libclang_directory.exists() {
        panic!("LibClang libraries could not be found on '.thrustlang/backends/llvm/build/lib'. You should execute the 'compiler-dependency-builder' (https://github.com/thrustlang/compiler-dependency-builder) before compile the compiler.")
    }

    println!(
        "cargo:rustc-link-search=native={}",
        thrustlang_libclang_directory.display()
    );

    let clang_libraries: Vec<String> = self::get_clang_libraries(&thrustlang_libclang_directory);

    for library in clang_libraries {
        println!("cargo:rustc-link-lib=static={}", library);
    }

    let mode: Option<String> =
        common::run_llvm_config(&["--shared-mode"]).map(|m| m.trim().to_owned());

    let prefix: &str = if mode.is_some_and(|m| m == "static") {
        "static="
    } else {
        ""
    };

    println!(
        "cargo:rustc-link-search=native={}",
        common::run_llvm_config(&["--libdir"]).unwrap().trim_end()
    );

    let llvm_libraries: Vec<String> = self::get_llvm_libraries();

    for library in llvm_libraries {
        println!("cargo:rustc-link-lib={}{}", prefix, library);
    }

    // Specify required system libraries.
    // MSVC doesn't need this, as it tracks dependencies inside `.lib` files.
    if cfg!(target_os = "freebsd") {
        println!("cargo:rustc-flags=-l ffi -l ncursesw -l c++ -l z");
    } else if cfg!(any(target_os = "haiku", target_os = "linux")) {
        if cfg!(feature = "libcpp") {
            println!("cargo:rustc-flags=-l c++");
        } else {
            println!("cargo:rustc-flags=-l ffi -l ncursesw -l stdc++ -l z");
        }
    } else if cfg!(target_os = "macos") {
        println!("cargo:rustc-flags=-l ffi -l ncurses -l c++ -l z");
    }

    cep.discard();
}

/// Gets the Clang static libraries required to link to `libclang`.
fn get_clang_libraries<P: AsRef<Path>>(directory: P) -> Vec<String> {
    let original_directory: PathBuf = directory.as_ref().to_path_buf();

    let escaped_directory: String = Pattern::escape(original_directory.to_str().unwrap());

    let escaped_path: &Path = Path::new(&escaped_directory);

    // On Windows the C API lives in `libclang.lib` (the static archive produced
    // with `LIBCLANG_BUILD_STATIC=ON` and `LLVM_ENABLE_PIC=OFF`) while the C++
    // libraries are `clang*.lib`, so both patterns must be collected. On Unix a
    // single `libclang*.a` pattern covers the C API and the C++ libraries.
    let patterns: Vec<String> = if cfg!(target_os = "windows") {
        vec![
            escaped_path.join("libclang*.lib").to_str().unwrap().to_owned(),
            escaped_path.join("clang*.lib").to_str().unwrap().to_owned(),
        ]
    } else {
        vec![escaped_path
            .join("libclang*.a")
            .to_str()
            .unwrap()
            .to_owned()]
    };

    let mut collected: Vec<String> = Vec::new();

    for pattern in patterns.iter() {
        if let Ok(libraries) = glob::glob(pattern) {
            for name in libraries
                .filter_map(|l| l.ok())
                .filter_map(|l| self::get_library_name(&l))
            {
                if !collected.contains(&name) {
                    collected.push(name);
                }
            }
        }
    }

    if !collected.is_empty() {
        return collected;
    }

    let fallback: Vec<String> = CLANG_LIBRARIES
        .iter()
        .filter_map(|l| {
            if cfg!(target_os = "windows") {
                if original_directory.join(format!("{}.lib", l)).exists() {
                    return Some((*l).to_string());
                }

                if original_directory.join(format!("lib{}.lib", l)).exists() {
                    return Some(format!("lib{}", l));
                }

                None
            } else if original_directory.join(format!("lib{}.a", l)).exists() {
                Some((*l).to_string())
            } else {
                None
            }
        })
        .collect();

    fallback
}

fn get_llvm_libraries() -> Vec<String> {
    common::run_llvm_config(&["--libs", "--link-static"])
        .unwrap()
        .split_whitespace()
        .filter_map(|p| {
            if let Some(path) = p.strip_prefix("-l") {
                Some(path.into())
            } else {
                self::get_library_name(Path::new(p))
            }
        })
        .collect()
}

/// Gets the name of an LLVM or Clang static library from a path.
fn get_library_name(path: &Path) -> Option<String> {
    path.file_stem().map(|p| {
        let string = p.to_string_lossy();

        if !cfg!(target_os = "windows") {
            if let Some(name) = string.strip_prefix("lib") {
                return name.to_owned();
            }
        }

        string.to_string()
    })
}
