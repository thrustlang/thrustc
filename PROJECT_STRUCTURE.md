<img src="https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt="logo" style="width: 80%; height: 80%;">

# Thrust Compiler: Project Structure

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

`thrustc` is a modular compiler for the Thrust programming language, a general-purpose, statically typed systems language focused on clear and fast code.

The frontend uses a handwritten recursive descent parser. The backend generates code through the LLVM C API (via `llvm-sys` and `inkwell`) with custom abstractions and techniques to reach the LLVM C++ API indirectly.

---

## Workspace Crates (`thrustc_*`)

### Entry Point and CLI

- **`thrustc`**  
  Main binary entry point. Delegates everything to `thrustc_core`.

- **`thrustc_cli`**  
  Command-line interface helpers and argument parsing utilities.

- **`thrustc_lsp`**  
  Language server binary for editor integrations. It communicates through the Language Server Protocol over stdio and ships with the compiler releases.

- **`thrustc_options`**  
  Compiler configuration and command-line options: backends, optimization levels, debug information, linkage, and target settings.

### Core Infrastructure

- **`thrustc_core`**  
  Central driver of the compiler. Manages the compilation pipeline with lifecycle stages for start, clean, finish, and validation. Contains emission for AST, LLVM IR, tokens, assembler, bitcode, and object files, plus printing, linkage, and interrupt handling.

- **`thrustc_diagnostician`**  
  Diagnostic and error reporting system with source positions and pretty-printed messages.

- **`thrustc_errors`**  
  Internal error types and utilities. Derive support lives in `thrustc_errors_macros`.

- **`thrustc_errors_macros`**  
  Procedural macros supporting `thrustc_errors`.

- **`thrustc_logging`**  
  Structured logging for compiler internals.

- **`thrustc_utils`**  
  General shared utilities used across crates.

### Frontend: Lexing and Parsing

- **`thrustc_lexer`**  
  Handwritten lexer supporting identifiers, numbers, strings, characters, and language-specific rules.

- **`thrustc_reader`**  
  Source file reading and input management.

- **`thrustc_token`** and **`thrustc_token_type`**  
  Token definitions, supporting traits, and type hierarchies.

- **`thrustc_code_location`**  
  Source span and location tracking used throughout the compiler.

- **`thrustc_preprocessor`**  
  Preprocessor for modules, imports, and early processing of source code. Handles high-level and submodule parsing, module tables, signatures, the standard library resolver, and compile-time conditionals.

- **`thrustc_parser`**  
  Handwritten recursive descent parser with layered precedence climbing. Parses expressions with precedence levels, statements, top-level declarations, attributes, modificators, and imports.

- **`thrustc_parser_context`**  
  Context state maintained by the parser during recursive descent.

- **`thrustc_parser_table`** and **`thrustc_parser_external_table`**  
  Symbol and declaration tables for fast lookups during parsing and external access.

### Frontend: AST

- **`thrustc_ast`**  
  Abstract Syntax Tree definitions, node types, visitor traits, metadata, builtins, and logic data.

- **`thrustc_ast_external`**  
  Thin re-export layer that exposes selected AST types to other crates without circular dependencies.

- **`thrustc_ast_verifier`**  
  Structural and consistency verification of the AST.

- **`thrustc_ast_modificators`**  
  Handling of language modifiers (visibility, mutability, and others) with traits and implementations.

### Builtins

- **`thrustc_builtins`**  
  Compile-time builtin system used by the parser and preprocessor. Provides a registry with builtins for size, alignment, layout, target, predicates, and location, plus value plumbing, type info, and traits. Compile-time folding itself lives in `thrustc_compile_time`.

- **`thrustc_compile_time`**  
  Shared compile-time value evaluation used by the frontend and the C import pipeline.

### Semantic Analysis and Middle-end

- **`thrustc_scoper`**  
  Scope analysis and resolution with context, table, and checks.

- **`thrustc_typesystem`**  
  Complete type system: arrays, fixed arrays, pointers, structures, function references, casting, inference, layout, modifiers, precedence, location, indexation, and dereference.

- **`thrustc_typechecker`**  
  Main type checker with type inference for expressions, operations, top-level declarations, and metadata.

- **`thrustc_generics`**  
  Generic type resolution and substitution over parameters, types, scopes, and pending instantiations.

- **`thrustc_generics_monomorphization`**  
  Parser-level monomorphization driver for generics: resolves generic calls and instantiates concrete templates during parsing.

- **`thrustc_import_synthesis`**  
  Import resolution and synthesis for functions, intrinsics, constants, statics, structs, enums and custom types, including collision handling. Thrust-side counterpart of `thrustc_c_import_synthesis`.

### C Interop: Import and Transpilation

- **`thrustc_c_import_synthesis`**  
  C declaration import pipeline. Thrust-side counterpart handling for C functions, types, records, and macros.

- **`thrustc_c_transpiler`**  
  C to Thrust transpiler behind `--import-c`, `--translate-c-*`, and `--emit-c-bindings-*`.

- **`thrustc_general_analyzer`**  
  General static analysis with context and expression visitors.

- **`thrustc_linter`**  
  Static linter for style and best-practice warnings.

- **`thrustc_entities`**  
  Shared entities consumed by the analyzer, parser, typechecker, and linter.

- **`thrustc_semantic_analysis`**  
  General semantic analysis layer.

- **`thrustc_attributes`** and **`thrustc_attribute_checker`**  
  Handling and validation of language and LLVM attributes (assembler, call conventions, linkage).

- **`thrustc_constants`**  
  Language-level constant definitions.

- **`thrustc_directive`**  
  Compiler directive handling.

- **`thrustc_atomic_ordering`** and **`thrustc_thread_mode`**  
  Shared atomic ordering and thread-local mode definitions, including LLVM conversions.

### LLVM Backend

- **`thrustc_llvm_codegen`**  
  Primary code generation backend over the LLVM C API with custom wrappers. Covers expressions, statements, top-level codegen, heap, stack and static memory, JIT, optimization, debug info, type generation, type casting, and attribute building. Atomic and variadic lowering live in the sibling crates below.

- **`thrustc_llvm_codegen_variatic`**  
  Variadic function code generation used by `thrustc_llvm_codegen`.

- **`thrustc_llvm_codegen_atomic`**  
  Atomic operation code generation used by `thrustc_llvm_codegen`.

- **`thrustc_llvm_target_triple`**  
  Helper for working with LLVM target triples and architecture queries.

- **`thrustc_llvm_attribute_architecture`**  
  Applies target architecture calling conventions to LLVM functions and call sites after code generation.

- **`thrustc_llvm_attributes`**  
  Mapping and emission of LLVM-specific attributes.

- **`thrustc_llvm_call_conventions`** and **`thrustc_llvm_call_conventions_checker`**  
  Support and validation of calling conventions.

- **`thrustc_llvm_compiler_intrinsic_checker`**  
  Validation of LLVM intrinsic usage.

- **`thrustc_llvm_abi`**  
  Core ABI handling abstractions.

- **`thrustc_llvm_system_v_abi`**  
  System V ABI implementation (x86-64 Linux and macOS).

- **`thrustc_llvm_nvidia_cuda_abi`**  
  NVIDIA CUDA ABI implementation.

- **`thrustc_llvm_webassembly_abi`**  
  WebAssembly Basic C ABI implementation for arguments and return values.

- **`thrustc_llvm_abi_representation`**  
  ABI data representation utilities.

- **`thrustc_llvm_linker_driver`**  
  Linker driver integration with platform-specific linkers.

### Backend Abstraction

- **`thrustc_backends`**  
  Backend abstraction layer (currently focused on LLVM). Includes CPU, debug, info, JIT, linker, passes, and target modules.

### Support

- **`thrustc_abi`**  
  ABI type representation and utilities.

- **`thrustc_heap_allocator`**  
  Custom heap allocation logic used by the compiler itself.

### Standard Library

- **`thrustc_std`**  
  Standard library crate. Embeds versioned Thrust sources and manages installation into the user home directory with version-aware resolution.

- **`std/`**  
  Versioned standard library sources. Each version directory contains the modules for that release, plus C interop helpers. Consumed by `thrustc_std` and the preprocessor standard library resolver.

---

## LLVM Vendor Crates (`crates/llvm/`)

Vendored forks of LLVM Rust bindings, patched for thrustc compatibility:

- **`crates/llvm/17/llvm-sys`**: Raw FFI bindings to the LLVM C API (LLVM 17).
- **`crates/llvm/17/clang-sys`**: Raw FFI bindings to the Clang C API.
- **`crates/llvm/inkwell`**: Safe Rust wrappers over `llvm-sys` with additional context and builder abstractions.
- **`crates/llvm/clang`**: Safe Rust wrappers over `clang-sys`.
- **`crates/llvm/lld-wrapper`**: LLD linker wrapper.

---

## Fuzzing (`fuzz/`)

A fuzzing infrastructure using `cargo-fuzz`:

- **`fuzz_targets/`**: Fuzz targets for the lexer, LLVM codegen (local and top-level), and the full pipeline.
- **`src/`**: Shared fuzzing helpers: corpus generation (`gen_local_common.rs`), AST and IR dumps (`dumps.rs`), backlog tracking (`backlog.rs`), and target-specific harnesses (lexer, LLVM codegen local, loops, and top-level).
- **`fuzz_pipeline/`**: More than 1984 valid AST corpus files used for pipeline regression fuzzing.
- **`fuzz_continuous/`**: Continuous fuzzing setup (see `COMPILER_CONTINUOUS_FUZZING.md`).
- **`corpus_stable/`**, **`corpus_universal/`**, **`corpus_unstable/`**: Categorized fuzzing corpora.
- **`fuzz_reproduce_logs/`**: Logs from reproduced fuzzing failures.
- **`ast_dumps/`**: AST dumps generated during fuzzing.
- **`llvm_ir_dumps/`**: LLVM IR dumps captured from crashing inputs.
- **`artifacts/`**: Crashes found by each fuzz target.
- **`backlog/`**: Pending fuzzing issues per target.
- **`scripts/`**: Fuzzing directory setup scripts (`.sh`, `.bat`, `.ps1`, `.fish`).
- Dictionary files (`thrust-stable.dict`, `thrust-unstable.dict`) for coverage-guided fuzzing.

---

## Editor Support and Highlighting (`highlighting/`)

- **Sublime Text**: `highlighting/thrust.tmLanguage`, `highlighting/thrust.tmLanguage.json`, `highlighting/llvm.sublime-syntax` for Thrust and LLVM IR syntax.
- **VS Code**: `highlighting/vscode/thrust-vscode/` extension and packaged `highlighting/vscode/thrustlang-highlighting-0.2.1.vsix` for Thrust language support.
- **Neovim and Vim**: `highlighting/neovim/thrust.vim` syntax file, `highlighting/neovim/thrust.nvim/` plugin package, plus `highlighting/neovim/thrust.nvim.zip`.
- **Theme**: `highlighting/One Dark.tmTheme` compatible theme.
- **Assets**: `highlighting/assets/highlighting-example.png`.

## Language Server (`lsp/`)

- **VS Code**: `lsp/vscode/` extension with syntax highlighting, themes, file icons, and `thrustc_lsp` client integration.

---

## CI and CD (`.github/workflows/`)

GitHub Actions workflows for four target platforms (dev tags `thrustc-*-dev-v*.*.*`, release tags `thrustc-*-v*.*.*`):

| Platform | Runner | Dev | Release |
|---|---|---|---|
| `x86_64-linux-ubuntu` | `ubuntu-latest` | Yes | Yes |
| `x86_64-macos` | `macos-15-intel` | Yes | Yes |
| `aarch64-macos` | `macos-latest` | Yes | Yes |
| `x86_64-windows-msvc` | `windows-latest` | Yes | Yes |

Dev workflows publish prereleases from `target/debug/` plus `target/release-stripped/*-stripped`; release workflows publish stable binaries from `target/release/` plus stripped variants. All attach `thrustlang-vscode-*.vsix`.

---

## Scripts (`scripts/`)

Cross-platform automation scripts. Each base exists in four flavors (`.sh`, `.bat`, `.ps1`, `.fish`), except `license_updater.py` which is Python-only:

- **`cargo-dependencies.*`**: Install project cargo tools.
- **`deploy-code-docs.*`**: Deploy compiler documentation.
- **`deploy-version.*`**: Version deployment automation.
- **`embed-std.*`**: Embed standard library sources.
- **`license_updater.py`**: Automated license header updates across source files.
- **`release-changelog.*`**: Generate and deploy changelogs for releases.
- **`tag-manager.*`**: Git tag management helpers.

---

## Changelogs (`changelogs/`)

Per-platform changelogs for release versions. Each release is a directory with a `README.md` (41 releases), e.g. `changelogs/thrustc-x86_64-linux-ubuntu-v0.2.3/README.md`:

- `thrustc-x86_64-linux-ubuntu-v*`
- `thrustc-x86_64-macos-v*`
- `thrustc-aarch64-macos-v*`
- `thrustc-x86_64-windows-msvc-v*`

---

## Assets and Examples (`assets/`)

- **`assets/examples/diagnostics/`**: Example diagnostic output files.

---

## References

- **`LINKER_REFERENCE.md`**: Linker documentation.
- **`LLVM_REFERENCE.md`**: LLVM integration and ABI documentation.

---

## Showcase (`showcase/`)

Example Thrust projects demonstrating language capabilities:

- **`showcase/Algorithms/`**: Algorithm implementations.
- **`showcase/Cuda/`**: CUDA integration examples.
- **`showcase/HttpServer/`**: HTTP server implementation.
- **`showcase/OpenGL/`**: OpenGL graphics examples.

## Tests (`tests/`)

Test suites organized by feature area, plus a test runner and documentation. `tests/std/` mirrors standard library modules under test.

---

## Compiler Pipeline

```
Source File (.thrust)
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 1. Reader          (thrustc_reader)             │
│    - Reads source files into memory             │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 2. Lexer           (thrustc_lexer)              │
│    - Tokenizes source into tokens               │
│    - Handles identifiers, numbers, strings,     │
│      characters, and language-specific tokens   │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 3. Preprocessor   (thrustc_preprocessor)        │
│    - Module resolution and import handling      │
│    - Standard library resolution (thrustc_std)  │
│    - Compile-time conditionals                  │
│      (thrustc_builtins)                         │
│    - High-level and submodule parsing           │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 4. Parser          (thrustc_parser)             │
│    - Handwritten recursive descent parser       │
│    - Precedence climbing for expressions        │
│    - Builds AST nodes (thrustc_ast)             │
│    - Uses parser context, tables, and AST       │
│    - Token definitions (thrustc_token,          │
│      thrustc_token_type)                        │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 5. AST Verification (thrustc_ast_verifier)      │
│    - Structural and consistency checks          │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 6. Semantic Analysis                            │
│                                                 │
│    a. Scoper        (thrustc_scoper)            │
│       - Scope resolution and binding            │
│                                                 │
│    b. Type Checker  (thrustc_typechecker)       │
│       + Type System (thrustc_typesystem)        │
│       - Type inference, checking, and layout    │
│                                                 │
│    c. Analyzer      (thrustc_general_analyzer)  │
│       - General static analysis                 │
│                                                 │
│    d. Linter        (thrustc_linter)            │
│       - Warnings                                │
│                                                 │
│    e. Attributes    (thrustc_attributes,        │
│       thrustc_attribute_checker)                │
│       - Language and LLVM attribute validation  │
│                                                 │
│    f. Semantic      (thrustc_semantic_analysis) │
│       - General semantic analysis               │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 7. Shared IR data                               │
│    (thrustc_atomic_ordering, thrustc_thread_mode,│
│     thrustc_abi)                                 │
│    - Atomic operations, thread mode, and ABI     │
│      type representation                         │
│    - LLVM conversion helpers                     │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 8. LLVM Codegen    (thrustc_llvm_codegen)       │
│    + LLVM vendor crates (llvm-sys, inkwell)     │
│    + Variadic codegen                           │
│      (thrustc_llvm_codegen_variatic)            │
│    + Atomic codegen                             │
│      (thrustc_llvm_codegen_atomic)              │
│    - Expression, statement, toplevel codegen    │
│    - Heap, stack, and static memory management  │
│    - JIT compilation and optimization           │
│    - Debug info and metadata                    │
│                                                 │
│    ABI handling:                                │
│    - thrustc_llvm_abi                           │
│    - thrustc_llvm_system_v_abi                  │
│    - thrustc_llvm_nvidia_cuda_abi               │
│    - thrustc_llvm_webassembly_abi                │
│    - thrustc_llvm_abi_representation            │
│                                                 │
│    Target and conventions:                      │
│    - thrustc_llvm_target_triple                 │
│    - thrustc_llvm_attribute_architecture        │
│    - thrustc_llvm_attributes                    │
│    - thrustc_llvm_call_conventions              │
│    - thrustc_llvm_call_conventions_checker      │
│    - thrustc_llvm_compiler_intrinsic_checker    │
│                                                 │
│    Linker driver:                               │
│    - thrustc_llvm_linker_driver                 │
└─────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────┐
│ 9. Emission and Output                          │
│    (thrustc_core with emission and printing)     │
│                                                 │
│    Output formats:                              │
│    • Object file                                │
│    • LLVM IR                                   │
│    • LLVM Bitcode                              │
│    • Assembly                                  │
│    • AST dump                                 │
│    • Token dump                               │
│    • JIT execution                             │
└─────────────────────────────────────────────────┘
    │
    ▼
  Binary, Library, or Executable
```

---

## Supported Compiler Host Platforms

Matches `README.md` (`Platform | Architecture | Release tag`), ordered Linux, Windows, macOS ARM, macOS Intel:

| Platform | Architecture | Release tag |
|---|---|---|
| Linux Ubuntu GNU | x64 `x86_64-unknown-linux-gnu` | `thrustc-x86_64-linux-ubuntu-v*.*.*` |
| Windows MSVC | x64 `x86_64-pc-windows-msvc` | `thrustc-x86_64-windows-msvc-v*.*.*` |
| macOS Apple Silicon | aarch64 `aarch64-apple-darwin` | `thrustc-aarch64-macos-v*.*.*` |
| macOS Intel | x64 `x86_64-apple-darwin` | `thrustc-x86_64-macos-v*.*.*` |

---
