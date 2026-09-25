<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

This is a short guide to the commit conventions used for the Thrust Compiler (**thrustc**).

### Title

The title should be clear. It may use technical terms when needed. A good commit title follows this syntax:

#### Title for features

Use this syntax:

`feat(...)`

Valid locations:

- `llvm_backend` Any change related to the LLVM backend.
- `llvm_linker_driver` Any change related to the LLVM Linker Driver invocation.
- `gcc_backend` Any change related to the GNU Compiler Collection (GCC) backend.
- `frontend` Any change related to the Abstract Syntax Tree (AST), Lexer, Parser or TypeChecker.
- `project-visual` General or visual changes to the compiler project on Github (examples: README.md, assets, highlighting, banners).
- `project` Changes to Cargo, the Rust toolchain, the Github repository, Github actions, or the creation of a new part of the compiler (Cargo workspaces).
- `fuzzing` Any change related to the compiler fuzzing suite (`fuzz/`), its corpora and fuzz targets.
- `doc` Any change related to the compiler documentation and guides in the repository (examples: CONTRIBUTING.md, COMPILER_DIAGNOSTICS.md, COMPILER_FUZZING.md).
- `abi` Any change related to the Application Binary Interface (ABI) representation, lowering, calling conventions or target ABI handling.
- `preprocessador` Any change related to the preprocessor and module or import resolution (`thrustc_preprocessor`).
- `std` Any change related to the standard library (`std/`).
- `lsp` Any change related to the language server and editor integration (`thrustc_lsp`, `lsp/`).

Example:

`feat(llvm_backend)` Adding support for TLS thread priority.

#### Title for fixes

Use this syntax:

`fix(...)`

Valid locations:

- `llvm_backend` Any change related to the LLVM backend.
- `llvm_linker_driver` Any change related to the LLVM Linker Driver invocation.
- `gcc_backend` Any change related to the GNU Compiler Collection (GCC) backend.
- `frontend` Any change related to the Abstract Syntax Tree (AST), Lexer, Parser or TypeChecker.
- `project-visual` General or visual changes to the compiler project on Github (examples: README.md, assets, highlighting, banners).
- `fuzzing` Any change related to the compiler fuzzing suite (`fuzz/`), its corpora and fuzz targets.
- `doc` Any change related to the compiler documentation and guides in the repository (examples: CONTRIBUTING.md, COMPILER_DIAGNOSTICS.md, COMPILER_FUZZING.md).
- `abi` Any change related to the Application Binary Interface (ABI) representation, lowering, calling conventions or target ABI handling.
- `preprocessador` Any change related to the preprocessor and module or import resolution (`thrustc_preprocessor`).
- `std` Any change related to the standard library (`std/`).
- `lsp` Any change related to the language server and editor integration (`thrustc_lsp`, `lsp/`).

When you list more than one location, separate them with a comma `,`.

Example:

`fix(frontend)` Fixing several issues on the abstract syntax analyzer.

#### Combinatory title

To write a combined title, use this syntax:

`(feat(...), fix(...))`

- Enclose the whole title in parentheses `()`.
- Separate each feature or fix with a comma `,`.

### Description

Keep the description short and clear. It may use technical terms when needed.
