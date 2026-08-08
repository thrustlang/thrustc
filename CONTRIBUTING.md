<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

Contributing to the Thrust Compiler (`thrustc`). If you are reading this, you probably hit a bug, or you want to add something, or you are just curious how a compiler with no build system test suite survives. Either way, welcome.

This guide is long on purpose. It covers how the repo is laid out, how the code is written, and how to get a change from your head into a pull request without tripping over conventions you did not know existed. Skim it once, then come back to the sections you need.

## A quick word on how this project is run

Thrust is written and reviewed by people who want to understand every line that goes in. See the note at the bottom of `README.md`:

> Agentic AI: No, I don't use it and I never will. This compiler will always have code analyzed, processed, and studied by a human.

What that means in practice: do not submit AI-generated code as if you wrote it, and do not expect an automated agent to review or merge your work. You are responsible for understanding the code you propose, line by line, and for explaining it if asked. Read the [Code of Conduct](CODE_OF_CONDUCT.md) before anything else.

## Before you start

You need a machine that can build the project. There is no way around the LLVM part.

- Rust 1.85 or newer, edition 2024. The repo pins `stable` at the root (`rust-toolchain.toml`); the `fuzz/` workspace pins `nightly` because cargo-fuzz needs it.
- LLVM 17. The project does not use system LLVM directly. You build it once with the [compiler-dependency-builder](https://github.com/thrustlang/compiler-dependency-builder), following the README instructions under "From Scratch".
- The cargo tool deps: `sccache`, `panic-analyzer`, `git-cliff`. Install them with `scripts/cargo-dependencies.sh` (or the `.ps1`, `.fish`, `.bat` twins — every script in this repo ships all four flavors).

Then:

```console
$ cargo build --release
$ ./target/release/thrustc --help
```

The binary is a thin `main` that parses the command line and hands off to `thrustc_core` (`thrustc/src/main.rs`). If `--help` prints, you are ready to work.

## Where things live

Read `PROJECT_STRUCTURE.md` first. It walks the whole pipeline with a diagram: reader → lexer → preprocessor → parser → AST verification → semantic analysis (scoper, typechecker, analyzer, linter, attributes) → optional MIR → LLVM codegen → emission and linking.

The short version, by crate:

- `thrustc` — the binary entry point, nothing else.
- `thrustc_cli`, `thrustc_options` — argument parsing and configuration.
- `thrustc_core` — the driver. Orchestrates the pipeline through stage modules: `cleaner`, `starter`, `interrupt`, `finisher`, `validate`, plus `emitters/` and `printers/`.
- `thrustc_lexer`, `thrustc_reader`, `thrustc_token`, `thrustc_token_type`, `thrustc_span`, `thrustc_preprocessor`, `thrustc_parser` (+ `thrustc_parser_context`, `thrustc_parser_table`, `thrustc_parser_external_table`) — the frontend.
- `thrustc_ast`, `thrustc_ast_external`, `thrustc_ast_verifier`, `thrustc_ast_modificators` — the AST and its checks.
- `thrustc_scoper`, `thrustc_typesystem`, `thrustc_typechecker`, `thrustc_general_analyzer`, `thrustc_linter`, `thrustc_semantic`, `thrustc_entities`, `thrustc_attributes`, `thrustc_attribute_checker`, `thrustc_constants`, `thrustc_directive`, `thrustc_mir` — semantic analysis and the middle end.
- `thrustc_llvm_*` — the backend: codegen, ABI (System V, NVIDIA CUDA), attributes, call conventions, target triples, linker driver.
- `thrustc_backends`, `thrustc_heap_allocator`, `thrustc_abi`, `thrustc_utils`, `thrustc_errors`, `thrustc_diagnostician`, `thrustc_logging` — shared infrastructure.
- `crates/llvm/` — vendored, patched bindings (`llvm-sys`, `inkwell`, `clang`, `clang-sys`). You almost never touch these.
- `fuzz/` — the fuzzing suite. Separate cargo workspace, excluded from the main one (`Cargo.toml:54-56`).
- `scripts/`, `.github/workflows/`, `changelogs/`, `showcase/`, `tests/`, `highlighting/` — tooling, CI, release artifacts, examples, and editor support.

The `tests/` folder is git-ignored scratch space. Do not commit files there.

## Where to start

If you want a first change that you can actually finish, in increasing order of difficulty:

1. **Fix an issue from the fuzz backlog.** The continuous fuzzer archives crashes under `fuzz/backlog/` with the AST dump and the LLVM IR. Pick a target, reproduce with `cargo fuzz-reproduce-case <target> <input.bin>`, find the faulty crate, fix it, then mark it done with `cargo fuzz-backlog fixed <target> <issue-id>`. See `COMPILER_FUZZING.md` and `fuzz/COMPILER_CONTINUOUS_FUZZING.md`.
2. **Add or improve a diagnostic.** New error and warning codes are small, self-contained changes. The codes live in `thrustc_errors`, the formatting in `thrustc_diagnostician`. Details in the [diagnostics section](#diagnostics) below.
3. **Extend an AST trait.** Need a predicate or accessor on all AST nodes? Add a trait in `thrustc_ast/src/traits.rs` and implement it in `getters.rs` or `impls/mod.rs`. There is a mechanical pattern for it; see [AST extension traits](#ast-extension-traits).
4. **Touch the lexer.** `thrustc_lexer` is small and self-contained (five files). Good way to learn the frontend style without the parser's complexity.
5. **Take on a parser or codegen task.** These are the hard ones. Do these only after you have a feel for the conventions.

## The license header

Every `.rs` file starts with the same GPL-3.0 block comment, lines 1-18, followed by one blank line. Copy it from an existing file rather than retyping it:

```rust
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
```

A few files skip it (a handful of `lib.rs` files), but new files should include it. If the year in the header is out of date, `scripts/license_updater.py` fixes the whole tree; do not hand-edit years file by file.

## How the code is written

These are not suggestions. A PR that ignores them gets sent back.

### File and module layout

- One crate per folder, named `thrustc_<name>`. Modules are declared in `lib.rs` right after the `use` statements — `mod foo;` for private, `pub mod foo;` for public (`thrustc_core/src/lib.rs:20-30`).
- Split by concern, one concern per file. The lexer has `identifier.rs`, `number.rs`, `string.rs`, `character.rs`, and `lex.rs` dispatches between them (`thrustc_lexer/src/lex.rs:107-114`).
- Use subdirectories when a group gets big, each with its own `mod.rs`. The parser is the example: `expressions/`, `expressions/precedences/`, `statements/`, `toplevel/` (`thrustc_parser/src/lib.rs:34-43`).

### Style

- 4 spaces, no tabs. Allman braces — opening brace on its own line for `fn`, `impl`, `struct`, `enum`, and match arm blocks.
- `snake_case` for functions and variables, `CamelCase` for types, `SCREAMING_SNAKE_CASE` for constants (`PREALLOCATED_TOKENS_CAPACITY`, `thrustc_lexer/src/lib.rs:31`).
- Accessors are named `get_xxx` and return by reference (`get_any_type`, `get_span`). Predicates are `is_xxx` and return `bool`, implemented with `matches!` where possible.
- Annotate local bindings with their type. The codebase does `let span: Span = ...;` everywhere (`thrustc_parser/src/toplevel/global_function.rs:51`). Follow it.
- Use `Self` in constructors and struct literals (`thrustc_lexer/src/lib.rs:58`).
- Mark small accessors `#[inline]` or `#[inline(always)]`. There are ~160 of them; yours will be one more.
- No doc comments (`///`, `//!`). Documentation lives in the `*.md` guides, not in the code. Plain `//` comments are sparse and used mostly as section markers inside long enums and matches (`thrustc_ast/src/lib.rs:188`, `// Loops`). A brief comment explaining a non-obvious choice is welcome; commenting every line is not.

### No external error crates

The project does not use `thiserror` or `anyhow`, and there are no custom macros. Error handling is a single ADT and a dispatcher:

- `CompilationIssue` in `thrustc_errors/src/lib.rs:25-47` is the one error type. Variants carry data positionally: `Error(CompilationIssueCode, help, message, Option<note>, span)`.
- Construct errors inline where they happen. See `thrustc_parser/src/lib.rs:303-309` and `thrustc_lexer/src/lex.rs:57-63`.
- Dispatch through `Diagnostician::dispatch_diagnostic(&mut self, &CompilationIssue, LoggingType)` (`thrustc_diagnostician/src/lib.rs:81-202`).
- Fatal internal bugs go through `abort_compilation` (frontend: `thrustc_parser/src/abort.rs`, backend: `thrustc_llvm_abi/src/abort.rs`), which dispatches a `FrontendBug`/`BackendBug` and exits with `thrustc_constants::FAILURE_CODE`.
- Pipeline-boundary crates often return `Result<T, ()>` — the coarse failed/ok channel — with `#![allow(clippy::result_unit_err)]` at the top of the file.

### AST extension traits

To add a behavior to every AST node you do two things:

1. Declare the trait in `thrustc_ast/src/traits.rs` (`AstGetType`, `AstStandardExtensions`, `AstExpressionExtensions`, `AstCodeLocation`).
2. Implement it for `Ast<'_>`:
   - a big exhaustive `match` over every variant, in `thrustc_ast/src/getters.rs:30-229`, for things that reach into variant fields;
   - or a series of `matches!` one-liners, in `thrustc_ast/src/impls/mod.rs:46-50`, for boolean predicates.

Recursive predicates call the trait method on their children, like `has_terminator` walking `If`/`Elif`/`Else` blocks (`thrustc_ast/src/impls/mod.rs:253-304`).

### AST nodes and serde / fuzzing

`Ast` is one big enum with struct-variants (`thrustc_ast/src/lib.rs:49-537`). Every node derives `Debug, Clone, Serialize`, and the AST crates add `#[cfg_attr(feature = "fuzz", derive(Arbitrary))]` (`thrustc_ast/src/lib.rs:49-50`, `thrustc_span/src/lib.rs:24-25`).

If you add an AST variant, you must also:

- keep it serializable (`Serialize`) and `Arbitrary` behind the `fuzz` feature;
- handle it in the accessor `match`es in `getters.rs` (the compiler will not compile until you do — that is the point);
- if it matters for fuzzing, cover it in the scoped generators under `fuzz/src/` (`llvm_codegen_local.rs`, `llvm_codegen_local_loops.rs`, `llvm_codegen_top_level.rs`).

### Lints

The project's lint policy is deliberately permissive, configured once in `.cargo/config.toml:4-10,48-52`: `dead_code` and `missing_abi` are allowed, and the clippy `style`, `complexity`, and `pedantic` groups are allowed. Individual files opt out of specific lints with an inner attribute right after the license header, e.g. `#![allow(clippy::result_unit_err)]` (`thrustc_lexer/src/lib.rs:20`).

Do not widen these global allowances. If you hit a lint, silence it locally in your file with a justification, or fix the code. Do not add a `rustfmt.toml` or a per-crate `[lints]` section without talking about it first.

## A walkthrough: adding a diagnostic

Say you want a new error. The concrete steps:

1. **Define the code.** Add a variant to `CompilationIssueCode` in `thrustc_errors/src/lib.rs:118-176`. Codes are `E0001`..`E0040` for errors and `W0001`..`W0018` for warnings, each with a trailing `// comment` saying what it means.
2. **Give it a title.** The `to_title()` method (`thrustc_errors/src/lib.rs:179-328`) maps the code to the colored string printed in the header.
3. **Emit it.** Build a `CompilationIssue::Error(code, help, message, None, span)` at the failure site (parser, semantic, wherever), and route it to `dispatch_diagnostic` following the pattern in `thrustc_parser/src/lib.rs:160-178` (`verify`).
4. **Document it.** Add an example to `COMPILER_DIAGNOSTICS.md`. Screenshots of terminal output live in `assets/examples/diagnostics/`.

Warnings work the same way with `CompilationIssue::Warning(code, message, span)`. The linter (`thrustc_linter`) is a good place to add style warnings, and the linter codes (`W0005`, local not used, etc.) show the kind of thing that gets a code.

## Verifying your change

Be honest with yourself about this part: the project does not have a test suite in the usual sense. There is exactly one `#[test]` in the whole workspace (`thrustc_llvm_linker_driver/src/lib.rs:464`). CI builds the compiler on four platforms and cuts releases; it does not run tests, clippy, or fmt. The testing burden sits on you and on the fuzzers.

What you should do before opening a PR:

- `cargo build --release` and fix every warning you introduced.
- Compile and run at least one real program end to end. `showcase/` has examples; `tests/` has scratch files if you want a quick loop.
- If you touched the frontend, run the fuzzers that cover it. `cargo fuzz-lexer`, `cargo fuzz-pipeline-stable` for a bounded sanity check, and the continuous supervisor if you have time: `cargo fuzz-continuous-<target>-<mode>` with a `--max-time`. The fuzz docs (`COMPILER_FUZZING.md`, `fuzz/COMPILER_CONTINUOUS_FUZZING.md`) explain the workflow.
- If you fixed a bug the fuzzer found, reproduce the old artifact first (`cargo fuzz-reproduce-case <target> <input.bin>`) to confirm it crashed before, then confirm it no longer crashes after, and finish by marking the issue `fixed` in the backlog.

If you feel the absence of a test suite is a problem, that is a real gap. Adding tests is one of the most useful contributions you can make. There is room to build an integration harness on top of `tests/` or the fuzz corpus — propose it.

## Commits and pull requests

Commit titles follow `COMMIT_CONVENTIONS.md`. The shape is `feat(scope)` or `fix(scope)` (or `(feat(...), fix(...))` when combined), where scope is one of:

- `llvm_backend` — the LLVM backend.
- `llvm_linker_driver` — the linker driver invocation.
- `gcc_backend` — reserved for the (not yet existing) GCC backend.
- `frontend` — AST, lexer, parser, typechecker, and friends.
- `fuzzing` — the `fuzz/` suite, corpora, fuzz targets.
- `project` — Cargo, Rust toolchain, GitHub Actions, new crates.
- `project-visual` — general and visual project changes (README, assets, editor highlighting, banners).
- `doc` — the compiler documentation and guides (CONTRIBUTING, the `COMPILER_*.md` files).

Title first, then a short, specific description. "feat(frontend) Adding support for X" reads better than "Update parser".

Branch off `main`, open the PR, and fill in the description with what changed and how you verified it. There is no PR template in the repo; keep the description honest and complete.

## A checklist before you push

- New `.rs` file has the GPL header.
- New AST variant is `Serialize` + `Arbitrary` (fuzz feature), handled in `getters.rs`, and covered by the fuzz generators if relevant.
- New diagnostic code has a variant, a `to_title()` entry, and a doc example.
- `cargo build --release` is clean.
- You ran the relevant fuzzer or a real program, and you can say what you ran in the PR.
- Commit title follows `COMMIT_CONVENTIONS.md` and the scope is right.
- You understand the code you are submitting, line by line, and you can defend it in review.

## Other guides

- `PROJECT_STRUCTURE.md` — the full architecture and pipeline.
- `CLI.md` — commands and flags.
- `COMPILER_DIAGNOSTICS.md` — diagnostic examples and codes.
- `COMPILER_FUZZING.md` and `fuzz/COMPILER_CONTINUOUS_FUZZING.md` — fuzzing, one-shot and continuous.
- `COMMIT_CONVENTIONS.md` — commit titles.
- `COMPILER_RELEASING.md` — how releases are cut (mostly scripts, mostly not your concern unless you maintain releases).
- `CODE_OF_CONDUCT.md` — the ground rules.

If a guide is missing something or wrong, fix the guide. Documentation is `project-visual` work and it counts.
