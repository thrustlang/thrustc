<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

Fuzzing is an important task in order to detect and fix issues could've been found on the compiler, then, you've to select either test with "stable" or "unstable"
features the compiler.

> [!IMPORTANT]
> A **`rustc` nightly** toolchain is required in order to run fuzzing (this project uses `cargo-fuzz`/libFuzzer), even if you only intend to use the auxiliary
> binaries (e.g. `fuzz-dump-ast-top-level`, `fuzz-dump-ast-local`, `fuzz-dump-llvm-ir`, `fuzz-reproduce-case`). Make sure to install it before building or running anything under `fuzz/`, for example:
>
> ```sh
> rustup toolchain install nightly
> ```

## Fuzzing Suite Structure

The fuzzing project lives under `fuzz/` and is structured as follows:

- **`Cargo.toml`**: Workspace manifest defining all fuzz targets and auxiliary binaries.
- **`Cargo.lock`**: Lockfile for the fuzzing workspace.
- **`rust-toolchain.toml`**: Pins the **nightly** toolchain and its cross-compilation targets for all `fuzz/` builds.
- **`fuzz_targets/`**: Fuzzer entry points:
  - `lexer.rs`: Generates arbitrary UTF-8 source code and feeds it to the lexer.
  - `pipeline.rs`: Generates arbitrary ASTs via the `Arbitrary` trait, then runs semantic analysis and AST validation.
  - `llvm_codegen_top_level.rs`: Generates ASTs with manual scoping (delegates to the shared `thrustc_fuzz::llvm_codegen_top_level::gen_root` generator) and runs semantic analysis + full LLVM codegen (top-level configuration).
  - `llvm_codegen_local.rs`: Same as top-level but with different limits (deeper nesting, more statements per block, includes `Write` AST node).
  - `llvm_codegen_local_loops.rs`: A fuzzer focused on loops. It builds ASTs around loops (`for`, `while`, `loop`) and makes loop control statements (`break`, `continue`, `break all`, `continue all`) common, but only inside a loop scope. Loop nesting is limited (`MAX_LOOP_NESTING`) and loop conditions use variables that are already in scope so the loop can finish.
- **`src/`**: Shared library (thrustc_fuzz) with the scoped AST generators and the continuous-fuzzing infrastructure:
  - `lib.rs`: Re-exports the generator and support modules.
  - `llvm_codegen_local.rs`: Scoped AST generator used by the `llvm-codegen-local` target.
  - `llvm_codegen_local_loops.rs`: Scoped AST generator for the `llvm-codegen-local-loops` target, loop oriented (see above).
  - `llvm_codegen_top_level.rs`: Scoped AST generator used by the `llvm-codegen-top-level` target (`gen_root`).
  - `dumps.rs`: Shared AST/IR reconstruction and crash classification. Defines `CRASH_MARKERS`, `classify()`, `reconstruct_ast()`, `ast_dump()`, `emit_llvm_ir()`/`emit_llvm_ir_core()` (which do **not** run `module.verify()`), and `contains_unstable_ast()`.
  - `backlog.rs`: Persistent backlog of every recorded error. Writes the payload files under `fuzz/backlog/<target>/<issue-id>/`, the metadata (`meta.json`), and the cascading registry `fuzz/fuzz_continuous/<target>.log`. Handles status changes (`open`/`ignored`/`fixed`) and content-hash deduplication.
- **`bin/`**: Auxiliary binaries:
  - `dump_ast_top_level.rs`: Reconstructs and prints the AST from a fuzzer crash artifact (uses the `Arbitrary` trait). Matches the `pipeline` fuzzer.
  - `dump_ast_local.rs`: Same but uses the scoped AST generator with controlled depth. Matches the `llvm-codegen-local` fuzzer.
  - `dump_ast_local_loops.rs`: Same as `dump_ast_local` but for the loop oriented AST generator. Matches the `llvm-codegen-local-loops` fuzzer.
  - `dump_llvm_ir.rs`: Reconstructs the AST using a scoped generator, then runs semantic analysis + LLVM codegen and dumps the module IR to `fuzz/llvm_ir_dumps/`. It does **not** run `module.verify()`, so the IR is dumped even when it is invalid. `--stable` rejects ASTs that use unstable constructs (e.g. inline assembly).
  - `reproduce.rs`: Automates crash reproduction. It accepts a target and a crash artifact (or shows an interactive selection menu), rebuilds and runs the fuzzer against the artifact, classifies the result against known crash signatures, and writes a log under `fuzz/fuzz_reproduce_logs/<target>/`.
  - `fuzz_supervisor.rs`: The continuous fuzz supervisor. Runs a fuzzer in a loop, archives every crash/panic it finds, and keeps going (see [Continuous fuzzing](#continuous-fuzzing)).
- **`corpus_stable/`**: Input corpora for stable features fuzzing, organized per fuzzer (`llvm-codegen-top-level/`, `llvm-codegen-local/`, `llvm-codegen-local-loops/`, `pipeline/`).
- **`corpus_unstable/`**: Input corpora for unstable features fuzzing, organized per fuzzer (same layout as `corpus_stable/`).
- **`corpus_universal/`**: Input corpus used by the lexer fuzzer (`lexer/`, no stable/unstable mode).
- **`fuzz_pipeline/`**: Interesting ASTs saved by the pipeline fuzzer (`valid_ast_*.txt`).
- **`thrust-stable.dict`**: Token/keyword dictionary for stable fuzzing.
- **`thrust-unstable.dict`**: Token/keyword dictionary for unstable fuzzing.
- **`artifacts/`**: Crash artifacts generated by libFuzzer.
- **`ast_dumps/`**: Dumps of interesting ASTs produced during fuzzing.
- **`llvm_ir_dumps/`**: Dumps of LLVM IR modules produced by the `dump_llvm_ir` binary (even when the IR is invalid).
- **`fuzz_reproduce_logs/`**: Logs generated by the `reproduce` binary.
- **`backlog/`**: The error backlog. One directory per issue (`<target>/<issue-id>/`) containing `input.bin` (the crashing input), `ast.txt` (reconstructed AST dump), `ir.ll` (LLVM IR) or `ir_error.txt` (panic/error message when IR generation failed), and `meta.json` (metadata). Ignored by git.
- **`fuzz_continuous/`**: The continuous registry. One cascading log per target (`<target>.log`), generated from the backlog. Ignored by git.

## Stable fuzzing

Stable fuzzing is a predeterminated configured fuzzing suite to only test "stable" features on the compiler.

Cargo's alias:

- `cargo fuzz-llvm-top-level-stable` It fuzz the LLVM backend with stable features (top-level codegen). RSS limit: 2048 MB.
- `cargo fuzz-llvm-local-stable` It fuzz the LLVM backend with stable features (local codegen). RSS limit: 4096 MB.
- `cargo fuzz-llvm-local-loops-stable` It fuzz the LLVM backend with stable features (local codegen, focused on loops). RSS limit: 4096 MB.
- `cargo fuzz-pipeline-stable` It fuzz the AST validation pipeline with stable features. RSS limit: 2048 MB.

## Unstable fuzzing

Unstable fuzzing is a predeterminated configured fuzzing suite to only test "unstable" features on the compiler.

Cargo's alias:

- `cargo fuzz-llvm-top-level-unstable` It fuzz the LLVM backend with unstable features (top-level codegen). RSS limit: 2048 MB.
- `cargo fuzz-llvm-local-unstable` It fuzz the LLVM backend with unstable features (local codegen). RSS limit: 2048 MB.
- `cargo fuzz-llvm-local-loops-unstable` It fuzz the LLVM backend with unstable features (local codegen, focused on loops). RSS limit: 2048 MB.
- `cargo fuzz-pipeline-unstable` It fuzz the AST validation pipeline with unstable features. RSS limit: 2048 MB.

## Lexer fuzzing

- `cargo fuzz-lexer` It fuzz the lexer with a universal corpus and stable dictionary. RSS limit: 2048 MB.

## Continuous fuzzing

The one-shot aliases above stop as soon as libFuzzer finds a crash. The **continuous supervisor** instead runs a fuzzer in a loop: whenever a crash or panic is found it archives it, records it, and immediately starts fuzzing again — so it can accumulate every bug a target hits, not just the first one.

### How it works

1. The supervisor spawns the fuzzer exactly like the one-shot aliases do (same corpus, dictionary, RSS limits and `--stable` flag), picking the **nightly** toolchain automatically (see [Toolchain](#toolchain)).
2. A monitor thread polls `fuzz/artifacts/<target>/` while the fuzzer runs. When libFuzzer writes a crash artifact, the supervisor:
   - copies the input to the backlog,
   - reconstructs and dumps the AST,
   - runs LLVM codegen and dumps the IR (or the panic/error message if codegen failed),
   - classifies the crash against known signatures,
   - records it and regenerates the registry log,
   - restarts the fuzzer.
3. Only real crashes are archived. An artifact whose AST is rejected by semantic analysis (an expected diagnostic) is discarded; the `marker` column shows which crash signature matched (e.g. `panicked at`, `ERROR: AddressSanitizer`, `index out of bounds`), or `-` when there is none (imported artifacts).

The default mode is **`stable`**. Pass `--mode unstable` explicitly to fuzz unstable features.

### Commands

The supervisor is `cargo fuzz-continuous <command> ...`:

- `run <target> [--mode stable|unstable] [--runs N] [--max-time S]`
  Runs the target in a loop. `--runs N` / `--max-time S` turn it into a single bounded run (libFuzzer stops after `N` runs or `S` seconds).
- `run-all [--mode stable|unstable]`
  Same, but one fuzzer thread per target (all 5 targets at once).
- `import <target>`
  Archive crash artifacts that already exist under `fuzz/artifacts/<target>/` without fuzzing (useful to backfill an existing backlog).
- `list [--all]`
  List the pending (`open`) errors per target. With `--all`, also shows `ignored` and `fixed` ones.
- `history [<target>]`
  Print the registry log(s).
- `ignore <target> <issue-id>`
  Mark an error as ignored so the same input is never archived again.
- `reopen <target> <issue-id>`
  Move an error back to the pending pile.
- `fixed <target> <issue-id>`
  Mark an error as solved (once the compiler bug is fixed).

Valid targets: `lexer`, `pipeline`, `llvm-codegen-top-level`, `llvm-codegen-local`, `llvm-codegen-local-loops`.

### Shorthand aliases

| Alias | Target | Mode |
|---|---|---|
| `cargo fuzz-continuous-llvm-top-level-stable` | `llvm-codegen-top-level` | stable |
| `cargo fuzz-continuous-llvm-top-level-unstable` | `llvm-codegen-top-level` | unstable |
| `cargo fuzz-continuous-llvm-local-stable` | `llvm-codegen-local` | stable |
| `cargo fuzz-continuous-llvm-local-unstable` | `llvm-codegen-local` | unstable |
| `cargo fuzz-continuous-llvm-local-loops-stable` | `llvm-codegen-local-loops` | stable |
| `cargo fuzz-continuous-llvm-local-loops-unstable` | `llvm-codegen-local-loops` | unstable |
| `cargo fuzz-continuous-pipeline-stable` | `pipeline` | stable |
| `cargo fuzz-continuous-pipeline-unstable` | `pipeline` | unstable |
| `cargo fuzz-continuous-lexer` | `lexer` | — (universal corpus) |

### The registry log

Every recorded error lives in the backlog at `fuzz/backlog/<target>/<issue-id>/` with:

- `input.bin` — the exact crashing input,
- `ast.txt` — the reconstructed AST dump,
- `ir.ll` — the generated LLVM IR, or `ir_error.txt` — the panic/error message when IR generation failed,
- `meta.json` — metadata (id, target, mode, content hash, discovery time, status, absolute paths).

The human-readable registry is written in a **cascading** format, one indented block per issue, to `fuzz/fuzz_continuous/<target>.log`:

```
# cascade log for target `llvm-codegen-local` (/path/to/thrustc/fuzz/fuzz_continuous/llvm-codegen-local.log)
# 10 issue(s)

[1] issue_089665036ccf2e0f
    ├─ discovered_at : 2026-08-05 16:05:21
    ├─ hash          : 089665036ccf2e0f
    ├─ mode          : unstable
    ├─ marker        : -
    ├─ status        : ignored
    ├─ input_path    : /path/to/thrustc/fuzz/backlog/llvm-codegen-local/issue_089665036ccf2e0f/input.bin
    ├─ ast_path      : /path/to/thrustc/fuzz/backlog/llvm-codegen-local/issue_089665036ccf2e0f/ast.txt
    └─ ir_path       : -
```

The `hash` is an FNV-1a content hash of `input.bin`; the `issue-id` is derived from it, and it is what makes deduplication work — importing or fuzzing the same input twice never creates a second entry. Issue statuses are `open`, `ignored` and `fixed`.

### Toolchain

Because the repository root pins a **stable** toolchain but cargo-fuzz needs nightly, the supervisor (and the `reproduce` binary) automatically prepend the channel declared in `fuzz/rust-toolchain.toml` to every `cargo` invocation (i.e. they run `cargo +nightly fuzz run ...`). You can keep working from the repo root without worrying about the toolchain.

## Corpus directories

The corpus directories (`corpus_stable/`, `corpus_unstable/`, `corpus_universal/`) are ignored by git (see `fuzz/.gitignore`), so they are **not** shipped when you clone the repository. If they are missing, fuzzing will fail. Before running any fuzzer, create them with one of the bundled scripts:

```sh
# Bash (Linux/macOS)
bash fuzz/scripts/create_fuzzing_dirs.sh

# Fish shell (Linux/MacOS)
fish fuzz/scripts/create_fuzzing_dirs.fish

# Windows (PowerShell)
powershell -ExecutionPolicy Bypass -File fuzz\scripts\create_fuzzing_dirs.ps1

# Windows (Command Prompt)
fuzz\scripts\create_fuzzing_dirs.bat
```

All variants are idempotent: they resolve paths relative to `fuzz/` regardless of the current working directory, create any missing folder with `mkdir -p`, and leave existing ones untouched. The scripts also create the `backlog/` and `fuzz_continuous/` directories used by the continuous supervisor.

## Fuzzing workflow

This is the general workflow when a fuzzer finds an issue:

1. **Create the required directories.** If this is a fresh clone, run one of the `create_fuzzing_dirs` scripts above (see [Corpus directories](#corpus-directories)).
2. **Run the fuzzer.** Start one of the fuzzing suites (e.g. `cargo fuzz-llvm-local-unstable`). libFuzzer keeps generating inputs and feeding them to the target until the compiler crashes (a panic, an ICE, an LLVM verification error, an out of memory, etc.). When that happens the fuzzer stops and writes the crashing input under `fuzz/artifacts/<target>/`.

   Alternatively, use the [continuous supervisor](#continuous-fuzzing) (`cargo fuzz-continuous-llvm-local-unstable`), which never stops: it archives each crash automatically — with AST and LLVM IR dumps — and keeps fuzzing. You can inspect the backlog with `cargo fuzz-backlog list`.
3. **Take the crash artifact.** The crash file is a raw bytes input, the exact data that made the compiler panic.
4. **Inspect the AST.** Pass the crash file to the `dump-ast` binary that matches the fuzzer that crashed, to reconstruct and print the AST:
   - `cargo fuzz-dump-ast-top-level <crash-file>` for the `pipeline` fuzzer.
   - `cargo fuzz-dump-ast-local <crash-file>` for the `llvm-codegen-local` fuzzer.
   - `cargo fuzz-dump-ast-local-loops <crash-file>` for the `llvm-codegen-local-loops` fuzzer.
   The dump is written to `fuzz/ast_dumps/`, so you can see which AST shape triggered the bug. For the lexer the crash file is plain UTF-8 source code, so you can inspect it directly.

   For LLVM codegen crashes you can additionally dump the generated module IR:
   - `cargo fuzz-dump-llvm-ir <crash-file>` for the `llvm-codegen-local` fuzzer.
   - `cargo fuzz-dump-llvm-ir <crash-file> llvm-codegen-local-loops` for the `llvm-codegen-local-loops` fuzzer.
   The IR is written to `fuzz/llvm_ir_dumps/` and is dumped **without** running `module.verify()`, so you get the IR even when it is invalid.
5. **Fix the compiler bug.** With the AST in front of you, locate the faulty code in the compiler (semantic analysis, AST validation, or LLVM codegen), fix it, and rebuild.
6. **Reproduce to verify.** Run the same artifact again with `cargo fuzz-reproduce-case [<target> <artifact>]`, which rebuilds the target fuzzer and runs it against the crash file:
   - If the `reproduce` result is **`REAL CRASH`**, the bug is still there. Go back to step 5.
   - If the result is **`no crash detected`**, the artifact no longer crashes the compiler, the bug was solved. The full output is logged under `fuzz/fuzz_reproduce_logs/<target>/`.
7. **Continue fuzzing.** When verified, resume the fuzzer (`cargo fuzz-<target>-<mode>`) to look for the next issue. libFuzzer keeps the old artifacts in its corpus, so a regression that brings back the same bug is caught fast.

## Auxiliary binaries

- `cargo fuzz-dump-ast-top-level <crash-file>` Reconstructs and prints the AST from a fuzzer crash artifact (uses the `Arbitrary` trait).
- `cargo fuzz-dump-ast-local <crash-file>` Same as `dump_ast_top_level` but uses the scoped AST generator.
- `cargo fuzz-dump-ast-local-loops <crash-file>` Same as `dump_ast_local` but uses the loop focused scoped AST generator.
- `cargo fuzz-dump-llvm-ir <crash-file> [generator] [--stable]` Reconstructs the AST with a scoped generator, runs semantic analysis + LLVM codegen and dumps the module IR to `fuzz/llvm_ir_dumps/` **without** running `module.verify()`, so the IR is dumped even when it is invalid. `generator` selects the AST generator (`llvm-codegen-local` by default, or `llvm-codegen-local-loops`) and `--stable` rejects ASTs that use unstable constructs (e.g. inline assembly).
- `cargo fuzz-reproduce-case [<target> <artifact>]` Automates crash reproduction. It rebuilds and runs the target fuzzer against the crash artifact, classifies the result against known crash signatures, and writes a log under `fuzz/fuzz_reproduce_logs/<target>/`. Run without arguments to get an interactive selection menu.
