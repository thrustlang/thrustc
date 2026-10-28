<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

# Continuous Fuzzing

One-shot fuzzers (`cargo fuzz-<target>-<mode>`) stop at the first crash. The continuous supervisor saves each crash to the logs and restarts the fuzzer.

> [!IMPORTANT]
> A nightly toolchain and `cargo-fuzz` are required. The supervisor and the `reproduce` binary read the channel from `fuzz/rust-toolchain.toml` and run `cargo +nightly fuzz run ...`, so they can be launched from the repository root. The full list of prerequisites is in [COMPILER_FUZZING.md](../COMPILER_FUZZING.md#prerequisites).

## Requirements

- A nightly toolchain: `rustup toolchain install nightly`.
- `cargo-fuzz`: `cargo +nightly install cargo-fuzz --locked`. The supervisor shells out to `cargo +nightly fuzz run ...` and cannot start without it.
- The corpus directories. Run one of the `fuzz/scripts/create_fuzzing_dirs.{sh,fish,ps1,bat}` scripts on a fresh clone; they also create `logs/` and `fuzz_continuous/`.

## How it works

1. The supervisor starts the fuzzer with the same corpus, dictionary, RSS limit and `--stable` flag as the plain `fuzz-*` aliases.
2. While the fuzzer runs, a monitor thread watches `fuzz/artifacts/<target>/`. When libFuzzer writes a crash artifact, the supervisor:
   - copies the input into the logs,
   - dumps the AST,
   - runs LLVM codegen and dumps the IR (or the panic/error message when codegen fails),
   - classifies the crash against known signatures,
   - records the issue and regenerates the registry log,
   - restarts the fuzzer.
3. Only real crashes are kept. An artifact whose AST is rejected by semantic analysis (an expected diagnostic) is discarded. The `marker` field shows the matched signature (`panicked at`, `ERROR: AddressSanitizer`, `index out of bounds`, ...) or `-` when none matched.

The default mode is `stable`. Pass `--mode unstable` to fuzz unstable features.

## Using the supervisor

Two cargo aliases point at the same binary: `cargo fuzz-continuous` drives the fuzzers, and `cargo fuzz-logs` manages the recorded issues.

### Running fuzzers

```
cargo fuzz-continuous run <target> [--mode stable|unstable] [--runs N] [--max-time S]
```

Runs the target in an endless loop, archiving every crash and continuing. With `--runs N` or `--max-time S` the run is bounded (libFuzzer stops after `N` executions or `S` seconds).

```
cargo fuzz-continuous run-all [--mode stable|unstable]
```

Runs every fuzzer at once, one thread per target.

Valid targets: `lexer`, `pipeline`, `llvm-codegen-top-level`, `llvm-codegen-local`, `llvm-codegen-local-loops`.

#### Shorthand aliases

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
| `cargo fuzz-continuous-lexer` | `lexer` | (universal corpus) |

Each shorthand alias is equivalent to `cargo fuzz-continuous run <target> --mode <mode>`.

### Managing the logs

```
cargo fuzz-logs list [--all]
```

Lists the pending (`open`) issues per target. With `--all`, it also lists `ignored` and `fixed` ones.

```
cargo fuzz-logs history [<target>]
```

Prints the registry log. Without a target it prints every log that exists.

```
cargo fuzz-logs import <target>
```

Archives crash artifacts already sitting under `fuzz/artifacts/<target>/` without fuzzing. Useful to backfill the logs from artifacts recorded before the supervisor existed.

```
cargo fuzz-logs ignore <target> <issue-id>
cargo fuzz-logs reopen <target> <issue-id>
cargo fuzz-logs fixed <target> <issue-id>
```

Changes an issue's status. `ignore` makes the supervisor skip that input permanently, `reopen` moves it back to the pending pile, and `fixed` marks it as solved after fixing the compiler bug.

## The logs

Every recorded issue lives under `fuzz/logs/<target>/<issue-id>/` with:

- `input.bin`: the exact crashing input,
- `ast.txt`: the reconstructed AST dump,
- `ir.ll`: the generated LLVM IR, or `ir_error.txt`: the panic or error message when IR generation failed,
- `meta.json`: the metadata (id, target, mode, content hash, discovery time, status, absolute paths).

## The registry log

The registry is a plain text log, one indented block per issue, written to `fuzz/fuzz_continuous/<target>.log`. Each issue is recorded once: recording or importing the same input twice never creates a second entry.

```
# registry log for target `llvm-codegen-local` (/path/to/thrustc/fuzz/fuzz_continuous/llvm-codegen-local.log)
# 10 issue(s)

[1] issue_089665036ccf2e0f
    ├─ discovered_at : 2026-08-05 16:05:21
    ├─ hash          : 089665036ccf2e0f
    ├─ mode          : unstable
    ├─ marker        : -
    ├─ status        : ignored
    ├─ input_path    : /path/to/thrustc/fuzz/logs/llvm-codegen-local/issue_089665036ccf2e0f/input.bin
    ├─ ast_path      : /path/to/thrustc/fuzz/logs/llvm-codegen-local/issue_089665036ccf2e0f/ast.txt
    └─ ir_path       : -
```

- `mode` is `stable` or `unstable`; `marker` is the matched crash signature, or `-`.
- `status` is `open`, `ignored` or `fixed`. The paths point at the payload files inside the logs.

## A typical session

1. Start a continuous fuzzer, e.g. the LLVM local backend in stable mode:

   ```console
   cargo fuzz-continuous-llvm-local-stable
   ```

   The fuzzer runs until stopped; every crash it finds is archived automatically.

2. In another terminal, inspect what has been found so far:

   ```console
   cargo fuzz-logs list
   cargo fuzz-logs history llvm-codegen-local
   ```

3. Look at an issue's payload (AST and IR) under `fuzz/logs/llvm-codegen-local/issue_<id>/`.

4. If an issue is a known false positive or already tracked, stop it from being archived again:

   ```console
   cargo fuzz-logs ignore llvm-codegen-local issue_<id>
   ```

5. After fixing the compiler bug, verify with `cargo fuzz-reproduce-case <target> fuzz/logs/.../input.bin`, then mark the issue as solved:

   ```console
   cargo fuzz-logs fixed llvm-codegen-local issue_<id>
   ```

6. Stop fuzzing with Ctrl+C. The logs and the registry log persist in `fuzz/logs/` and `fuzz/fuzz_continuous/`.
