<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

# Continuous Fuzzing

The regular fuzzers (`cargo fuzz-<target>-<mode>`) stop as soon as libFuzzer finds a crash. The **continuous supervisor** works the other way around: it runs a fuzzer in a loop, and each time a crash or panic surfaces it saves the input, records it, and goes back to fuzzing.

> [!IMPORTANT]
> You need a **nightly** toolchain and **`cargo-fuzz`** to use this (the full list of prerequisites is in [COMPILER_FUZZING.md](../COMPILER_FUZZING.md#prerequisites)). The supervisor and the `reproduce` binary pick up the channel declared in `fuzz/rust-toolchain.toml` automatically — they run `cargo +nightly fuzz run ...` — so you can launch them from the repository root even though it pins a stable toolchain.

## Requirements

- A nightly toolchain installed (`rustup toolchain install nightly`).
- `cargo-fuzz` installed (`cargo +nightly install cargo-fuzz --locked`). The supervisor shells out to `cargo +nightly fuzz run ...`, so nothing will start without it.
- The corpus directories must exist (run one of the `fuzz/scripts/create_fuzzing_dirs.{sh,fish,ps1,bat}` scripts on a fresh clone). Those scripts also create the `backlog/` and `fuzz_continuous/` directories used here.

## How it works

1. The supervisor starts the fuzzer exactly as the plain `fuzz-*` aliases do: same corpus, same dictionary, same RSS limit, and the `--stable` flag for the chosen mode.
2. While the fuzzer runs, a monitor thread watches `fuzz/artifacts/<target>/`. When libFuzzer writes a crash artifact, the supervisor:
   - copies the input into the backlog,
   - rebuilds and dumps the AST,
   - runs LLVM codegen and dumps the IR (or the panic/error message when codegen fails),
   - matches the crash against known signatures,
   - records it and rewrites the registry log,
   - restarts the fuzzer.
3. Only genuine crashes are kept. An artifact whose AST is rejected by semantic analysis (an expected diagnostic) is discarded. The `marker` field shows which signature matched (e.g. `panicked at`, `ERROR: AddressSanitizer`, `index out of bounds`), or `-` when none did.

The default mode is **`stable`**. Pass `--mode unstable` explicitly to fuzz unstable features.

## Using the supervisor

Two cargo aliases point at the same binary: `cargo fuzz-continuous` runs the fuzzers, and `cargo fuzz-backlog` manages the recorded errors.

### Running fuzzers

```
cargo fuzz-continuous run <target> [--mode stable|unstable] [--runs N] [--max-time S]
```

Runs the target in an endless loop, archiving every crash and carrying on. With `--runs N` or `--max-time S` the run becomes a single bounded one (libFuzzer stops after `N` executions or `S` seconds).

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
| `cargo fuzz-continuous-lexer` | `lexer` | — (universal corpus) |

Each shorthand alias is equivalent to `cargo fuzz-continuous run <target> --mode <mode>`.

### Managing the backlog

```
cargo fuzz-backlog list [--all]
```

Shows the pending (`open`) errors per target. With `--all`, it also shows the `ignored` and `fixed` ones.

```
cargo fuzz-backlog history [<target>]
```

Prints the cascading registry log(s) to stdout. Without a target it prints every log that exists.

```
cargo fuzz-backlog import <target>
```

Archives crash artifacts that already sit under `fuzz/artifacts/<target>/` without fuzzing. Useful to backfill a backlog from artifacts found before the supervisor existed.

```
cargo fuzz-backlog ignore <target> <issue-id>
cargo fuzz-backlog reopen <target> <issue-id>
cargo fuzz-backlog fixed <target> <issue-id>
```

Change an issue's status. `ignore` makes the supervisor skip that input forever (it is never archived again), `reopen` moves an issue back to the pending pile, and `fixed` marks it as solved once the compiler bug is fixed.

## The backlog

Every recorded error lives under `fuzz/backlog/<target>/<issue-id>/` with:

- `input.bin` — the exact crashing input,
- `ast.txt` — the reconstructed AST dump,
- `ir.ll` — the generated LLVM IR, or `ir_error.txt` — the panic/error message when IR generation failed,
- `meta.json` — the metadata (id, target, mode, content hash, discovery time, status, absolute paths).

## The registry log

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

- `hash` is an FNV-1a content hash of `input.bin`; the `issue-id` derives from it, and that is what makes deduplication work — importing or fuzzing the same input twice never creates a second entry.
- `mode` is `stable` or `unstable`; `marker` is the crash signature that matched, or `-`.
- `status` is `open`, `ignored` or `fixed`. Absolute paths point at the payload files inside the backlog.

## A typical session

1. Start a continuous fuzzer, e.g. the LLVM local backend in stable mode:

   ```console
   cargo fuzz-continuous-llvm-local-stable
   ```

   Leave it running; it archives crashes and keeps going on its own.

2. In another terminal, inspect what it has found so far:

   ```console
   cargo fuzz-backlog list
   cargo fuzz-backlog history llvm-codegen-local
   ```

3. Look at a specific issue's payload (AST + IR) under `fuzz/backlog/llvm-codegen-local/issue_<hash>/`.

4. If an issue is a known false positive or already tracked, stop it from being re-archived:

   ```console
   cargo fuzz-backlog ignore llvm-codegen-local issue_<hash>
   ```

5. Once you fix the compiler bug, verify with `cargo fuzz-reproduce-case <target> fuzz/backlog/.../input.bin`, then mark the issue as solved:

   ```console
   cargo fuzz-backlog fixed llvm-codegen-local issue_<hash>
   ```

6. To stop fuzzing, press Ctrl+C. Backlog and logs persist in `fuzz/backlog/` and `fuzz/fuzz_continuous/`.