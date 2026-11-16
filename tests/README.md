<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler - tests

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

The `tests/` directory contains executable Thrust source tests that validate the compiler pipeline.

The automation scripts live in `tests/scripts/` and run from the repository root.

## Scripts

- `tests/scripts/run-tests.py` compiles and runs the Thrust test suite.
- `tests/scripts/create-test.py` creates `.thrust` tests from literal content or another file.

Both scripts need Python 3 and use no third-party packages.

## Run all tests

Build `thrustc` first, then run the full suite:

```console
$ tests/scripts/run-tests.py
```

If `target/debug/thrustc` is already built, skip the Cargo build:

```console
$ tests/scripts/run-tests.py --no-build
```

Stop on the first failure:

```console
$ tests/scripts/run-tests.py --no-build --fail-fast
```

Keep generated binaries and build artifacts for debugging:

```console
$ tests/scripts/run-tests.py --no-build --keep-dist
```

Increase per-test timeouts:

```console
$ tests/scripts/run-tests.py --no-build --compile-timeout 300 --run-timeout 60
```

## Run a subset

Use `--filter` to run only tests whose relative path contains a substring:

```console
$ tests/scripts/run-tests.py --no-build --filter "std/math"
```

Run a single test:

```console
$ tests/scripts/run-tests.py --no-build --filter "arbitrary/const_arithmetic.thrust"
```

Use a custom compiler binary:

```console
$ tests/scripts/run-tests.py --compiler target/release/thrustc --filter "std/io"
```

Forward extra arguments to the external C linker compiler:

```console
$ tests/scripts/run-tests.py --no-build --cc-args "-lz"
```

Forward raw arguments directly to `thrustc` (repeatable):

```console
$ tests/scripts/run-tests.py --no-build --compiler-arg --fast-math --compiler-arg -O2 --filter "optimization"
```

Exclude tests whose relative path contains a substring (combinable with `--filter`):

```console
$ tests/scripts/run-tests.py --no-build --filter "abi" --exclude-only "wasm"
```

Emit compiler artifacts while running a test, for example to inspect LLVM IR:

```console
$ tests/scripts/run-tests.py --no-build --filter "typesystem/native_vector_ir_shape.thrust" --emit llvm-ir --keep-dist
```

Emitted LLVM IR is written under `tests/dist/build/<test-id>/emit/llvm-ir/`.

## CLI flags

`run-tests.py` accepts these flags:

| Flag | Default | Effect |
|---|---|---|
| `--compiler <path>` | `target/debug/thrustc` | Runs a prebuilt binary and skips the implicit `cargo build --bin thrustc`. Exits `1` with `compiler not found` when the path does not exist. |
| `--no-build` | off | Skips `cargo build` and uses the existing binary or `--compiler`. |
| `--filter <text>` | `""` | Runs only tests whose path relative to `tests/` contains the text (case-sensitive). Applies to `.thrust` and `c_transpile` tests. |
| `--exclude-only <text>` | `""` | Skips tests whose relative path contains the text. Works with `--filter`. |
| `--fail-fast` | off | Stops after the first failure and skips the remaining phases, including `c_transpile`. |
| `--keep-dist` | off | Keeps `tests/dist/` after the run for debugging. Without it, the runner removes the directory even after a failure. |
| `--compile-timeout <s>` | `120` | Maximum seconds per compilation and per C translation. A timeout fails the test (`compile timeout after Xs` or `translate timeout ...`). |
| `--run-timeout <s>` | `30` | Maximum seconds per binary execution (`run timeout ...`). |
| `--cc-args "<args>"` | `""` | Extra arguments for the external linker compiler. The runner always prepends `-o <binary>`, plus `-lm` when needed, and joins the arguments with `;`. |
| `--emit <value>` | none (repeatable) | Forwards each `-emit <value>` to `thrustc`. When a positive test produces no binary but `--emit` was given, it passes as `compile emitted artifact`. |
| `--compiler-arg <arg>` | none (repeatable) | Forwards each raw argument to `thrustc`, after `-std`/`-build-dir` and before `-emit`/`-mode`. |

When no `--compiler` is given and `--no-build` is off, the runner builds `thrustc` from the repository root with `cargo build --bin thrustc`. When the filters match no tests, it exits `1` with `no tests found`.

## Test layout

Tests are organized by feature area. Each directory contains `.thrust` test roots and importable libraries. A library is a file without `fn main` that another test includes with `import "..."`.

| Directory | Contents |
|---|---|
| `arbitrary/` | Miscellaneous smoke tests. |
| `asm/` | Inline assembler functions, assembler values, and global assembler. Compiles with `-mode unstable`. |
| `vector/` | NativeVector arithmetic, bitwise, shifts, unary, comparison, logical, compound, const and static operations. |
| `builtins/` + `builtins/introspection/` | Compile-time and type builtins (`sizeOf`, `alignOf`, predicates, `staticAssert`, target builtins). |
| `compiletime/if/` | `@if`/`@elif`/`@else` conditional compilation. |
| `compiletime/if_imports/` | Conditional imports, with a local `stdroot/` (`-std-version 0.1.8`) used by `if_std_import*` tests. |
| `compiletime/target/` | Target and host introspection (`targetArch`, `hostOsName`, …). |
| `linter/` | Warning suppression via `directive "--disable-warnings=..."`. |
| `memory/` + `memory/deref/` + `memory/load/` | Stack allocations, `->` dereference properties, and `load`/`ref` semantics. |
| `modules/` + `modules/reexportation/` | Module imports, aliases, and re-exports. |
| `c_transpile/` | C-to-Thrust transpiler goldens (separate runner phase, see below). |
| `std/` | Standard library module tests. |
| `stress/` | Excluded from discovery (see below). |
| Other feature areas (`abi/`, `atomics/`, `import_c/`, `sanitizers/`, `optimization/`, …) | End-to-end tests for a specific feature. |

Tests under `atomics/`, tests using `NativeVector`/`native[...]`, and tests using `importC` compile with `-mode unstable`.

## Test metadata

Tests can declare host requirements and compiler arguments in comments at the top of the file.

| Metadata | Effect |
|---|---|
| `// test-requires: x86_64 avx512f` | Skips the test when the host architecture or a listed CPU feature is missing. A skipped test counts as passed. |
| `// test-cpu-features: +sse2,+avx512f` | Adds `-cpu-features <value>` to the test compilation. |
| `// test-expect-error: E0061` | Compiles the file without running it and expects that diagnostic code. The file does not need `fn main`. |

## Test results

Positive tests compile, link, run, and return exit code `0`.

Diagnostic tests fail compilation with the code listed in `test-expect-error`.

## C transpile tests

Tests under `tests/c_transpile/` validate the C-to-Thrust transpiler. Each case is a C file with a sibling golden file:

- `tests/c_transpile/<group>/<name>.c`: the input, plus optional `.h` helpers included with `#include`.
- `tests/c_transpile/<group>/<name>.expected.thrust`: the exact expected output. A case without this file is ignored.

An optional `tests/c_transpile/<group>/<name>.run.thrust` driver provides a `fn main` for runtime checks when the translated output has none.

For each case the runner:

1. Runs `thrustc --translate-c-to-thrust <name.c> --translate-c-out-dir tests/dist/c_transpile/<test-id>/` with `--compile-timeout` as the limit.
2. Fails when translation fails or the `<name>.thrust` output is missing.
3. Compares the output byte-for-byte with `<name>.expected.thrust` and prints a unified diff on a mismatch.
4. Recompiles the translated output with `-emit ast`, unless it contains `directive`, `importC`, `union`, `enum`, or `deref ... = ...` lines.
5. Runs it when it declares `fn main`, or concatenates it with its `.run.thrust` driver (`translated + "\n\n" + driver`) and runs that. Both runtimes must exit `0`. Without `main` and without a driver, the test passes after translation.

There is no `--bless` flag. Update a golden file by translating with the new binary and copying the result over `.expected.thrust`:

```console
$ target/debug/thrustc --translate-c-to-thrust tests/c_transpile/macros/foo.c --translate-c-out-dir /tmp/out
$ cp /tmp/out/foo.thrust tests/c_transpile/macros/foo.expected.thrust
```

Use `--keep-dist` to inspect generated files under `tests/dist/c_transpile/<test-id>/` before copying.

Tests using `importC` (for example, everything under `tests/import_c/`) compile with `-mode unstable`. They check behavior through exit codes and have no golden files.

## Create a test

Create a test with literal content:

```console
$ tests/scripts/create-test.py -path arbitrary/new_test.thrust -content 'fn main() s32 @public { return 0; }' --append-newline
```

Create a test from a template file:

```console
$ tests/scripts/create-test.py -path arbitrary/new_test.thrust -content path/to/template.thrust
```

Overwrite an existing test:

```console
$ tests/scripts/create-test.py -path arbitrary/new_test.thrust -content path/to/template.thrust --force
```

Relative paths are created inside `tests/`. These commands create the same file:

```console
$ tests/scripts/create-test.py -path arbitrary/new_test.thrust -content 'fn main() s32 @public { return 0; }'
$ tests/scripts/create-test.py -path tests/arbitrary/new_test.thrust -content 'fn main() s32 @public { return 0; }'
```

The destination must use the `.thrust` extension and stay inside `tests/`.

## Test conventions

A minimal positive test returns `0` on success:

```thrust
fn main() s32 @public {
    return 0;
}
```

Use a non-zero return code to identify the failing assertion:

```thrust
fn main() s32 @public {
    if 1 + 1 != 2 { return 1; }

    return 0;
}
```

If a test imports another user file, keep the import relative to the importing file:

```thrust
import "module.thrust";
```

The runner compiles that dependency with the test.
