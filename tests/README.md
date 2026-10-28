<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler Tests

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

The `tests/` directory contains executable Thrust source tests used to validate the compiler pipeline.

The automation scripts live in `tests/scripts/` and are intended to be executed from the repository root.

## Scripts

- `tests/scripts/run-tests.py` compiles and runs the Thrust test suite.
- `tests/scripts/create-test.py` creates new `.thrust` test files from literal content or from another file.

Both scripts require Python 3 and do not depend on third-party Python packages.

## Run All Tests

Build `thrustc` first and then run the full suite:

```console
$ tests/scripts/run-tests.py
```

If `target/debug/thrustc` is already built, skip the Cargo build step:

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

## Run A Subset

Use `--filter` to run only tests whose relative path contains a specific substring:

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

Emit compiler artifacts while running a test. This is useful for checking LLVM IR:

```console
$ tests/scripts/run-tests.py --no-build --filter "typesystem/native_vector_ir_shape.thrust" --emit llvm-ir --keep-dist
```

Emitted LLVM IR is written under `tests/dist/build/<test-id>/emit/llvm-ir/`.

## CLI Flags

All flags of `tests/scripts/run-tests.py`:

| Flag | Default | Effect |
|---|---|---|
| `--compiler <path>` | `target/debug/thrustc` | Uses a prebuilt binary and skips the implicit `cargo build --bin thrustc`. Fails with `compiler not found` (exit `1`) when the binary does not exist. |
| `--no-build` | off | Never invokes `cargo build`; uses the existing binary (or `--compiler`). |
| `--filter <text>` | `""` | Runs only tests whose path relative to `tests/` contains the substring (case-sensitive). Applies to `.thrust` and `c_transpile` tests. |
| `--exclude-only <text>` | `""` | Skips tests whose relative path contains the substring. Combinable with `--filter`. |
| `--fail-fast` | off | Stops after the first failure and skips the remaining phases (including `c_transpile`). |
| `--keep-dist` | off | Keeps `tests/dist/` after the run for debugging; otherwise it is always removed, even on failure. |
| `--compile-timeout <s>` | `120` | Maximum seconds per compilation and per C translation. On timeout the test fails (`compile timeout after Xs` / `translate timeout ...`). |
| `--run-timeout <s>` | `30` | Maximum seconds per binary execution (`run timeout ...`). |
| `--cc-args "<args>"` | `""` | Extra arguments forwarded to the external linker compiler. The runner always prepends `-o <binary>` (plus `-lm` when needed), joined with `;`. |
| `--emit <value>` | none (repeatable) | Forwards each `-emit <value>` to `thrustc`. When a positive test produces no binary but `--emit` was given, it passes as `compile emitted artifact`. |
| `--compiler-arg <arg>` | none (repeatable) | Forwards each raw argument directly to `thrustc`, after `-std`/`-build-dir` and before `-emit`/`-mode`. |

When no `--compiler` is given and `--no-build` is off, the runner builds `thrustc` with `cargo build --bin thrustc` from the repository root. When filters match zero tests it exits `1` with `no tests found`.

## Test Layout

Tests are organized by feature area. Each directory holds executable `.thrust` roots (files declaring `fn main`) plus importable libraries (files without `fn main`, included via `import "..."`).

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
| other `api`-like areas (`abi/`, `asm/`, `atomics/`, `import_c/`, `sanitizers/`, `optimization/`, …) | Feature-specific end-to-end tests; `asm/`, `atomics/`, tests using `NativeVector`/`native[...]`, and `importC` compile with `-mode unstable`. |

## Runner Behavior

The runner discovers test roots by scanning `.thrust` files under `tests/` (sorted) and selecting files that declare `fn main`. Files without `fn main` are treated as importable libraries, not tests. The following directories are never scanned: `dist`, `scripts`, `build`, `c_transpile`, `stdroot`, `stress`.

For each discovered test, the runner:

- Resolves user imports written as `import "file.thrust"` recursively.
- Adds imported user modules to the same compiler invocation as the root test.
- Resolves `std::...` imports from the repository's `std/` directory (via `-std`), except `compiletime/if_imports`/`if_std_import*` tests, which use their local `stdroot` with `-std-version 0.1.8`.
- Adds `-lm` automatically for tests that use `std::math`, `std::ffi::c::math`, or floating-point modulo operations.
- Adds `-mode unstable` automatically for tests under `asm/`, `atomics/`, files or dependencies that use `NativeVector`/`native[...]`, or tests using `importC`, plus `--import-c-system-include <dir>` for each Clang system include directory it detects (via `clang -E -x c - -v`, 10s timeout).
- Compiles with `-build-dir tests/dist/build/<test-id>` into `tests/dist/bin/<test-id>`.
- Runs the binary from `tests/dist/run/<test-id>` so temporary runtime files stay isolated.
- Removes `tests/dist/` when finished, unless `--keep-dist` is used.

The test identifier joins the path relative to `tests/` without extension using `__` (for example `imports/only_then_full.thrust` becomes `imports__only_then_full`). Runtime variants append `__runtime` or `__translated_runtime`.

The final executable path is passed through `-cc-args` because the current linker flow receives output flags through the external linker arguments.

## Test Results

Positive tests are expected to compile, link, run, and return exit code `0`.

## C Transpile Tests

Tests under `tests/c_transpile/` validate the C-to-Thrust transpiler. Each case is a C file with a sibling golden file:

- `tests/c_transpile/<group>/<name>.c` — the input (plus optional `.h` helpers included via `#include`).
- `tests/c_transpile/<group>/<name>.expected.thrust` — the exact expected translation output (mandatory; inputs without it are ignored).

An optional `tests/c_transpile/<group>/<name>.run.thrust` driver provides a `fn main` for runtime checks when the translated output has none.

For each case the runner:

1. Runs `thrustc --translate-c-to-thrust <name.c> --translate-c-out-dir tests/dist/c_transpile/<test-id>/` (bounded by `--compile-timeout`).
2. Fails when translation fails or the `<name>.thrust` output is missing.
3. Compares the output byte-for-byte against `<name>.expected.thrust` and fails with a unified diff on mismatch.
4. Recompiles the translated output with `-emit ast`, unless it contains `directive`, `importC`, `union`, `enum`, or `deref ... = ...` lines.
5. Executes it when it declares `fn main`, or concatenates it with its `.run.thrust` driver (`translated + "\n\n" + driver`) and executes that. Both runtimes must exit `0`; without `main` and without driver the test passes after translation.

There is no `--bless` flag: golden files are updated manually by translating with the new binary and copying the result over the `.expected.thrust`:

```console
$ target/debug/thrustc --translate-c-to-thrust tests/c_transpile/macros/foo.c --translate-c-out-dir /tmp/out
$ cp /tmp/out/foo.thrust tests/c_transpile/macros/foo.expected.thrust
```

Note: run with `--keep-dist` to inspect the generated files under `tests/dist/c_transpile/<test-id>/` before copying.

Tests using `importC` (for example everything under `tests/import_c/`) compile with `-mode unstable` automatically; they assert through exit codes only and have no golden files.

## Excluded Directories

Entire directories are excluded from discovery: `dist`, `scripts`, `build`, `c_transpile` (covered by its own phase), `stdroot`, and `stress`.

## Create A Test

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

Relative paths are created inside `tests/`. These two commands target the same destination:

```console
$ tests/scripts/create-test.py -path arbitrary/new_test.thrust -content 'fn main() s32 @public { return 0; }'
$ tests/scripts/create-test.py -path tests/arbitrary/new_test.thrust -content 'fn main() s32 @public { return 0; }'
```

The destination must use the `.thrust` extension and must stay inside `tests/`.

## Test Conventions

A minimal positive test should return `0` on success:

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

The runner will include that dependency automatically in the compilation pipeline.
