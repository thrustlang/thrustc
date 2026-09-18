<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler Tests

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

The `tests/` directory contains executable and negative Thrust source tests used to validate the compiler pipeline end to end.

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
$ tests/scripts/run-tests.py --no-build --filter "basics/test.thrust"
```

Use a custom compiler binary:

```console
$ tests/scripts/run-tests.py --compiler target/release/thrustc --filter "std/io"
```

Forward extra arguments to the external C linker compiler:

```console
$ tests/scripts/run-tests.py --no-build --cc-args "-lz"
```

## Runner Behavior

The runner discovers test roots by scanning `.thrust` files under `tests/` and selecting files that declare `fn main`.

For each discovered test, the runner:

- Resolves user imports written as `import "file.thrust"` recursively.
- Adds imported user modules to the same compiler invocation as the root test.
- Lets `thrustc` resolve `std::...` imports internally.
- Adds `-lm` automatically for tests that use `std::math`, `std::ffi::c::math`, or floating-point modulo operations.
- Compiles into `tests/dist/bin/<test-id>`.
- Stores build artifacts under `tests/dist/build/<test-id>`.
- Runs the binary from `tests/dist/run/<test-id>` so temporary runtime files stay isolated.
- Removes `tests/dist/` when finished, unless `--keep-dist` is used.

The final executable path is passed through `-cc-args` because the current linker flow receives output flags through the external linker arguments.

## Test Results

Positive tests are expected to compile, link, run, and return exit code `0`.

Negative tests are expected to fail during compilation and are not executed.

Negative tests are detected by naming convention:

- `_invalid`
- `_error`
- `_unknown`
- `_duplicated`
- `_duplicate`
- `duplicate_`
- `_inactive`
- `invalid_`

Some legacy tests intentionally return a non-zero value as the observed result. Those expected exit codes are registered inside `tests/scripts/run-tests.py`.

## Skipped Tests

The runner currently skips these tests explicitly:

- `stress/stress_test_80k.thrust`
- `load/load_index.thrust`
- `module_reexportation/std_reexport.thrust`
- `imports/collision_qualified_invalid.thrust`
- `imports/only_then_full.thrust`

These files remain in the repository, but are not part of the automated run.

## Create A Test

Create a test with literal content:

```console
$ tests/scripts/create-test.py -path basics/new_test.thrust -content 'fn main() s32 @public { return 0; }' --append-newline
```

Create a test from a template file:

```console
$ tests/scripts/create-test.py -path basics/new_test.thrust -content path/to/template.thrust
```

Overwrite an existing test:

```console
$ tests/scripts/create-test.py -path basics/new_test.thrust -content path/to/template.thrust --force
```

Relative paths are created inside `tests/`. These two commands target the same destination:

```console
$ tests/scripts/create-test.py -path basics/new_test.thrust -content 'fn main() s32 @public { return 0; }'
$ tests/scripts/create-test.py -path tests/basics/new_test.thrust -content 'fn main() s32 @public { return 0; }'
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

Use `_invalid` or another negative marker in the file name when the test is expected to fail at compile time.

If a test imports another user file, keep the import relative to the importing file:

```thrust
import "module.thrust";
```

The runner will include that dependency automatically in the compilation pipeline.