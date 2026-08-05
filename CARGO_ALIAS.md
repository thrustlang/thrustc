<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

These are the available cargo alias in the whole project, to perform either compiler fuzzing or generation of Rust's code documentation.

## Stable Compiler Fuzzing

### LLVM (Top-Level)

It fuzz the LLVM backend with supposed stable features (top-level codegen).
```console
cargo fuzz-llvm-top-level-stable
```

### LLVM (Local)

It fuzz the LLVM backend with supposed stable features (local codegen).
```console
cargo fuzz-llvm-local-stable
```

### LLVM (Local, Loops)

It fuzz the LLVM backend with supposed stable features (local codegen, focused on loops).
```console
cargo fuzz-llvm-local-loops-stable
```

### Pipeline

It fuzz the AST validation pipeline, including semantic analysis with supposed stable features.
```console
cargo fuzz-pipeline-stable
```

## Unstable Compiler Fuzzing

### LLVM (Top-Level)

It fuzz the LLVM backend with supposed unstable features (top-level codegen).
```console
cargo fuzz-llvm-top-level-unstable
```

### LLVM (Local)

It fuzz the LLVM backend with supposed unstable features (local codegen).
```console
cargo fuzz-llvm-local-unstable
```

### LLVM (Local, Loops)

It fuzz the LLVM backend with supposed unstable features (local codegen, focused on loops).
```console
cargo fuzz-llvm-local-loops-unstable
```

### Pipeline

It fuzz the AST validation pipeline, including semantic analysis with supposed unstable features.
```console
cargo fuzz-pipeline-unstable
```

## Lexer Fuzzing

It fuzz the lexer with a universal corpus and stable dictionary.
```console
cargo fuzz-lexer
```

## Auxiliary Binaries

These helpers inspect a fuzzer crash artifact (raw bytes input) without running the fuzzer.

### AST dump (Top-Level)

Reconstructs and prints the AST using the `Arbitrary` trait. Matches the `pipeline` fuzzer.
```console
cargo fuzz-dump-ast-top-level <crash-file>
```

### AST dump (Local)

Reconstructs and prints the AST using the scoped AST generator. Matches the `llvm-codegen-local` fuzzer.
```console
cargo fuzz-dump-ast-local <crash-file>
```

### AST dump (Local, Loops)

Same as `fuzz-dump-ast-local` but for the loop oriented AST generator. Matches the `llvm-codegen-local-loops` fuzzer.
```console
cargo fuzz-dump-ast-local-loops <crash-file>
```

### LLVM IR dump

Reconstructs the AST with a scoped generator, runs semantic analysis + LLVM codegen and dumps the module IR to `fuzz/llvm_ir_dumps/`. It does **not** run `module.verify()`, so the IR is dumped even when it is invalid. Optional `generator` (`llvm-codegen-local` by default, or `llvm-codegen-local-loops`) and `--stable` flag are supported.
```console
cargo fuzz-dump-llvm-ir <crash-file> [generator] [--stable]
```

### Crash reproduction

Automates crash reproduction. Rebuilds and runs the target fuzzer against the crash artifact and classifies the result against known crash signatures. Run without arguments to get an interactive selection menu.
```console
cargo fuzz-reproduce-case [<target> <artifact>]
```

-------------------------------------------------

## Rust Code Documentation

It compiles and generate the standard rust code documentation along thrustc's codebase.
```console
cargo docs
```

It compiles, generate and open on the browser, the standard rust code documentation along thrustc's codebase.
```console
cargo docs-open
```