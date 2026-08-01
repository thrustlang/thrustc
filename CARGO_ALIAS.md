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