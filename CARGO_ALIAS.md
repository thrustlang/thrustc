<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

These are the available cargo alias in the whole project, to perform either compiler fuzzing or generation of Rust's code documentation.

## Compiler Fuzzing

It means the fuzzer only will perform tests including "stable" features on the compiler.

## Stable Compiler Fuzzing

### LLVM

It fuzz the LLVM backend with supposed stable features.
```console
cargo fuzz-llvm-stable
```

### Pipeline

It fuzz the AST validation pipeline, including semantic analysis with supposed stable features.
```console
cargo fuzz-pipeline-stable
```
## Unstable Compiler Fuzzing

It means the fuzzer only will perform tests including "unstable" features on the compiler.

### LLVM

It fuzz the LLVM backend with supposed unstable features.
```console
cargo fuzz-llvm-unstable
```

### Pipeline

It fuzz the AST validation pipeline, including semantic analysis with supposed unstable features.
```console
cargo fuzz-pipeline-unstable
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