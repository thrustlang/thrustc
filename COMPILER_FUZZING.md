<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

Fuzzing is an important task in order to detect and fix issues could've been found on the compiler, then, you've to select either test with "stable" or "unstable"
features the compiler.

## Stable fuzzing

Stable fuzzing is a predeterminated configured fuzzing suite to only test "stable" features on the compiler.

Cargo's alias:

- `cargo fuzz-llvm-stable` It fuzz the LLVM backend with stable features.
- `cargo fuzz-pipeline-stable` It fuzz the AST validation pipelione with stable features.

## Unstable fuzzing

Unstable fuzzing is a predeterminated configured fuzzing suite to only test "stable" features on the compiler.

Cargo's alias:

- `cargo fuzz-llvm-unstable` It fuzz the LLVM backend with unstable features.
- `cargo fuzz-pipeline-unstable`It fuzz the AST validation pipelione with unstable features.