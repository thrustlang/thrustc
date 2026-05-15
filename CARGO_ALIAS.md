<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

These are the available cargo alias in the project:

### Compiler Fuzzing

It fuzz the LLVM backend, using an inteligent fuzz approach. 
```console
cargo fuzz-llvm
```

It fuzz the AST validation pipeline, including semantic analysis.
```console
cargo fuzz-pipeline
```

### Rust Code Documentations

It compiles and generate the standard rust code documentation along thrustc's codebase.
```console
cargo docs
```

It compiles, generate and open on the browser, the standard rust code documentation along thrustc's codebase.
```console
cargo docs-open
```