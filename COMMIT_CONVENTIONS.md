<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

There is a simple guide of standard conventions to follow in order to delivery a good Github commit for the Thrust Compiler (**thrustc**).

### Title

It needs to be detailed. It can be include a lot of technical slang. The base of a well designed Github commit title always will be and needs a specific syntax as:

#### Title - features

Following the syntax:

`feat(...)`

Valid locations:

- `llvm_backend` Any location that usually involucrates the LLVM backend.
- `llvm_linker_driver` Any location that usually involucrates the compiler LLVM Linker Driver invocation.
- `gcc_backend` Any location that usually involucrates the GNU Compiler Collection (GCC) backend.
- `frontend` Any locations that usually involucrates the Abstract Syntax Tree (AST), Lexer, Parser, TypeChecker and we could continue...
- `project-visual` Any location that usually involucrates the visual representation or human guide for the compiler available on Github (Example: README.md).
- `project` Any location that usually involucrates Cargo, Rust Compiler and Github repository changes, Github actions or the conception of a new part of the compiler (Cargo Workspaces).

Example:

`feat(llvm_backend)` Adding support for TLS Thread priority.

#### Title - fixes

Following the syntax:

`fix(...)`

Valid locations:

- `llvm_backend` Any location that usually involucrates the LLVM backend.
- `llvm_linker_driver` Any location that usually involucrates the compiler LLVM Linker Driver invocation.
- `gcc_backend` Any location that usually involucrates the GNU Compiler Collection (GCC) backend.
- `frontend` Any locations that usually involucrates the Abstract Syntax Tree (AST), Lexer, Parser, TypeChecker and we could continue...

Any consecutive location written to the next one needs to be follow for a COMMA character `,`.

Example:

`fix(frontend)` Fixing several issues on the abstract syntax analyzer.

#### Title - Combinatory

In order to create a well disigned combinatory title, you need to use the following syntax:

`(feat(...), fix(...))`

- It needs to be encapsulated for a pair characters PAREN `()`.
- Each next feature or fix needs to be followed for a COMMA character `,`. 

### Description

It needs to be concise, short, but detailed in the same time. It can be include a lot of technical slang.







