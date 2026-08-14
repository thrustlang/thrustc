<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The LLVM Linker Wrapper

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

There is a simple guide of standard conventions to follow in order to delivery a good Github commit for the LLVM Linker Wrapper (lld-wrapper).

### Title

It needs to be detailed. It can be include a lot of technical slang. The base of a well designed Github commit title always will be and needs a specific syntax as:

#### Title - features

Following the syntax:

`feat(...)`

Valid locations:

- `rust_library` Any location that usually involucrates the Rust library: `src/`, `Cargo.toml`, `Cargo.lock` and `build.rs`.
- `lld_wrapper` Any location that usually involucrates the C++ LLVM Linker (LLD) wrapper: `wrapper/` and the C++ headers located on `include/`.
- `project` Any location that usually involucrates the repository itself: `README.md`, `CITATION.cff`, `LICENSE.txt`, or the conception of a new part of the wrapper repository.

Example:

`feat(rust_library)` Adding the COFF flavor support to the Rust library.

#### Title - fixes

Following the syntax:

`fix(...)`

Valid locations:

- `rust_library` Any location that usually involucrates the Rust library: `src/`, `Cargo.toml`, `Cargo.lock` and `build.rs`.
- `lld_wrapper` Any location that usually involucrates the C++ LLVM Linker (LLD) wrapper: `wrapper/` and the C++ headers located on `include/`.
- `project` Any location that usually involucrates the repository itself: `README.md`, `CITATION.cff`, `LICENSE.txt`, or the conception of a new part of the wrapper repository.

Any consecutive location written to the next one needs to be follow for a COMMA character `,`.

Example:

`fix(lld_wrapper)` Fixing the usage of C++ header files to link correctly the driver at `lld.cpp`.

#### Title - Combinatory

In order to create a well disigned combinatory title, you need to use the following syntax:

`(feat(...), fix(...))`

- It needs to be encapsulated for a pair characters PAREN `()`.
- Each next feature or fix needs to be followed for a COMMA character `,`.

### Description

It needs to be concise, short, but detailed in the same time. It can be include a lot of technical slang.
