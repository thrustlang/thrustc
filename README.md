<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

The **Thrust Compiler** is a compiler that transfers the source code of Thrust files directly to the specified destination. The process includes static type analysis, code generation, destination-specific optimizations, machine-specific code compilation, and finally, emitting or linking.

> [!IMPORTANT]  
> The compiler is in an early development phase. It still hasn't made the major releases.

## Getting Started

### Build dependencies 

Among the dependencies required by the compiler is LLVM infrastructure.

Automatically:

```console
$ git clone --depth=1 https://github.com/thrustlang/compiler-dependency-builder 
$ cd compiler-dependency-builder 
$ cargo build --release
$ ./target/release/compiler-dependency-builder
```

Another way:

```console
$ git clone --depth=1 https://github.com/thrustlang/compiler-dependency-builder 
$ cd compiler-dependency-builder 
$ cargo run 
```

> A high-level command line view localized on: https://github.com/thrustlang/compiler-dependency-builder/blob/master/README.md

You must first clone the repository and access it locally. 

```console
$ git clone --depth=1 https://github.com/thrustlang/thrustc 
$ cd thrustc
```

### Build Cargo dependencies 

Among other obligatory dependencies we need also other libraries.

You must install each Cargo dependency automatically:

#### Bash

```console
sh scripts/cargo-dependencies.sh
```

#### Fish

```console
fish scripts/cargo-dependencies.fish
```

#### CMD

```console
scripts\cargo-dependencies.bat
```

#### PowerShell

```console
.\scripts\cargo-dependencies.ps1
```

### Build the Compiler

Now you need to have Rust installed with a recent version.

- \>= [Rust](https://www.rust-lang.org/) (v1.18.5) 
- Rust 2024 Edition

Now you need to compile the compiler with Rust. 

```console
$ cargo build --release
$ ./target/release/thrustc --help
```

Another way:

```console
$ cargo run -- --help
```

## Cross Compilation

The Thrust Compiler offers powerful cross-compilation support, just like [Clang](https://github.com/llvm/llvm-project/tree/main/clang).

```console
./thrustc main.thrust my_library.thrust \
        -target-triple x86_64-apple-darwin \
        -cpu haswell \
        -opt O3 \
        --macos-version 13.0 
```

You can target macOS, Windows, Linux, and other platforms directly from your current system without needing to switch machines.

## Commands & Flags

If you want to know a high-level command line view, you should check out: [Thrust Compiler - Commands & Flags](https://github.com/thrustlang/thrustc/blob/master/CLI.md)

## Compiler Structure

If you want to know a little about the compiler's structure, you should check out: [Thrust Programming Language - Compiler Structure](https://github.com/thrustlang/blob/master/PROJECT_STRUCTURE.md)

## Syntax 

The language syntax is under construction at the same time as the compiler. It may be outdated compared to the compiler, as the latter progresses more rapidly. This will be normalized once a valid and sufficiently stable beta is released.

[Thrust Programming Language - General Syntax](https://github.com/thrustlang/syntax)

## LLVM

The LLVM backend infrastructure is the default code generator for the [Thrust Programming Language](https://github.com/thrustlang/). It offers full scope and portability across many architectures or targets.

### LLVM Version

- ``17.0.6``
 
#### Why this specific version of LLVM for the compiler?

Between version 16-17, the introduction to the change of typed pointers was made, which are now almost a standard in the backend. 

Some programming languages like Swift tend to use versions lower than 16 of LLVM, for reasons of compatibility with code generation that differs between higher and lower versions of LLVM, and version 16 offers legacy support for languages that need it.

We only need support for C and nothing else. We are not interested in FFI with C++ for the moment, nor in mangling with it either.
17 is enough and from there on.

#### LLVM Targets

Beyond the standard triple targets, the compiler also supports all architectures available through the **[LLVM](https://llvm.org)**-C API. These include:

- ``x86_64``
- ``AArch64``
- ``RISC-V``
- ``ARM``
- ``MIPS``
- ``PowerPC``
- ``SystemZ``
- ``AMDGPU``
- ``Hexagon``
- ``Lanai``
- ``LoongArch``
- ``MSP430``
- ``NVPTX``
- ``SPARC``
- ``XCore``
- ``BPF``
- ``SPIR-V``
- ``WebAssembly``

## GCC

The GCC compiler backend is still under construction.

In the future, you will be able to use it with the ``-gcc-backend`` flag to use the GCC backend code generator instead of the default LLVM one.

However, it is only available on **GNU/Linux**.

You must also have ``libgccjit.so`` dynamically installed in your distribution so that the compiler doesn't get scared at runtime when using GCC.

### Notes

Currently, the very same Rust is using ``libgccjit`` as a library for an AOT backend prototype for Rust. Called ``rustc_codegen_gcc``. Thrust will integrate it in his own way for use in the language.

For more information: [Rust - GCC AOT Code Generation](https://github.com/rust-lang/rustc_codegen_gcc)

# Frequent Questions

#### > Why isn't the compiler designed to use it as a bootstrap compiler?

Regarding the concept of bootstrapping in compilers (For more information: https://www.bootstrappable.org/).

The decision was made to fully implement all the programming language functions in the compiler written in Rust, because it proposes a development approach similar to what Gleam Team did for Gleam Programming Language, and also to lighten the workload, given that we are already using LLVM.

#### > When will this be released?

https://github.com/user-attachments/assets/2cb6a406-eb2d-41d5-b5d8-784074a490d5
