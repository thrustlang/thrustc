<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

**Thrust Compiler** is a compiler that transfers the source code of Thrust files directly to the specified destination. The process includes static type analysis, code generation, destination-specific optimizations, machine-specific code compilation, and finally, emitting or linking.

> [!IMPORTANT]  
> The compiler is in an early development phase. It still hasn't made the major releases.

## Getting Started

## Compiled

If you don't want to build the project from scratch, you can check if there are available versions in Github releases.

[Thrust Compiler - Github Releases](https://github.com/thrustlang/thrustc/releases)

Supported operating systems:

- Windows x64 (MSVC) 
- MacOS x64 (arm)
- MacOS x64 (intel)
- Linux x64 (GNU)

## From Scratch

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

If you want to know a high-level command line view, you should check out: [Compiler Dependency Builder - Commands & Flags](https://github.com/thrustlang/compiler-dependency-builder/blob/master/README.md)

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

Thrust Compiler offers powerful cross-compilation support, just like [Clang](https://github.com/llvm/llvm-project/tree/main/clang).

### RISC-V 64-bit

```console
./thrustc main.thrust my_library.thrust \
  -target-triple riscv64-unknown-linux-gnu \
  -cpu sifive-u74 \
  -opt O3 
```

### WebAssembly

```console
./thrustc main.thrust my_library.thrust \
  -target-triple wasm32-unknown-unknown \
  -opt O3 
```

You can target macOS, Windows, Linux, and other platforms directly from your current system without needing to switch machines.

## Commands & Flags

If you want to know a high-level command line view, you should check out: [Thrust Compiler - Commands & Flags](https://github.com/thrustlang/thrustc/blob/master/CLI.md)

## Compiler Structure

If you want to know a little about the compiler's structure, you should check out: [Thrust Programming Language - Compiler Structure](https://github.com/thrustlang/thrustc/blob/master/PROJECT_STRUCTURE.md)

## Rust Code Documentation

If you want to know regarding Rust code's structure that resides in the compiler, you should check out: [Thrust Programming Language - Rust Code Documentation](https://thrustlang.github.io/thrustc/)

## Cargo Alias

If you require to know the command line shorcuts available in the project across rust-cargo, you should check out: [Thrust Programming Language - Cargo Alias](https://github.com/thrustlang/thrustc/blob/master/CARGO_ALIAS.md)

## License

Thrust Compiler is distributed under the terms of the GNU General Public License (version 3). See LICENSE.txt file for details.

## Q&A

#### > Why isn't the compiler designed to use it as a bootstrap compiler?

Regarding the concept of bootstrapping in compilers (For more information: https://www.bootstrappable.org/).

The decision was made to fully implement all the programming language functions in the compiler written in Rust, because it proposes a development approach similar to what Gleam Team did for Gleam Programming Language, and also to lighten the workload, given that we are already using LLVM.

#### > Agentic AI

No, I don't use it and I never will. This compiler will always have code analyzed, processed, and studied by a human.

I don't care. 
You can't use agentic AI in this project.
