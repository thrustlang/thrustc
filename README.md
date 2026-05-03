<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/thrustlang-logo-name.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

The **Thrust Compiler** is a compiler that transfers the source code of Thrust files directly to the specified destination. The process includes static type analysis, code generation, destination-specific optimizations, machine-specific code compilation, and finally, emitting or linking.

> [!IMPORTANT]  
> The compiler is in an early development phase

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

You must first clone the repository and access it locally. 

```console
$ git clone --depth=1 https://github.com/thrustlang/thrustc 
$ cd thrustc
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

## Commands & Flags

A list of the commands supported by the Thrust Compiler command line.

> [!WARNING]  
> This might be a bit outdated, it could be information that's somewhat distant from the changes.

```console
The Thrust Compiler

Usage: thrustc [-flags|--flags] [files..]

General Commands:

• -h, --help optional[opt|emit|print|code-model|
        reloc-model|sanitizer|symbol-linkage-strategy|denormal-floating-point-behavior|
        denormal-floating-point-32-bits-behavior] Show help message.
• -v, --version Show the version.

Linkage flags:

• -clang-link [path/to/clang] Specifies the path for use of an external Clang for linking purpose.
• -gcc-link [path/to/gcc] Specifies GNU Compiler Collection (GCC) for linking purpose.
• -start Marks the start of arguments to the active external or built-in linking compiler.
• -end Marks the end of arguments to the active external or built-in linker compiler.

Compiler flags:

• -build-dir Specify the compiler artifacts directory.
• -tools-dir Specify the compiler tools directory for search tools and expand compiler capatibilities.
• -target [x86_64] Set the target arquitecture.
• -target-triple [x86_64-pc-linux-gnu|x86_64-pc-windows-msvc] Set the target triple. For more information, see 'https://clang.llvm.org/docs/CrossCompilation.html'.
• -cpu [haswell|alderlake|ivybridge|pentium|pantherlake] It specify the CPU to optimize.
• -cpu-enable-features [sse2;cx16;sahf;tbm] It specify to enable certain CPU features to use.
• -cpu-disable-features [sse2;cx16;sahf;tbm] It specify to disable certain CPU features to use.
• -cpu-features [+sse2,+cx16,+sahf,-tbm] It overwrites the CPU features to use.
• -emit [llvm-bc|llvm-ir|asm|unopt-llvm-ir|unopt-llvm-bc|unopt-asm|obj|unchecked-ast|ast|tokens] Compile the code into specified representation.
• -print [llvm-ir|unopt-llvm-ir|asm|unopt-asm|unchecked-ast|ast|tokens] Displays the final compilation on standard output.
• -opt [O0|O1|O2|O3|Os|Oz] Optimization level.
• -jit Enable the use of the JIT compiler for code execution.
• -jit-libc [path/to/libc.so] Specify the C runtime to link for code execution via the JIT compiler.
• -jit-link [path/to/raylib.so] Specify, add, and link an external dynamic library for code execution via the JIT compiler.
• -jit-entry [main] Specify the entry point name for the JIT compiler.
• -dbg Enable generation of debug information (DWARF).
• -dbg-for-inlining Enable debug information specifically optimized for inlined functions.
• -dbg-for-profiling Emit extra debug info to support source-level profiling tools.
• -dbg-dwarf-version [v4|v5] Configure the Dwarf version for debugging purposes.
• --denormal-floating-point-behavior ["IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic"] Configure how denormal floating-point values are handled during calculations.
• --denormal-floating-point-32-bits-behavior ["IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic"] Configure how denormal 32-bit floating-point values are handled during calculations.
• --symbol-linkage-strategy [any|exact|large|samesize|noduplicates] Configure the symbol linkage merge strategy.
• --stack-protector It built a stack state guard that battles memory hacks and prevents memory corruptions.
• --sanitizer [address|hwaddress|memory|thread|memtag] Enable the specified sanitizer. Adds runtime checks for bugs like memory errors, data races and others, with potential performance overhead.
• --no-sanitize [bounds;coverage] Modifies certain code emissions for the selected sanitizer.
• --opt-passes [-p{passname,passname}] Pass a list of custom optimization passes. For more information, see: 'https://releases.llvm.org/17.0.1/docs/CommandGuide/opt.html#cmdoption-opt-passname'.
• --modificator-passes [loopvectorization;loopunroll;loopinterleaving;loopsimplifyvectorization;mergefunctions;callgraphprofile;forgetallscevinloopunroll;licmmssaaccpromcap=0;licmmssaoptcap=0;] Pass a list of custom modificator optimization passes.
• --reloc-model [static|pic|dynamic] Indicate how references to memory addresses and linkage symbols are handled.
• --code-model [small|medium|large|kernel] Define how code is organized and accessed at machine code level.
• --target-triple-darwin-variant [arm64-apple-ios15.0-macabi] Specify the darwin target variant triple.
• --macos-version [15.0.0] Specify the MacOS SDK version.
• --ios-version [17.4.0] Specify the iOS SDK version.
• --enable-ansi-color It allows ANSI color formatting in compiler diagnostics.

Disable compiler flags:

• --disable-frame-pointer Regardless of the optimization level, it omits the emission of the frame pointer.
• --disable-uwtable It omits the unwind table required for exception handling and stack tracing.
• --disable-direct-access-external-data It omits direct access to external data references, forcing all external data loads to be performed indirectly via the Global Offset Table (GOT).
• --disable-rtlib-got It omits the runtime library dependency on the Global Offset Table (GOT), essential when generating non-Position Independent Code (PIC) with ARM.
• --disable-safe-trapping-math It allow trapping math operations that can cause exceptions. Useful for floating-point operations.
• --disable-safe-math Disable safe math for integer operations (allows overflow and undefined behavior).
• --disable-default-optimization It omits default optimization that occurs even without specified optimization.
• --disable-all-sanitizers Disable all sanitizers that may be enabled.
• --disable-all-cpu-features Disable the all CPU features. that may be enabled.

Other compiler flags:

• --copy-output-to-clipboard Copy the total printable output of the compiler into the operating system clipboard. It only works using '-print' compiler flag.
• --debug-clang-command Displays the generated command for Clang in the phase of linking.
• --debug-gcc-command Displays the generated command for GCC in the phase of linking.
• --export-compiler-errors Export compiler error diagnostics to files.
• --export-compiler-warnings Export compiler warning diagnostics to files.
• --export-diagnostics-path [diagnostics/] Specify the path where diagnostic files will be exported.
• --clean-exported-diagnostics Clean the exported diagnostics directory.
• --clean-build Clean the compiler build folder that holds everything.
• --clean-tokens Clean the compiler folder that holds the lexical analysis tokens.
• --clean-assembler Clean the compiler folder containing emitted assembler.
• --clean-llvm-ir Clean the compiler folder containing the emitted LLVM IR.
• --clean-llvm-bitcode Clean the compiler folder containing emitted LLVM bitcode.
• --clean-objects Clean the compiler folder containing emitted object files.
• --no-obfuscate-archive-names Stop generating name obfuscation for each file; this does not apply to the final build.
• --no-obfuscate-ir Stop generating name obfuscation in the emitted IR code.
• --print-targets Show the current target supported.
• --print-supported-cpus Show the current supported CPUs for the current target.
• --print-host-target-triple Show the host target triple.
• --print-opt-passes Show all available optimization passes through '--opt-passes=p{passname, passname}'.
```

## Compiler Structure

If you want to know a little about the compiler's structure, you should check: [Thrust Programming Language - Compiler Structure](https://github.com/thrustlang/blob/master/PROJECT_STRUCTURE.md)

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



