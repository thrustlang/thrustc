# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [Unreleased]

### Bug Fixes
- **project-visual**: `fix(project-visual)` Fixing broken links on README.md ([`7f85246`](https://github.com/thrustlang/thrustc/commit/7f85246758d1e60bb2be0d73bda473e1aa77c1d2))


### Documentation
- **project-visual**: `feat(project-visual)` Adding visual examples of compiler diagnostics. ([`f2950fa`](https://github.com/thrustlang/thrustc/commit/f2950fa51ff1f1bcd8a3cbcd32f9f44dfaf9d6fc))


## [thrustc-x86_64-linux-ubuntu-v0.1.3] - 2026-06-06

### Bug Fixes
- `fix(llvm_codegen)` Adding LValue changes on the System-V call epilogue. ([`24eeffc`](https://github.com/thrustlang/thrustc/commit/24eeffcafdf7362a1ab5e130b91544af49e77cd3))
- **frontend**: `fix(frontend)` I fixed the rare issue case when the source isn't a pointer without metadata and the provided it is, typechecking. ([`0c2dd24`](https://github.com/thrustlang/thrustc/commit/0c2dd2496d702ec02a4ab95f6491a03a2f567117))
- **project-visual**: `fix(project-visual)` Fixing a type on README.md ([`c1ae1de`](https://github.com/thrustlang/thrustc/commit/c1ae1de77a18df848ca5409b3b7d9694e31f1778))
- **frontend**: `(fix(llvm_codegen), fix(frontend))` Several fixes around type casting on the LLVM backend, and also, on the compiler frontend, a better understable naming on variables, types, etc. ([`d46543d`](https://github.com/thrustlang/thrustc/commit/d46543d26ce77118ea2c5774b2217666665241e4))
- `fix(llvm_codegen)` I issues regarding integer and float literal generation on the LLVM backend. ([`ee7a3e3`](https://github.com/thrustlang/thrustc/commit/ee7a3e3e9350c5469d9f3eed64f661cc5fe43e9d))
- `fix(llvm_codegen)` I fixed some several issues found regarding void type on ABI type generation. ([`3248b84`](https://github.com/thrustlang/thrustc/commit/3248b8400303c5d086d354fb053495c082614c9c))
- `fix(llvm_codegen)` Fixing a serious issue inside integer and float generation, and also, integrating partial return clasification to System-V. ([`f9ca954`](https://github.com/thrustlang/thrustc/commit/f9ca954588621235cb872330e502e1d57ea76b60))
- `fix(llvm_codegen)` hot fix to x86 System-V ABI type classification and, also I fixed an issue on call lowering. ([`ddd5c17`](https://github.com/thrustlang/thrustc/commit/ddd5c176807fa7a3614d43059d1c66e41ed9b4c0))
- **project-visual**: `fix(project-visual)` I fixed some typos on README.md ([`e559559`](https://github.com/thrustlang/thrustc/commit/e55955925ad0c4bbd9262fe388d9143c13643810))
- **project-visual**: `fix(project-visual)` Removing automatic cargo docs. ([`caa4d5e`](https://github.com/thrustlang/thrustc/commit/caa4d5eb9ad8fd4c4c64a57ab9193fd55900f815))
- **project**: `fix(project)` I fixed a possible bad utilization regarding boolean binary operations on expression compiling on the LLVM backend. ([`ebe0359`](https://github.com/thrustlang/thrustc/commit/ebe035967fae119a69fb85766aebfd42604a39f0))


### Documentation
- **project-visual**: `feat(project-visual)` Adding another explanation to utilize the compiler compiled. ([`9d02371`](https://github.com/thrustlang/thrustc/commit/9d023716a4ed64853ec457854b18e6f8a2a0b40f))
- **project-visual**: `feat(project-visual)` Adding extra clarifications regarding AI usage. ([`de12e8f`](https://github.com/thrustlang/thrustc/commit/de12e8f5053615596f0b85c255c3ac54fd200420))
- **project-visual**: `(feat(project), feat(project-visual))` Adding LLVM_CODEGEN fuzzing target and, also, a guide regarding Cargo alias. ([`d200be5`](https://github.com/thrustlang/thrustc/commit/d200be5c0e0e597251927ebd5748137b0c89350d))
- **project-visual**: `feat(project-visual)` Adding rust-code documentation via cargo docs. ([`1e64ded`](https://github.com/thrustlang/thrustc/commit/1e64ded75fd3710b9ce166d57264e8a426083fa9))


### Features
- **frontend**: `(feat(frontend), fix(llvm_codegen), fix(frontend))` I fixed some issues found on the call lowering and standard an ABI mode. Also I implemented the CudaCaller on Thrust. ([`132535b`](https://github.com/thrustlang/thrustc/commit/132535b5a611fe84e9596d89fb3a6fe76ced5858))
- `feat(llvm_codegen)` Fixed issues regarding to integer and boolean types on code generation. ([`d787416`](https://github.com/thrustlang/thrustc/commit/d787416fbd656b93d6076da412bca358f50b9c24))
- **frontend**: `(feat(llvm_codegen), feat(frontend))` Adding address_space cast on LLVM codegen phase and support to Nvidia Cuda. ([`3b2cf2e`](https://github.com/thrustlang/thrustc/commit/3b2cf2edd7e67aaa461f738093048aae8d201905))
- **frontend**: `(feat(frontend), feat(llvm_codegen))` Preparing Cuda code generation support, also, integrating `@cuda` attributee, and pointer address_space specifier. ([`bf07e12`](https://github.com/thrustlang/thrustc/commit/bf07e123c1b3356ef5361b1c5ecc782e6dc2a503))
- `feat(llvm_codegen)` Adding array values lowering for System V ABI ([`a56d2c1`](https://github.com/thrustlang/thrustc/commit/a56d2c114f26736853987563149d7eb926dfe9e8))
- **frontend**: `feat(frontend)` Adding syntax highlighting for LLVM IR print. ([`be40909`](https://github.com/thrustlang/thrustc/commit/be40909629010edea74384cbd11a1fe44705e51a))
- `feat(llvm_codegen)` Adding System-V ABI lowering to functions calls. ([`1e1ed8e`](https://github.com/thrustlang/thrustc/commit/1e1ed8eef7800adfce16eeb826e383c5a6281350))
- `feat(llvm_codegen)` Adding more support regarding System-V ABI on the compiler and LLVM code generation. ([`c2d6176`](https://github.com/thrustlang/thrustc/commit/c2d6176a9bb6062bdac4eebf8f9cdd700c19b983))
- `feat(llvm_codegen)` Adding more support to x86 System V ABI. ([`f0259f0`](https://github.com/thrustlang/thrustc/commit/f0259f0fc199ca8f96a8cff148a32daddfcf926b))
- **frontend**: `(feat(project), feat(frontend))` Adding `stop-at` flag to stop the compilation at specific phase, and also integrating `git-cliff` to the project. ([`b0ef464`](https://github.com/thrustlang/thrustc/commit/b0ef4641b2e8b47f00edb799e0e073c75a513f4f))


### Project
- **project**: `feat(project)` Bumping Linux x86_64 Ubuntu v0.1.3 changelog. ([`a96d1d8`](https://github.com/thrustlang/thrustc/commit/a96d1d8f2a3cd77ee143e1a405e5966e35a4c872))
- **project**: `feat(project)` Bumping v0.1.3 ([`ccdf70d`](https://github.com/thrustlang/thrustc/commit/ccdf70d5e820baa727fdc1e65763061e3e3856d3))
- **project**: `(feat(llvm_codegen), feat(project))` Adding full System-V ABI implementation, and integrating control via compiler CLI. ([`d5dfaf2`](https://github.com/thrustlang/thrustc/commit/d5dfaf24276ee83fb5f8fc378552d368c3921067))
- **project**: `feat(project)` Adding automatic changelog generation and also, integrating as a all-in-one script `deploy-version.X` on each platform. ([`e7fb70e`](https://github.com/thrustlang/thrustc/commit/e7fb70e8f3c507e417cc4dc18556a01cd2a0af0f))
- **project**: `feat(project)` Adding automatic rust code docs. ([`e1f3a29`](https://github.com/thrustlang/thrustc/commit/e1f3a29a3cbfd1f94c13af4468ca1ba2f118eec4))
- **project**: `feat(project)` Adding MacOS support on Rust Toolchain. ([`84f4939`](https://github.com/thrustlang/thrustc/commit/84f49392c6c29f51f9119056a957e04a15a4ab42))


## [thrustc-x86_64-macos-v0.1.1] - 2026-05-11

### Bug Fixes
- **project**: `fix(project)` Adding thrustc-x86_64-macos-v0.1.1 Changelog. ([`743024f`](https://github.com/thrustlang/thrustc/commit/743024ff2225a75c298ed8a3e96aefe1829e8133))
- **project**: `fix(project)` Migrating x86_64 macOS build to macos-15-intel ([`cade0d2`](https://github.com/thrustlang/thrustc/commit/cade0d25d02024ab08f15ff772aeba398d23256d))
- **project**: `fix(project)` I fixed an issue found on the Generating Unique ID name on the aarch64 MacOS action. ([`8a35452`](https://github.com/thrustlang/thrustc/commit/8a35452ddd8340eeed77f80ca8bbc64b480b3653))
- **project-visual**: `fix(project-visual)` Reducing the amount of cross-compilation examples on README.md ([`a24c0e1`](https://github.com/thrustlang/thrustc/commit/a24c0e1f0a89b71441d995d8cc0ec2fe77b5538b))
- **project**: `fix(project)` I fixed some misunderstanding on the latest MacOS release. ([`d27f8bd`](https://github.com/thrustlang/thrustc/commit/d27f8bdf92a3641c4f60e8dbea0d6c00f77219f5))


### Documentation
- **project-visual**: `feat(project-visual)` Adding more example of cross-compilation. ([`08fbc44`](https://github.com/thrustlang/thrustc/commit/08fbc442c9be14088e72c27b8e967beb52b4bdba))
- **project-visual**: `feat(project-visual)` Adding more cross-compilation examples. ([`7b3190c`](https://github.com/thrustlang/thrustc/commit/7b3190c0c85133f5e0befe1a7fe44f916dc8a412))


---
*Generated by [git-cliff](https://github.com/orhun/git-cliff)*

## Command Line
```console
The Thrust Compiler

Usage: thrustc [-flags|--flags] [files..]

General Commands:

• -h, --help optional[opt|emit|print|code-model|
	reloc-model|sanitizer|symbol-linkage-strategy|
	denormal-floating-point-behavior|
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
• -emit [llvm-bc|llvm-ir|asm|unopt-llvm-ir|unopt-llvm-bc|unopt-asm|obj|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens] Compile the code into specified representation.
• -print [llvm-ir|unopt-llvm-ir|asm|unopt-asm|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens] Displays the final compilation on standard output.
• -opt [O0|O1|O2|O3|Os|Oz] Optimization level.
• -stop-at [lexing|parsing|scope-analysis|ast-verification|type-checking|general-analysis|attribute-checking|linter|compiler-intrinsic-checking|compiler-callconventions-checking|codegen] Stop the compilation at specific stage.
• -reloc-model [static|pic|dynamic] Indicate how references to memory addresses and linkage symbols are handled.
• -code-model [small|medium|large|kernel] Define how code is organized and accessed at machine code level.
• -macos-version [15.0.0] Specify the MacOS SDK version.
• -ios-version [17.4.0] Specify the iOS SDK version.
• -cuda-version [2.0] Specify the Nvidia CUDA version.
• -jit Enable the use of the JIT compiler for code execution.
• -jit-libc [path/to/libc.so] Specify the C runtime to link for code execution via the JIT compiler.
• -jit-link [path/to/raylib.so] Specify, add, and link an external dynamic library for code execution via the JIT compiler.
• -jit-entry [main] Specify the entry point name for the JIT compiler.
• -abi [system-v] Configure the use of a specific ABI (Application Binary Interface) for code generation. This can affect how functions are called, how data is passed, and how the generated code interacts with other libraries and system components.
• -dbg Enable generation of debug information (DWARF).
• -dbg-for-inlining Enable debug information specifically optimized for inlined functions.
• -dbg-for-profiling Emit extra debug info to support source-level profiling tools.
• -dbg-dwarf-version [v4|v5] Configure the Dwarf version for debugging purposes.
• --disable-abi Disable the ABI detection and utilization, which may lead to less optimized code but can be useful for debugging or targeting non-standard environments.
• --denormal-floating-point-behavior ["IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic"] Configure how denormal floating-point values are handled during calculations.
• --denormal-floating-point-32-bits-behavior ["IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic"] Configure how denormal 32-bit floating-point values are handled during calculations.
• --symbol-linkage-strategy [any|exact|large|samesize|noduplicates] Configure the symbol linkage merge strategy.
• --stack-protector It built a stack state guard that battles memory hacks and prevents memory corruptions.
• --sanitizer [address|hwaddress|memory|thread|memtag] Enable the specified sanitizer. Adds runtime checks for bugs like memory errors, data races and others, with potential performance overhead.
• --no-sanitize [bounds;coverage] Modifies certain code emissions for the selected sanitizer.
• --opt-passes [-p{passname,passname}] Pass a list of custom optimization passes. For more information, see: 'https://releases.llvm.org/17.0.1/docs/CommandGuide/opt.html#cmdoption-opt-passname'.
• --modificator-passes [loopvectorization;loopunroll;loopinterleaving;loopsimplifyvectorization;mergefunctions;callgraphprofile;forgetallscevinloopunroll;licmmssaaccpromcap=0;licmmssaoptcap=0;] Pass a list of custom modificator optimization passes.
• --target-triple-darwin-variant [arm64-apple-ios15.0-macabi] Specify the darwin target variant triple.
• --enable-ansi-color It allows ANSI color formatting in compiler diagnostics.

Disable compiler flags:

• --disable-frame-pointer Regardless of the optimization level, it omits the emission of the frame pointer.
• --disable-uwtable It omits the unwind table required for exception handling and stack tracing.
• --disable-direct-access-external-data It omits direct access to external data references, forcing all external data loads to be performed indirectly via the Global Offset Table (GOT).
• --disable-rtlib-got It omits the runtime library dependency on the Global Offset Table (GOT), essential when generating non-Position Independent Code (PIC) with ARM.
• --disable-safe-trapping-math It allow trapping math operations that can cause exceptions. Useful for floating-point operations.
• --disable-safe-math Disable safe math for integer operations (allows overflow and undefined behavior).
• --disable-default-optimization It omits default optimization that occurs even without specified optimization.
• --disable-all-sanitizers Disable all sanitizers.
• --disable-all-cpu-features Disable the all CPU features.

Warning compiler flags:

• --disable-all-warnings Disable all the general and specific warnings.

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
