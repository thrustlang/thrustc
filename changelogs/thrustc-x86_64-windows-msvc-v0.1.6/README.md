# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [thrustc-aarch64-macos-v0.1.6] - 2026-08-05

### Bug Fixes
- **project-visual**: Fix(project-visual) Updating command line overview to compiler v0.1.6 ([`27d6460`](https://github.com/thrustlang/thrustc/commit/27d6460289076383a09fcd82fe5b83b219827233))


## [thrustc-x86_64-linux-ubuntu-v0.1.6] - 2026-08-05

### Bug Fixes
- **llvm_backend**: (feat(fuzzing),fix(llvm_backend)) Adding support for dumping unverified LLVM IR, and fixing various issues on the llvm backend. ([`2dea432`](https://github.com/thrustlang/thrustc/commit/2dea4320000991a4f2f4ee7ee281fec6948ec8c4))
- **project-visual**: Fix(project-visual) Being more clear with the separator in a flag specification through the CLI. ([`f4f1af1`](https://github.com/thrustlang/thrustc/commit/f4f1af1c171a2ca18af9f09d5c4fe66fe0500e5d))
- **project-visual**: Fix(project-visual) Changing the not about nigtly to an actual github note. ([`ca68526`](https://github.com/thrustlang/thrustc/commit/ca68526409c8ffec83764b53b84df44f3079bfb0))
- **project-visual**: Fix(project-visual) Removing redundancy on README.md

Removed the article 'The' from the title and adjusted the description formatting. ([`8bb9367`](https://github.com/thrustlang/thrustc/commit/8bb9367d662ad159ccf2c2c99aa7d756578735b7))
- **project**: Fix(project) Switching the lang logo to the new one. ([`efc8f0a`](https://github.com/thrustlang/thrustc/commit/efc8f0a5e12573001777b6fdb35730744a5cdb1e))
- **project-visual**: Fix(project-visual) Fixing a typo structure on the compiler host platform on PROJECT_STRUCTURE.md. ([`585a63f`](https://github.com/thrustlang/thrustc/commit/585a63f71a1c28a8d964c8b3d1602297b6513f89))
- Fix(llvm_codegen) A fix on `type_cast::` when provided is float and the target is int. Also, integrating fixes on attribute checking for functions. ([`db88ea2`](https://github.com/thrustlang/thrustc/commit/db88ea2a900ca9ef35473202164c9ec874b45bea))
- **project**: Fix(project) Adjusting the fuzzing program to a fair depth for testing purposes. ([`b83c1c7`](https://github.com/thrustlang/thrustc/commit/b83c1c7bff1aaa382f7f06403dce4708b70d208c))
- Fix(llvm_codegen) Issues regarding atomic ordering instruction selection was fixed for load instruction only. ([`8e1dc87`](https://github.com/thrustlang/thrustc/commit/8e1dc876638411dbfa8f0ab157ae5fbd7e4ccb55))
- **frontend**: Fix(frontend) Renaming files on typechecker for better understanding. ([`4dd6655`](https://github.com/thrustlang/thrustc/commit/4dd6655c3b1d2705478dc7f033798396fd2594ff))
- **frontend**: (fix(llvm_codegen),fix(frontend)) Mantissa type fix on System-V lowering and adding a better support for detection of unsupported types on typechecking phase. ([`8ad7ffb`](https://github.com/thrustlang/thrustc/commit/8ad7ffb1694b60b1a9b8e12dc1dd72ed3e8a1cee))
- **frontend**: (fix(llvm_codegen),fix(frontend)) SSA type selection for literals integer type was fixed, also, integrating a scoper that analyzes the name references. ([`6c45007`](https://github.com/thrustlang/thrustc/commit/6c450076b1aaae9061d30f70114bae976b86a0dd))
- **frontend**: Fix(frontend) Fixing memory alignment issue detection on the compiler frontend, via attribute checker. ([`f3e04e6`](https://github.com/thrustlang/thrustc/commit/f3e04e6f84f7a75efdfb60d4924c872c1beb4503))
- **project**: Fix(project) Improving the fuzz suite and, fixing many bugs across llvm_codegen and semantic analysis using fuzzing. ([`e68f265`](https://github.com/thrustlang/thrustc/commit/e68f2653df7cb60fc19ff4e7a40492fdea7328a5))
- **project**: Fix(project) Matching pattern issues on git-cliff. ([`53af31f`](https://github.com/thrustlang/thrustc/commit/53af31fbe26eee278e0377aecd8c76ca6a5c0fa9))
- **project-visual**: Fix(project-visual) Trying to being more clearly on the compiler fuzzing explanations. ([`4becd7c`](https://github.com/thrustlang/thrustc/commit/4becd7c25d412cbaf45f8644856e56ae984836d1))


### Documentation
- **project-visual**: Feat(project-visual) Adding missing useful markdown guides on the principal README. ([`441376f`](https://github.com/thrustlang/thrustc/commit/441376f3e957ad0b223b8f74c72e024bcc7f5555))
- **project-visual**: Feat(project-visual) Adding 'fuzzing' as a valid commit convention label, for changes on the fuzzing suite. ([`6929657`](https://github.com/thrustlang/thrustc/commit/6929657bc16bf167c7ed88c5136a3dc877f67114))
- **project-visual**: Feat(project-visual) Integrating a comprehensive guide to deploy a version of the compiler. ([`2e9389a`](https://github.com/thrustlang/thrustc/commit/2e9389a5b18e1a4a19b122910d1ea6e29f2be537))
- **project-visual**: Feat(project-visual) Adding more compiler fuzzing workflow explanation. ([`8329c91`](https://github.com/thrustlang/thrustc/commit/8329c910df1b139859fe7d95e5d77ab0c75cf8f2))
- **project-visual**: Feat(project-visual) Clarifying Getting Started section on README.md ([`610ab48`](https://github.com/thrustlang/thrustc/commit/610ab4879824f72867086733e0c9cc204b4053ac))
- **project-visual**: Feat(project-visual) Adding a License section in README.md

Added license information for the Thrust Compiler. ([`f1c0dab`](https://github.com/thrustlang/thrustc/commit/f1c0dab86873e29e08272a4aef2129ddfcc670e7))
- **project-visual**: Feat(project-visual) Updating PROJECT_STRUCTURE.md ([`826f541`](https://github.com/thrustlang/thrustc/commit/826f5416f61b2dc3cf8cb20de8940f14b74286df))
- **project-visual**: Feat(project-visual) Updating CARGO_ALIAS.md and COMPILER_FUZZING with updated information. ([`6e662dc`](https://github.com/thrustlang/thrustc/commit/6e662dc238c916177db71dc00e34a5fd78ae4a54))
- **project-visual**: Feat(project-visual) Adding linker resources. ([`b526bc2`](https://github.com/thrustlang/thrustc/commit/b526bc2f5f36c2dc74d1fbfcc6e59dd596874529))
- **project-visual**: Feat(project-visual) Adding llvm resources for future development. ([`8b28219`](https://github.com/thrustlang/thrustc/commit/8b28219e2226fe3ed712bbd27efccc9acf45499a))


### Features
- **llvm_linker_driver**: Feat(llvm_linker_driver) Adding parallelism search to the finders of C runtime, LibGCC and Dynamic Linker to speed up the whole search process. ([`4a6b5f3`](https://github.com/thrustlang/thrustc/commit/4a6b5f3a5e7ea14d0bda7ef77c9bc5599c19253a))


### Project
- **project**: Feat(project) Adding scripts that create the necessary corpus folders for the fuzzer. ([`ac19d89`](https://github.com/thrustlang/thrustc/commit/ac19d8952c4bedd3a53d90f7b4d5e8dbbc7a5498))
- **project**: Feat(project) Adding decoration to github actions README releases. ([`13a14a3`](https://github.com/thrustlang/thrustc/commit/13a14a3cc3df0f5b388f26c23cf9cb337d91d9ac))
- **project**: Feat(project) Introducing development versioning to the compiler build. ([`e74850f`](https://github.com/thrustlang/thrustc/commit/e74850fa2b2aa0317d46ca259e7b33f865df4aa1))
- **project**: Feat(project) Adding 'stable' and 'unstable' fuzzing categorization. ([`1c04d02`](https://github.com/thrustlang/thrustc/commit/1c04d024183e2197589a0ab3241fc6d134e21742))
- **project**: Feat(project) Adding more resources ABI & LLVM stuff  for future development. ([`5209376`](https://github.com/thrustlang/thrustc/commit/520937661ad6986c049fc27cc2ac2ae54d11e785))
- **project**: Feat(project) Adding pure tmLanguage for syntax highlighting ([`19e14ef`](https://github.com/thrustlang/thrustc/commit/19e14efea2ab27eb355f0e94dac571edf06394a4))


## [thrustc-aarch64-macos-v0.1.5] - 2026-07-12

---
*Thrust Compiler (thrustc) Changelog*

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

• -link-with-clang [path/to/clang] Specifies the path for use of an external Clang for linking purpose.
• -link-with-gcc [path/to/gcc] Specifies GNU Compiler Collection (GCC) for linking purpose.
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
• -abi [system-v|cuda] Configure the use of a specific ABI (Application Binary Interface) for code generation. This can affect how functions are called, how data is passed, and how the generated code interacts with other libraries and system components.
• -mode [stable|unstable] Enable or disable compiler features to limit to stable features only or add support to unstable features.
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
• --dump-compiler-version It writes the compiler version into flat .txt file, named as 'COMPILER_VERSION.txt'.
• --no-obfuscate-archive-names Stop generating name obfuscation for each file; this does not apply to the final build.
• --no-obfuscate-ir Stop generating name obfuscation in the emitted IR code.
• --print-targets Show the current target supported.
• --print-supported-cpus Show the current supported CPUs for the current target.
• --print-host-target-triple Show the host target triple.
• --print-opt-passes Show all available optimization passes through '--opt-passes=p{passname, passname}'.
```
