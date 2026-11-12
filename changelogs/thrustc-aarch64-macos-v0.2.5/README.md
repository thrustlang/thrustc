# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [thrustc-x86_64-windows-msvc-v0.2.5] - 2026-11-12

### Refactoring
- **fuzzing**: (refac(doc), refac(project-visual), refac(fuzzing)) Renaming the compiler guides and fixing their links

- Renaming the compiler guides to drop the COMPILER_ prefix

- Updating the README and continuous fuzzing links to the new names

- Rewriting the releasing guide around the current scripts and workflows ([`1747658`](https://github.com/thrustlang/thrustc/commit/1747658adb05fbb0fd12c90d4fcfa5057aefab1f))


## [thrustc-x86_64-linux-ubuntu-v0.2.5] - 2026-11-09

### Bug Fixes
- **llvm_backend**: (fix(project), fix(llvm_backend), fix(lsp)) Removing vulnerable transitive dependencies and the unused clipboard feature ([`69722de`](https://github.com/thrustlang/thrustc/commit/69722def649d580ff1c6ea28b34e2b8a85d7170e))
- **project-visual**: Fix(project-visual) Removing unncessary signs ([`9a53db2`](https://github.com/thrustlang/thrustc/commit/9a53db267996c0a4c4c6d28376ef94c9704f2d17))


### Features
- **frontend**: (feat(frontend), fix(frontend), feat(project), refac(frontend))

Expand importC macro coverage and fix unary, string and array lowering. ([`e7ba9e7`](https://github.com/thrustlang/thrustc/commit/e7ba9e775c2f156d76860cbd76a6aa09a4701a69))
- **std**: (feat(std),feat(project)) Updating to v0.2.5 ([`7b289b7`](https://github.com/thrustlang/thrustc/commit/7b289b7a1f396f7848563e9963f6718c80406ba0))
- **frontend**: (feat(frontend), feat(project), refac(project), feat(preprocessador), doc)

Add complete importC macro expansion, C symbol fallback and the high-level macro API. ([`d7f1c50`](https://github.com/thrustlang/thrustc/commit/d7f1c50f43a4a03e146d0ff9281eedfeb7e6be4f))
- **frontend**: (feat(frontend), feat(project)) Add full C statements, variables and indexing to macro expansion

Support declarations, variables, switch, return/break/continue, initializers and type-aware indexing in C macros. Expand context-sensitive macros inline, track struct fields and value shapes, reject goto with a specific diagnostic, and add 56 multiline macro tests. ([`a38ff26`](https://github.com/thrustlang/thrustc/commit/a38ff268fe2bda7f631e3baf8486ee46a43288c6))
- **frontend**: (feat(frontend), feat(llvm_backend), feat(fuzzing), feat(project), feat(doc)) Add raw assembler blocks, NativeVector operations and unstable lexer diagnostics

Unify asm fn/asm value/global_asm around #asm blocks, add element-wise NativeVector operations with constant support and 50 tests, gate unstable lexemes behind -mode unstable with E0001 help, and update fuzzing, tests and documentation. ([`60a6561`](https://github.com/thrustlang/thrustc/commit/60a6561dac67b908c057fbb078120b762824e9f6))


### Project
- **project**: (feat(project), refac(project), feat(doc)) Notify website after compiler releases and remove Windows LLVM diagnostics ([`500756c`](https://github.com/thrustlang/thrustc/commit/500756c1d81dfa4c05fed12fa564c666f5c08af3))
- **project**: Feat(project) Adding enhancement template. ([`c4819a8`](https://github.com/thrustlang/thrustc/commit/c4819a8337680dc13f35c0ef26cfce0fde8df085))
- **project**: Feat(project) Update issue templates to be up to date ([`d6ec391`](https://github.com/thrustlang/thrustc/commit/d6ec3914d09ba3b066d1ebc553f61808c9a8fbb0))


### Refactoring
- **frontend**: (refac(frontend), refac(doc), refac(project)) Renaming get_span to get_code_location and tightening the contributing guide

- Renaming get_span to get_code_location across the AST, type system, token, codegen and ABI crates

- Humanizing CONTRIBUTING.md and updating the accessor example

- Normalizing whitespace in the release and tag scripts ([`2dad5c1`](https://github.com/thrustlang/thrustc/commit/2dad5c15b03be88415eb10da9749365ff08d6bb5))
- **frontend**: (refac(frontend),refac(preprocessador)) Flattening header resolution and moving shared helpers into AST traits

- Flattening nested header lookup in import_c

- Renaming Generic*Entry, ExternalSymbolTable and imported_scope module

- Moving contains_deferred_builtin into AstBuiltinExtensions

- Commenting generic instantiation key and FNV-1a hash ([`f47d962`](https://github.com/thrustlang/thrustc/commit/f47d962d76dc39d81a2845d72e3719a74f485027))
- **project-visual**: (refac(project-visual), refac(doc), refac(lsp)) Rewriting user guides to remove repetitive and promotional prose ([`2be06b2`](https://github.com/thrustlang/thrustc/commit/2be06b2c84ede06e5e208909106737c3a5ef9f3c))
- **project-visual**: Refac(project-visual) Removing the Q&A section from the README ([`aa04765`](https://github.com/thrustlang/thrustc/commit/aa0476553207021b803af7d25f945debeb3fd030))
- **fuzzing**: Refac(fuzzing) Renaming the backlog to logs and updating the fuzzing documentation ([`35aa374`](https://github.com/thrustlang/thrustc/commit/35aa37480057a8a60335609a4e66200f9088b875))
- **project**: Refac(project) Removing negative compile-failure tests and legacy expected exit codes from the test suite ([`da8d164`](https://github.com/thrustlang/thrustc/commit/da8d164d94ae91035630cbb8337872a8c714bbde))
- **frontend**: Refac(frontend)

Remove the redundant add_macro_error shortcut from the macro diagnostic trait. ([`ddb4e11`](https://github.com/thrustlang/thrustc/commit/ddb4e1106e54255ff129d96611976352a089d197))
- **frontend**: (refac(project), refac(frontend), refac(preprocessador), refac(std), refac(doc)) Rename C import crates and reorganize macro translation, importC and std helpers

Rename 'thrustc_import_synthesis' to 'thrustc_import' and 'thrustc_c_import_synthesis' to 'thrustc_import_c'. Split the C transpiler macro translation into dedicated 'macro_translation'/'lex' modules, move shared clang helpers into 'util' modules (preprocessor and std), and split impl blocks across analyzer/preprocessor to match the code style. Make diagnostic prefixes optional (expansion_prefix/origin_note return Option) and normalize importC header-spec naming to outside_header. ([`7b9b16c`](https://github.com/thrustlang/thrustc/commit/7b9b16cbeb91a497dbaa2a115dd2df022b36c1ac))
- **project**: Refac(project) Reorganize tests/ suite layout

Move warnings/ to linter/, module_reexportation/ to modules/reexportation/, builtin_introspection/ to builtins/introspection/, compiletime_* to compiletime/{if,if_imports,target}/, allocas+deref-property+load to memory/{,deref,load}/, basics/ to arbitrary/ and array_size/ to array/. Update runner paths, skips and README. ([`cf012fc`](https://github.com/thrustlang/thrustc/commit/cf012fc7ef255a82001c16a221c25d709abdd2fe))


## [thrustc-x86_64-linux-ubuntu-v0.2.4] - 2026-10-09

### Bug Fixes
- **project**: Fix(project) Refresh apt package lists before installing Ubuntu dependencies

The Ubuntu runner's stale apt cache caused 404s for libpng-dev, libfreetype-dev and libpng-tools. Running 'apt-get update' first resolves the failing dependency install in both the release and dev workflows. ([`309852a`](https://github.com/thrustlang/thrustc/commit/309852a538770728a426a0cf6875eaf889fd730b))


---
*Thrust Compiler (thrustc) Changelog*

## Command Line
```console
Thrust Compiler

Usage: thrustc [-flags|--flags] [files..]

General Commands:

• -h, --help optional[opt|emit|print|code-model|
	reloc-model|sanitizer|symbol-linkage-strategy|
	denormal-floating-point-behavior|
	denormal-floating-point-32-bits-behavior] Show help message.
• -v, --version Show the version.
• --explain [E0001|W0001] Show the explanation of a compiler error or warning code.

Linkage flags:

• -link-with-clang [path/to/clang] Specifies the path for use of an external Clang for linking purpose.
• -link-with-gcc [path/to/gcc] Specifies GNU Compiler Collection (GCC) for linking purpose.
• -cc-args ["-lm;-lz"] Specifies arguments to forward to the active external linking compiler (Clang or GCC). Arguments are separated by spaces or semicolons.

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
• -jit-args ["--foo;bar"] Specifies the arguments passed to the program executed via the JIT compiler. Arguments are separated by spaces or semicolons.
• -abi [system-v|nvidia-cuda|webassembly] Configure the use of a specific ABI (Application Binary Interface) for code generation. This can affect how functions are called, how data is passed, and how the generated code interacts with other libraries and system components.
• -mode [stable|unstable] Enable or disable compiler features to limit to stable features only or add support to unstable features.
• -std [path/to/std] Set the standard library root path.
• -std-version [x.x.x] Set the standard library version to use.
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
• --disable-default-optimizations It omits default optimization that occurs even without specified optimization.
• --disable-all-sanitizers Disable all sanitizers.
• --disable-all-cpu-features Disable the all CPU features.

C transpiler flags:

• --emit-c-bindings-thrust [header.h] Emit C bindings as a .thrust file and exit.
• --emit-c-bindings-out-dir [out/] Directory where generated bindings will be written.
• --emit-c-bindings-output [path/to/file.thrust] Explicit output file path for generated bindings.

• --import-c-include [path/] Add an include directory (-I) for importC.
• --import-c-system-include [path/] Add a system include directory (-isystem) for importC.
• --import-c-define [NAME[=VALUE]] Define a macro (-D) for importC.
• --import-c-undef [NAME] Undefine a macro (-U) for importC.
• --import-c-scope [main-only|transitive-no-system|transitive-all] Set the declaration import scope used by importC and emitted C bindings.
• --import-c-target [triple] Set the clang target triple for importC (--target=...).
• --import-c-sysroot [path/] Set the clang sysroot for importC (--sysroot=...).
• --import-c-std [c11|gnu11|c99|...] Set the C standard for importC (-std=...).
• --import-c-arg [<clang-arg>] Append a raw clang argument for importC.

• --translate-c-to-thrust [file.c] Translate a C source file into a .thrust file and exit. Can be repeated.
• --translate-c-include [path/] Add an include directory (-I) for translateC.
• --translate-c-system-include [path/] Add a system include directory (-isystem) for translateC.
• --translate-c-define [NAME[=VALUE]] Define a macro (-D) for translateC.
• --translate-c-undef [NAME] Undefine a macro (-U) for translateC.
• --translate-c-target [triple] Set the clang target triple for translateC (--target=...).
• --translate-c-sysroot [path/] Set the clang sysroot for translateC (--sysroot=...).
• --translate-c-std [c11|gnu11|c99|...] Set the C standard for translateC (-std=...).
• --translate-c-arg [<clang-arg>] Append a raw clang argument for translateC.
• --translate-c-out-dir [out/] Directory where translated .thrust files will be written.
• --translate-c-output [path/to/file.thrust] Explicit output file path for a single translated C input.

Warning compiler flags:

• --disable-warnings  W0001;W0005;W0010 Disable the specified warnings.
• --disable-all-warnings Disable all the general and specific warnings.

Other compiler flags:

• --quiet Suppress compiler progress and timing output while preserving diagnostics.
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
