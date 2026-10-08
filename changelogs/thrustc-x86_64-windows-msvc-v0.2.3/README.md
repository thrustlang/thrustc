# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [thrustc-x86_64-windows-msvc-v0.2.3] - 2026-10-08

### Bug Fixes
- **llvm_backend**: (fix(llvm_backend), fix(project)) Build static libclang on Windows MSVC via LLVM_ENABLE_PIC=OFF ([`e2c2f6c`](https://github.com/thrustlang/thrustc/commit/e2c2f6c0e77ecb3d317003ad76f4130b70f16295))
- **project-visual**: Fix(project-visual) Renaming Thrust Comoiker to Thrust Programming Language ([`8281fb8`](https://github.com/thrustlang/thrustc/commit/8281fb8789799601d6c6a355889bd8a0f93fea32))
- Fix(llvm_backend, project) Link libclang statically on Windows MSVC and inspect build outputs ([`9de6674`](https://github.com/thrustlang/thrustc/commit/9de667419cd4cbfbe5dd2c9b92ccc7ae0301f49e))


### Documentation
- **project-visual**: (feat(project-visual), feat(doc)) Improve central README with platform support table and update spec pointer ([`fa0fee8`](https://github.com/thrustlang/thrustc/commit/fa0fee877073a861d400183e857031770afeb48a))


### Refactoring
- **frontend**: (refac(frontend), fix(frontend)) Route macro constitution through the internal parser ([`8afd465`](https://github.com/thrustlang/thrustc/commit/8afd465d3df9143a28ec274075d0fab9ca469f47))
- **doc**: Refac(doc) Update project structure without function code mentions ([`c3ae801`](https://github.com/thrustlang/thrustc/commit/c3ae8018ec7466e29137078046a4384d02598b55))


## [thrustc-x86_64-linux-ubuntu-v0.2.3] - 2026-10-07

### Bug Fixes
- **project**: Fix(project) Updating spec ([`8a19e53`](https://github.com/thrustlang/thrustc/commit/8a19e53d31bbe8ed4ab7db9bf31576644f5db73d))


### Features
- **frontend**: (feat(frontend), refac(frontend), fix(frontend)) Expand C transpiler macro lowering coverage and normalize internals ([`251166a`](https://github.com/thrustlang/thrustc/commit/251166a1fdc9bdf5613d47c0010a62479c710ef8))
- **frontend**: (feat(frontend), refac(frontend), fix(frontend)) Handle ternary hoisting and complete casting for mutable expressions

Translate C ternary as hoisted if-else with thrust_temporary_ prefix and
type-aware branch casting via cast_expression_to_type. Prefix generated
macro symbols with __c_macro_ and clean expression wrappers. Copy the
typechecker casting matrix for full validity, emit E0110 for non-castable
cases and handle narrowing as valid. Coerce assignment RHS for all compound
assignments with pointer arithmetic exclusion, unify var init and return
paths and keep narrowing exclusion. Update c_transpile expected outputs
for the new temporaries and casts. ([`2aca3ee`](https://github.com/thrustlang/thrustc/commit/2aca3ee9290cef5cba2c5581ff27b0d4890459e4))
- **frontend**: (feat(frontend), refac(frontend), fix(frontend), fix(llvm_backend)) C transpiler builtins, macro tables and unified error context

Map C memory and heap primitives to Thrust builtins and outline statement macros through generic definition-driven lowering with shared Location rules. Restructure the macro subsystem into MacroTable, macro_expr, location and entrypoint modules with a TranspilerContext error collector and abort pattern. Report all translation issues at the entrypoint instead of returning them one by one. Fix backend load of inline fixed-array struct fields. ([`25af33c`](https://github.com/thrustlang/thrustc/commit/25af33c15c95e8793332db5bcddac0cfcf2e389c))
- **frontend**: (feat(project), feat(frontend), fix(llvm_backend), fix(frontend), fix(preprocessador), refac(project-visual)) Complete C transpilation programs and importC support

Expand the C transpiler and test runner flow, add importC/global binding support, fix compound assignment and aggregate lowering across the frontend and LLVM backend, and sync the translated C program fixtures with the current compiler behavior. ([`4d389ec`](https://github.com/thrustlang/thrustc/commit/4d389ec2718620a31c9edbc2628b7d52f7604950))
- **frontend**: (feat(frontend), feat(preprocessador), feat(project), refac(project), fix(frontend)) Implementing experimental C transpiler and cbindgen flow for v0.3.0 ([`97af617`](https://github.com/thrustlang/thrustc/commit/97af617504e6ea8caac6cfd5d67df3360b64d728))
- **frontend**: (feat(frontend), feat(llvm_backend), feat(abi), feat(lsp), feat(fuzzing), feat(doc), feat(project-visual)) Add native LLVM vectors ([`e003825`](https://github.com/thrustlang/thrustc/commit/e00382592af03ecd97846ab0127e0506a04a9fe9))


### Project
- **project**: (feat(project), fix(frontend), fix(preprocessador)) Embed std v0.3.0 and fix importC transpilation ([`9fca743`](https://github.com/thrustlang/thrustc/commit/9fca74394e75e9c7ce3c61c4e48027457215ab77))


### Refactoring
- **std**: (fix(frontend), fix(llvm_backend), fix(preprocessador), fix(lsp), fix(fuzzing), refac(project), refac(project-visual), refac(std)) Align NativeVector handling, sync highlighting, and stabilize toolchain wiring ([`c8c8b59`](https://github.com/thrustlang/thrustc/commit/c8c8b59db90655f40bcb2627f46ec90d275c79c8))
- **frontend**: Refac(frontend) Rename statement macro extraction flow and inline tiny accessors ([`8e4f6fc`](https://github.com/thrustlang/thrustc/commit/8e4f6fc01bc4e6704bfb4f092b077911063d130f))
- **frontend**: (refac(frontend), fix(frontend)) Reorganizing C transpiler contexts/impl blocks and restoring stable type formatting flow ([`0a86f60`](https://github.com/thrustlang/thrustc/commit/0a86f60618b5395a5940454da81fdf5ad0ee7340))
- **frontend**: (feat(frontend, doc), refac(frontend), fix(frontend)) Unify C transpiler error handling and stabilize Thrust emission order

Unify format_clang_type_thrust to String with internal registration via ctx
and collapse all call sites to direct receives. Deduplicate sanitization
through util::sanitize_thrust_identifier and clean_expression_wrappers
renaming. Introduce macro_error with definition body ranges and per-child
snapshot collapse to emit a single E0110 per macro with notes
defined/expanded/failed at. Extract classify_top_level_declarations,
rename promote_statement_delegates to reclassify_macros_calling_statements,
and reorder imports/directive before structs/unions/enums/typedefs/statics.
Update tests/README with full CLI flags, c_transpile phases and dist layout. ([`02794bf`](https://github.com/thrustlang/thrustc/commit/02794bfcee783cf0f3fef2694cbbdedfbf20c883))
- **project**: (refac(project), refac(lsp)) Bumping version to 0.3.0 ([`e1e6c29`](https://github.com/thrustlang/thrustc/commit/e1e6c29572d1843c948a8ab4e033d2e9b2a670c5))
- **frontend**: (refac(frontend), refac(abi), refac(preprocessador)) Reorganizing impl blocks and inlining trivial accessors ([`76cd6a2`](https://github.com/thrustlang/thrustc/commit/76cd6a25f5f4d997ea534042581c9d98f2883d34))


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
