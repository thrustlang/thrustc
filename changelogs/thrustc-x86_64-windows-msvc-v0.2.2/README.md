# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [thrustc-x86_64-linux-ubuntu-v0.2.2] - 2026-09-26

### Bug Fixes
- Fix(fuzz) Updating the fuzzing suite to works with the current compiler version ([`a13ceea`](https://github.com/thrustlang/thrustc/commit/a13ceeabc15b337f248e862151c8a701c097467a))
- **llvm_backend**: Fix(llvm_backend) Backend panic when using `->` deref arrow shortcut inside a mutation expression on supposed LValue state. ([`5b33307`](https://github.com/thrustlang/thrustc/commit/5b33307514be9889967a0b1eb044be7aa6502a97))
- Fix(lsp) Refactoring the lsp for a better code quality and, comprehensible understanding. ([`72517a4`](https://github.com/thrustlang/thrustc/commit/72517a48ca75fc2a032f0770b3ca4e7bd29c65c0))
- **frontend**: Fix(frontend) Reassembling the generics and import resolution on different crate for a better project layout and frontend. ([`738dbb7`](https://github.com/thrustlang/thrustc/commit/738dbb7059a6f7a750d30fea5dbe2699e2316a0b))
- **doc**: Fix(doc) Removing unnecesary buzz-words ([`db3b461`](https://github.com/thrustlang/thrustc/commit/db3b461bad368fa0194c185970279761e106c9f4))
- **project-visual**: Fix(project-visual) Moving release binary variants to compiler releasing ([`f645404`](https://github.com/thrustlang/thrustc/commit/f645404bd9f8b7b4507cc2ccd29bfa170271c4e8))
- **project-visual**: Fix(project-visual) Moving resources to the root. ([`875e63e`](https://github.com/thrustlang/thrustc/commit/875e63eac2cc13568bbdfc1157b083daebfc3204))
- **project**: Fix(project) Fixing an old issue in git cliff changelog generation. ([`5b38206`](https://github.com/thrustlang/thrustc/commit/5b38206911939906f59f474dba9c5a3dfc202e87))
- Fix(lsp) Fixing type suggestion in deteterminate contexts. ([`496baa7`](https://github.com/thrustlang/thrustc/commit/496baa72c1108bc654899f9125858f10572e080e))
- Fix(lsp) For loop snippet syntax. ([`08f0458`](https://github.com/thrustlang/thrustc/commit/08f045863e0a954d7fcc748d7ce11c0a71cdf312))
- **abi**: Fix(abi) Simplifying integer bits decision. ([`73e8d40`](https://github.com/thrustlang/thrustc/commit/73e8d40f1a2b750c7ac503ffaca17e6c7bd9e464))
- **frontend**: Fix(frontend) Fixing module collisions on std. ([`a65f8fe`](https://github.com/thrustlang/thrustc/commit/a65f8fe779104043873256ed6656d083c4008291))
- **project-visual**: Fix(project-visual) Adding the prologue for the SYNTAX HIGHLIGHTING markdown. ([`17d0b48`](https://github.com/thrustlang/thrustc/commit/17d0b486b743c2b0655f5010a0cfde716e8f7085))


### Documentation
- **doc**: Feat(doc) Updating LLVM resources to include intrinsic references. ([`5714774`](https://github.com/thrustlang/thrustc/commit/571477406e1b0635b20a49403ad7a809125e72d1))
- **doc**: Feat(doc) Updating the principal README with torio and spec reference. ([`d210977`](https://github.com/thrustlang/thrustc/commit/d210977c5ceceac60221f1ad2f7bbb281513b9de))


### Features
- **frontend**: Feat(frontend) Adding more target specific identification builtins. ([`cf390b5`](https://github.com/thrustlang/thrustc/commit/cf390b5e45afd10bb046ada84cc299031d005486))
- **frontend**: (feat(frontend),feat(llvm_backend)) Introducing function variadic manipulation. ([`47296bb`](https://github.com/thrustlang/thrustc/commit/47296bb0df4ea456cf7c90689ffd33132be1e90e))
- **frontend**: (feat(frontend),feat(llvm_backend)) Introducing variatic function manipulation. ([`c4ddf32`](https://github.com/thrustlang/thrustc/commit/c4ddf321e54494391d4a7d754a6b7c10b0c823bd))
- **std**: Feat(std) Adding .env module to the std and tstring quality updates. ([`0972a11`](https://github.com/thrustlang/thrustc/commit/0972a11f28bd0e4045e07967a134cbfb0bca954c))
- **llvm_backend**: Feat(llvm_backend) Adding experimental and unstable atomic operations. ([`e0b608e`](https://github.com/thrustlang/thrustc/commit/e0b608e50fa3ab7fe14776764ea680e715a3780e))
- **llvm_backend**: Feat(llvm_backend) Adding experimental and unstable atomics operations along their tests. ([`8328cde`](https://github.com/thrustlang/thrustc/commit/8328cdee07950a28077bd833523310f720ada510))
- **abi**: Feat(abi) Adding arm fixed abi rule convention, setting the attribute call convention al function declaration. ([`4b8748f`](https://github.com/thrustlang/thrustc/commit/4b8748f99ec0283ab963a893a1815a13ca7950a6))
- Feat(fuzz) Integrating more relavant test for the current compiler. ([`39e1908`](https://github.com/thrustlang/thrustc/commit/39e19080b5eace6605a4c498030e741ca96e158a))
- **std**: (feat(std),fix(llvm_backend),fix(frontend)) Implementing Queue , Stack in the std, and fixing various issues regarding generics and imports. ([`daad8e4`](https://github.com/thrustlang/thrustc/commit/daad8e4ba1586932f5b0cb21edac54d3ed2d125a))
- **std**: (feat(std),fix(abi)) Optional value module and new compiler intrisincs for integer and floating-pointer number. And, also a fix in the SystemV regarding fixed array sized that fits in two registers. ([`c549840`](https://github.com/thrustlang/thrustc/commit/c549840e196062b4abf6e1499eccde4a13035878))
- **frontend**: (feat(frontend),feat(llvm_backend)) Adding automatic dealloc and deallocator attribute. ([`4f5fbef`](https://github.com/thrustlang/thrustc/commit/4f5fbef53c61356e955431ec64dc1a5d0e152c61))
- **std**: (feat(lsp),feat(std)) Adding the current lsp server for the current compiler state. Adding HashMap, HashSet and VecDeque based in Rust approach. ([`384b376`](https://github.com/thrustlang/thrustc/commit/384b37623c332ca45deb574545c65a69800bfed4))
- **std**: (feat(std),fix(project)) Updating std to v0.2.2 and the compiler, also updating the syntax hightlighting for vscode and neovim. ([`930514b`](https://github.com/thrustlang/thrustc/commit/930514bce7b0f2cd5d42878fa4074d7297cfe95c))


## [thrustc-x86_64-macos-v0.2.1] - 2026-09-11

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
