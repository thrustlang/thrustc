# Changelog

All notable changes to the Thrust Compiler (thrustc) are documented here.

## [Unreleased]

### Bug Fixes
- **frontend**: (fix(frontend),fix(llvm_backend)) Fixing a issue found using arrow dereference shorthand in indexation. (Duplicated load). ([`65f9290`](https://github.com/thrustlang/thrustc/commit/65f92903003883721d70d492a592f0f32dd1bbdc))
- Fix(fuzz) Making the fuzz more accurate regarding metadata and type generation to not generate masked/non-issue codegen issues. ([`0eeeff3`](https://github.com/thrustlang/thrustc/commit/0eeeff3f54ec3c381eea6aa2fa9451a72be62b02))
- **frontend**: (fix(llvm_backend),fix(frontend)) Fixing the behavior for index and property access with have relation with memory operations through LValue and RValue. Also fixing issues found on the scoper when analyzes while and for loops intermediate variables. ([`ca6e6ee`](https://github.com/thrustlang/thrustc/commit/ca6e6eefdf3ade5401bf191f2842ebf3df7fa2a1))
- **frontend**: Fix(frontend) Changing the external linker args flag for a better one, which, is more accurate and descriptive for "C/C++ Compiler linker flag". ([`5c891a3`](https://github.com/thrustlang/thrustc/commit/5c891a3ac1b7163ba66eb61ad46dd265d6941943))
- **frontend**: Fix(frontend) Fixing issues found on generics regarding mangled naming on the linter. ([`c6e42f9`](https://github.com/thrustlang/thrustc/commit/c6e42f9e1538145c208a35925af54a074b1056cb))
- **frontend**: Fix(frontend) Removing dead store warnings as a useful warning, and including a custom errors for generics ([`30341b6`](https://github.com/thrustlang/thrustc/commit/30341b6fcfb5ee25ca449ea2fccbc96906e0d9e1))
- **frontend**: Fix(frontend) Fixing generics parsing on cross-modulation situation and fixing mangling name on a warning message on the linter. ([`75c3d2f`](https://github.com/thrustlang/thrustc/commit/75c3d2fbc9a256fa3fa0f0276b2d1aab4575f6c4))
- **frontend**: Fix(frontend) Refactoring for better code understanding. ([`bc7a772`](https://github.com/thrustlang/thrustc/commit/bc7a772054175272bb3c5413821989dbb6754369))
- **frontend**: Fix(frontend) Removing a fuzz fix. ([`562b48b`](https://github.com/thrustlang/thrustc/commit/562b48b7bdf956ef251f54605fd26a9f1374725f))
- Fix(fuzz) Fixing readme tone and gen local on fuzzing. ([`071643f`](https://github.com/thrustlang/thrustc/commit/071643fd27c118ec66e6f72f2b1ea15b94727a3a))
- Fix(fuzz) Updating the fuzzing suite with the new features scope. ([`de20dea`](https://github.com/thrustlang/thrustc/commit/de20dea1c01f4f813289a1444cb75d2ac14cfbdb))
- Fix(llvm_codegen) Fixing MinusEq and PlusEq in the codegen when tries to cast a lower int representation. ([`19854b8`](https://github.com/thrustlang/thrustc/commit/19854b8c1d4f6050474eca424b563748db979014))
- **frontend**: Fix(frontend) Parsing issue on while var inlined var declaration. ([`b6ff58d`](https://github.com/thrustlang/thrustc/commit/b6ff58da6dafc95e6b354d66bf8657dbd1cdf0c8))
- **llvm_backend**: Fix(llvm_backend) Adding a guard for determinate atomic configuration. ([`48a43b6`](https://github.com/thrustlang/thrustc/commit/48a43b6009d953285dcc0c4d70ed07a6262d0bac))
- **frontend**: Fix(frontend) Fixing an issue on attribute checker filter warnings mechanism. ([`d46066a`](https://github.com/thrustlang/thrustc/commit/d46066a53f9db429c35e06951aa33013efc610c3))
- **project**: Fix(project) Fixing issues found when the script release-changelog tries to update the sewer version of the compiler. ([`01c3f05`](https://github.com/thrustlang/thrustc/commit/01c3f05897f1e1e9a2ada54ded4693ca06012fab))


### Features
- **std**: (feat(std),fix(llvm_backend)) Implementing std vector as an experimental API., and fixing gdb debug info generation on the llvm backend with actual few limitations. ([`ae01c31`](https://github.com/thrustlang/thrustc/commit/ae01c31eca98c467d97a3e07122445d6ec511594))
- **frontend**: (feat(llvm_backend),feat(frontend)) Load keyword to disable ambiguity found when you need to load a pointer type from a raw address access calculation (gep). ([`da69be0`](https://github.com/thrustlang/thrustc/commit/da69be039817e17c242bb181ccbd044dcc50f182))
- **std**: (fix(frontend),feat(std)) Fixing duplicated static signatures generaion the reference is qualified. And also implementing a memory standard library module. ([`b6e282b`](https://github.com/thrustlang/thrustc/commit/b6e282b412847773bbf261d69ea9c8bbb4d9de1e))
- **frontend**: Feat(frontend) Introducing experimental generatics. (Stable until v0.2.1 approx) ([`047150d`](https://github.com/thrustlang/thrustc/commit/047150d71deade198c381d366a3333c22042ec91))
- **std**: Feat(std) Adding C primitives. ([`b3927e6`](https://github.com/thrustlang/thrustc/commit/b3927e646f4edf59e7e768226899ce3491ffeedd))
- **frontend**: Feat(frontend) Adding the explanation of each error and warning of the compiler. ([`42f697e`](https://github.com/thrustlang/thrustc/commit/42f697e8487064ad36959514d9fe1aff5af6491b))
- **std**: Feat(std) Extending the math and io on the std with single floating point precision. ([`3f082c5`](https://github.com/thrustlang/thrustc/commit/3f082c580cfb4db2ee58627a29852bdd85377075))
- **frontend**: Feat(frontend) Integrating a sugar syntax for indexation and property dereference. ([`402df74`](https://github.com/thrustlang/thrustc/commit/402df74f098e736f0e2febc64935b1e227373509))
- **frontend**: Feat(frontend) New useful compiler builtins ([`a138205`](https://github.com/thrustlang/thrustc/commit/a1382051c7c607cef96ffd3be986de7039ac74ee))
- **frontend**: (feat(frontend),feat(llvm_backend)) Adding more compound operators for arithmetic and bit a bit operations. ([`394ade3`](https://github.com/thrustlang/thrustc/commit/394ade3e26f6eeb258ec7dcbfb8a3d79f5a5c917))
- **std**: Feat(std) Adding the v0.2.0 std. ([`8032e28`](https://github.com/thrustlang/thrustc/commit/8032e2829bbba2226e6d1619616fc9374a969063))
- **frontend**: Feat(frontend) Adding support for array size calculation on compile time and more useful builtins. ([`322e27c`](https://github.com/thrustlang/thrustc/commit/322e27c60871a8e99962e7c2e2d5f15a6ac13f20))
- **frontend**: Feat(frontend) Integrating more usable builtins and compile time conditionals ([`5e49e95`](https://github.com/thrustlang/thrustc/commit/5e49e9598342693998353407e8a58eec93edd3e6))
- **frontend**: (feat(frontend), fix(llvm_backend)) Adding support to builtint in the preprocessador and fixing vararg lowering in the llvm backend. ([`f8f9fae`](https://github.com/thrustlang/thrustc/commit/f8f9fae72eb8a28065aa627c41728d1525acad0d))
- **llvm_backend**: Feat(llvm_backend) Adding support to variatic functions construction. ([`7a8eac5`](https://github.com/thrustlang/thrustc/commit/7a8eac5e15abd196f2900e6c35a0556f69b65998))
- **frontend**: (feat(frontend), feat(preprocessador)) Adding support to reexportation to the 'std' and integrating naming parameters. ([`2705257`](https://github.com/thrustlang/thrustc/commit/27052570e9d4d7ee1cd57d6b531be389df927fe5))
- **frontend**: Feat(frontend) Introducing a builtin compiler system for usage in the future. ([`70f32b2`](https://github.com/thrustlang/thrustc/commit/70f32b2a0d2e288b748a4d7adb7e2d0d139e02f0))
- **preprocessador**: Feat(preprocessador) Implementing unique import for each symbol and integrating structure qualification. ([`512f0ad`](https://github.com/thrustlang/thrustc/commit/512f0ad7fbe99779767d4e095010f3d7f6b0b94f))
- Feat(fuzz) Integrating a more complete fuzz suite that is capable of pass the semantic analysis for binding bugs on code generation (llvm backend). ([`edc445d`](https://github.com/thrustlang/thrustc/commit/edc445dff8af2d5f04b32ff1f0022430b0ebeaf8))
- **frontend**: (fix(frontend), feat(frontend), fix(preprocessador)) Adding warning 'not used' resolution on linter. Adding std support to jit compilation and fixing name resolution on preprocessador. ([`5d91ab6`](https://github.com/thrustlang/thrustc/commit/5d91ab6adb9333fdba186df4f67b4f4924dea67c))
- **llvm_backend**: (feat(llvm_backend), fix(frontend)) Adding exhaustive match pattern to enhance the compiler robustness, and also, integrating full access to atomic modificators for store/load in llvm codegen. ([`52aaeb0`](https://github.com/thrustlang/thrustc/commit/52aaeb000bd66867d52c67a232127bab7096c459))
- **std**: Feat(std) adding the std compiler parsing and detection mechanism. ([`16b3af3`](https://github.com/thrustlang/thrustc/commit/16b3af3ae4f62aa68b0322e06a5689bee3510a58))
- **frontend**: Feat(frontend) Adding stack guard mechanism for preventing the compiler too parse illogical ultra-nested ast nodes, the fuzzing won't reproduce on this way, because is designed to be an intelligent fuzzing and random on some parts. ([`2439b9c`](https://github.com/thrustlang/thrustc/commit/2439b9c1e6c33da01154166a057085aa0f21fca7))
- **frontend**: (feat(preprocessador), feat(frontend)) Adding the missing part to the preprocessador. ([`188e701`](https://github.com/thrustlang/thrustc/commit/188e701c56b5f6527844df7f4cb13ea040990954))
- **llvm_backend**: Feat(llvm_backend) Adding the system-v changes for Windows x64. ([`99cca99`](https://github.com/thrustlang/thrustc/commit/99cca990473bd08ccfa7d6cccbd615c4d24f6f9e))


## [thrustc-x86_64-windows-msvc-v0.1.8] - 2026-08-15

### Bug Fixes
- **project**: Fix(project) pushing the new version ([`b386911`](https://github.com/thrustlang/thrustc/commit/b3869112e1a6ceb3dbe47ef12ff4becf6b2aa8c7))


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
• -abi [system-v|nvidia-cuda] Configure the use of a specific ABI (Application Binary Interface) for code generation. This can affect how functions are called, how data is passed, and how the generated code interacts with other libraries and system components.
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
• --disable-default-optimization It omits default optimization that occurs even without specified optimization.
• --disable-all-sanitizers Disable all sanitizers.
• --disable-all-cpu-features Disable the all CPU features.

Warning compiler flags:

• --disable-warnings  W0001;W0005;W0010 Disable the specified warnings.
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
