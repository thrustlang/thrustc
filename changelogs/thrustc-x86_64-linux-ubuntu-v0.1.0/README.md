# Changelog

## Command Line

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