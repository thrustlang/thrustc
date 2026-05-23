/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use colored::Colorize;

pub fn show_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{}",
            "The Thrust Compiler".custom_color((141, 141, 142)).bold()
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "\n\n{} {} {}\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "[-flags|--flags] [files..]"
        ),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stderr, "General Commands:\n\n");

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {}, {} {} {}\n",
            "•".bold(),
            "-h".custom_color((141, 141, 142)).bold(),
            "--help".custom_color((141, 141, 142)).bold(),
            "optional[opt|emit|print|code-model|\n\treloc-model|sanitizer|symbol-linkage-strategy|\n\tdenormal-floating-point-behavior|\n\tdenormal-floating-point-32-bits-behavior]"
                .custom_color((141, 141, 142))
                .bold(),
            "Show help message.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {}, {} {}\n",
            "•".bold(),
            "-v".custom_color((141, 141, 142)).bold(),
            "--version".custom_color((141, 141, 142)).bold(),
            "Show the version.",
        ),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stderr, "\nLinkage flags:\n\n");

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-clang-link".custom_color((141, 141, 142)).bold(),
            "path/to/clang",
            "Specifies the path for use of an external Clang for linking purpose.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-gcc-link".custom_color((141, 141, 142)).bold(),
            "path/to/gcc",
            "Specifies GNU Compiler Collection (GCC) for linking purpose.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-start".custom_color((141, 141, 142)).bold(),
            "Marks the start of arguments to the active external or built-in linking compiler.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-end".custom_color((141, 141, 142)).bold(),
            "Marks the end of arguments to the active external or built-in linker compiler.",
        ),
    );

    thrustc_logging::write(thrustc_logging::OutputIn::Stderr, "\nCompiler flags:\n\n");

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-build-dir".custom_color((141, 141, 142)).bold(),
            "Specify the compiler artifacts directory.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-tools-dir".custom_color((141, 141, 142)).bold(),
            "Specify the compiler tools directory for search tools and expand compiler capatibilities.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-target".custom_color((141, 141, 142)).bold(),
            "x86_64",
            "Set the target arquitecture.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-target-triple".custom_color((141, 141, 142)).bold(),
            "x86_64-pc-linux-gnu|x86_64-pc-windows-msvc",
            "Set the target triple. For more information, see 'https://clang.llvm.org/docs/CrossCompilation.html'.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-cpu".custom_color((141, 141, 142)).bold(),
            "haswell|alderlake|ivybridge|pentium|pantherlake",
            "It specify the CPU to optimize.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-cpu-enable-features".custom_color((141, 141, 142)).bold(),
            "sse2;cx16;sahf;tbm",
            "It specify to enable certain CPU features to use.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-cpu-disable-features".custom_color((141, 141, 142)).bold(),
            "sse2;cx16;sahf;tbm",
            "It specify to disable certain CPU features to use.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-cpu-features".custom_color((141, 141, 142)).bold(),
            "+sse2,+cx16,+sahf,-tbm",
            "It overwrites the CPU features to use.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-emit".custom_color((141, 141, 142)).bold(),
            "llvm-bc|llvm-ir|asm|unopt-llvm-ir|unopt-llvm-bc|unopt-asm|obj|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens",
            "Compile the code into specified representation.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-print".custom_color((141, 141, 142)).bold(),
            "llvm-ir|unopt-llvm-ir|asm|unopt-asm|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens",
            "Displays the final compilation on standard output.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-opt".custom_color((141, 141, 142)).bold(),
            "O0|O1|O2|O3|Os|Oz",
            "Optimization level.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-stop-at".custom_color((141, 141, 142)).bold(),
            "lexing|parsing|scope-analysis|ast-verification|type-checking|general-analysis|attribute-checking|linter|compiler-intrinsic-checking|compiler-callconventions-checking|codegen",
            "Stop the compilation at specific stage."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-reloc-model".custom_color((141, 141, 142)).bold(),
            "static|pic|dynamic",
            "Indicate how references to memory addresses and linkage symbols are handled."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "-code-model".custom_color((141, 141, 142)).bold(),
            "[small|medium|large|kernel]",
            "Define how code is organized and accessed at machine code level."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "-macos-version".custom_color((141, 141, 142)).bold(),
            "[15.0.0]",
            "Specify the MacOS SDK version."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "-ios-version".custom_color((141, 141, 142)).bold(),
            "[17.4.0]",
            "Specify the iOS SDK version."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-jit".custom_color((141, 141, 142)).bold(),
            "Enable the use of the JIT compiler for code execution.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-jit-libc".custom_color((141, 141, 142)).bold(),
            "path/to/libc.so",
            "Specify the C runtime to link for code execution via the JIT compiler.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-jit-link".custom_color((141, 141, 142)).bold(),
            "path/to/raylib.so",
            "Specify, add, and link an external dynamic library for code execution via the JIT compiler.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-jit-entry".custom_color((141, 141, 142)).bold(),
            "main",
            "Specify the entry point name for the JIT compiler.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-dbg".custom_color((141, 141, 142)).bold(),
            "Enable generation of debug information (DWARF).",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-dbg-for-inlining".custom_color((141, 141, 142)).bold(),
            "Enable debug information specifically optimized for inlined functions.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "-dbg-for-profiling".custom_color((141, 141, 142)).bold(),
            "Emit extra debug info to support source-level profiling tools.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "-dbg-dwarf-version".custom_color((141, 141, 142)).bold(),
            "v4|v5",
            "Configure the Dwarf version for debugging purposes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--abi".custom_color((141, 141, 142)).bold(),
            "[system-v]",
            "Configure the use of a specific ABI (Application Binary Interface) for code generation. This can affect how functions are called, how data is passed, and how the generated code interacts with other libraries and system components.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-abi".custom_color((141, 141, 142)).bold(),
            "Disable the ABI detection and utilization, which may lead to less optimized code but can be useful for debugging or targeting non-standard environments.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--denormal-floating-point-behavior"
                .custom_color((141, 141, 142))
                .bold(),
            "[\"IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic\"]",
            "Configure how denormal floating-point values are handled during calculations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--denormal-floating-point-32-bits-behavior"
                .custom_color((141, 141, 142))
                .bold(),
            "[\"IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic,IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic\"]",
            "Configure how denormal 32-bit floating-point values are handled during calculations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--symbol-linkage-strategy"
                .custom_color((141, 141, 142))
                .bold(),
            "[any|exact|large|samesize|noduplicates]",
            "Configure the symbol linkage merge strategy.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--stack-protector".custom_color((141, 141, 142)).bold(),
            "It built a stack state guard that battles memory hacks and prevents memory corruptions.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--sanitizer".custom_color((141, 141, 142)).bold(),
            "[address|hwaddress|memory|thread|memtag]",
            "Enable the specified sanitizer. Adds runtime checks for bugs like memory errors, data races and others, with potential performance overhead.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--no-sanitize".custom_color((141, 141, 142)).bold(),
            "[bounds;coverage]",
            "Modifies certain code emissions for the selected sanitizer.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--opt-passes".custom_color((141, 141, 142)).bold(),
            "[-p{passname,passname}]",
            "Pass a list of custom optimization passes. For more information, see: 'https://releases.llvm.org/17.0.1/docs/CommandGuide/opt.html#cmdoption-opt-passname'.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--modificator-passes".custom_color((141, 141, 142)).bold(),
            "[loopvectorization;loopunroll;loopinterleaving;loopsimplifyvectorization;mergefunctions;callgraphprofile;forgetallscevinloopunroll;licmmssaaccpromcap=0;licmmssaoptcap=0;]",
            "Pass a list of custom modificator optimization passes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} {}\n",
            "•".bold(),
            "--target-triple-darwin-variant"
                .custom_color((141, 141, 142))
                .bold(),
            "[arm64-apple-ios15.0-macabi]",
            "Specify the darwin target variant triple."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--enable-ansi-color".custom_color((141, 141, 142)).bold(),
            "It allows ANSI color formatting in compiler diagnostics.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        "\nDisable compiler flags:\n\n",
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-frame-pointer"
                .custom_color((141, 141, 142))
                .bold(),
            "Regardless of the optimization level, it omits the emission of the frame pointer.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-uwtable".custom_color((141, 141, 142)).bold(),
            "It omits the unwind table required for exception handling and stack tracing.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-direct-access-external-data"
                .custom_color((141, 141, 142))
                .bold(),
            "It omits direct access to external data references, forcing all external data loads to be performed indirectly via the Global Offset Table (GOT).",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-rtlib-got".custom_color((141, 141, 142)).bold(),
            "It omits the runtime library dependency on the Global Offset Table (GOT), essential when generating non-Position Independent Code (PIC) with ARM.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-safe-trapping-math"
                .custom_color((141, 141, 142))
                .bold(),
            "It allow trapping math operations that can cause exceptions. Useful for floating-point operations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-safe-math".custom_color((141, 141, 142)).bold(),
            "Disable safe math for integer operations (allows overflow and undefined behavior).",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-default-optimization"
                .custom_color((141, 141, 142))
                .bold(),
            "It omits default optimization that occurs even without specified optimization.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-all-sanitizers"
                .custom_color((141, 141, 142))
                .bold(),
            "Disable all sanitizers.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-all-cpu-features"
                .custom_color((141, 141, 142))
                .bold(),
            "Disable the all CPU features.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        "\nWarning compiler flags:\n\n",
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--disable-all-warnings"
                .custom_color((141, 141, 142))
                .bold(),
            "Disable all the general and specific warnings.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        "\nOther compiler flags:\n\n",
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--copy-output-to-clipboard"
                .custom_color((141, 141, 142))
                .bold(),
            "Copy the total printable output of the compiler into the operating system clipboard. It only works using '-print' compiler flag."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--debug-clang-command".custom_color((141, 141, 142)).bold(),
            "Displays the generated command for Clang in the phase of linking."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--debug-gcc-command".custom_color((141, 141, 142)).bold(),
            "Displays the generated command for GCC in the phase of linking."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--export-compiler-errors"
                .custom_color((141, 141, 142))
                .bold(),
            "Export compiler error diagnostics to files."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--export-compiler-warnings"
                .custom_color((141, 141, 142))
                .bold(),
            "Export compiler warning diagnostics to files."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} [{}] {}\n",
            "•".bold(),
            "--export-diagnostics-path"
                .custom_color((141, 141, 142))
                .bold(),
            "diagnostics/",
            "Specify the path where diagnostic files will be exported."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-exported-diagnostics"
                .custom_color((141, 141, 142))
                .bold(),
            "Clean the exported diagnostics directory."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-build".custom_color((141, 141, 142)).bold(),
            "Clean the compiler build folder that holds everything."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-tokens".custom_color((141, 141, 142)).bold(),
            "Clean the compiler folder that holds the lexical analysis tokens."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-assembler".custom_color((141, 141, 142)).bold(),
            "Clean the compiler folder containing emitted assembler."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-llvm-ir".custom_color((141, 141, 142)).bold(),
            "Clean the compiler folder containing the emitted LLVM IR."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-llvm-bitcode".custom_color((141, 141, 142)).bold(),
            "Clean the compiler folder containing emitted LLVM bitcode."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--clean-objects".custom_color((141, 141, 142)).bold(),
            "Clean the compiler folder containing emitted object files."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--no-obfuscate-archive-names"
                .custom_color((141, 141, 142))
                .bold(),
            "Stop generating name obfuscation for each file; this does not apply to the final build."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--no-obfuscate-ir".custom_color((141, 141, 142)).bold(),
            "Stop generating name obfuscation in the emitted IR code."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--print-targets".custom_color((141, 141, 142)).bold(),
            "Show the current target supported."
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--print-supported-cpus"
                .custom_color((141, 141, 142))
                .bold(),
            "Show the current supported CPUs for the current target.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--print-host-target-triple"
                .custom_color((141, 141, 142))
                .bold(),
            "Show the host target triple.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "--print-opt-passes".custom_color((141, 141, 142)).bold(),
            "Show all available optimization passes through '--opt-passes=p{passname, passname}'.",
        ),
    );

    std::process::exit(1);
}

pub fn show_optimization_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "-opt value; -opt=value; -opt:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "O0|O1|O2|O3|Os|Oz",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "O0".custom_color((141, 141, 142)).bold(),
            "No optimization. Minimal compile time; produces the most predictable code for debugging.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "O1".custom_color((141, 141, 142)).bold(),
            "Basic optimization. Reduces code size and execution time without significantly increasing compile time.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "O2".custom_color((141, 141, 142)).bold(),
            "Standard optimization. Enables most stable optimizations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "O3".custom_color((141, 141, 142)).bold(),
            "Enables SIMD vectorization and heavy inlining to maximize execution speed.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "Os".custom_color((141, 141, 142)).bold(),
            "Optimize for size. Enables all 'O2' optimizations that do not increase the size of the generated binary.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "Oz".custom_color((141, 141, 142)).bold(),
            "Aggressive size optimization. Further reduces binary size by disabling certain 'O2' optimization passes.",
        ),
    );

    std::process::exit(1);
}

pub fn show_emission_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "-emit value; -emit=value; -emit:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "llvm-bc|llvm-ir|asm|unopt-llvm-ir|unopt-llvm-bc|unopt-asm|obj|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "llvm-bc".custom_color((141, 141, 142)).bold(),
            "Emit optimized LLVM bitcode.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "llvm-ir".custom_color((141, 141, 142)).bold(),
            "Emit optimized LLVM Intermediate Representation.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "asm".custom_color((141, 141, 142)).bold(),
            "Emit target-specific optimized assembly code.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unopt-llvm-ir".custom_color((141, 141, 142)).bold(),
            "Emit unoptimized LLVM IR before any optimization passes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unopt-llvm-bc".custom_color((141, 141, 142)).bold(),
            "Emit unoptimized LLVM bitcode before any optimization passes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unopt-asm".custom_color((141, 141, 142)).bold(),
            "Emit unoptimized target-specific assembly before any optimizations passes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "obj".custom_color((141, 141, 142)).bold(),
            "Emit machine-specific object file.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unchecked-pretty-ast".custom_color((141, 141, 142)).bold(),
            "Emit a pretty unchecked Abstract Syntax Tree before semantic analysis.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unchecked-ast".custom_color((141, 141, 142)).bold(),
            "Emit a unchecked Abstract Syntax Tree before semantic analysis.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "pretty-ast".custom_color((141, 141, 142)).bold(),
            "Emit a pretty and validated Abstract Syntax Tree.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "ast".custom_color((141, 141, 142)).bold(),
            "Emit a validated and typed Abstract Syntax Tree.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "pretty-tokens".custom_color((141, 141, 142)).bold(),
            "Emit a pretty formated lexical tokens.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "tokens".custom_color((141, 141, 142)).bold(),
            "Emit the compiler lexical tokens.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_printing_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "-print value; -print=value; -print:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "llvm-ir|unopt-llvm-ir|asm|unopt-asm|unchecked-pretty-ast|unchecked-ast|pretty-ast|ast|pretty-tokens|tokens",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "llvm-ir".custom_color((141, 141, 142)).bold(),
            "Show on console the optimized LLVM Intermediate Representation.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unopt-llvm-ir".custom_color((141, 141, 142)).bold(),
            "Show on console the unoptimized LLVM IR before any optimization passes.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "asm".custom_color((141, 141, 142)).bold(),
            "Show on console the optimized target-specific assembly code.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unopt-asm".custom_color((141, 141, 142)).bold(),
            "Show on console the unoptimized assembly code before any optimizations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unchecked-pretty-ast".custom_color((141, 141, 142)).bold(),
            "Show on console a unchecked pretty-formated Abstract Syntax Tree before semantic analysis.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "unchecked-ast".custom_color((141, 141, 142)).bold(),
            "Show on console the unchecked Abstract Syntax Tree before semantic analysis.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "pretty-ast".custom_color((141, 141, 142)).bold(),
            "Show on console a typed, validated and pretty formated Abstract Syntax Tree.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "ast".custom_color((141, 141, 142)).bold(),
            "Show on console a typed and validated Abstract Syntax Tree.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "pretty-tokens".custom_color((141, 141, 142)).bold(),
            "Show on console a pretty formated lexical tokens.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "tokens".custom_color((141, 141, 142)).bold(),
            "Show on console the lexical tokens.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_code_model_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--code-model value; --code-model=value; --code-model:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "small|medium|large|kernel",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "small".custom_color((141, 141, 142)).bold(),
            "Default model. Assumes the code and data fit within a 2GB address space.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "medium".custom_color((141, 141, 142)).bold(),
            "Allows code to be in the 2GB range, but data sections can be larger or located further away.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "large".custom_color((141, 141, 142)).bold(),
            "No assumptions about addresses. Code and data can be anywhere in the 64-bit address space.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "kernel".custom_color((141, 141, 142)).bold(),
            "Maps code to the high end of the address spaces.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_reloc_model_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--reloc-model value; --reloc-model=value; --reloc-model:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "static|pic|dynamic",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "static".custom_color((141, 141, 142)).bold(),
            "Non-relocatable code. Addresses are fixed at link time. Fastest, but not suitable for shared libraries.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "pic".custom_color((141, 141, 142)).bold(),
            "Position Independent Code. Required for shared libraries.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "dynamic".custom_color((141, 141, 142)).bold(),
            "Generates code that relies on a dynamic linker to resolve addresses at runtime.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_symbol_linkage_strategy_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--symbol-linkage-strategy value; --symbol-linkage-strategy=value; --symbol-linkage-strategy:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "any|exact|large|samesize|noduplicates",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "any".custom_color((141, 141, 142)).bold(),
            "Allow any symbol to be selected during linkage merge operations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "exact".custom_color((141, 141, 142)).bold(),
            "Require exact match for symbol linkage merge operations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "large".custom_color((141, 141, 142)).bold(),
            "Select the largest symbol during linkage merge operations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "samesize".custom_color((141, 141, 142)).bold(),
            "Only merge symbols of the same size during linkage operations.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "noduplicates".custom_color((141, 141, 142)).bold(),
            "Prevent duplicate symbols during linkage merge operations.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_sanitizer_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--sanitizer value; --sanitizer=value; --sanitizer:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "address|hwaddress|memory|thread|memtag",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "address".custom_color((141, 141, 142)).bold(),
            "AddressSanitizer (ASan). Detects buffer overflows, use-after-free, and other memory errors.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "hwaddress".custom_color((141, 141, 142)).bold(),
            "Hardware-assisted AddressSanitizer (HWASan). Similar to ASan but uses hardware features for better performance.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "memory".custom_color((141, 141, 142)).bold(),
            "MemorySanitizer (MSan). Detects reads of uninitialized memory.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "thread".custom_color((141, 141, 142)).bold(),
            "ThreadSanitizer (TSan). Detects data races and other threading bugs.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "memtag".custom_color((141, 141, 142)).bold(),
            "Memory Tagging (MTE). Uses ARM Memory Tagging Extension for memory safety.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_denormal_floating_point_behavior_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--denormal-floating-point-behavior value; --denormal-floating-point-behavior=value; --denormal-floating-point-behavior:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "IEEE".custom_color((141, 141, 142)).bold(),
            "Standard IEEE 754 behavior for denormal floating-point values.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "preserve-sign-signature"
                .custom_color((141, 141, 142))
                .bold(),
            "Preserve the sign of denormal values while treating them as zero.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "transform-to-positive-zero"
                .custom_color((141, 141, 142))
                .bold(),
            "Transform all denormal values to positive zero.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "dynamic".custom_color((141, 141, 142)).bold(),
            "Use dynamic behavior based on runtime conditions.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}

pub fn show_denormal_floating_point_32_bits_behavior_help() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {} [{}]\n\n",
            "Usage:".bold(),
            "thrustc".custom_color((141, 141, 142)).bold(),
            "--denormal-floating-point-32-bits-behavior value; --denormal-floating-point-32-bits-behavior=value; --denormal-floating-point-32-bits-behavior:value;"
                .custom_color((141, 141, 142))
                .bold(),
            "IEEE|preserve-sign-signature|transform-to-positive-zero|dynamic",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "IEEE".custom_color((141, 141, 142)).bold(),
            "Standard IEEE 754 behavior for denormal 32-bit floating-point values.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "preserve-sign-signature"
                .custom_color((141, 141, 142))
                .bold(),
            "Preserve the sign of denormal 32-bit values while treating them as zero.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "transform-to-positive-zero"
                .custom_color((141, 141, 142))
                .bold(),
            "Transform all denormal 32-bit values to positive zero.",
        ),
    );

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stderr,
        &format!(
            "{} {} {}\n",
            "•".bold(),
            "dynamic".custom_color((141, 141, 142)).bold(),
            "Use dynamic behavior based on runtime conditions for 32-bit values.",
        ),
    );

    std::process::exit(thrustc_constants::FAILURE_CODE)
}
