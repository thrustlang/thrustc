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

use thrustc_core::CompileTime;

#[cfg(not(target_pointer_width = "64"))]
compile_error!("This compiler requires a 64-bit target.");
#[rustversion::before(1.85)]
compile_error!("This compiler requires Rust 1.85 or newer.");

#[global_allocator]
static GLOBAL: thrustc_heap_allocator::ThrustCompilerHeapAllocator =
    thrustc_heap_allocator::ThrustCompilerHeapAllocator;

fn main() -> ! {
    use thrustc_core::ThrustCompiler;

    thrustc_cli::set_up_basic();

    let cli: thrustc_cli::CommandLine = thrustc_cli::CommandLine::parse(std::env::args().collect());
    let options: &thrustc_options::CompilerOptions = cli.get_options();

    thrustc_cli::set_up_ansi(options);

    let start_time: std::time::Instant = std::time::Instant::now();

    let compilation_units: &[thrustc_options::CompilationUnit] = options.get_units();

    let mut compiler_instance: ThrustCompiler<'_> = ThrustCompiler::new(compilation_units, options);

    let compile_time: CompileTime = compiler_instance.compile();

    thrustc_cli::report_compile_time(options, start_time, compile_time)
}
