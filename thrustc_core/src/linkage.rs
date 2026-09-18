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

#![allow(clippy::result_unit_err)]

use colored::Colorize;

use inkwell::targets::TargetTriple;
use thrustc_backends::llvm::LLVMBackend;
use thrustc_backends::llvm::Sanitizer;
use thrustc_options::linkage::LinkingCompilersConfiguration;

use crate::ThrustCompiler;

#[derive(Debug)]
pub struct ClangLinker<'clang> {
    files: &'clang [std::path::PathBuf],
    config: &'clang LinkingCompilersConfiguration,
    backend: &'clang LLVMBackend,
    sanitizers: &'clang [Sanitizer],
}

impl<'clang> ClangLinker<'clang> {
    pub fn new(
        files: &'clang [std::path::PathBuf],
        config: &'clang LinkingCompilersConfiguration,
        backend: &'clang LLVMBackend,
        sanitizers: &'clang [Sanitizer],
    ) -> Self {
        Self {
            files,
            config,
            backend,
            sanitizers,
        }
    }
}

impl<'clang> ClangLinker<'clang> {
    pub fn link(&self) -> Result<std::time::Duration, ()> {
        let start_time: std::time::Instant = std::time::Instant::now();

        if !self.config.get_use_clang() {
            return Err(());
        }

        let clang_path: &std::path::Path = self.config.get_custom_clang();

        let mut cmd: std::process::Command = self.build_clang_command(clang_path);

        if self.handle_command(&mut cmd) {
            return Ok(start_time.elapsed());
        }

        Ok(start_time.elapsed())
    }
}

impl ClangLinker<'_> {
    pub fn build_clang_command(&self, clang_path: &std::path::Path) -> std::process::Command {
        let mut clang_command: std::process::Command = std::process::Command::new(clang_path);

        clang_command.arg("-v");

        let triple: &TargetTriple = self.backend.get_target().get_target_triple();
        let triple_display: String = triple.as_str().to_string_lossy().into_owned();

        clang_command.arg("-target");
        clang_command.arg(triple_display);

        clang_command.args(self.files.iter());
        self::add_sanitizer_link_flags(&mut clang_command, self.sanitizers);
        clang_command.args(self.config.get_args().iter());

        if self.config.get_debug_clang_commands() {
            thrustc_logging::print_debug(
                thrustc_logging::LoggingType::Debug,
                &format!("Generated Clang command: '{:?}'.\n", clang_command),
            );
        }

        clang_command
    }
}

impl ClangLinker<'_> {
    fn handle_command(&self, command: &mut std::process::Command) -> bool {
        match command.output() {
            Ok(output) => {
                if output.status.success() {
                    return true;
                }

                let stderr: String = String::from_utf8_lossy(&output.stderr)
                    .trim_end()
                    .to_string();

                if !stderr.is_empty() {
                    thrustc_logging::print_error(thrustc_logging::LoggingType::Error, &stderr);
                }

                let stdout: String = String::from_utf8_lossy(&output.stdout)
                    .trim_end()
                    .to_string();

                if !stdout.is_empty() {
                    thrustc_logging::print_warning(thrustc_logging::LoggingType::Warning, &stdout);
                }

                false
            }

            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct GCCLinker<'gcc> {
    files: &'gcc [std::path::PathBuf],
    config: &'gcc LinkingCompilersConfiguration,
    sanitizers: &'gcc [Sanitizer],
}

impl<'gcc> GCCLinker<'gcc> {
    #[inline]
    pub fn new(
        files: &'gcc [std::path::PathBuf],
        config: &'gcc LinkingCompilersConfiguration,
        sanitizers: &'gcc [Sanitizer],
    ) -> Self {
        Self {
            files,
            config,
            sanitizers,
        }
    }
}

impl<'gcc> GCCLinker<'gcc> {
    pub fn link(&self) -> Result<std::time::Duration, ()> {
        let start_time: std::time::Instant = std::time::Instant::now();

        if !self.config.get_use_gcc() {
            return Err(());
        }

        let gcc_path: &std::path::Path = self.config.get_custom_gcc();

        let mut cmd: std::process::Command = self.build_gcc_command(gcc_path);

        if self.handle_command(&mut cmd) {
            return Ok(start_time.elapsed());
        }

        Ok(start_time.elapsed())
    }
}

impl GCCLinker<'_> {
    pub fn build_gcc_command(&self, gcc_path: &std::path::Path) -> std::process::Command {
        let mut gcc_command: std::process::Command = std::process::Command::new(gcc_path);

        gcc_command.arg("-v");
        gcc_command.args(self.files.iter());
        self::add_sanitizer_link_flags(&mut gcc_command, self.sanitizers);
        gcc_command.args(self.config.get_args().iter());

        if self.config.get_debug_gcc_commands() {
            thrustc_logging::print_debug(
                thrustc_logging::LoggingType::Debug,
                &format!("Generated GCC command: {:?}\n", gcc_command),
            );
        }

        gcc_command
    }
}

impl GCCLinker<'_> {
    pub fn handle_command(&self, command: &mut std::process::Command) -> bool {
        match command.output() {
            Ok(output) if output.status.success() => true,

            Ok(output) => {
                if !output.stderr.is_empty() {
                    thrustc_logging::print_error(
                        thrustc_logging::LoggingType::Error,
                        String::from_utf8_lossy(&output.stderr).trim_end(),
                    );
                }

                if !output.stdout.is_empty() {
                    thrustc_logging::print_warning(
                        thrustc_logging::LoggingType::Warning,
                        String::from_utf8_lossy(&output.stdout).trim_end(),
                    );
                }

                false
            }

            _ => false,
        }
    }
}

pub fn link_with_clang(compiler: &mut ThrustCompiler) {
    let llvm_backend: &LLVMBackend = compiler.get_compilation_options().get_llvm_backend();

    let linking_compiler_config: &LinkingCompilersConfiguration = compiler
        .get_compilation_options()
        .get_linking_compilers_configuration();

    let all_compiled_files: &[std::path::PathBuf] = compiler.get_compiled_files();
    let sanitizers: &[Sanitizer] = compiler.get_linked_sanitizers();

    if let Ok(clang_time) = ClangLinker::new(
        all_compiled_files,
        linking_compiler_config,
        llvm_backend,
        sanitizers,
    )
    .link()
    {
        compiler.linking_time = compiler.linking_time.saturating_add(clang_time);

        thrustc_logging::write(
            thrustc_logging::OutputIn::Stdout,
            &format!(
                "{} {}\n",
                "Linking".custom_color((141, 141, 142)).bold(),
                "FINISHED".bright_green().bold()
            ),
        );
    } else {
        thrustc_logging::write(
            thrustc_logging::OutputIn::Stderr,
            &format!(
                "\r{} {}\n",
                "Linking".custom_color((141, 141, 142)).bold(),
                "FAILED".bright_red().bold()
            ),
        );
    }
}

pub fn link_with_gcc(compiler: &mut ThrustCompiler) {
    let linking_compiler_configuration: &LinkingCompilersConfiguration = compiler
        .get_compilation_options()
        .get_linking_compilers_configuration();

    let all_compiled_files: &[std::path::PathBuf] = compiler.get_compiled_files();
    let sanitizers: &[Sanitizer] = compiler.get_linked_sanitizers();

    if let Ok(gcc_time) = GCCLinker::new(
        all_compiled_files,
        linking_compiler_configuration,
        sanitizers,
    )
    .link()
    {
        compiler.linking_time = compiler.linking_time.saturating_add(gcc_time);

        thrustc_logging::write(
            thrustc_logging::OutputIn::Stdout,
            &format!(
                "{} {}\n",
                "Linking".custom_color((141, 141, 142)).bold(),
                "FINISHED".bright_green().bold()
            ),
        );
    } else {
        thrustc_logging::write(
            thrustc_logging::OutputIn::Stderr,
            &format!(
                "\r{} {}\n",
                "Linking".custom_color((141, 141, 142)).bold(),
                "FAILED".bright_red().bold()
            ),
        );
    }
}

fn add_sanitizer_link_flags(command: &mut std::process::Command, sanitizers: &[Sanitizer]) {
    if sanitizers.is_empty() {
        return;
    }

    let flags: Vec<&'static str> = sanitizers
        .iter()
        .filter_map(self::sanitizer_link_name)
        .collect();

    if flags.is_empty() {
        return;
    }

    command.arg(format!("-fsanitize={}", flags.join(",")));
}

fn sanitizer_link_name(sanitizer: &Sanitizer) -> Option<&'static str> {
    match sanitizer {
        Sanitizer::Address(..) => Some("address"),
        Sanitizer::Memory(..) => Some("memory"),
        Sanitizer::Thread(..) => Some("thread"),
        Sanitizer::Hwaddress(..) => Some("hwaddress"),
        Sanitizer::Memtag(..) => Some("memtag"),
        Sanitizer::None => None,
    }
}
