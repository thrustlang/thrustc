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

use std::path::Path;
use std::path::PathBuf;
use std::process::Stdio;

#[derive(Debug)]
pub struct LinkingCompilersConfiguration {
    use_clang: bool,
    use_gcc: bool,
    compiler_args: Vec<String>,
    custom_clang: PathBuf,
    custom_gcc: PathBuf,
    debug_clang_commands: bool,
    debug_gcc_commands: bool,
}

impl LinkingCompilersConfiguration {
    #[inline]
    pub fn new() -> Self {
        Self {
            use_clang: true,
            use_gcc: false,

            compiler_args: Vec::with_capacity(50),

            custom_clang: "clang".into(),
            custom_gcc: "gcc".into(),

            debug_clang_commands: false,
            debug_gcc_commands: false,
        }
    }
}

impl LinkingCompilersConfiguration {
    pub fn comprobate_status_to_determinate_usage_automatically(&mut self) {
        let mut clang_binding: std::process::Command =
            std::process::Command::new(&self.custom_clang);

        let clang_command: &mut std::process::Command = clang_binding
            .arg("-v")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .stdin(Stdio::null());

        let mut gcc_binding: std::process::Command = std::process::Command::new(&self.custom_gcc);

        let gcc_command: &mut std::process::Command = gcc_binding
            .arg("-v")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .stdin(Stdio::null());

        let clang_result: Result<std::process::ExitStatus, std::io::Error> = clang_command.status();
        let gcc_result: Result<std::process::ExitStatus, std::io::Error> = gcc_command.status();

        if self.use_clang
            && (clang_result.is_err() || clang_result.is_ok_and(|status| !status.success()))
            && self.custom_clang.components().count() == 0
        {
            self.use_clang = false;
            self.use_gcc = true;
        }

        if self.use_gcc
            && (gcc_result.is_err() || gcc_result.is_ok_and(|status| !status.success()))
            && self.custom_gcc.components().count() == 0
        {
            self.use_gcc = false;
            self.use_clang = true;
        }
    }
}

impl LinkingCompilersConfiguration {
    #[inline]
    pub fn get_args(&self) -> &[String] {
        &self.compiler_args
    }

    #[inline]
    pub fn get_custom_clang(&self) -> &Path {
        &self.custom_clang
    }

    #[inline]
    pub fn get_debug_clang_commands(&self) -> bool {
        self.debug_clang_commands
    }

    #[inline]
    pub fn get_debug_gcc_commands(&self) -> bool {
        self.debug_gcc_commands
    }

    #[inline]
    pub fn get_custom_gcc(&self) -> &Path {
        &self.custom_gcc
    }

    #[inline]
    pub fn get_use_clang(&self) -> bool {
        self.use_clang
    }

    #[inline]
    pub fn get_use_gcc(&self) -> bool {
        self.use_gcc
    }
}

impl LinkingCompilersConfiguration {
    #[inline]
    pub fn set_use_clang(&mut self, value: bool) {
        self.use_clang = value;
    }

    #[inline]
    pub fn set_use_gcc(&mut self, value: bool) {
        self.use_gcc = value;
    }

    #[inline]
    pub fn set_custom_clang(&mut self, value: PathBuf) {
        self.custom_clang = value;
    }

    #[inline]
    pub fn set_custom_gcc(&mut self, value: PathBuf) {
        self.custom_gcc = value;
    }

    #[inline]
    pub fn set_debug_clang_commands(&mut self, value: bool) {
        self.debug_clang_commands = value;
    }

    #[inline]
    pub fn set_debug_gcc_commands(&mut self, value: bool) {
        self.debug_gcc_commands = value;
    }
}

impl LinkingCompilersConfiguration {
    #[inline]
    pub fn add_argument(&mut self, value: String) {
        self.compiler_args.push(value);
    }
}
