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

use std::path::PathBuf;

#[derive(Debug)]
pub struct LinkerConfiguration {
    build_executable: bool,
    extra_library_paths: Vec<PathBuf>,
    link_libraries: Vec<String>,
    debug_command: bool,
    link_dynamic: bool,
    link_static: bool,
    output: String,
}

impl LinkerConfiguration {
    pub fn new() -> Self {
        Self {
            extra_library_paths: Vec::with_capacity(u8::MAX as usize),
            link_libraries: Vec::with_capacity(u8::MAX as usize),
            build_executable: true,
            debug_command: false,
            link_dynamic: true,
            link_static: false,
            output: String::new(),
        }
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn add_library_path(&mut self, path: PathBuf) {
        self.extra_library_paths.push(path);
    }

    #[inline]
    pub fn add_link_library(&mut self, prefix: String) {
        self.link_libraries.push(prefix);
    }

    #[inline]
    pub fn set_build_executable(&mut self, value: bool) {
        self.build_executable = value;
    }

    #[inline]
    pub fn set_debug_linker_command(&mut self, value: bool) {
        self.debug_command = value
    }

    #[inline]
    pub fn set_output(&mut self, output: String) {
        self.output = output
    }

    #[inline]
    pub fn set_link_static(&mut self, value: bool) {
        self.link_static = value
    }

    #[inline]
    pub fn set_link_dynamic(&mut self, value: bool) {
        self.link_dynamic = value;
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn get_libraries_paths(&self) -> &[PathBuf] {
        &self.extra_library_paths
    }

    #[inline]
    pub fn get_link_libraries(&self) -> &[String] {
        &self.link_libraries
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn build_executable(&self) -> bool {
        self.build_executable
    }

    #[inline]
    pub fn debug_command(&self) -> bool {
        self.debug_command
    }

    #[inline]
    pub fn output(&self) -> &str {
        &self.output
    }

    #[inline]
    pub fn link_dynamic(&self) -> bool {
        self.link_dynamic
    }

    #[inline]
    pub fn link_static(&self) -> bool {
        self.link_static
    }
}
