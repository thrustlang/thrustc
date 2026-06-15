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
    build_dynamic_library: bool,
    build_static_library: bool,
    build_relocatable_object: bool,
    extra_library_paths: Vec<PathBuf>,
    link_libraries: Vec<String>,
    debug_command: bool,
    link_dynamic: bool,
    link_static: bool,
    ansi_colors: bool,
    output: String,
}

impl LinkerConfiguration {
    pub fn new() -> Self {
        Self {
            extra_library_paths: Vec::with_capacity(u8::MAX as usize),
            link_libraries: Vec::with_capacity(u8::MAX as usize),
            build_executable: true,
            build_dynamic_library: false,
            build_static_library: false,
            build_relocatable_object: false,
            debug_command: false,
            link_dynamic: true,
            link_static: false,
            ansi_colors: false,
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

    #[inline]
    pub fn set_use_ansi_colors(&mut self) {
        self.ansi_colors = true;
    }

    #[inline]
    pub fn set_build_dynamic_library(&mut self) {
        self.build_dynamic_library = true;
        self.build_static_library = false;
        self.build_executable = false;
        self.build_relocatable_object = false;
    }

    #[inline]
    pub fn set_build_static_library(&mut self) {
        self.build_static_library = true;
        self.build_dynamic_library = false;
        self.build_executable = false;
        self.build_relocatable_object = false;
    }

    #[inline]
    pub fn set_build_relocatable_object(&mut self) {
        self.build_relocatable_object = true;
        self.build_dynamic_library = false;
        self.build_static_library = false;
        self.build_executable = false;
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
    pub fn build_dynamic_library(&self) -> bool {
        self.build_dynamic_library
    }

    #[inline]
    pub fn build_static_library(&self) -> bool {
        self.build_static_library
    }

    #[inline]
    pub fn build_relocatable_object(&self) -> bool {
        self.build_relocatable_object
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

    #[inline]
    pub fn use_ansi_colors(&self) -> bool {
        self.ansi_colors
    }
}
