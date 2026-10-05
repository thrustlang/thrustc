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

use std::path::{Path, PathBuf};

use thrustc_errors::CompilationIssue;

pub(crate) type TranslateCOutput = (PathBuf, String, Vec<CompilationIssue>);

#[derive(Debug, Clone)]
pub struct EmitCBindingsOptions {
    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            out_dir: None,
            output: None,
        }
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn out_dir(&self) -> Option<&Path> {
        self.out_dir.as_deref()
    }

    #[inline]
    pub fn output(&self) -> Option<&Path> {
        self.output.as_deref()
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn set_out_dir(&mut self, out_dir: PathBuf) {
        self.out_dir = Some(out_dir);
    }

    #[inline]
    pub fn set_output(&mut self, output: PathBuf) {
        self.output = Some(output);
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn out_dir_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.out_dir
    }

    #[inline]
    pub fn output_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.output
    }
}
