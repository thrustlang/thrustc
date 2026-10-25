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

use crate::context::TranspilerContext;

#[derive(Debug)]
pub struct TranslateCUnit {
    output_path: Option<PathBuf>,
    contents: Option<String>,
    context: TranspilerContext,
}

impl TranslateCUnit {
    #[inline]
    pub fn new(
        output_path: Option<PathBuf>,
        contents: Option<String>,
        context: TranspilerContext,
    ) -> Self {
        Self {
            output_path,
            contents,
            context,
        }
    }
}

impl TranslateCUnit {
    #[inline]
    pub fn get_output_path(&self) -> Option<&Path> {
        self.output_path.as_deref()
    }

    #[inline]
    pub fn get_contents(&self) -> Option<&str> {
        self.contents.as_deref()
    }

    #[inline]
    pub fn get_context(&self) -> &TranspilerContext {
        &self.context
    }
}

impl TranslateCUnit {
    #[inline]
    pub fn get_mut_context(&mut self) -> &mut TranspilerContext {
        &mut self.context
    }
}

impl TranslateCUnit {
    #[inline]
    pub fn set_output_path(&mut self, output_path: Option<PathBuf>) {
        self.output_path = output_path;
    }

    #[inline]
    pub fn set_contents(&mut self, contents: Option<String>) {
        self.contents = contents;
    }
}
