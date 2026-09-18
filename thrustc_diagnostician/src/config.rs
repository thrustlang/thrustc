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

#[derive(Debug, Clone, Default)]
pub struct DiagnosticianConfig {
    export_path: PathBuf,
    export_errors: bool,
    export_warnings: bool,
}

impl DiagnosticianConfig {
    #[inline]
    pub fn new(export_path: PathBuf, export_errors: bool, export_warnings: bool) -> Self {
        Self {
            export_path,
            export_errors,
            export_warnings,
        }
    }
}

impl DiagnosticianConfig {
    #[inline]
    pub fn export_path(&self) -> &Path {
        &self.export_path
    }

    #[inline]
    pub fn export_errors(&self) -> bool {
        self.export_errors
    }

    #[inline]
    pub fn export_warnings(&self) -> bool {
        self.export_warnings
    }
}
