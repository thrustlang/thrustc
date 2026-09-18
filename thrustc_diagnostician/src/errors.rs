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

use crate::traits::ErrorDisassembler;
use crate::traits::IssueDisassembler;

use thrustc_errors::CompilationPosition;
use thrustc_logging::LoggingType;

use std::path::Path;

pub type Error<'a> = (&'a str, &'a Path, Option<&'a str>, LoggingType);

pub type FrontendError<'a> = (
    &'a str,
    CompilationPosition,
    LoggingType,
    &'a Path,
    &'a Path,
    u32,
);

pub type BackendError<'a> = (
    &'a str,
    CompilationPosition,
    LoggingType,
    &'a Path,
    &'a Path,
    u32,
);

impl ErrorDisassembler for FrontendError<'_> {
    #[inline]
    fn get_title(&self) -> &str {
        self.0
    }

    #[inline]
    fn get_position(&self) -> CompilationPosition {
        self.1
    }

    #[inline]
    fn get_logging_type(&self) -> LoggingType {
        self.2
    }

    #[inline]
    fn get_source_path(&self) -> &Path {
        self.3
    }

    #[inline]
    fn get_compiler_source_path(&self) -> &Path {
        self.4
    }

    #[inline]
    fn get_line(&self) -> u32 {
        self.5
    }
}

impl IssueDisassembler for Error<'_> {
    #[inline]
    fn get_title(&self) -> &str {
        self.0
    }

    #[inline]
    fn get_path(&self) -> &Path {
        self.1
    }

    #[inline]
    fn get_note(&self) -> Option<&str> {
        self.2
    }

    #[inline]
    fn get_logging_type(&self) -> LoggingType {
        self.3
    }
}
