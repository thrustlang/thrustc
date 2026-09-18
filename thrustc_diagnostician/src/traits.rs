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

use thrustc_errors::CompilationPosition;
use thrustc_logging::LoggingType;

use std::path::Path;

pub trait IssueDisassembler {
    fn get_title(&self) -> &str;
    fn get_logging_type(&self) -> LoggingType;
    fn get_path(&self) -> &Path;
    fn get_note(&self) -> Option<&str>;
}

pub trait ErrorDisassembler {
    fn get_title(&self) -> &str;
    fn get_position(&self) -> CompilationPosition;
    fn get_logging_type(&self) -> LoggingType;
    fn get_source_path(&self) -> &Path;
    fn get_compiler_source_path(&self) -> &Path;
    fn get_line(&self) -> u32;
}
