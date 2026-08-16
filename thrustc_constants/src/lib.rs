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

pub const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const COMPILER_ID: &str = const_format::formatcp!("thrustc version {}", COMPILER_VERSION);
pub const COMPILER_GITHUB_URL: &str = "https://github.com/thrustlang/thrustc";

pub const COMPILER_OWN_FILE_EXTENSIONS: [&str; 3] = ["thrust", "tht", "🐦"];
pub const COMPILER_TOO_MANY_EXPRESSION_DEPTH: u32 = 1 << 12;
pub const COMPILER_TOO_MANY_TYPE_DEPTH: u32 = 1 << 12;
pub const COMPILER_TOO_MANY_BLOCK_DEPTH: u32 = 1 << 12;

pub const SUCCESFUL_CODE: i32 = 0;
pub const FAILURE_CODE: i32 = 1;

pub const COMPILER_HARD_OBFUSCATION_LEVEL: usize = 30;
pub const COMPILER_LOW_OBFUSCATION_LEVEL: usize = 15;

pub static LLVM_SYNTAX_HIGHLIGHTING: &[u8] =
    include_bytes!("../../highlighting/llvm.sublime-syntax");

pub static ONE_DARK_THEME: &[u8] = include_bytes!("../../highlighting/One Dark.tmTheme");
