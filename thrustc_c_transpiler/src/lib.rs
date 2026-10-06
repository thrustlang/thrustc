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

pub mod abort;
pub mod builtins;
pub mod clang_util;
pub mod context;
pub mod entrypoint;
pub mod expr;
pub mod expr_analysis;
pub mod location;
pub mod macro_error;
pub mod macro_ast;
pub mod macro_expr;
pub mod macro_expand;
pub mod macro_lex;
pub mod macro_stmt;
pub mod macro_table;
pub mod macro_token;
pub mod macro_type;
pub mod macros;
pub mod manager;
pub mod options;
pub mod stmt;
pub mod stmt_analysis;
pub mod top_level;
pub mod type_format;
pub mod util;
