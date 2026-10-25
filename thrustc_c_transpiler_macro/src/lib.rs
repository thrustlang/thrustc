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
pub mod error;
pub mod lex;
pub mod location;
pub mod macro_ast;
pub mod macro_context;
pub mod macro_error;
pub mod macro_expand;
pub mod macro_expand_variadic;
pub mod macro_expr;
pub mod macro_lex;
pub mod macro_parser;
pub mod macro_stmt;
pub mod macro_table;
pub mod macro_token;
pub mod macro_translation;
pub mod macro_type;
pub mod macros;
pub mod pointer;
pub mod thrust;
pub mod type_format;
pub mod util;
