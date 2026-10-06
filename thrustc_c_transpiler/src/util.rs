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

pub fn sanitize_thrust_identifier(raw: impl AsRef<str>) -> String {
    let __sanitized: String = raw.as_ref().to_string();

    match __sanitized.as_str() {
        "array" | "asm" | "bool" | "break" | "char" | "const" | "continue" | "deref"
        | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if" | "import" | "importC"
        | "load" | "loop" | "ptr" | "ref" | "return" | "struct" | "true" | "type" | "union"
        | "var" | "void" | "while" => {
            format!("{__sanitized}_")
        }

        _ => __sanitized,
    }
}
