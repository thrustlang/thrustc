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

use std::fs;
use std::path::PathBuf;

fn main() {
    let path: String = std::env::args()
        .nth(1)
        .expect("usage: dump_ast_local <crash-file>");

    let data: Vec<u8> = fs::read(&path).expect("could not read crash file");

    match thrustc_fuzz::dumps::ast_dump("llvm-codegen-local", &data) {
        Ok(contents) => {
            let out_dir = PathBuf::from("fuzz/ast_dumps");
            fs::create_dir_all(&out_dir).unwrap();

            let name = PathBuf::from(&path)
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();
            let out_path = out_dir.join(format!("{name}.txt"));

            fs::write(&out_path, contents).unwrap();
            println!("AST dumped successfully to: {}", out_path.display());
        }
        Err(e) => eprintln!("{e}"),
    }
}
