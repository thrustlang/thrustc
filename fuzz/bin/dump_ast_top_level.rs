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

use arbitrary::{Arbitrary, Unstructured};
use std::fs;
use std::path::PathBuf;
use thrustc_ast::Ast;

fn main() {
    let path: String = std::env::args()
        .nth(1)
        .expect("usage: dump_ast <crash-file>");

    let data: Vec<u8> = fs::read(&path).expect("could not read crash file");

    let mut unstructured = Unstructured::new(&data);

    match Ast::arbitrary(&mut unstructured) {
        Ok(ast) => {
            let out_dir = PathBuf::from("fuzz/ast_dumps");
            fs::create_dir_all(&out_dir).unwrap();

            let name = PathBuf::from(&path)
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();
            let out_path = out_dir.join(format!("{name}.txt"));

            fs::write(&out_path, format!("{ast:#?}")).unwrap();
            println!("AST dumped to: {}", out_path.display());
        }
        Err(e) => eprintln!("Arbitrary failed to reconstruct the AST: {e}"),
    }
}
