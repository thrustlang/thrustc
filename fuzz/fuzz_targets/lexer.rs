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

#![no_main]

use libfuzzer_sys::{fuzz_target, Corpus};
use thrustc_lexer::Lexer;
use thrustc_options::{CompilationUnit, CompilerOptions};

fuzz_target!(|data: &[u8]| -> Corpus {
    let Ok(source) = std::str::from_utf8(data) else {
        return Corpus::Reject;
    };

    let options = CompilerOptions::new();

    let file = CompilationUnit::new(
        "lexer.fuzz".into(),
        std::path::PathBuf::from(file!()),
        source.to_string(),
        "lexer".into(),
    );

    let _ = Lexer::lex(&file, &options);

    Corpus::Keep
});
