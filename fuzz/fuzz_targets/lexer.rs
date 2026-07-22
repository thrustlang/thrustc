#![no_main]

use libfuzzer_sys::{Corpus, fuzz_target};
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
