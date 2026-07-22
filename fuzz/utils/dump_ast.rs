use arbitrary::{Arbitrary, Unstructured};
use std::fs;
use std::path::PathBuf;
use thrustc_ast::Ast;

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("usage: dump_ast <crash-file>");
    let data = fs::read(&path).expect("could not read crash file");

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
