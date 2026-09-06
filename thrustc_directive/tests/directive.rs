use thrustc_code_location::Span;
use thrustc_directive::{FileDirectives, FileOptions, apply_directive, apply_file_directives};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::CompilerOptions;
use thrustc_token::Token;
use thrustc_token_type::TokenType;

#[test]
fn applies_local_optimization() {
    let mut directives: FileDirectives = FileDirectives::default();

    apply_directive("-opt=O3", &mut directives).unwrap();

    assert!(directives.optimization.unwrap().is_high_opt());
}

#[test]
fn rejects_global_flags() {
    let mut directives: FileDirectives = FileDirectives::default();

    let error: String =
        apply_directive("-target-triple=x86_64-unknown-linux-gnu", &mut directives).unwrap_err();

    assert!(error.contains("global"));
}

#[test]
fn accumulates_warning_directives() {
    let mut directives: FileDirectives = FileDirectives::default();

    apply_directive("--disable-warnings=W0020", &mut directives).unwrap();
    apply_directive("--disable-warnings=W0030", &mut directives).unwrap();

    assert_eq!(directives.warnings_to_disable.len(), 2);
}

#[test]
fn generated_warning_codes_only_contain_warnings() {
    assert!(
        CompilationIssueCode::ALL_WARNING_CODES
            .iter()
            .copied()
            .all(CompilationIssueCode::is_warning)
    );
    assert!(!CompilationIssueCode::E0054.is_warning());
}

#[test]
fn accepts_every_file_scoped_cli_flag() {
    let mut directives: FileDirectives = FileDirectives::default();
    let specs: [&str; 30] = [
        "-opt=O2",
        "-reloc-model=pic",
        "-code-model=small",
        "-dbg",
        "-dbg-for-inlining",
        "-dbg-for-profiling",
        "-dbg-dwarf-version=v4",
        "-stop-at=type-checking",
        "-emit=llvm-ir",
        "-print=ast",
        "--stack-protector",
        "--symbol-linkage-strategy=exact",
        "--denormal-floating-point-behavior=IEEE",
        "--denormal-floating-point-32-bits-behavior=dynamic",
        "--sanitizer=address",
        "--no-sanitize=bounds;coverage",
        "--disable-all-sanitizers",
        "--disable-frame-pointer",
        "--disable-uwtable",
        "--disable-direct-access-external-data",
        "--disable-rtlib-got",
        "--disable-safe-trapping-math",
        "--disable-safe-math",
        "--disable-default-optimizations",
        "--opt-passes=default<O2>",
        "--modificator-opt-passes=loopunroll",
        "--disable-warnings=W0020",
        "--disable-all-warnings",
        "--no-obfuscate-archive-names",
        "--no-obfuscate-ir",
    ];

    for spec in specs {
        apply_directive(spec, &mut directives).unwrap();
    }
}

#[test]
fn local_scalar_value_overrides_the_global_value() {
    let global: CompilerOptions = CompilerOptions::new();
    let mut directives: FileDirectives = FileDirectives::default();

    apply_directive("-opt=O3", &mut directives).unwrap();

    let effective: FileOptions<'_, '_> = FileOptions::new(&global, &directives);

    assert!(global.get_llvm_backend().get_optimization().is_none_opt());
    assert!(effective.optimization().is_high_opt());
}

#[test]
fn invalid_directive_uses_e0054_and_the_string_span() {
    let span: Span = Span::new((4, (10, 31)));
    let tokens: Vec<Token> = vec![
        Token {
            lexeme: "directive".into(),
            ascii: "directive".into(),
            kind: TokenType::Directive,
            span: Span::new((4, (0, 9))),
        },
        Token {
            lexeme: "-target-triple=x86_64".into(),
            ascii: "-target-triple=x86_64".into(),
            kind: TokenType::CString,
            span,
        },
    ];

    let error: CompilationIssue = apply_file_directives(&tokens).unwrap_err();

    match error {
        CompilationIssue::Error(code, _, _, _, error_span) => {
            assert_eq!(code, CompilationIssueCode::E0054);
            assert_eq!(error_span, span);
        }
        _ => panic!("expected an invalid-directive compiler error"),
    }
}
