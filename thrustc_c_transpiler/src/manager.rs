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

use std::path::{Path, PathBuf};

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::ImportCOptions;
use thrustc_options::TranslateCOptions;

use crate::options::{EmitCBindingsOptions, TranslateCOutput};

pub fn translate_single_c_to_thrust(
    input: &Path,
    translate_opts: &TranslateCOptions,
) -> Result<TranslateCOutput, Vec<CompilationIssue>> {
    let span: thrustc_code_location::Span = thrustc_code_location::Span::nothing();

    let mut issues: Vec<CompilationIssue> = Vec::new();

    let input_path: PathBuf = input.to_path_buf();

    let canonical_input: PathBuf = input_path
        .canonicalize()
        .unwrap_or_else(|_| input_path.clone());

    let mut clang_args: Vec<String> = Vec::new();

    // Be explicit; libclang sometimes sees unsaved or non-standard inputs.
    clang_args.push("-xc".into());

    clang_args.extend(crate::clang_util::build_clang_arguments(translate_opts));

    let clang: clang::Clang = match clang::Clang::new() {
        Ok(c) => c,
        Err(_) => {
            return Err(vec![CompilationIssue::Error(
                CompilationIssueCode::E0111,
                "Failed to initialize libclang.".into(),
                "Ensure libclang is available in this build.".into(),
                None,
                span,
            )]);
        }
    };

    let index: clang::Index = clang::Index::new(&clang, false, false);

    let args_refs: Vec<&str> = clang_args.iter().map(String::as_str).collect();

    let mut parser: clang::Parser = index.parser(&canonical_input);

    parser.detailed_preprocessing_record(true);
    parser.arguments(&args_refs);

    let translation_unit: clang::TranslationUnit<'_> = match parser.parse() {
        Ok(translation_unit) => translation_unit,
        Err(e) => {
            return Err(vec![CompilationIssue::Error(
                CompilationIssueCode::E0111,
                format!("Failed to parse C file '{}'.", canonical_input.display()),
                format!("Clang error: {e}"),
                None,
                span,
            )]);
        }
    };

    let mut has_errors: bool = false;

    let mut error_count: usize = 0;

    let mut warning_count: usize = 0;

    let mut fatal_diagnostics: Vec<String> = Vec::new();

    for diagnostic in translation_unit.get_diagnostics() {
        let severity: clang::diagnostic::Severity = diagnostic.get_severity();
        let text: String = diagnostic.get_text();

        let expansion: clang::source::Location<'_> =
            diagnostic.get_location().get_expansion_location();

        let prefix: String = expansion
            .file
            .map(|file| {
                format!(
                    "{}:{}:{}: ",
                    file.get_path().display(),
                    expansion.line,
                    expansion.column
                )
            })
            .unwrap_or_default();

        if matches!(
            severity,
            clang::diagnostic::Severity::Error | clang::diagnostic::Severity::Fatal
        ) {
            has_errors = true;
            error_count = error_count.saturating_add(1);
            fatal_diagnostics.push(format!("{prefix}{severity:?}: {text}"));
        } else {
            warning_count = warning_count.saturating_add(1);
        }

        issues.push(CompilationIssue::Warning(
            CompilationIssueCode::W0100,
            format!("{prefix}{severity:?}: {text}"),
            span,
        ));
    }

    if has_errors {
        let message: String = format!(
            "Clang reported errors while parsing the C input:\n{}",
            fatal_diagnostics.join("\n")
        );

        let note: String =
            format!("{error_count} errors, {warning_count} warnings emitted by clang.");

        return Err(vec![CompilationIssue::Error(
            CompilationIssueCode::E0111,
            message,
            "Fix the C input or provide the correct --translate-c-* options.".into(),
            Some(note),
            span,
        )]);
    }

    let Some(main_file) = translation_unit.get_file(&canonical_input) else {
        let errors: Vec<CompilationIssue> = vec![CompilationIssue::Error(
            CompilationIssueCode::E0111,
            "Unable to locate the main file inside the translation unit.".into(),
            "Ensure the input path points to a real file.".into(),
            None,
            span,
        )];

        return Err(errors);
    };

    let cwd: PathBuf = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    let includes: Vec<clang::Entity<'_>> = main_file.get_includes();

    let mut import_c_include_dirs: Vec<PathBuf> = Vec::new();
    let mut import_c_specs: Vec<String> = Vec::new();

    for inc in includes.iter() {
        if let Some(spec) = crate::macros::extract_include_spec(inc) {
            import_c_specs.push(spec);

            if let Some(file) = inc.get_file() {
                let path: PathBuf = file.get_path();

                if let Some(parent) = path.parent() {
                    let mut dir: PathBuf = parent.to_path_buf();

                    if let Ok(rel) = dir.strip_prefix(&cwd) {
                        dir = rel.to_path_buf();
                    }

                    if !file.get_location(1, 1).is_in_system_header()
                        && !import_c_include_dirs.contains(&dir)
                    {
                        import_c_include_dirs.push(dir);
                    }
                }
            }
        }
    }

    import_c_include_dirs.sort();
    import_c_specs.sort();
    import_c_specs.dedup();

    let mut out: String = String::with_capacity(64 * 1024);

    for dir in import_c_include_dirs.iter() {
        out.push_str("directive \"--import-c-include=");
        out.push_str(&dir.to_string_lossy());
        out.push_str("\";\n");
    }

    if !import_c_include_dirs.is_empty() {
        out.push('\n');
    }

    for spec in import_c_specs.iter() {
        out.push_str("importC \"");
        out.push_str(spec);
        out.push_str("\";\n");
    }

    if !import_c_specs.is_empty() {
        out.push('\n');
    }

    let root: clang::Entity<'_> = translation_unit.get_entity();

    let mut macro_ctx: crate::macros::MacroContext<'_> =
        crate::macros::MacroContext::new(canonical_input.clone());

    crate::top_level::append_translated_top_level_declarations(
        &root,
        &mut macro_ctx,
        &mut out,
        span,
    );

    issues.extend(macro_ctx.get_mut_transpiler_context().take_warnings());
    issues.extend(macro_ctx.get_mut_transpiler_context().take_errors());
    issues.extend(macro_ctx.get_mut_transpiler_context().take_macros_errors());

    let output_path: PathBuf = translate_opts
        .output()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| {
            let stem: String = canonical_input
                .file_stem()
                .map_or_else(String::new, |s| s.to_string_lossy().to_string());

            let file_name: String = format!("{stem}.thrust");

            translate_opts
                .out_dir()
                .map(|out_dir| out_dir.join(&file_name))
                .or_else(|| {
                    canonical_input
                        .parent()
                        .map(|parent| parent.join(&file_name))
                })
                .unwrap_or_else(|| PathBuf::from(file_name))
        });

    Ok((output_path, out, issues))
}

pub fn emit_c_bindings_thrust(
    header: PathBuf,
    import_opts: &ImportCOptions,
    emit_opts: &EmitCBindingsOptions,
) -> Result<(PathBuf, String, Vec<CompilationIssue>), String> {
    let span = thrustc_code_location::Span::nothing();

    let clang_args: Vec<String> = crate::clang_util::build_clang_arguments(import_opts);

    let mut warnings: Vec<CompilationIssue> = Vec::new();

    let mut opts: thrustc_c_import_synthesis::options::CImportOptions =
        thrustc_c_import_synthesis::options::CImportOptions::new();

    let import_scope: thrustc_c_import_synthesis::options::CImportScope =
        match import_opts.import_scope() {
            thrustc_options::ImportCScope::MainOnly => {
                thrustc_c_import_synthesis::options::CImportScope::MainOnly
            }
            thrustc_options::ImportCScope::TransitiveNoSystem => {
                thrustc_c_import_synthesis::options::CImportScope::TransitiveNoSystem
            }
            thrustc_options::ImportCScope::TransitiveAll => {
                thrustc_c_import_synthesis::options::CImportScope::TransitiveAll
            }
        };

    opts.set_import_scope(import_scope);
    *opts.clang_args_mut() = clang_args;

    let mut ctx: thrustc_c_import_synthesis::context::CImportContext =
        thrustc_c_import_synthesis::context::CImportContext::new(header.clone(), span, opts);

    ctx.import_header()?;

    for diagnostic in ctx.diagnostics() {
        let code: CompilationIssueCode = match diagnostic.kind() {

            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::ClangDiagnostic => {
                CompilationIssueCode::W0100
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::InventedFieldName => {
                CompilationIssueCode::W0101
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedUnion => {
                CompilationIssueCode::W0102
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedBitfieldStruct => {
                CompilationIssueCode::W0103
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::UnsupportedCallingConvention => {
                CompilationIssueCode::W0104
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedDeclaration => {
                CompilationIssueCode::W0104
            }
        };

        warnings.push(CompilationIssue::Warning(
            code,
            diagnostic.message().to_string(),
            span,
        ));
    }

    let mut out: String = String::with_capacity(32 * 1024);

    let mut macro_ctx: crate::macros::MacroContext<'_> =
        crate::macros::MacroContext::new(header.clone());

    crate::top_level::append_imported_top_level_declarations(&ctx, span, &mut out, &mut macro_ctx);

    warnings.extend(macro_ctx.get_mut_transpiler_context().take_warnings());
    warnings.extend(macro_ctx.get_mut_transpiler_context().take_errors());
    warnings.extend(macro_ctx.get_mut_transpiler_context().take_macros_errors());

    let output_path: PathBuf = emit_opts
        .output()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| {
            let stem: String = header
                .file_stem()
                .map_or_else(String::new, |s| s.to_string_lossy().to_string());

            let file_name: String = format!("{stem}.bindings.thrust");

            emit_opts
                .out_dir()
                .map(|out_dir| out_dir.join(&file_name))
                .unwrap_or_else(|| PathBuf::from(file_name))
        });

    Ok((output_path, out, warnings))
}
