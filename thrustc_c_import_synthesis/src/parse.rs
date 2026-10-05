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

use clang::diagnostic::Severity;

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::options::CImportScope;

pub fn build_parse_inputs(header_path: &Path) -> (bool, PathBuf, PathBuf, Vec<clang::Unsaved>) {
    let header_exists: bool = header_path.exists();
    let wrapper_path: PathBuf = PathBuf::from("__thrustc_importc_wrapper.c");

    if header_exists {
        return (true, wrapper_path, header_path.to_path_buf(), Vec::new());
    }

    let spec: String = header_path.to_string_lossy().to_string();
    let escaped: String = spec.replace('\\', "\\\\").replace('"', "\\\"");
    let wrapper_contents: String = format!("#include \"{}\"\n", escaped);
    let unsaved: clang::Unsaved = clang::Unsaved::new(&wrapper_path, wrapper_contents);

    (false, wrapper_path.clone(), wrapper_path, vec![unsaved])
}

pub fn collect_diagnostics(
    tu: &clang::TranslationUnit<'_>,
) -> (Vec<crate::diagnostics::CImportDiagnostic>, bool) {
    let mut diagnostics: Vec<CImportDiagnostic> = Vec::new();
    let mut has_errors: bool = false;

    for diagnostic in tu.get_diagnostics() {
        let severity: Severity = diagnostic.get_severity();
        let text: String = diagnostic.get_text();

        let expansion = diagnostic.get_location().get_expansion_location();

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

        diagnostics.push(CImportDiagnostic::new(
            CImportDiagnosticKind::ClangDiagnostic,
            format!("{prefix}{severity:?}: {text}"),
        ));

        if matches!(severity, Severity::Error | Severity::Fatal) {
            has_errors = true;
        }
    }

    (diagnostics, has_errors)
}

pub fn resolve_main_only_file<'clang>(
    import_scope: CImportScope,
    tu: &'clang clang::TranslationUnit<'_>,
    header_exists: bool,
    header_path: &PathBuf,
    wrapper_path: &PathBuf,
) -> Option<clang::source::File<'clang>> {
    if import_scope != CImportScope::MainOnly {
        return None;
    }

    if header_exists {
        return tu.get_file(header_path);
    }

    let wrapper_file: Option<clang::source::File<'clang>> = tu.get_file(wrapper_path);
    let includes: Vec<clang::Entity<'_>> = wrapper_file.map_or_else(Vec::new, |f| f.get_includes());

    includes.first().and_then(|inc| inc.get_file())
}
