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

use std::path::PathBuf;

use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;

pub(crate) fn collapse_macro_site_errors(
    ctx: &mut crate::macros::MacroContext<'_>,
    site: &clang::Entity<'_>,
    macro_name: &str,
    base_error_count: usize,
) {
    let mut fresh: Vec<CompilationIssue> = ctx
        .get_mut_transpiler_context()
        .take_new_errors_since(base_error_count);

    if fresh.is_empty() {
        return;
    }

    let site_key: String =
        crate::macro_table::MacroTable::make_location_key(site).unwrap_or_default();

    let definition_site: String = ctx
        .get_macro_table()
        .get_macro_definition_site(macro_name)
        .unwrap_or_default();

    let mut notes: Vec<String> = Vec::new();

    if !definition_site.is_empty() {
        notes.push(format!("defined at {definition_site}"));
    }

    if !site_key.is_empty() {
        notes.push(format!("expanded at {site_key}"));
    }

    let note: Option<String> = if notes.is_empty() {
        None
    } else {
        Some(notes.join("\n"))
    };

    let first: CompilationIssue = fresh.remove(0);

    match first {
        CompilationIssue::Error(code, message, help, ..) => {
            ctx.get_mut_transpiler_context().add_macros_error(
                CompilationIssue::Error(code, message, help, note, Span::nothing()),
            );
        }

        other => {
            ctx.get_mut_transpiler_context().add_macros_error(other);
        }
    }
}

pub(crate) fn collapse_macro_site_errors_with_failed_at(
    ctx: &mut crate::macros::MacroContext<'_>,
    site: &clang::Entity<'_>,
    macro_name: &str,
    failed_child: &clang::Entity<'_>,
    base_error_count: usize,
) {
    let mut fresh: Vec<CompilationIssue> = ctx
        .get_mut_transpiler_context()
        .take_new_errors_since(base_error_count);

    if fresh.is_empty() {
        return;
    }

    let site_key: String =
        crate::macro_table::MacroTable::make_location_key(site).unwrap_or_default();

    let definition_site: String = ctx
        .get_macro_table()
        .get_macro_definition_site(macro_name)
        .unwrap_or_default();

    let mut notes: Vec<String> = Vec::new();

    if !definition_site.is_empty() {
        notes.push(format!("defined at {definition_site}"));
    }

    if !site_key.is_empty() {
        notes.push(format!("expanded at {site_key}"));
    }

    let failed_at: String = failed_child
        .get_range()
        .map(|range| {
            let spelling = range.get_start().get_spelling_location();

            match spelling.file {
                Some(file) => format!(
                    "{}:{}:{}",
                    file.get_path().display(),
                    spelling.line,
                    spelling.column
                ),
                None => String::new(),
            }
        })
        .unwrap_or_default();

    if !failed_at.is_empty() {
        notes.push(format!("failed at {failed_at}"));

        if let Some(spelling_file) = failed_child
            .get_range()
            .and_then(|range| range.get_start().get_spelling_location().file)
        {
            let spelling_offset: u32 = failed_child
                .get_range()
                .map(|range| range.get_start().get_spelling_location().offset)
                .unwrap_or(0);

            let mut chain: Vec<String> = Vec::new();

            let mut current_file: PathBuf = spelling_file.get_path();

            let mut current_offset: u32 = spelling_offset;

            for _ in 0..8 {
                let Some(inner) = ctx
                    .get_macro_table()
                    .find_innermost_macro_at(&current_file, current_offset)
                else {
                    break;
                };

                if inner == macro_name {
                    continue;
                }

                let Some((body_file, _, _)) = ctx
                    .get_macro_table()
                    .get_macro_body_range(&inner)
                else {
                    break;
                };

                chain.push(format!("through macro '{inner}' at {}", body_file.display()));

                current_file = body_file;
                current_offset = 0;
            }

            notes.extend(chain);
        }
    }

    let note: Option<String> = if notes.is_empty() {
        None
    } else {
        Some(notes.join("\n"))
    };

    let first: CompilationIssue = fresh.remove(0);

    match first {
        CompilationIssue::Error(code, message, help, ..) => {
            ctx.get_mut_transpiler_context().add_macros_error(
                CompilationIssue::Error(code, message, help, note, Span::nothing()),
            );
        }

        other => {
            ctx.get_mut_transpiler_context().add_macros_error(other);
        }
    }
}
