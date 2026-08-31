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

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use thrustc_errors::CompilationIssueCode;
use thrustc_options::CompilerOptions;

#[derive(Debug, Default, Clone)]
pub struct FileDirectives {
    pub warnings_to_disable: Vec<CompilationIssueCode>,
}

thread_local! {
    static FILE_DIRECTIVES: RefCell<std::collections::BTreeMap<PathBuf, FileDirectives>> =
        const { RefCell::new(std::collections::BTreeMap::new()) };
}

pub fn parse_warning_codes(value: &str) -> Result<Vec<CompilationIssueCode>, String> {
    let mut warnings: Vec<CompilationIssueCode> = Vec::new();

    for warning in value.split(';') {
        let warning: &str = warning.trim();

        if warning.is_empty() {
            continue;
        }

        let code: CompilationIssueCode = CompilationIssueCode::parse(warning)
            .map_err(|_| format!("Invalid warning to disable: '{}'.", warning))?;

        warnings.push(code);
    }

    Ok(warnings)
}

pub fn parse_directive(spec: &str) -> Result<Vec<CompilationIssueCode>, String> {
    let spec: &str = spec.trim();

    if !spec.starts_with("--") {
        return Err("The directive must be a compiler flag written as '--flag=value'.".into());
    }

    let flag_spec: &str = &spec[2..];

    let (flag, value): (&str, &str) = flag_spec
        .split_once('=')
        .ok_or_else(|| format!("The directive '{}' expects a value using '='.", spec))?;

    match flag {
        "disable-warnings" => self::parse_warning_codes(value),
        _ => Err(format!("Unknown directive flag '--{}'.", flag)),
    }
}

pub fn register_directives(path: &Path, directives: FileDirectives) {
    FILE_DIRECTIVES.with(|cell| {
        let mut map: std::cell::RefMut<'_, std::collections::BTreeMap<PathBuf, FileDirectives>> =
            cell.borrow_mut();

        let entry: &mut FileDirectives = map.entry(path.to_path_buf()).or_default();

        for code in directives.warnings_to_disable {
            if !entry.warnings_to_disable.contains(&code) {
                entry.warnings_to_disable.push(code);
            }
        }
    });
}

pub fn combined_warnings_to_disable(
    options: &CompilerOptions,
    path: &Path,
) -> Vec<CompilationIssueCode> {
    let mut list: Vec<CompilationIssueCode> = options.get_warnings_to_disable().to_vec();

    if let Some(directives) = self::get_directives(path) {
        for code in directives.warnings_to_disable {
            if !list.contains(&code) {
                list.push(code);
            }
        }
    }

    list
}

pub fn get_directives(path: &Path) -> Option<FileDirectives> {
    FILE_DIRECTIVES.with(|cell| cell.borrow().get(path).cloned())
}

pub fn clear_directives() {
    FILE_DIRECTIVES.with(|cell| cell.borrow_mut().clear());
}
