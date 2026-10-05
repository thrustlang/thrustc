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

use std::collections::HashSet;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub(crate) struct MacroTable {
    input_source_file: PathBuf,
    statement_macro_definitions: Vec<(String, Vec<String>, Vec<String>)>,
    macro_expansion_sites: Vec<(String, String)>,
    emitted_outline_names: HashSet<String>,
    pending_outline_functions: Vec<String>,
}

impl MacroTable {
    #[inline]
    pub(crate) fn new(input_source_file: PathBuf) -> Self {
        Self {
            input_source_file,
            statement_macro_definitions: Vec::new(),
            macro_expansion_sites: Vec::new(),
            emitted_outline_names: HashSet::new(),
            pending_outline_functions: Vec::new(),
        }
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn get_input_source_file(&self) -> &Path {
        &self.input_source_file
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn add_statement_macro_definition(
        &mut self,
        macro_name: String,
        parameter_names: Vec<String>,
        body_tokens: Vec<String>,
    ) {
        if self
            .statement_macro_definitions
            .iter()
            .any(|entry| entry.0 == macro_name)
        {
            return;
        }

        self.statement_macro_definitions
            .push((macro_name, parameter_names, body_tokens));
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn get_parameter_names(&self, macro_name: &str) -> Option<Vec<String>> {
        self.statement_macro_definitions
            .iter()
            .find(|entry| entry.0 == macro_name)
            .map(|entry| entry.1.clone())
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn get_body_tokens(&self, macro_name: &str) -> Option<Vec<String>> {
        self.statement_macro_definitions
            .iter()
            .find(|entry| entry.0 == macro_name)
            .map(|entry| entry.2.clone())
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn register_statement_macros(
        &mut self,
        items: &[(clang::Entity<'_>, String, crate::macros::MacroKind)],
    ) {
        for (_, macro_name, kind) in items.iter() {
            if let crate::macros::MacroKind::Statement { parameters, body } = kind {
                self.add_statement_macro_definition(
                    macro_name.clone(),
                    parameters.clone(),
                    body.clone(),
                );
            }
        }
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn record_macro_expansion(&mut self, location_key: String, macro_name: String) {
        if self
            .macro_expansion_sites
            .iter()
            .any(|entry| entry.0 == location_key)
        {
            return;
        }

        self.macro_expansion_sites.push((location_key, macro_name));
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn find_macro_name(&self, location_key: &str) -> Option<String> {
        self.macro_expansion_sites
            .iter()
            .find(|entry| entry.0 == location_key)
            .map(|entry| entry.1.clone())
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn has_emitted_outline(&self, macro_name: &str) -> bool {
        self.emitted_outline_names
            .iter()
            .any(|entry| entry == macro_name)
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn mark_outline_emitted(&mut self, macro_name: &str) {
        self.emitted_outline_names.insert(macro_name.to_string());
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn add_pending_outline_function(&mut self, text: String) {
        self.pending_outline_functions.push(text);
    }
}

impl MacroTable {
    #[inline]
    pub(crate) fn take_pending_outline_functions(&mut self) -> Vec<String> {
        std::mem::take(&mut self.pending_outline_functions)
    }
}

impl MacroTable {
    pub(crate) fn make_location_key(entity: &clang::Entity<'_>) -> Option<String> {
        let range: clang::source::SourceRange<'_> = entity.get_range()?;

        let location = range.get_start().get_expansion_location();

        let file: clang::source::File<'_> = location.file?;

        Some(format!(
            "{}:{}:{}",
            file.get_path().display(),
            location.line,
            location.column
        ))
    }
}

impl MacroTable {
    pub(crate) fn scan_macro_expansions(&mut self, root: &clang::Entity<'_>) {
        if root.get_kind() == clang::EntityKind::MacroExpansion {
            if let (Some(macro_name), Some(location_key)) =
                (root.get_name(), Self::make_location_key(root))
            {
                self.record_macro_expansion(location_key, macro_name);
            }
        }

        for child in root.get_children().iter() {
            self.scan_macro_expansions(child);
        }
    }
}

impl MacroTable {
    pub(crate) fn entity_originates_in_input(&self, entity: &clang::Entity<'_>) -> bool {
        if entity.is_in_main_file() {
            return true;
        }

        let Some(location) = entity.get_location() else {
            return false;
        };

        location
            .get_expansion_location()
            .file
            .map(|file| {
                file.get_path()
                    .canonicalize()
                    .unwrap_or_else(|_| file.get_path())
                    == self.input_source_file
            })
            .unwrap_or(false)
    }
}
