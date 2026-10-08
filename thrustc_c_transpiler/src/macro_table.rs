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

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroFunctionLikeKind {
    PureFunction,
    Statement,
    Unsupported(crate::macro_error::MacroLimit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroFunctionLikeDefinition {
    name: String,
    parameters: Vec<String>,
    body: Vec<crate::macro_token::MacroToken>,
    kind: MacroFunctionLikeKind,
}

impl MacroFunctionLikeDefinition {
    #[inline]
    pub fn new(
        name: String,
        parameters: Vec<String>,
        body: Vec<crate::macro_token::MacroToken>,
        kind: MacroFunctionLikeKind,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            kind,
        }
    }
}

impl MacroFunctionLikeDefinition {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_parameters(&self) -> &[String] {
        &self.parameters
    }

    #[inline]
    pub fn get_body(&self) -> &[crate::macro_token::MacroToken] {
        &self.body
    }

    #[inline]
    pub fn get_kind(&self) -> MacroFunctionLikeKind {
        self.kind
    }
}

#[derive(Debug)]
pub struct MacroTable {
    input_source_file: PathBuf,
    statement_macro_definitions: Vec<(String, Vec<String>, Vec<crate::macro_token::MacroToken>)>,
    function_like_macro_definitions: HashMap<String, MacroFunctionLikeDefinition>,
    object_macro_definitions: HashMap<String, Vec<crate::macro_token::MacroToken>>,
    macro_definition_sites: HashMap<String, String>,
    macro_body_ranges: HashMap<String, (PathBuf, u32, u32)>,
    macro_expansion_sites: Vec<(String, String)>,
    emitted_statement_macro_functions: HashSet<String>,
    pending_statement_macro_functions: Vec<String>,
}

impl MacroTable {
    #[inline]
    pub fn new(input_source_file: PathBuf) -> Self {
        Self {
            input_source_file,
            statement_macro_definitions: Vec::new(),
            function_like_macro_definitions: HashMap::new(),
            object_macro_definitions: HashMap::new(),
            macro_definition_sites: HashMap::new(),
            macro_body_ranges: HashMap::new(),
            macro_expansion_sites: Vec::new(),
            emitted_statement_macro_functions: HashSet::new(),
            pending_statement_macro_functions: Vec::new(),
        }
    }
}

impl MacroTable {
    #[inline]
    pub fn get_input_source_file(&self) -> &Path {
        &self.input_source_file
    }

    #[inline]
    pub fn get_macro_definition_site(&self, macro_name: &str) -> Option<String> {
        self.macro_definition_sites.get(macro_name).cloned()
    }

    #[inline]
    pub fn get_macro_body_range(&self, macro_name: &str) -> Option<(PathBuf, u32, u32)> {
        self.macro_body_ranges.get(macro_name).cloned()
    }

    #[inline]
    pub fn get_parameter_names(&self, macro_name: &str) -> Option<Vec<String>> {
        self.statement_macro_definitions
            .iter()
            .find(|entry| entry.0 == macro_name)
            .map(|entry| entry.1.clone())
    }

    #[inline]
    pub fn get_body_tokens(&self, macro_name: &str) -> Option<Vec<String>> {
        self.statement_macro_definitions
            .iter()
            .find(|entry| entry.0 == macro_name)
            .map(|entry| crate::macro_token::texts(&entry.2))
    }

    #[inline]
    pub fn get_body_macro_tokens(
        &self,
        macro_name: &str,
    ) -> Option<Vec<crate::macro_token::MacroToken>> {
        self.statement_macro_definitions
            .iter()
            .find(|entry| entry.0 == macro_name)
            .map(|entry| entry.2.clone())
    }

    #[inline]
    pub fn find_macro_name(&self, location_key: &str) -> Option<String> {
        self.macro_expansion_sites
            .iter()
            .find(|entry| entry.0 == location_key)
            .map(|entry| entry.1.clone())
    }

    #[inline]
    pub fn has_emitted_statement_macro_function(&self, macro_name: &str) -> bool {
        self.emitted_statement_macro_functions
            .iter()
            .any(|entry| entry == macro_name)
    }

    #[inline]
    pub fn get_function_like_definition(
        &self,
        macro_name: &str,
    ) -> Option<&MacroFunctionLikeDefinition> {
        self.function_like_macro_definitions.get(macro_name)
    }

    #[inline]
    pub fn get_function_like_definition_cloned(
        &self,
        macro_name: &str,
    ) -> Option<MacroFunctionLikeDefinition> {
        self.function_like_macro_definitions
            .get(macro_name)
            .cloned()
    }

    #[inline]
    pub fn get_object_macro_definition(
        &self,
        macro_name: &str,
    ) -> Option<Vec<crate::macro_token::MacroToken>> {
        self.object_macro_definitions.get(macro_name).cloned()
    }
}

impl MacroTable {
    #[inline]
    pub fn register_object_macro(
        &mut self,
        macro_name: String,
        body: Vec<crate::macro_token::MacroToken>,
    ) {
        self.object_macro_definitions.insert(macro_name, body);
    }
}

impl MacroTable {
    #[inline]
    pub fn register_function_like_macros(
        &mut self,
        items: &[(clang::Entity<'_>, String, crate::macros::MacroKind)],
    ) {
        for (definition, macro_name, kind) in items.iter() {
            let mut function_like_kind: MacroFunctionLikeKind = match kind {
                crate::macros::MacroKind::Object => continue,
                crate::macros::MacroKind::PureFunction { .. } => {
                    MacroFunctionLikeKind::PureFunction
                }
                crate::macros::MacroKind::Statement { .. } => MacroFunctionLikeKind::Statement,
                crate::macros::MacroKind::Unsupported(limit) => {
                    MacroFunctionLikeKind::Unsupported(*limit)
                }
            };

            let (parameters, body): (Vec<String>, Vec<crate::macro_token::MacroToken>) = match kind
            {
                crate::macros::MacroKind::PureFunction { parameters, body }
                | crate::macros::MacroKind::Statement { parameters, body } => {
                    (parameters.clone(), body.clone())
                }
                crate::macros::MacroKind::Unsupported(_) => {
                    let tokens: Vec<crate::macro_token::MacroToken> = definition
                        .get_range()
                        .map(|range| {
                            let clang_tokens: Vec<clang::token::Token<'_>> = range.tokenize();
                            crate::macro_lex::to_macro_tokens(
                                &clang_tokens,
                                crate::macro_token::MacroTokenOrigin::DefinitionBody,
                            )
                        })
                        .unwrap_or_default();

                    (Vec::new(), tokens)
                }
                crate::macros::MacroKind::Object => (Vec::new(), Vec::new()),
            };

            if function_like_kind == MacroFunctionLikeKind::PureFunction {
                let spellings: Vec<String> = crate::macro_token::texts(&body);

                if crate::macro_expr::MacroCursor::parse(&spellings).is_err() {
                    function_like_kind = MacroFunctionLikeKind::Unsupported(
                        crate::macro_error::MacroLimit::ExpressionMalformed,
                    );
                }
            }

            if function_like_kind == MacroFunctionLikeKind::Statement
                && crate::macro_stmt::parse_statement_body_tokens(&body).is_err()
            {
                function_like_kind = MacroFunctionLikeKind::Unsupported(
                    crate::macro_error::MacroLimit::StatementMalformed,
                );
            }

            self.function_like_macro_definitions.insert(
                macro_name.clone(),
                MacroFunctionLikeDefinition::new(
                    macro_name.clone(),
                    parameters,
                    body,
                    function_like_kind,
                ),
            );
        }
    }

    #[inline]
    pub fn add_statement_macro_definition(
        &mut self,
        macro_name: String,
        parameter_names: Vec<String>,
        body_tokens: Vec<crate::macro_token::MacroToken>,
        definition: &clang::Entity<'_>,
    ) {
        if self
            .statement_macro_definitions
            .iter()
            .any(|entry| entry.0 == macro_name)
        {
            return;
        }

        if crate::macro_stmt::parse_statement_body_tokens(&body_tokens).is_err() {
            return;
        }

        let definition_site: String = definition
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

        let body_range: Option<(PathBuf, u32, u32)> = definition.get_range().map(|range| {
            let start: clang::source::Location<'_> = range.get_start().get_spelling_location();
            let end: clang::source::Location<'_> = range.get_end().get_spelling_location();

            let path: PathBuf = start
                .file
                .as_ref()
                .map(|file| file.get_path())
                .unwrap_or_else(|| PathBuf::from(""));

            (path, start.offset, end.offset)
        });

        self.macro_definition_sites
            .entry(macro_name.clone())
            .or_insert(definition_site);

        if let Some(range) = body_range {
            self.macro_body_ranges
                .entry(macro_name.clone())
                .or_insert(range);
        }

        self.statement_macro_definitions
            .push((macro_name, parameter_names, body_tokens));
    }

    #[inline]
    pub fn register_statement_macros(
        &mut self,
        items: &[(clang::Entity<'_>, String, crate::macros::MacroKind)],
    ) {
        for (definition, macro_name, kind) in items.iter() {
            if let crate::macros::MacroKind::Statement { parameters, body } = kind {
                self.add_statement_macro_definition(
                    macro_name.clone(),
                    parameters.clone(),
                    body.clone(),
                    definition,
                );
            }
        }
    }

    #[inline]
    pub fn record_macro_expansion(&mut self, location_key: String, macro_name: String) {
        if self
            .macro_expansion_sites
            .iter()
            .any(|entry| entry.0 == location_key)
        {
            return;
        }

        self.macro_expansion_sites.push((location_key, macro_name));
    }

    #[inline]
    pub fn mark_statement_macro_function_emitted(&mut self, macro_name: &str) {
        self.emitted_statement_macro_functions
            .insert(macro_name.to_string());
    }
}

impl MacroTable {
    #[inline]
    pub fn add_pending_statement_macro_function(&mut self, text: String) {
        self.pending_statement_macro_functions.push(text);
    }
}

impl MacroTable {
    #[inline]
    pub fn take_pending_statement_macro_functions(&mut self) -> Vec<String> {
        std::mem::take(&mut self.pending_statement_macro_functions)
    }
}

impl MacroTable {
    pub fn make_location_key(entity: &clang::Entity<'_>) -> Option<String> {
        let range: clang::source::SourceRange<'_> = entity.get_range()?;

        let location: clang::source::Location<'_> = range.get_start().get_expansion_location();

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
    pub fn scan_macro_expansions(&mut self, root: &clang::Entity<'_>) {
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
    pub fn entity_originates_in_input(&self, entity: &clang::Entity<'_>) -> bool {
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

impl MacroTable {
    #[inline]
    pub fn find_macro_at(&self, file: &Path, offset: u32) -> Option<String> {
        let mut best: Option<(String, u32)> = None;

        for (name, (range_file, start, end)) in self.macro_body_ranges.iter() {
            if range_file != file {
                continue;
            }

            if offset < *start || offset > *end {
                continue;
            }

            let length: u32 = end.saturating_sub(*start);

            match best.as_ref() {
                Some((_, best_length)) if length >= *best_length => continue,
                _ => best = Some((name.clone(), length)),
            }
        }

        best.map(|(name, _)| name)
    }
}
