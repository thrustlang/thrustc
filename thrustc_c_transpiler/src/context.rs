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

use thrustc_errors::CompilationIssue;
use thrustc_logging::LoggingType;

#[derive(Debug)]
pub struct TranspilerContext {
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
    macros_errors: Vec<CompilationIssue>,
}

impl TranspilerContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            macros_errors: Vec::new(),
        }
    }
}

impl TranspilerContext {
    #[inline]
    pub fn add_error(&mut self, issue: CompilationIssue) {
        self.errors.push(issue);
    }
}

impl TranspilerContext {
    #[inline]
    pub fn add_warning(&mut self, issue: CompilationIssue) {
        self.warnings.push(issue);
    }
}

impl TranspilerContext {
    #[inline]
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    #[inline]
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    #[inline]
    pub fn macros_error_count(&self) -> usize {
        self.macros_errors.len()
    }
}

impl TranspilerContext {
    #[inline]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    #[inline]
    pub fn has_macros_errors(&self) -> bool {
        !self.macros_errors.is_empty()
    }
}

impl TranspilerContext {
    #[inline]
    pub fn take_errors(&mut self) -> Vec<CompilationIssue> {
        let taken: Vec<CompilationIssue> = std::mem::take(&mut self.errors);

        let mut unique: Vec<CompilationIssue> = Vec::with_capacity(taken.len());

        for issue in taken {
            let seen: bool = unique
                .iter()
                .any(|known| format!("{known:?}") == format!("{issue:?}"));

            if !seen {
                unique.push(issue);
            }
        }

        unique
    }

    #[inline]
    pub fn take_warnings(&mut self) -> Vec<CompilationIssue> {
        let taken: Vec<CompilationIssue> = std::mem::take(&mut self.warnings);

        let mut unique: Vec<CompilationIssue> = Vec::with_capacity(taken.len());

        for issue in taken {
            let seen: bool = unique
                .iter()
                .any(|known| format!("{known:?}") == format!("{issue:?}"));

            if !seen {
                unique.push(issue);
            }
        }

        unique
    }

    #[inline]
    pub fn add_macros_error(&mut self, issue: CompilationIssue) {
        let seen: bool = self
            .macros_errors
            .iter()
            .any(|known| format!("{known:?}") == format!("{issue:?}"));

        if !seen {
            self.macros_errors.push(issue);
        }
    }
}

impl TranspilerContext {
    #[inline]
    pub fn take_macros_errors(&mut self) -> Vec<CompilationIssue> {
        std::mem::take(&mut self.macros_errors)
    }

    #[inline]
    pub fn take_new_errors_since(&mut self, base_error_count: usize) -> Vec<CompilationIssue> {
        if base_error_count >= self.errors.len() {
            return Vec::new();
        }

        self.errors.split_off(base_error_count)
    }
}

impl TranspilerContext {
    #[inline]
    pub fn merge_macro_context(
        &mut self,
        context: &mut thrustc_c_transpiler_macro::macro_context::MacroContext<'_>,
    ) {
        for issue in context.take_warnings() {
            self.add_warning(issue);
        }

        for issue in context.take_errors() {
            self.add_error(issue);
        }

        for issue in context.take_macros_errors() {
            self.add_macros_error(issue);
        }
    }

    #[inline]
    pub fn merge(&mut self, other: &mut TranspilerContext) {
        for issue in other.take_warnings() {
            self.add_warning(issue);
        }

        for issue in other.take_errors() {
            self.add_error(issue);
        }

        for issue in other.take_macros_errors() {
            self.add_macros_error(issue);
        }
    }
}

impl TranspilerContext {
    #[inline]
    pub fn has_any_errors(&self) -> bool {
        self.has_errors() || self.has_macros_errors()
    }
}

impl TranspilerContext {
    pub fn report_all(&mut self) -> bool {
        let mut issues: Vec<CompilationIssue> = self.take_warnings();

        issues.extend(self.take_errors());
        issues.extend(self.take_macros_errors());

        let has_errors: bool = issues
            .iter()
            .any(|issue| matches!(issue, CompilationIssue::Error(..)));

        for issue in issues {
            match issue {
                CompilationIssue::Warning(code, message, ..) => {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        &format!("{}: {}\n", code.to_title(), message),
                    );
                }

                CompilationIssue::Error(code, message, help, note, ..) => {
                    let mut full: String =
                        format!("{}: {}\nhelp: {}", code.to_title(), message, help);

                    if let Some(note) = note {
                        full.push_str("\nnote: ");
                        full.push_str(&note);
                    }

                    thrustc_logging::print_error(LoggingType::Error, &full);
                }

                _ => {}
            }
        }

        has_errors
    }
}
