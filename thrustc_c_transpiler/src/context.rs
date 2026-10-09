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
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    #[inline]
    pub fn has_macros_errors(&self) -> bool {
        !self.macros_errors.is_empty()
    }

    #[inline]
    pub fn macros_error_count(&self) -> usize {
        self.macros_errors.len()
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
    pub fn add_error_fail(&mut self, issue: CompilationIssue) {
        self.errors.push(issue);
    }
}
