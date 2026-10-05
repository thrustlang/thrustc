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
pub(crate) struct TranspilerContext {
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn add_error(&mut self, issue: CompilationIssue) {
        self.errors.push(issue);
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn add_warning(&mut self, issue: CompilationIssue) {
        self.warnings.push(issue);
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn error_count(&self) -> usize {
        self.errors.len()
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn warning_count(&self) -> usize {
        self.warnings.len()
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn take_errors(&mut self) -> Vec<CompilationIssue> {
        std::mem::take(&mut self.errors)
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn take_warnings(&mut self) -> Vec<CompilationIssue> {
        std::mem::take(&mut self.warnings)
    }
}

impl TranspilerContext {
    #[inline]
    pub(crate) fn fail<T: Default>(&mut self, issue: CompilationIssue) -> T {
        self.errors.push(issue);

        T::default()
    }
}
