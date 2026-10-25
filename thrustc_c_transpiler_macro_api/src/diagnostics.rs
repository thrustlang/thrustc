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

use crate::traits::MacroDiagnostic;

#[derive(Debug, Default)]
pub struct Diagnostics {
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
}

impl Diagnostics {
    #[inline]
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }
}

impl MacroDiagnostic for Diagnostics {
    #[inline]
    fn add_error(&mut self, issue: CompilationIssue) {
        self.errors.push(issue);
    }

    #[inline]
    fn add_warning(&mut self, issue: CompilationIssue) {
        self.warnings.push(issue);
    }

    #[inline]
    fn take_diagnostics(&mut self) -> Vec<CompilationIssue> {
        let mut diagnostics: Vec<CompilationIssue> = Vec::new();

        diagnostics.append(&mut self.errors);
        diagnostics.append(&mut self.warnings);

        diagnostics
    }
}
