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

use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_options::CompilationUnit;
use thrustc_options::CompilerOptions;
use thrustc_typesystem::type_layout::TargetInfo;

#[derive(Debug)]
pub struct BuiltinContext<'builtin> {
    target_info: &'builtin mut TargetInfo,
    options: &'builtin CompilerOptions,
    file: &'builtin CompilationUnit,
    call_span: Span,
    current_function: Option<&'builtin str>,
    warnings: &'builtin mut Vec<CompilationIssue>,
}

impl<'builtin> BuiltinContext<'builtin> {
    #[inline]
    pub fn new(
        target_info: &'builtin mut TargetInfo,
        options: &'builtin CompilerOptions,
        file: &'builtin CompilationUnit,
        call_span: Span,
        current_function: Option<&'builtin str>,
        warnings: &'builtin mut Vec<CompilationIssue>,
    ) -> Self {
        Self {
            target_info,
            options,
            file,
            call_span,
            current_function,
            warnings,
        }
    }
}

impl<'builtin> BuiltinContext<'builtin> {
    #[inline]
    pub fn get_target_info(&self) -> &TargetInfo {
        self.target_info
    }

    #[inline]
    pub fn get_options(&self) -> &CompilerOptions {
        self.options
    }

    #[inline]
    pub fn get_file(&self) -> &CompilationUnit {
        self.file
    }

    #[inline]
    pub fn get_call_span(&self) -> Span {
        self.call_span
    }

    #[inline]
    pub fn get_current_function(&self) -> Option<&'builtin str> {
        self.current_function
    }

    #[inline]
    pub fn get_warnings(&self) -> &Vec<CompilationIssue> {
        self.warnings
    }
}

impl<'builtin> BuiltinContext<'builtin> {
    #[inline]
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        self.target_info
    }

    #[inline]
    pub fn get_mut_warnings(&mut self) -> &mut Vec<CompilationIssue> {
        self.warnings
    }
}

impl<'builtin> BuiltinContext<'builtin> {
    #[inline]
    pub fn set_target_info(&mut self, target_info: &'builtin mut TargetInfo) {
        self.target_info = target_info;
    }

    #[inline]
    pub fn set_options(&mut self, options: &'builtin CompilerOptions) {
        self.options = options;
    }

    #[inline]
    pub fn set_file(&mut self, file: &'builtin CompilationUnit) {
        self.file = file;
    }

    #[inline]
    pub fn set_call_span(&mut self, call_span: Span) {
        self.call_span = call_span;
    }

    #[inline]
    pub fn set_current_function(&mut self, current_function: Option<&'builtin str>) {
        self.current_function = current_function;
    }

    #[inline]
    pub fn set_warnings(&mut self, warnings: &'builtin mut Vec<CompilationIssue>) {
        self.warnings = warnings;
    }
}
