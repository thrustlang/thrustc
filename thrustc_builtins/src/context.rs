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
    pub target_info: &'builtin mut TargetInfo,
    pub options: &'builtin CompilerOptions,
    pub file: &'builtin CompilationUnit,
    pub call_span: Span,
    pub current_function: Option<&'builtin str>,
    pub warnings: &'builtin mut Vec<CompilationIssue>,
}
