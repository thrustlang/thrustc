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

use crate::{CompilationIssue, CompilationIssueCode};

pub trait SimdUsageAst {
    fn simd_usage_span(&self) -> Option<Span>;
}

#[inline]
pub fn simd_usage_issue<T, F>(ast: &[T], validate_features: F) -> Option<CompilationIssue>
where
    T: SimdUsageAst,
    F: FnOnce() -> Result<(), String>,
{
    let span: Span = ast.iter().find_map(|node| node.simd_usage_span())?;
    let Err(help): Result<(), String> = validate_features() else {
        return None;
    };

    Some(CompilationIssue::Error(
        CompilationIssueCode::E0060,
        "SIMD code requires explicit target CPU features.".into(),
        help,
        None,
        span,
    ))
}

#[inline]
pub fn unsupported_variadic_simd_issue(span: Span) -> CompilationIssue {
    CompilationIssue::Error(
        CompilationIssueCode::E0061,
        "SIMD values are not supported in variadic functions on x86_64 System V.".into(),
        "Pass the NativeVector through a fixed-arity function or a pointer.".into(),
        None,
        span,
    )
}
