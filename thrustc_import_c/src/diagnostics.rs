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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CImportDiagnosticKind {
    ClangDiagnostic,
    InventedFieldName,
    SkippedUnion,
    SkippedBitfieldStruct,
    UnsupportedCallingConvention,
    SkippedDeclaration,
    SkippedMacro,
    UnsupportedMacro,
}

#[derive(Debug, Clone)]
pub struct CImportDiagnostic {
    kind: CImportDiagnosticKind,
    message: String,
    help: Option<String>,
    note: Option<String>,
    span: Option<Span>,
}

impl CImportDiagnostic {
    #[inline]
    pub fn new(kind: CImportDiagnosticKind, message: String) -> Self {
        Self {
            kind,
            message,
            help: None,
            note: None,
            span: None,
        }
    }
}

impl CImportDiagnostic {
    #[inline]
    pub fn from_compilation_issue(kind: CImportDiagnosticKind, issue: CompilationIssue) -> Self {
        match issue {
            CompilationIssue::Error(_, message, help, note, span) => Self {
                kind,
                message,
                help: Some(help),
                note,
                span: Some(span),
            },

            CompilationIssue::Warning(_, message, span) => Self {
                kind,
                message,
                help: None,
                note: None,
                span: Some(span),
            },

            CompilationIssue::FrontendBug(message, help, span, ..)
            | CompilationIssue::BackendBug(message, help, span, ..) => Self {
                kind,
                message,
                help: Some(help),
                note: None,
                span: Some(span),
            },
        }
    }
}

impl CImportDiagnostic {
    #[inline]
    pub fn kind(&self) -> CImportDiagnosticKind {
        self.kind
    }

    #[inline]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[inline]
    pub fn help(&self) -> Option<&str> {
        self.help.as_deref()
    }

    #[inline]
    pub fn note(&self) -> Option<&str> {
        self.note.as_deref()
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}
