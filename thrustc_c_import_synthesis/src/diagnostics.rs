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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CImportDiagnosticKind {
    ClangDiagnostic,
    InventedFieldName,
    SkippedUnion,
    SkippedBitfieldStruct,
    UnsupportedCallingConvention,
    SkippedDeclaration,
}

#[derive(Debug, Clone)]
pub struct CImportDiagnostic {
    kind: CImportDiagnosticKind,
    message: String,
}

impl CImportDiagnostic {
    #[inline]
    pub fn new(kind: CImportDiagnosticKind, message: String) -> Self {
        Self { kind, message }
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
}
