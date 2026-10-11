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

use std::path::PathBuf;

use thrustc_code_location::Span;

use crate::diagnostics::CImportDiagnostic;
use crate::import;
use crate::model::{
    CImportedConstant, CImportedEnum, CImportedFunction, CImportedStatic, CImportedStruct,
    CImportedTypedef,
};
use crate::options::CImportOptions;

#[derive(Debug)]
pub struct CImportContext {
    header_path: PathBuf,
    span: Span,
    options: CImportOptions,
    functions: Vec<CImportedFunction>,
    structs: Vec<CImportedStruct>,
    enums: Vec<CImportedEnum>,
    typedefs: Vec<CImportedTypedef>,
    statics: Vec<CImportedStatic>,
    constants: Vec<CImportedConstant>,
    diagnostics: Vec<CImportDiagnostic>,
}

impl CImportContext {
    #[inline]
    pub fn new(header_path: PathBuf, span: Span, options: CImportOptions) -> Self {
        Self {
            header_path,
            span,
            options,
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            typedefs: Vec::new(),
            statics: Vec::new(),
            constants: Vec::new(),
            diagnostics: Vec::new(),
        }
    }
}

impl CImportContext {
    #[inline]
    pub fn header_path(&self) -> &PathBuf {
        &self.header_path
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn options(&self) -> &CImportOptions {
        &self.options
    }

    #[inline]
    pub fn functions(&self) -> &[CImportedFunction] {
        &self.functions
    }

    #[inline]
    pub fn structs(&self) -> &[CImportedStruct] {
        &self.structs
    }

    #[inline]
    pub fn enums(&self) -> &[CImportedEnum] {
        &self.enums
    }

    #[inline]
    pub fn typedefs(&self) -> &[CImportedTypedef] {
        &self.typedefs
    }

    #[inline]
    pub fn statics(&self) -> &[CImportedStatic] {
        &self.statics
    }

    #[inline]
    pub fn constants(&self) -> &[CImportedConstant] {
        &self.constants
    }

    #[inline]
    pub fn diagnostics(&self) -> &[CImportDiagnostic] {
        &self.diagnostics
    }
}

impl CImportContext {
    #[inline]
    pub fn functions_mut(&mut self) -> &mut Vec<CImportedFunction> {
        &mut self.functions
    }

    #[inline]
    pub fn structs_mut(&mut self) -> &mut Vec<CImportedStruct> {
        &mut self.structs
    }

    #[inline]
    pub fn enums_mut(&mut self) -> &mut Vec<CImportedEnum> {
        &mut self.enums
    }

    #[inline]
    pub fn typedefs_mut(&mut self) -> &mut Vec<CImportedTypedef> {
        &mut self.typedefs
    }

    #[inline]
    pub fn statics_mut(&mut self) -> &mut Vec<CImportedStatic> {
        &mut self.statics
    }

    #[inline]
    pub fn constants_mut(&mut self) -> &mut Vec<CImportedConstant> {
        &mut self.constants
    }

    #[inline]
    pub fn diagnostics_mut(&mut self) -> &mut Vec<CImportDiagnostic> {
        &mut self.diagnostics
    }
}

impl CImportContext {
    #[inline]
    pub fn import_header(&mut self) -> Result<(), String> {
        import::run_import(self)
    }
}
