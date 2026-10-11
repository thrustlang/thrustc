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
pub enum CImportScope {
    MainOnly,
    TransitiveNoSystem,
    TransitiveAll,
}

#[derive(Debug)]
pub struct CImportOptions {
    clang_args: Vec<String>,
    import_scope: CImportScope,
}

impl CImportOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            clang_args: Vec::new(),
            import_scope: CImportScope::TransitiveNoSystem,
        }
    }
}

impl CImportOptions {
    #[inline]
    pub fn clang_args(&self) -> &[String] {
        &self.clang_args
    }

    #[inline]
    pub fn import_scope(&self) -> CImportScope {
        self.import_scope
    }
}

impl CImportOptions {
    #[inline]
    pub fn clang_args_mut(&mut self) -> &mut Vec<String> {
        &mut self.clang_args
    }

    #[inline]
    pub fn set_import_scope(&mut self, import_scope: CImportScope) {
        self.import_scope = import_scope;
    }
}
