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

use std::collections::HashSet;

use clang::EntityKind;

use crate::options::CImportScope;

#[derive(Debug)]
pub struct ClangDeclIndex<'clang> {
    function_decls: Vec<clang::Entity<'clang>>,
    var_decls: Vec<clang::Entity<'clang>>,
    struct_decls: Vec<clang::Entity<'clang>>,
    union_decls: Vec<clang::Entity<'clang>>,
    enum_decls: Vec<clang::Entity<'clang>>,
    typedef_decls: Vec<clang::Entity<'clang>>,
    macro_decls: Vec<clang::Entity<'clang>>,
}

impl<'clang> ClangDeclIndex<'clang> {
    #[inline]
    pub fn new() -> Self {
        Self {
            function_decls: Vec::new(),
            var_decls: Vec::new(),
            struct_decls: Vec::new(),
            union_decls: Vec::new(),
            enum_decls: Vec::new(),
            typedef_decls: Vec::new(),
            macro_decls: Vec::new(),
        }
    }
}

impl<'clang> ClangDeclIndex<'clang> {
    #[inline]
    pub fn get_function_decls(&self) -> &[clang::Entity<'clang>] {
        &self.function_decls
    }

    #[inline]
    pub fn get_struct_decls(&self) -> &[clang::Entity<'clang>] {
        &self.struct_decls
    }

    #[inline]
    pub fn get_var_decls(&self) -> &[clang::Entity<'clang>] {
        &self.var_decls
    }

    #[inline]
    pub fn get_union_decls(&self) -> &[clang::Entity<'clang>] {
        &self.union_decls
    }

    #[inline]
    pub fn get_enum_decls(&self) -> &[clang::Entity<'clang>] {
        &self.enum_decls
    }

    #[inline]
    pub fn get_typedef_decls(&self) -> &[clang::Entity<'clang>] {
        &self.typedef_decls
    }

    #[inline]
    pub fn get_macro_decls(&self) -> &[clang::Entity<'clang>] {
        &self.macro_decls
    }
}

impl<'clang> ClangDeclIndex<'clang> {
    #[inline]
    pub fn get_mut_function_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.function_decls
    }

    #[inline]
    pub fn get_mut_struct_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.struct_decls
    }

    #[inline]
    pub fn get_mut_var_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.var_decls
    }

    #[inline]
    pub fn get_mut_union_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.union_decls
    }

    #[inline]
    pub fn get_mut_enum_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.enum_decls
    }

    #[inline]
    pub fn get_mut_typedef_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.typedef_decls
    }

    #[inline]
    pub fn get_mut_macro_decls(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.macro_decls
    }
}

pub fn collect_declaration_index<'clang>(
    entities: Vec<clang::Entity<'clang>>,
    import_scope: CImportScope,
    main_only_file: Option<clang::source::File<'clang>>,
) -> ClangDeclIndex<'clang> {
    let mut index: ClangDeclIndex<'clang> = ClangDeclIndex::new();

    let mut function_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut var_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut struct_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut union_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut enum_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut typedef_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut macro_decls: HashSet<clang::Entity<'clang>> = HashSet::new();

    for entity in entities {
        let is_all: bool = import_scope == CImportScope::TransitiveAll;

        let is_visible_system: bool =
            import_scope == CImportScope::TransitiveNoSystem && !entity.is_in_system_header();

        let is_main_file: bool = main_only_file.as_ref().is_some_and(|main_file| {
            entity.get_location().is_some_and(|location| {
                location
                    .get_file_location()
                    .file
                    .is_some_and(|file| file == *main_file)
            })
        });

        let should_import: bool = is_all || is_visible_system || is_main_file;

        if !should_import {
            continue;
        }

        let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

        if entity.get_kind() == EntityKind::FunctionDecl {
            if function_decls.insert(canonical) {
                index.get_mut_function_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::VarDecl {
            if var_decls.insert(canonical) {
                index.get_mut_var_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::StructDecl {
            if struct_decls.insert(canonical) {
                index.get_mut_struct_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::UnionDecl {
            if union_decls.insert(canonical) {
                index.get_mut_union_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::EnumDecl {
            if enum_decls.insert(canonical) {
                index.get_mut_enum_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::TypedefDecl {
            if typedef_decls.insert(canonical) {
                index.get_mut_typedef_decls().push(entity);
            }

            continue;
        }

        if entity.get_kind() == EntityKind::MacroDefinition && macro_decls.insert(canonical) {
            index.get_mut_macro_decls().push(entity);
        }
    }

    index
}
