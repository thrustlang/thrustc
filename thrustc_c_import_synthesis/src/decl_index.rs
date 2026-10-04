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
pub struct DeclIndex<'clang> {
    function_decls: Vec<clang::Entity<'clang>>,
    struct_decls: Vec<clang::Entity<'clang>>,
    union_decls: Vec<clang::Entity<'clang>>,
    enum_decls: Vec<clang::Entity<'clang>>,
    typedef_decls: Vec<clang::Entity<'clang>>,
    macro_decls: Vec<clang::Entity<'clang>>,
}

impl<'clang> DeclIndex<'clang> {
    #[inline]
    pub fn new() -> Self {
        Self {
            function_decls: Vec::new(),
            struct_decls: Vec::new(),
            union_decls: Vec::new(),
            enum_decls: Vec::new(),
            typedef_decls: Vec::new(),
            macro_decls: Vec::new(),
        }
    }
}

impl<'clang> DeclIndex<'clang> {
    #[inline]
    pub fn function_decls(&self) -> &[clang::Entity<'clang>] {
        &self.function_decls
    }

    #[inline]
    pub fn struct_decls(&self) -> &[clang::Entity<'clang>] {
        &self.struct_decls
    }

    #[inline]
    pub fn union_decls(&self) -> &[clang::Entity<'clang>] {
        &self.union_decls
    }

    #[inline]
    pub fn enum_decls(&self) -> &[clang::Entity<'clang>] {
        &self.enum_decls
    }

    #[inline]
    pub fn typedef_decls(&self) -> &[clang::Entity<'clang>] {
        &self.typedef_decls
    }

    #[inline]
    pub fn macro_decls(&self) -> &[clang::Entity<'clang>] {
        &self.macro_decls
    }
}

impl<'clang> DeclIndex<'clang> {
    #[inline]
    pub fn function_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.function_decls
    }

    #[inline]
    pub fn struct_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.struct_decls
    }

    #[inline]
    pub fn union_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.union_decls
    }

    #[inline]
    pub fn enum_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.enum_decls
    }

    #[inline]
    pub fn typedef_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.typedef_decls
    }

    #[inline]
    pub fn macro_decls_mut(&mut self) -> &mut Vec<clang::Entity<'clang>> {
        &mut self.macro_decls
    }
}

pub fn collect_decl_index<'clang>(
    entities: Vec<clang::Entity<'clang>>,
    import_scope: CImportScope,
    main_only_file: Option<clang::source::File<'clang>>,
) -> DeclIndex<'clang> {
    let mut index: DeclIndex<'clang> = DeclIndex::new();

    let mut seen_function_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut seen_struct_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut seen_union_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut seen_enum_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut seen_typedef_decls: HashSet<clang::Entity<'clang>> = HashSet::new();
    let mut seen_macro_decls: HashSet<clang::Entity<'clang>> = HashSet::new();

    for entity in entities {
        let should_import: bool = match import_scope {
            CImportScope::MainOnly => match main_only_file {
                Some(main_file) => match entity
                    .get_location()
                    .and_then(|loc| loc.get_file_location().file)
                {
                    Some(file) => file == main_file,
                    None => false,
                },
                None => false,
            },
            CImportScope::TransitiveNoSystem => !entity.is_in_system_header(),
            CImportScope::TransitiveAll => true,
        };

        if !should_import {
            continue;
        }

        match entity.get_kind() {
            EntityKind::FunctionDecl => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_function_decls.insert(canonical) {
                    index.function_decls_mut().push(entity);
                }
            }
            EntityKind::StructDecl => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_struct_decls.insert(canonical) {
                    index.struct_decls_mut().push(entity);
                }
            }
            EntityKind::UnionDecl => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_union_decls.insert(canonical) {
                    index.union_decls_mut().push(entity);
                }
            }
            EntityKind::EnumDecl => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_enum_decls.insert(canonical) {
                    index.enum_decls_mut().push(entity);
                }
            }
            EntityKind::TypedefDecl => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_typedef_decls.insert(canonical) {
                    index.typedef_decls_mut().push(entity);
                }
            }
            EntityKind::MacroDefinition => {
                let canonical: clang::Entity<'clang> = entity.get_canonical_entity();

                if seen_macro_decls.insert(canonical) {
                    index.macro_decls_mut().push(entity);
                }
            }
            _ => {}
        }
    }

    index
}
