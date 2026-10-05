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

mod enums;
mod functions;
mod macros;
mod records;
mod statics;
mod typedefs;

use std::collections::HashSet;
use std::path::PathBuf;

use clang::{Clang, Index};
use thrustc_code_location::Span;

use crate::context::CImportContext;
use crate::diagnostics::CImportDiagnostic;
use crate::model::{
    CImportedConstant, CImportedEnum, CImportedFunction, CImportedStatic, CImportedStruct,
    CImportedTypedef,
};
use crate::options::CImportScope;
use crate::record_layout::StructCache;
use crate::{decl_index, parse};

#[derive(Debug)]
pub struct ImportState<'clang> {
    span: Span,
    struct_cache: StructCache<'clang>,
    functions: Vec<CImportedFunction>,
    structs: Vec<CImportedStruct>,
    enums: Vec<CImportedEnum>,
    typedefs: Vec<CImportedTypedef>,
    statics: Vec<CImportedStatic>,
    constants: Vec<CImportedConstant>,
    diagnostics: Vec<CImportDiagnostic>,
    exported_structs: HashSet<String>,
    exported_typedefs: HashSet<String>,
}

impl<'clang> ImportState<'clang> {
    #[inline]
    pub fn new(span: Span) -> Self {
        Self {
            span,
            struct_cache: StructCache::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            typedefs: Vec::new(),
            statics: Vec::new(),
            constants: Vec::new(),
            diagnostics: Vec::new(),
            exported_structs: HashSet::new(),
            exported_typedefs: HashSet::new(),
        }
    }
}

impl<'clang> ImportState<'clang> {
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
}

impl<'clang> ImportState<'clang> {
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

    #[inline]
    pub fn struct_cache_and_diagnostics_mut(
        &mut self,
    ) -> (&mut StructCache<'clang>, &mut Vec<CImportDiagnostic>) {
        (&mut self.struct_cache, &mut self.diagnostics)
    }

    #[inline]
    pub fn struct_cache_mut(&mut self) -> &mut StructCache<'clang> {
        &mut self.struct_cache
    }

    #[inline]
    pub fn exported_structs_mut(&mut self) -> &mut HashSet<String> {
        &mut self.exported_structs
    }

    #[inline]
    pub fn exported_typedefs_mut(&mut self) -> &mut HashSet<String> {
        &mut self.exported_typedefs
    }
}

pub fn run_import(context: &mut CImportContext) -> Result<(), String> {
    let header_path: PathBuf = context.header_path().clone();
    let span: Span = context.span();

    let clang: Clang = Clang::new().map_err(|_| "Failed to initialize libclang".to_string())?;
    let index: Index = Index::new(&clang, false, false);

    let args: Vec<&str> = context
        .options()
        .clang_args()
        .iter()
        .map(String::as_str)
        .collect();

    let (header_exists, wrapper_path, parse_path, unsaved_files) =
        parse::build_parse_inputs(&header_path);

    let mut parser: clang::Parser<'_> = index.parser(&parse_path);

    parser.skip_function_bodies(true);
    parser.detailed_preprocessing_record(true);
    parser.arguments(&args);

    if !unsaved_files.is_empty() {
        parser.unsaved(&unsaved_files);
    }

    let tu = parser
        .parse()
        .map_err(|e| format!("Failed to parse C header '{}': {e}", header_path.display()))?;

    let (diagnostics, has_errors): (Vec<CImportDiagnostic>, bool) = parse::collect_diagnostics(&tu);

    if has_errors {
        *context.diagnostics_mut() = diagnostics;

        return Err(format!(
            "Clang reported errors while parsing '{}'.",
            header_path.display()
        ));
    }

    let import_scope: CImportScope = context.options().import_scope();

    let main_only_file: Option<clang::source::File<'_>> = parse::resolve_main_only_file(
        import_scope,
        &tu,
        header_exists,
        &header_path,
        &wrapper_path,
    );

    let decl_index: decl_index::DeclIndex<'_> =
        decl_index::collect_decl_index(tu.get_entity().get_children(), import_scope, main_only_file);

    let mut state: ImportState<'_> = ImportState::new(span);

    *state.diagnostics_mut() = diagnostics;

    self::records::import_structs(
        decl_index.struct_decls(),
        decl_index.union_decls(),
        &mut state,
    );
    self::macros::import_macros(decl_index.macro_decls(), &mut state);
    self::enums::import_enums(
        decl_index.enum_decls(),
        decl_index.typedef_decls(),
        &mut state,
    );
    self::typedefs::import_typedefs(decl_index.typedef_decls(), &mut state);
    self::statics::import_statics(decl_index.var_decls(), &mut state);
    self::functions::import_functions(decl_index.function_decls(), &mut state);

    let ImportState {
        functions,
        structs,
        enums,
        typedefs,
        statics,
        constants,
        diagnostics,
        ..
    } = state;

    *context.functions_mut() = functions;
    *context.structs_mut() = structs;
    *context.enums_mut() = enums;
    *context.typedefs_mut() = typedefs;
    *context.statics_mut() = statics;
    *context.constants_mut() = constants;
    *context.diagnostics_mut() = diagnostics;

    Ok(())
}
