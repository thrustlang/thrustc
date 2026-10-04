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

use std::hash::{Hash, Hasher};

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportState;
use crate::model::CImportedStruct;
use crate::type_map;
use thrustc_typesystem::Type;

pub fn import_structs<'clang>(
    struct_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    for entity in struct_decls.iter() {
        let canonical_decl: clang::Entity<'clang> = entity.get_canonical_entity();
        let definition: clang::Entity<'clang> =
            canonical_decl.get_definition().unwrap_or(canonical_decl);

        if !definition.is_definition() {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C struct '{}' skipped: incomplete type",
                    entity.get_name().unwrap_or_else(|| "<anonymous>".into())
                ),
            ));

            continue;
        }

        let Some(ty) = definition.get_type() else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C struct '{}' skipped: missing type",
                    entity.get_name().unwrap_or_else(|| "<anonymous>".into())
                ),
            ));
            continue;
        };

        let Ok(_) = ({
            let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();
            type_map::map_type(&ty, span, struct_cache, diagnostics)
        }) else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C struct '{}' skipped: unsupported field types",
                    entity.get_name().unwrap_or_else(|| "<anonymous>".into())
                ),
            ));

            continue;
        };

        let Some((fields, kind)) = state.struct_cache_mut().get(&canonical_decl).cloned() else {
            continue;
        };

        let name: String = match &kind {
            Type::Struct { name, .. } => name.clone(),

            _ => entity
                .get_name()
                .or_else(|| definition.get_name())
                .unwrap_or_else(|| {
                    let mut hasher: std::collections::hash_map::DefaultHasher =
                        std::collections::hash_map::DefaultHasher::new();

                    canonical_decl.hash(&mut hasher);

                    format!("__c_anon_record_{}", hasher.finish())
                }),
        };

        if state.exported_structs_mut().insert(name.clone()) {
            state.structs_mut().push(CImportedStruct::new(name, fields));
        }
    }
}

pub fn report_skipped_unions<'clang>(
    union_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    for entity in union_decls.iter() {
        if let Some(name) = entity.get_name() {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedUnion,
                format!("C union '{name}' skipped: unions are not supported yet"),
            ));
        } else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedUnion,
                "C union '<anonymous>' skipped: unions are not supported yet".into(),
            ));
        }
    }
}
