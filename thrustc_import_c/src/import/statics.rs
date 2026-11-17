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

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportC;
use crate::model::CImportedStatic;
use crate::type_map;

pub fn import_statics<'clang>(var_decls: &[clang::Entity<'clang>], import_c: &mut ImportC<'clang>) {
    let span: thrustc_code_location::Span = import_c.get_span();

    for entity in var_decls.iter() {
        let Some(name) = entity.get_name() else {
            continue;
        };

        if entity.get_storage_class() != Some(clang::StorageClass::Extern) {
            continue;
        }

        let Some(var_ty) = entity.get_type() else {
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C global '{name}' skipped: missing type"),
            ));

            continue;
        };

        let kind_result = {
            let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

            type_map::map_type(&var_ty, span, struct_cache, diagnostics)
        };

        let kind: thrustc_typesystem::Type = match kind_result {
            Ok(kind) => kind,
            Err(message) => {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C global '{name}' skipped: unsupported type ({message})"),
                ));

                continue;
            }
        };

        let is_mutable: bool = !matches!(kind, thrustc_typesystem::Type::Const(..));
        let is_volatile: bool = var_ty.is_volatile_qualified();

        import_c
            .get_mut_statics()
            .push(CImportedStatic::new(name.clone(), name, kind, is_mutable, is_volatile));
    }
}
