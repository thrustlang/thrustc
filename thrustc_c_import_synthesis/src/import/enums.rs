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

use std::collections::{HashMap, HashSet};

use clang::TypeKind;
use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportState;
use crate::model::{CImportedConstant, CImportedEnum};
use crate::type_map;

pub fn import_enums<'clang>(
    enum_decls: &[clang::Entity<'clang>],
    typedef_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    let mut exported_enums: HashSet<String> = HashSet::new();
    let mut enum_name_override: HashMap<clang::Entity<'clang>, String> = HashMap::new();

    for entity in typedef_decls.iter() {
        let Some(name) = entity.get_name() else {
            continue;
        };

        let Some(underlying) = entity.get_typedef_underlying_type() else {
            continue;
        };

        let underlying_canonical = underlying.get_canonical_type();

        if underlying_canonical.get_kind() != TypeKind::Enum {
            continue;
        }

        let Some(decl) = underlying_canonical.get_declaration() else {
            continue;
        };

        enum_name_override
            .entry(decl.get_canonical_entity())
            .or_insert(name);
    }

    for entity in enum_decls.iter() {
        let definition: clang::Entity<'clang> = entity.get_definition().unwrap_or(*entity);

        let Some(underlying) = definition.get_enum_underlying_type() else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                "C enum skipped: missing underlying type".into(),
            ));

            continue;
        };

        #[allow(clippy::blocks_in_conditions)]
        let underlying_type: Type = match {
            let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();
            type_map::map_type(&underlying, span, struct_cache, diagnostics)
        } {
            Ok(ty) => ty,
            Err(message) => {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C enum skipped: unsupported underlying type ({message})"),
                ));
                continue;
            }
        };

        let mut fields: Vec<(String, u64)> = Vec::new();

        for child in definition.get_children() {
            if child.get_kind() != clang::EntityKind::EnumConstantDecl {
                continue;
            }

            let Some(field_name) = child.get_name() else {
                continue;
            };

            let Some((_signed, unsigned)) = child.get_enum_constant_value() else {
                continue;
            };

            fields.push((field_name, unsigned));
        }

        let canonical_enum: clang::Entity<'clang> = entity.get_canonical_entity();

        if let Some(name) = enum_name_override
            .get(&canonical_enum)
            .cloned()
            .or_else(|| definition.get_name().or_else(|| entity.get_name()))
        {
            if exported_enums.insert(name.clone()) {
                state
                    .enums_mut()
                    .push(CImportedEnum::new(name, underlying_type.clone(), fields));
            }
        } else {
            for (field_name, value) in fields {
                state.constants_mut().push(CImportedConstant::new(
                    field_name,
                    underlying_type.clone(),
                    BuiltinValue::Integer(value),
                ));
            }
        }
    }
}
