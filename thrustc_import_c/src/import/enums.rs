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
        if let Some(name) = entity.get_name()
            && let Some(underlying) = entity.get_typedef_underlying_type()
        {
            let underlying_canonical: clang::Type<'clang> = underlying.get_canonical_type();

            if underlying_canonical.get_kind() == TypeKind::Enum
                && let Some(decl) = underlying_canonical.get_declaration()
            {
                enum_name_override
                    .entry(decl.get_canonical_entity())
                    .or_insert(name);
            }
        }
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

        let fields: Vec<(String, u64)> = definition
            .get_children()
            .into_iter()
            .filter(|child| child.get_kind() == clang::EntityKind::EnumConstantDecl)
            .filter_map(|child| {
                let field_name: Option<String> = child.get_name();
                let field_value: Option<(i64, u64)> = child.get_enum_constant_value();

                match (field_name, field_value) {
                    (Some(field_name), Some((_signed, unsigned))) => Some((field_name, unsigned)),
                    _ => None,
                }
            })
            .collect();

        let canonical_enum: clang::Entity<'clang> = entity.get_canonical_entity();
        let exported_name: Option<String> = enum_name_override
            .get(&canonical_enum)
            .cloned()
            .or_else(|| definition.get_name().or_else(|| entity.get_name()));

        if let Some(name) = exported_name {
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
