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

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::StructTypeMetadata;
use thrustc_typesystem::type_modificators::{
    GCCStructureTypeModificator, LLVMStructureTypeModificator, StructureTypeModificator,
};

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::type_map;

pub type StructCacheEntry = (Vec<(String, Type)>, Type);
pub type StructCache<'clang> = HashMap<clang::Entity<'clang>, StructCacheEntry>;

pub fn build_struct<'clang>(
    decl: &clang::Entity<'clang>,
    span: Span,
    struct_cache: &mut StructCache<'clang>,
    diagnostics: &mut Vec<CImportDiagnostic>,
) -> Result<StructCacheEntry, String> {
    let canonical: clang::Entity<'clang> = decl.get_canonical_entity();

    if let Some(cached) = struct_cache.get(&canonical) {
        return Ok(cached.clone());
    }

    let definition: clang::Entity<'clang> = canonical.get_definition().unwrap_or(canonical);

    if !definition.is_definition() {
        return Err("incomplete struct type".into());
    }

    let name: String = definition.get_name().unwrap_or_else(|| {
        let mut hasher: std::collections::hash_map::DefaultHasher =
            std::collections::hash_map::DefaultHasher::new();

        canonical.hash(&mut hasher);

        format!("__c_anon_record_{}", hasher.finish())
    });

    let Some(record_type) = definition.get_type() else {
        return Err("missing struct type".into());
    };

    let children: Vec<clang::Entity<'clang>> = definition.get_children();

    let is_packed: bool = children
        .iter()
        .any(|child| child.get_kind() == clang::EntityKind::PackedAttr);
    let align: Option<u64> = if children
        .iter()
        .any(|child| child.get_kind() == clang::EntityKind::AlignedAttr)
    {
        record_type.get_alignof().ok().and_then(|align| {
            let align: u64 = align.try_into().ok()?;

            if align <= 1 {
                None
            } else {
                Some(align)
            }
        })
    } else {
        None
    };

    let llvm_mod: LLVMStructureTypeModificator =
        LLVMStructureTypeModificator::new(is_packed, align);
    let gcc_mod: GCCStructureTypeModificator = GCCStructureTypeModificator::new();
    let modificator: StructureTypeModificator = StructureTypeModificator::new(llvm_mod, gcc_mod);
    let metadata: StructTypeMetadata = StructTypeMetadata::new(modificator);

    // Seed the cache before walking fields so self-referential pointer fields do not recurse forever.
    let placeholder_kind: Type = Type::Struct {
        name: name.clone(),
        fields: Vec::new(),
        metadata,
        span,
    };

    struct_cache.insert(canonical, (Vec::new(), placeholder_kind));

    let fields: Vec<clang::Entity<'clang>> = children
        .into_iter()
        .filter(|child| child.get_kind() == clang::EntityKind::FieldDecl)
        .collect();

    let mut out_fields: Vec<(String, Type)> = Vec::with_capacity(fields.len());
    let mut field_types: Vec<Type> = Vec::with_capacity(fields.len());

    let mut invented_fields: u32 = 0;

    for (idx, field) in fields.iter().enumerate() {
        let field_kind: clang::TypeKind = match field.get_type() {
            Some(field_ty) => field_ty.get_canonical_type().get_kind(),
            None => {
                let field_name: String = field
                    .get_name()
                    .unwrap_or_else(|| format!("field{idx}"));

                return Err(format!("missing type for field '{field_name}'"));
            }
        };

        if field.get_bit_field_width().is_some() {
            diagnostics.push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedBitfieldStruct,
                format!("C struct '{name}' skipped: bitfields are not supported"),
            ));

            return Err("bitfields are not supported".into());
        }

        if field_kind == clang::TypeKind::VariableArray {
            diagnostics.push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C struct '{name}' skipped: VLA fields are not supported"),
            ));

            return Err("VLA fields are not supported".into());
        }

        if field_kind == clang::TypeKind::DependentSizedArray {
            diagnostics.push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C struct '{name}' skipped: dependent-sized array fields are not supported"
                ),
            ));

            return Err("dependent-sized array fields are not supported".into());
        }

        if field_kind == clang::TypeKind::IncompleteArray {
            let message: String = if idx + 1 == fields.len() {
                format!("C struct '{name}' skipped: flexible array members are not supported")
            } else {
                format!("C struct '{name}' skipped: incomplete array fields are not supported")
            };

            diagnostics.push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                message,
            ));

            return Err("incomplete array fields are not supported".into());
        }

        let field_name: String = match field.get_name() {
            Some(name) => name,
            None => {
                invented_fields = invented_fields.saturating_add(1);
                format!("field{idx}")
            }
        };

        let field_ty: clang::Type<'clang> = field.get_type().unwrap_or_else(|| unreachable!());

        let field_ty: Type = type_map::map_type(&field_ty, span, struct_cache, diagnostics)?;

        out_fields.push((field_name, field_ty.clone()));
        field_types.push(field_ty);
    }

    if invented_fields > 0 {
        diagnostics.push(CImportDiagnostic::new(
            CImportDiagnosticKind::InventedFieldName,
            format!(
                "C struct '{name}' has {invented_fields} unnamed field(s); generated placeholders like 'field0'."
            ),
        ));
    }

    let kind: Type = Type::Struct {
        name: name.clone(),
        fields: field_types,
        metadata,
        span,
    };

    let result: StructCacheEntry = (out_fields, kind);

    struct_cache.insert(canonical, result.clone());

    Ok(result)
}
