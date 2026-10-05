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

use clang::TypeKind;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;
use thrustc_typesystem::type_metadata::FixedArrayTypeMetadata;

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::record_layout::{self, StructCache};

pub fn map_type<'clang>(
    ty: &clang::Type<'clang>,
    span: Span,
    struct_cache: &mut StructCache<'clang>,
    diagnostics: &mut Vec<CImportDiagnostic>,
) -> Result<Type, String> {
    let is_const: bool = ty.is_const_qualified();
    let canonical: clang::Type<'clang> = ty.get_canonical_type();

    let mut mapped: Type = match canonical.get_kind() {
        TypeKind::Void => Type::Void { span },
        TypeKind::Bool => Type::Bool { span },

        TypeKind::CharS | TypeKind::CharU => Type::Char { span },
        TypeKind::SChar => Type::S8 { span },
        TypeKind::UChar => Type::U8 { span },

        TypeKind::Short => Type::S16 { span },
        TypeKind::UShort => Type::U16 { span },

        TypeKind::Int => Type::S32 { span },
        TypeKind::UInt => Type::U32 { span },

        TypeKind::Long => Type::SSize { span },
        TypeKind::ULong => Type::USize { span },

        TypeKind::LongLong => Type::S64 { span },
        TypeKind::ULongLong => Type::U64 { span },

        TypeKind::UInt128 => Type::U128 { span },

        TypeKind::Float => Type::F32 { span },
        TypeKind::Double => Type::F64 { span },
        TypeKind::LongDouble => return Err("'long double' is not supported yet".into()),

        TypeKind::Enum => {
            let Some(decl) = canonical.get_declaration() else {
                return Err("enum without declaration".into());
            };

            let Some(underlying) = decl.get_enum_underlying_type() else {
                return Err("enum missing underlying type".into());
            };

            self::map_type(&underlying, span, struct_cache, diagnostics)?
        }

        TypeKind::Pointer => {
            let pointee: clang::Type<'_> = canonical
                .get_pointee_type()
                .ok_or_else(|| "pointer without pointee type".to_string())?;

            let pointee_canonical: clang::Type<'clang> = pointee.get_canonical_type();

            if pointee_canonical.get_kind() == TypeKind::Record {
                let decl_kind = pointee_canonical
                    .get_declaration()
                    .map(|decl| decl.get_kind());

                if matches!(decl_kind, Some(clang::EntityKind::UnionDecl)) {
                    Type::Ptr {
                        subtype: None,
                        address_space: None,
                        span,
                    }
                } else {
                    let Some(decl) = pointee_canonical.get_declaration() else {
                        diagnostics.push(CImportDiagnostic::new(
                            CImportDiagnosticKind::SkippedDeclaration,
                            "C type mapped as opaque pointer: record pointer without declaration"
                                .into(),
                        ));

                        return Ok(Type::Ptr {
                            subtype: None,
                            address_space: None,
                            span,
                        });
                    };

                    if decl.get_kind() == clang::EntityKind::StructDecl {
                        if let Ok((_, struct_ty)) = crate::record_layout::build_struct(
                            &decl,
                            span,
                            struct_cache,
                            diagnostics,
                        ) {
                            return Ok(Type::Ptr {
                                subtype: Some(Box::new(struct_ty)),
                                address_space: None,
                                span,
                            });
                        }
                    }

                    diagnostics.push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        format!(
                            "C type mapped as opaque pointer: unable to synthesize record pointed by '{}'",
                            decl.get_name().unwrap_or_else(|| "<anonymous>".into())
                        ),
                    ));

                    Type::Ptr {
                        subtype: None,
                        address_space: None,
                        span,
                    }
                }
            } else {
                let subtype: Type = self::map_type(&pointee, span, struct_cache, diagnostics)?;

                Type::Ptr {
                    subtype: Some(Box::new(subtype)),
                    address_space: None,
                    span,
                }
            }
        }

        TypeKind::ConstantArray => {
            let element_type = canonical
                .get_element_type()
                .ok_or_else(|| "constant array without element type".to_string())?;

            let size: u32 = canonical
                .get_size()
                .and_then(|value| u32::try_from(value).ok())
                .ok_or_else(|| "constant array with unknown or too-large size".to_string())?;

            let base_type: Type = self::map_type(&element_type, span, struct_cache, diagnostics)?;

            Type::FixedArray {
                base_type: Box::new(base_type),
                size,
                metadata: FixedArrayTypeMetadata::new(None),
                span,
            }
        }

        TypeKind::IncompleteArray => {
            let element_type = canonical
                .get_element_type()
                .ok_or_else(|| "incomplete array without element type".to_string())?;

            let base_type: Type = self::map_type(&element_type, span, struct_cache, diagnostics)?;

            Type::Array {
                base_type: Box::new(base_type),
                infered_type: None,
                metadata: ArrayTypeMetadata::new(None, None),
                span,
            }
        }

        TypeKind::VariableArray => return Err("VLA is not supported yet".into()),

        TypeKind::DependentSizedArray => {
            return Err("dependent-sized arrays are not supported yet".into())
        }

        TypeKind::Record => {
            let Some(decl) = canonical.get_declaration() else {
                return Err("record without declaration".into());
            };

            if decl.get_kind() == clang::EntityKind::UnionDecl {
                return Err("union by-value is not supported yet".into());
            }

            if decl.get_kind() != clang::EntityKind::StructDecl {
                return Err(format!(
                    "unsupported record declaration: {:?}",
                    decl.get_kind()
                ));
            }

            let (_, ty) = record_layout::build_struct(&decl, span, struct_cache, diagnostics)?;

            ty
        }

        other => return Err(format!("unsupported C type kind: {other:?}")),
    };

    if is_const {
        mapped = Type::Const(Box::new(mapped), span);
    }

    Ok(mapped)
}
