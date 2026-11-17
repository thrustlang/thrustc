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
use thrustc_typesystem::type_modificators::{
    FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
    LLVMFunctionReferenceTypeModificator,
};
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportC;
use crate::model::{CImportedStruct, CImportedTypedef};
use crate::type_map;

pub fn import_typedefs<'clang>(
    typedef_decls: &[clang::Entity<'clang>],
    import_c: &mut ImportC<'clang>,
) {
    let span: thrustc_code_location::Span = import_c.get_span();

    for entity in typedef_decls.iter() {
        let Some(name) = entity.get_name() else {
            continue;
        };

        let Some(underlying) = entity.get_typedef_underlying_type() else {
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C typedef '{name}' skipped: missing underlying type"),
            ));

            continue;
        };

        let underlying_canonical: clang::Type<'clang> = underlying.get_canonical_type();

        if underlying_canonical.get_kind() == TypeKind::Pointer {
            let pointer_result: Result<Option<Type>, ()> =
                self::lower_pointer_typedef(&name, &underlying_canonical, span, import_c);

            match pointer_result {
                Ok(Some(pointer_ty)) => {
                    import_c
                        .get_mut_typedefs()
                        .push(CImportedTypedef::new(name, pointer_ty));

                    continue;
                }

                Ok(None) => {}
                Err(()) => continue,
            }
        }

        if underlying_canonical.get_kind() == TypeKind::Record {
            let record_result: Result<Option<Type>, ()> =
                self::lower_record_typedef(&name, &underlying_canonical, span, import_c);

            match record_result {
                Ok(Some(record_ty)) => {
                    if let Type::Struct {
                        name: struct_name, ..
                    } = &record_ty
                    {
                        if name != *struct_name
                            && import_c.get_mut_exported_typedefs().insert(name.clone())
                        {
                            import_c
                                .get_mut_typedefs()
                                .push(CImportedTypedef::new(name, record_ty));
                        }
                    }

                    continue;
                }

                Ok(None) => {}
                Err(()) => continue,
            }
        }

        let type_result: Result<Type, String> = {
            let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

            type_map::map_type(&underlying, span, struct_cache, diagnostics)
        };

        let ty: Type = match type_result {
            Ok(ty) => ty,
            Err(message) => {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C typedef '{name}' skipped: unsupported type ({message})"),
                ));

                continue;
            }
        };

        if import_c.get_mut_exported_typedefs().insert(name.clone()) {
            import_c
                .get_mut_typedefs()
                .push(CImportedTypedef::new(name, ty));
        }
    }
}

fn lower_pointer_typedef<'clang>(
    name: &str,
    underlying_canonical: &clang::Type<'clang>,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> Result<Option<Type>, ()> {
    let Some(pointee) = underlying_canonical.get_pointee_type() else {
        return Ok(None);
    };

    let pointee_kind: TypeKind = pointee.get_canonical_type().get_kind();

    if !matches!(
        pointee_kind,
        TypeKind::FunctionPrototype | TypeKind::FunctionNoPrototype
    ) {
        return Ok(None);
    }

    let Some(result) = pointee.get_result_type() else {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedDeclaration,
            format!("C typedef '{name}' skipped: fn pointer missing return type"),
        ));

        return Err(());
    };

    let return_type_result: Result<Type, String> = {
        let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

        type_map::map_type(&result, span, struct_cache, diagnostics)
    };

    let return_type: Type = match return_type_result {
        Ok(ty) => ty,
        Err(message) => {
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C typedef '{name}' skipped: unsupported return type ({message})"),
            ));

            return Err(());
        }
    };

    let mut parameter_types: Vec<Type> = Vec::new();

    if let Some(args) = pointee.get_argument_types() {
        for (idx, arg) in args.iter().enumerate() {
            let arg_kind: TypeKind = arg.get_canonical_type().get_kind();

            if arg_kind == TypeKind::VariableArray {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C typedef '{name}' skipped: VLA parameter {idx} is not supported"),
                ));

                return Err(());
            }

            if arg_kind == TypeKind::DependentSizedArray {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!(
                        "C typedef '{name}' skipped: dependent-sized array parameter {idx} is not supported"
                    ),
                ));

                return Err(());
            }

            let is_sized_array: bool = matches!(
                arg_kind,
                TypeKind::ConstantArray | TypeKind::IncompleteArray
            );

            let element_type: Option<clang::Type> = if is_sized_array {
                arg.get_element_type()
            } else {
                None
            };

            if is_sized_array && element_type.is_none() {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!(
                        "C typedef '{name}' skipped: array parameter {idx} is missing an element type"
                    ),
                ));

                return Err(());
            }

            let source: &clang::Type = match element_type.as_ref() {
                Some(element) => element,
                None => arg,
            };

            let arg_result: Result<Type, String> = {
                let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

                type_map::map_type(source, span, struct_cache, diagnostics)
            };

            let mapped: Type = match arg_result {
                Ok(ty) => ty,
                Err(message) => {
                    let detail: String = if is_sized_array {
                        format!("unsupported array parameter type ({message})")
                    } else {
                        format!("unsupported parameter type ({message})")
                    };

                    import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        format!("C typedef '{name}' skipped: {detail}"),
                    ));

                    return Err(());
                }
            };

            let lowered: Type = if is_sized_array {
                Type::Ptr {
                    subtype: Some(Box::new(mapped)),
                    address_space: None,
                    span,
                }
            } else {
                mapped
            };

            parameter_types.push(lowered);
        }
    }

    let variadic: bool = pointee.is_variadic();

    let ty: Type = Type::Fn {
        return_type: Box::new(return_type),
        parameter_types,
        modificator: FunctionReferenceTypeModificator::new(
            LLVMFunctionReferenceTypeModificator::new(variadic),
            GCCFunctionReferenceTypeModificator::default(),
        ),
        span,
    };

    Ok(Some(ty))
}

fn lower_record_typedef<'clang>(
    name: &str,
    underlying_canonical: &clang::Type<'clang>,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> Result<Option<Type>, ()> {
    let Some(decl) = underlying_canonical.get_declaration() else {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedDeclaration,
            format!("C typedef '{name}' skipped: record without declaration"),
        ));

        return Err(());
    };

    if decl.get_kind() == clang::EntityKind::UnionDecl {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedUnion,
            format!("C typedef '{name}' skipped: unions are not supported yet"),
        ));

        return Err(());
    }

    if decl.get_kind() != clang::EntityKind::StructDecl {
        return Ok(None);
    }

    let definition: clang::Entity<'clang> = decl.get_definition().unwrap_or(decl);

    let Some(record_ty) = definition.get_type() else {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedDeclaration,
            format!("C typedef '{name}' skipped: missing struct definition"),
        ));

        return Err(());
    };

    let Ok(struct_ty) = ({
        let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

        type_map::map_type(&record_ty, span, struct_cache, diagnostics)
    }) else {
        import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
            CImportDiagnosticKind::SkippedDeclaration,
            format!("C typedef '{name}' skipped: unsupported struct fields"),
        ));

        return Err(());
    };

    if let Type::Struct {
        name: struct_name, ..
    } = &struct_ty
    {
        let canonical: clang::Entity<'clang> = decl.get_canonical_entity();

        let cached_struct: Option<(Vec<(String, Type)>, Type)> =
            import_c.get_mut_struct_cache().get(&canonical).cloned();

        if let Some((fields, _)) = cached_struct {
            if import_c
                .get_mut_exported_structs()
                .insert(struct_name.clone())
            {
                let metadata: StructTypeMetadata = match &struct_ty {
                    Type::Struct { metadata, .. } => *metadata,
                    _ => StructTypeMetadata::default(),
                };

                import_c.get_mut_structs().push(CImportedStruct::new(
                    struct_name.clone(),
                    fields,
                    metadata,
                ));
            }
        }
    }

    Ok(Some(struct_ty))
}
