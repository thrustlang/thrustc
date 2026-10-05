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
use thrustc_typesystem::{Type, type_metadata::StructTypeMetadata};
use thrustc_typesystem::type_modificators::{
    FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
    LLVMFunctionReferenceTypeModificator,
};

use crate::diagnostics::{CImportDiagnostic, CImportDiagnosticKind};
use crate::import::ImportState;
use crate::model::{CImportedStruct, CImportedTypedef};
use crate::type_map;

pub fn import_typedefs<'clang>(
    typedef_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    for entity in typedef_decls.iter() {
        let Some(name) = entity.get_name() else {
            continue;
        };

        let Some(underlying) = entity.get_typedef_underlying_type() else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C typedef '{name}' skipped: missing underlying type"),
            ));

            continue;
        };

        let underlying_canonical: clang::Type<'clang> = underlying.get_canonical_type();

        if underlying_canonical.get_kind() == TypeKind::Pointer {
            if let Some(pointee) = underlying_canonical.get_pointee_type() {
                let pointee_kind: TypeKind = pointee.get_canonical_type().get_kind();

                if matches!(
                    pointee_kind,
                    TypeKind::FunctionPrototype | TypeKind::FunctionNoPrototype
                ) {
                    let Some(result) = pointee.get_result_type() else {
                        state.diagnostics_mut().push(CImportDiagnostic::new(
                            CImportDiagnosticKind::SkippedDeclaration,
                            format!("C typedef '{name}' skipped: fn pointer missing return type"),
                        ));
                        continue;
                    };

                    let return_type_result: Result<Type, String> = {
                        let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();

                        type_map::map_type(&result, span, struct_cache, diagnostics)
                    };

                    let return_type: Type = match return_type_result {
                        Ok(ty) => ty,
                        Err(message) => {
                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                CImportDiagnosticKind::SkippedDeclaration,
                                format!("C typedef '{name}' skipped: unsupported return type ({message})"),
                            ));

                            continue;
                        }
                    };

                    let parameter_types_result: Result<Vec<Type>, ()> =
                        if let Some(args) = pointee.get_argument_types() {
                            args.into_iter().enumerate().try_fold(
                                Vec::new(),
                                |mut parameter_types, (idx, arg)| {
                                    let arg_kind: TypeKind = arg.get_canonical_type().get_kind();

                                    if arg_kind == TypeKind::VariableArray {
                                        state.diagnostics_mut().push(CImportDiagnostic::new(
                                            CImportDiagnosticKind::SkippedDeclaration,
                                            format!(
                                                "C typedef '{name}' skipped: VLA parameter {idx} is not supported"
                                            ),
                                        ));

                                        return Err(());
                                    }

                                    if arg_kind == TypeKind::DependentSizedArray {
                                        state.diagnostics_mut().push(CImportDiagnostic::new(
                                            CImportDiagnosticKind::SkippedDeclaration,
                                            format!(
                                                "C typedef '{name}' skipped: dependent-sized array parameter {idx} is not supported"
                                            ),
                                        ));

                                        return Err(());
                                    }

                                    if matches!(
                                        arg_kind,
                                        TypeKind::ConstantArray | TypeKind::IncompleteArray
                                    )
                                    {
                                        let Some(element_type) = arg.get_element_type() else {
                                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                                CImportDiagnosticKind::SkippedDeclaration,
                                                format!(
                                                    "C typedef '{name}' skipped: array parameter {idx} is missing an element type"
                                                ),
                                            ));

                                            return Err(());
                                        };

                                        let arg_result: Result<Type, String> = {
                                            let (struct_cache, diagnostics) =
                                                state.struct_cache_and_diagnostics_mut();

                                            crate::type_map::map_type(
                                                &element_type,
                                                span,
                                                struct_cache,
                                                diagnostics,
                                            )
                                        };

                                        let arg_ty: Type = match arg_result {
                                            Ok(ty) => ty,
                                            Err(message) => {
                                                state.diagnostics_mut().push(CImportDiagnostic::new(
                                                    CImportDiagnosticKind::SkippedDeclaration,
                                                    format!("C typedef '{name}' skipped: unsupported array parameter type ({message})"),
                                                ));

                                                return Err(());
                                            }
                                        };

                                        parameter_types.push(Type::Ptr {
                                            subtype: Some(Box::new(arg_ty)),
                                            address_space: None,
                                            span,
                                        });

                                        return Ok(parameter_types);
                                    }

                                    let arg_result: Result<Type, String> = {
                                        let (struct_cache, diagnostics) =
                                            state.struct_cache_and_diagnostics_mut();

                                        crate::type_map::map_type(
                                            &arg,
                                            span,
                                            struct_cache,
                                            diagnostics,
                                        )
                                    };

                                    let arg_ty: Type = match arg_result {
                                        Ok(ty) => ty,
                                        Err(message) => {
                                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                                CImportDiagnosticKind::SkippedDeclaration,
                                                format!("C typedef '{name}' skipped: unsupported parameter type ({message})"),
                                            ));

                                            return Err(());
                                        }
                                    };

                                    parameter_types.push(arg_ty);

                                    Ok(parameter_types)
                                },
                            )
                        } else {
                            Ok(Vec::new())
                        };

                    let parameter_types: Vec<Type> = match parameter_types_result {
                        Ok(parameter_types) => parameter_types,
                        Err(()) => continue,
                    };

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

                    state.typedefs_mut().push(CImportedTypedef::new(name, ty));

                    continue;
                }
            }
        }

        if underlying_canonical.get_kind() == TypeKind::Record {
            let Some(decl) = underlying_canonical.get_declaration() else {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C typedef '{name}' skipped: record without declaration"),
                ));

                continue;
            };

            if decl.get_kind() == clang::EntityKind::UnionDecl {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedUnion,
                    format!("C typedef '{name}' skipped: unions are not supported yet"),
                ));

                continue;
            }

            if decl.get_kind() == clang::EntityKind::StructDecl {
                let definition: clang::Entity<'clang> = decl.get_definition().unwrap_or(decl);

                let Some(record_ty) = definition.get_type() else {
                    state.diagnostics_mut().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        format!("C typedef '{name}' skipped: missing struct definition"),
                    ));

                    continue;
                };

                let Ok(struct_ty) = ({
                    let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();

                    type_map::map_type(&record_ty, span, struct_cache, diagnostics)
                }) else {
                    state.diagnostics_mut().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        format!("C typedef '{name}' skipped: unsupported struct fields"),
                    ));

                    continue;
                };

                if let Type::Struct {
                    name: struct_name, ..
                } = &struct_ty
                {
                    let canonical: clang::Entity<'clang> = decl.get_canonical_entity();
                    let cached_struct: Option<(Vec<(String, Type)>, Type)> =
                        state.struct_cache_mut().get(&canonical).cloned();

                    if let Some((fields, _)) = cached_struct {
                        if state.exported_structs_mut().insert(struct_name.clone()) {
                            let metadata: StructTypeMetadata = match &struct_ty {
                                Type::Struct { metadata, .. } => *metadata,
                                _ => StructTypeMetadata::default(),
                            };

                            state
                                .structs_mut()
                                .push(CImportedStruct::new(struct_name.clone(), fields, metadata));
                        }
                    }

                    if name != *struct_name && state.exported_typedefs_mut().insert(name.clone()) {
                        state
                            .typedefs_mut()
                            .push(CImportedTypedef::new(name, struct_ty));
                    }
                }

                continue;
            }
        }

        let type_result = {
            let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();

            type_map::map_type(&underlying, span, struct_cache, diagnostics)
        };

        let ty: Type = match type_result {
            Ok(ty) => ty,
            Err(message) => {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C typedef '{name}' skipped: unsupported type ({message})"),
                ));

                continue;
            }
        };

        if state.exported_typedefs_mut().insert(name.clone()) {
            state.typedefs_mut().push(CImportedTypedef::new(name, ty));
        }
    }
}
