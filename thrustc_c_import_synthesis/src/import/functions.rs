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
use crate::import::ImportState;
use crate::model::CImportedFunction;
use crate::type_map;

use clang::{CallingConvention, Language, Linkage, StorageClass, TypeKind};
use thrustc_typesystem::Type;

pub fn import_functions<'clang>(
    function_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    for entity in function_decls.iter() {
        let canonical_decl: clang::Entity<'clang> = entity.get_canonical_entity();
        let resolved_decl: clang::Entity<'clang> = canonical_decl
            .get_definition()
            .unwrap_or(canonical_decl);

        let Some(name) = canonical_decl.get_name().or_else(|| entity.get_name()) else {
            continue;
        };

        if matches!(
            canonical_decl.get_storage_class(),
            Some(StorageClass::Static | StorageClass::PrivateExtern)
        ) || matches!(
            resolved_decl.get_storage_class(),
            Some(StorageClass::Static | StorageClass::PrivateExtern)
        ) {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C function '{name}' skipped: static/private storage functions are not importable"
                ),
            ));

            continue;
        }

        if matches!(
            canonical_decl.get_linkage(),
            Some(Linkage::Internal | Linkage::Automatic | Linkage::UniqueExternal)
        ) || matches!(
            resolved_decl.get_linkage(),
            Some(Linkage::Internal | Linkage::Automatic | Linkage::UniqueExternal)
        ) {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C function '{name}' skipped: function does not have external C linkage"
                ),
            ));

            continue;
        }

        if matches!(
            canonical_decl.get_language(),
            Some(Language::Cpp | Language::ObjectiveC | Language::Swift)
        ) || matches!(
            resolved_decl.get_language(),
            Some(Language::Cpp | Language::ObjectiveC | Language::Swift)
        ) {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C function '{name}' skipped: only C declarations are supported by importC"
                ),
            ));

            continue;
        }

        if canonical_decl.is_inline_function() || resolved_decl.is_inline_function() {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C function '{name}' skipped: inline-only functions are not importable"
                ),
            ));

            continue;
        }

        let convention: String = {
            let convention = canonical_decl
                .get_type()
                .and_then(|ty| ty.get_calling_convention())
                .or_else(|| resolved_decl.get_type().and_then(|ty| ty.get_calling_convention()));

            match convention.unwrap_or(CallingConvention::Cdecl) {
                CallingConvention::Cdecl => "C".into(),
                CallingConvention::SysV64 => "X86_64_SysV".into(),
                CallingConvention::Win64 => "Win64".into(),
                CallingConvention::Stdcall => "X86StdCall".into(),
                CallingConvention::Fastcall => "X86FastCall".into(),
                CallingConvention::Thiscall => "X86ThisCall".into(),
                CallingConvention::Vectorcall => "X86VectorCall".into(),
                CallingConvention::Swift => "Swift".into(),
                CallingConvention::PreserveMost => "weakReg".into(),
                CallingConvention::PreserveAll => "strongReg".into(),
                CallingConvention::Aapcs => "ARMAAPCS".into(),
                CallingConvention::AapcsVfp => "ARM_AAPCS_VFP".into(),
                CallingConvention::IntelOcl => "Intel_OCL_BI".into(),
                CallingConvention::RegCall => "X86RegCall".into(),
                other => {
                    state.diagnostics_mut().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::UnsupportedCallingConvention,
                        format!(
                            "C function '{name}' skipped: unsupported calling convention {other:?}"
                        ),
                    ));

                    continue;
                }
            }
        };

        let Some(result_type) = canonical_decl
            .get_result_type()
            .or_else(|| resolved_decl.get_result_type())
        else {
            state.diagnostics_mut().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C function '{name}' skipped: missing result type"),
            ));

            continue;
        };

        let return_type_result: Result<Type, String> = {
            let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();

            type_map::map_type(&result_type, span, struct_cache, diagnostics)
        };

        let return_type: Type = match return_type_result {
            Ok(ty) => ty,
            Err(message) => {
                state.diagnostics_mut().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C function '{name}' skipped: {message}"),
                ));

                continue;
            }
        };

        let parameters_result: Result<(Vec<Type>, Vec<String>), ()> =
            if let Some(arguments) = canonical_decl
                .get_arguments()
                .or_else(|| resolved_decl.get_arguments())
            {
                arguments.iter().enumerate().try_fold(
                    (Vec::new(), Vec::new()),
                    |(mut parameter_types, mut parameter_names), (idx, argument)| {
                        let arg_name: String =
                            argument.get_name().unwrap_or_else(|| format!("arg{idx}"));

                        let Some(arg_ty) = argument.get_type() else {
                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                CImportDiagnosticKind::SkippedDeclaration,
                                format!(
                                    "C function '{name}' skipped: missing type for parameter '{arg_name}'"
                                ),
                            ));

                            return Err(());
                        };

                        let arg_kind: TypeKind = arg_ty.get_canonical_type().get_kind();

                        if arg_kind == TypeKind::VariableArray {
                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                CImportDiagnosticKind::SkippedDeclaration,
                                format!(
                                    "C function '{name}' skipped: parameter '{arg_name}' uses a VLA, which is not supported"
                                ),
                            ));

                            return Err(());
                        }

                        if arg_kind == TypeKind::DependentSizedArray {
                            state.diagnostics_mut().push(CImportDiagnostic::new(
                                CImportDiagnosticKind::SkippedDeclaration,
                                format!(
                                    "C function '{name}' skipped: parameter '{arg_name}' uses a dependent-sized array, which is not supported"
                                ),
                            ));

                            return Err(());
                        }

                        if matches!(arg_kind, TypeKind::ConstantArray | TypeKind::IncompleteArray) {
                            let Some(element_type) = arg_ty.get_element_type() else {
                                state.diagnostics_mut().push(CImportDiagnostic::new(
                                    CImportDiagnosticKind::SkippedDeclaration,
                                    format!(
                                        "C function '{name}' skipped: parameter '{arg_name}' array is missing an element type"
                                    ),
                                ));

                                return Err(());
                            };

                            let arg_ty_result: Result<Type, String> = {
                                let (struct_cache, diagnostics) =
                                    state.struct_cache_and_diagnostics_mut();

                                type_map::map_type(&element_type, span, struct_cache, diagnostics)
                            };

                            let arg_ty: Type = match arg_ty_result {
                                Ok(ty) => Type::Ptr {
                                    subtype: Some(Box::new(ty)),
                                    address_space: None,
                                    span,
                                },
                                Err(message) => {
                                    state.diagnostics_mut().push(CImportDiagnostic::new(
                                        CImportDiagnosticKind::SkippedDeclaration,
                                        format!("C function '{name}' skipped: parameter '{arg_name}' array element type is unsupported ({message})"),
                                    ));

                                    return Err(());
                                }
                            };

                            parameter_names.push(arg_name);
                            parameter_types.push(arg_ty);

                            return Ok((parameter_types, parameter_names));
                        }

                        let arg_ty_result: Result<Type, String> = {
                            let (struct_cache, diagnostics) =
                                state.struct_cache_and_diagnostics_mut();

                            type_map::map_type(&arg_ty, span, struct_cache, diagnostics)
                        };

                        let arg_ty: Type = match arg_ty_result {
                            Ok(ty) => ty,
                            Err(message) => {
                                state.diagnostics_mut().push(CImportDiagnostic::new(
                                    CImportDiagnosticKind::SkippedDeclaration,
                                    format!("C function '{name}' skipped: parameter '{arg_name}' has unsupported type ({message})"),
                                ));

                                return Err(());
                            }
                        };

                        parameter_names.push(arg_name);
                        parameter_types.push(arg_ty);

                        Ok((parameter_types, parameter_names))
                    },
                )
            } else {
                Ok((Vec::new(), Vec::new()))
            };

        let (parameter_types, parameter_names): (Vec<Type>, Vec<String>) =
            match parameters_result {
                Ok(parameters) => parameters,
                Err(()) => continue,
            };

        let variadic: bool = canonical_decl
            .get_type()
            .or_else(|| resolved_decl.get_type())
            .is_some_and(|ty| ty.is_variadic());

        state.functions_mut().push(CImportedFunction::new(
            name.clone(),
            name,
            convention,
            return_type,
            parameter_types,
            parameter_names,
            variadic,
        ));
    }
}
