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
use crate::model::CImportedFunction;
use crate::type_map;

use clang::{CallingConvention, Language, Linkage, StorageClass, TypeKind};
use thrustc_typesystem::Type;

pub fn import_functions<'clang>(
    function_decls: &[clang::Entity<'clang>],
    import_c: &mut ImportC<'clang>,
) {
    let span: thrustc_code_location::Span = import_c.get_span();

    for entity in function_decls.iter() {
        let canonical_decl: clang::Entity<'clang> = entity.get_canonical_entity();
        let resolved_decl: clang::Entity<'clang> =
            canonical_decl.get_definition().unwrap_or(canonical_decl);

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
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
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
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C function '{name}' skipped: function does not have external C linkage"),
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
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!(
                    "C function '{name}' skipped: only C declarations are supported by importC"
                ),
            ));

            continue;
        }

        if canonical_decl.is_inline_function() || resolved_decl.is_inline_function() {
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C function '{name}' skipped: inline-only functions are not importable"),
            ));

            continue;
        }

        let convention: String = {
            let convention = canonical_decl
                .get_type()
                .and_then(|ty| ty.get_calling_convention())
                .or_else(|| {
                    resolved_decl
                        .get_type()
                        .and_then(|ty| ty.get_calling_convention())
                });

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
                    import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
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
            import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                CImportDiagnosticKind::SkippedDeclaration,
                format!("C function '{name}' skipped: missing result type"),
            ));

            continue;
        };

        let return_type_result: Result<Type, String> = {
            let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

            type_map::map_type(&result_type, span, struct_cache, diagnostics)
        };

        let return_type: Type = match return_type_result {
            Ok(ty) => ty,
            Err(message) => {
                import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                    CImportDiagnosticKind::SkippedDeclaration,
                    format!("C function '{name}' skipped: {message}"),
                ));

                continue;
            }
        };

        let Some((parameter_types, parameter_names)): Option<(Vec<Type>, Vec<String>)> =
            self::lower_function_parameters(&canonical_decl, &resolved_decl, &name, span, import_c)
        else {
            continue;
        };

        let variadic: bool = canonical_decl
            .get_type()
            .or_else(|| resolved_decl.get_type())
            .is_some_and(|ty| ty.is_variadic());

        import_c.get_mut_functions().push(CImportedFunction::new(
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

fn lower_single_parameter<'clang>(
    argument: &clang::Entity<'clang>,
    index: usize,
    name: &str,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> Result<(String, Type), String> {
    let arg_name: String = argument.get_name().unwrap_or_else(|| format!("arg{index}"));

    let Some(arg_ty) = argument.get_type() else {
        return Err(format!(
            "C function '{name}' skipped: missing type for parameter '{arg_name}'"
        ));
    };

    let arg_kind: TypeKind = arg_ty.get_canonical_type().get_kind();

    let is_sized_array: bool = matches!(
        arg_kind,
        TypeKind::ConstantArray | TypeKind::IncompleteArray
    );

    if arg_kind == TypeKind::VariableArray {
        return Err(format!(
            "C function '{name}' skipped: parameter '{arg_name}' uses a VLA, which is not supported"
        ));
    }

    if arg_kind == TypeKind::DependentSizedArray {
        return Err(format!(
            "C function '{name}' skipped: parameter '{arg_name}' uses a dependent-sized array, which is not supported"
        ));
    }

    if is_sized_array {
        let Some(element_type) = arg_ty.get_element_type() else {
            return Err(format!(
                "C function '{name}' skipped: parameter '{arg_name}' array is missing an element type"
            ));
        };

        let arg_ty_result: Result<Type, String> = {
            let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

            type_map::map_type(&element_type, span, struct_cache, diagnostics)
        };

        return match arg_ty_result {
            Ok(ty) => Ok((
                arg_name,
                Type::Ptr {
                    subtype: Some(Box::new(ty)),
                    address_space: None,
                    span,
                },
            )),
            Err(message) => Err(format!(
                "C function '{name}' skipped: parameter '{arg_name}' array element type is unsupported ({message})"
            )),
        };
    }

    let arg_ty_result: Result<Type, String> = {
        let (struct_cache, diagnostics) = import_c.get_mut_struct_cache_and_diagnostics();

        type_map::map_type(&arg_ty, span, struct_cache, diagnostics)
    };

    match arg_ty_result {
        Ok(ty) => Ok((arg_name, ty)),
        Err(message) => Err(format!(
            "C function '{name}' skipped: parameter '{arg_name}' has unsupported type ({message})"
        )),
    }
}

fn lower_function_parameters<'clang>(
    canonical_decl: &clang::Entity<'clang>,
    resolved_decl: &clang::Entity<'clang>,
    name: &str,
    span: thrustc_code_location::Span,
    import_c: &mut ImportC<'clang>,
) -> Option<(Vec<Type>, Vec<String>)> {
    let Some(arguments) = canonical_decl
        .get_arguments()
        .or_else(|| resolved_decl.get_arguments())
    else {
        return Some((Vec::new(), Vec::new()));
    };

    let mut parameter_types: Vec<Type> = Vec::new();
    let mut parameter_names: Vec<String> = Vec::new();

    for (idx, argument) in arguments.iter().enumerate() {
        let (arg_name, arg_ty): (String, Type) =
            match self::lower_single_parameter(argument, idx, name, span, import_c) {
                Ok(lowered) => lowered,
                Err(message) => {
                    import_c.get_mut_diagnostics().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        message,
                    ));

                    return None;
                }
            };

        parameter_names.push(arg_name);
        parameter_types.push(arg_ty);
    }

    Some((parameter_types, parameter_names))
}
