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

use thrustc_typesystem::Type;

pub fn import_functions<'clang>(
    function_decls: &[clang::Entity<'clang>],
    state: &mut ImportState<'clang>,
) {
    let span: thrustc_code_location::Span = state.span();

    for entity in function_decls.iter() {
        let Some(name) = entity.get_name() else {
            continue;
        };

        let Some(result_type) = entity.get_result_type() else {
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

        let mut parameter_types: Vec<Type> = Vec::new();
        let mut parameter_names: Vec<String> = Vec::new();

        let mut ok: bool = true;

        if let Some(arguments) = entity.get_arguments() {
            for (idx, argument) in arguments.iter().enumerate() {
                let arg_name: String = argument.get_name().unwrap_or_else(|| format!("arg{idx}"));

                let Some(arg_ty) = argument.get_type() else {
                    state.diagnostics_mut().push(CImportDiagnostic::new(
                        CImportDiagnosticKind::SkippedDeclaration,
                        format!(
                            "C function '{name}' skipped: missing type for parameter '{arg_name}'"
                        ),
                    ));
                    ok = false;
                    break;
                };

                let arg_ty_result = {
                    let (struct_cache, diagnostics) = state.struct_cache_and_diagnostics_mut();

                    type_map::map_type(&arg_ty, span, struct_cache, diagnostics)
                };

                let arg_ty: Type = match arg_ty_result {
                    Ok(ty) => ty,
                    Err(message) => {
                        state.diagnostics_mut().push(CImportDiagnostic::new(
                            CImportDiagnosticKind::SkippedDeclaration,
                            format!("C function '{name}' skipped: parameter '{arg_name}' has unsupported type ({message})"),
                        ));
                        ok = false;

                        break;
                    }
                };

                parameter_names.push(arg_name);
                parameter_types.push(arg_ty);
            }
        }

        if !ok {
            continue;
        }

        let variadic: bool = entity.get_type().is_some_and(|ty| ty.is_variadic());

        state.functions_mut().push(CImportedFunction::new(
            name.clone(),
            name,
            return_type,
            parameter_types,
            parameter_names,
            variadic,
        ));
    }
}
