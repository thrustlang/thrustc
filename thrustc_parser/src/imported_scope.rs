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

use thrustc_ast::{
    Ast, NodeId,
    ast_metadata::{ReferenceMetadata, ReferenceType, StaticMetadata},
};
use thrustc_attributes::traits::ThrustAttributesExtensions;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_preprocessor::signatures::{Signature, Symbol, Variant};
use thrustc_c_transpiler_macro_api::types::{Macro, MacroType};
use thrustc_preprocessor::module::Module;
use thrustc_typesystem::{
    Type,
    type_modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
};

use crate::ParserContext;

pub fn find_symbol<'parser>(
    ctx: &ParserContext<'parser>,
    name: &str,
    span: Span,
) -> Result<Option<(Vec<String>, &'parser Symbol)>, CompilationIssue> {
    if ctx.is_macro_argument_token(span) {
        return Ok(None);
    }

    let Some(scope): Option<&crate::macro_scope::MacroScope<'parser>> = ctx.get_macro_scope()
    else {
        return Ok(None);
    };

    let module: &'parser Module = scope.get_module();
    let access: Vec<String> = scope.get_access().to_vec();

    if let Some(symbol) = module
        .get_symbols()
        .iter()
        .find(|symbol| symbol.public && symbol.name == name)
    {
        return Ok(Some((access, symbol)));
    }

    let mut candidates: Vec<(Vec<String>, &'parser Symbol)> = Vec::new();

    for candidate_module in ctx.get_modules().iter() {
        if std::ptr::eq(candidate_module, module) {
            continue;
        }

        let candidate_access: Vec<String> = candidate_module
            .get_alias()
            .map_or_else(
                || vec![candidate_module.get_name().to_string()],
                |alias| alias.to_vec(),
            );

        for symbol in candidate_module.get_symbols().iter() {
            if !symbol.public || symbol.name != name {
                continue;
            }

            if candidates.iter().any(|(existing_access, existing)| {
                existing_access.as_slice() == candidate_access.as_slice()
                    && existing.name == symbol.name
            }) {
                continue;
            }

            candidates.push((candidate_access.clone(), symbol));
        }
    }

    if candidates.len() > 1 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0043,
            format!("The symbol '{name}' is exported by more than one imported module."),
            "You should disambiguate the macro symbol or avoid importing both modules.".into(),
            None,
            span,
        ));
    }

    Ok(candidates.into_iter().next())
}

pub type TypeMacroEntry<'parser> = (Vec<String>, &'parser Module, &'parser Macro);

pub fn find_type_macro<'parser>(
    ctx: &ParserContext<'parser>,
    name: &str,
    span: Span,
) -> Result<Option<TypeMacroEntry<'parser>>, CompilationIssue> {
    if ctx.is_macro_argument_token(span) {
        return Ok(None);
    }

    let Some(scope): Option<&crate::macro_scope::MacroScope<'parser>> = ctx.get_macro_scope()
    else {
        return Ok(None);
    };

    let module: &'parser Module = scope.get_module();
    let access: Vec<String> = scope.get_access().to_vec();

    if let Some(macro_definition) = module
        .find_macro(name)
        .filter(|macro_definition| macro_definition.get_kind() == MacroType::Type)
    {
        return Ok(Some((access, module, macro_definition)));
    }

    let mut candidates: Vec<(Vec<String>, &'parser Module, &'parser Macro)> = Vec::new();

    for candidate_module in ctx.get_modules().iter() {
        if std::ptr::eq(candidate_module, module) {
            continue;
        }

        let candidate_access: Vec<String> = candidate_module
            .get_alias()
            .map_or_else(
                || vec![candidate_module.get_name().to_string()],
                |alias| alias.to_vec(),
            );

        for macro_definition in candidate_module.get_macros().iter() {
            if macro_definition.get_name() != name
                || macro_definition.get_kind() != MacroType::Type
            {
                continue;
            }

            if candidates.iter().any(|(existing_access, existing_module, existing)| {
                existing_access.as_slice() == candidate_access.as_slice()
                    && existing_module.get_path() == candidate_module.get_path()
                    && existing.get_name() == macro_definition.get_name()
            }) {
                continue;
            }

            candidates.push((candidate_access.clone(), candidate_module, macro_definition));
        }
    }

    if candidates.len() > 1 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0043,
            format!("The type macro '{name}' is exported by more than one imported module."),
            "You should disambiguate the type macro or avoid importing both modules.".into(),
            None,
            span,
        ));
    }

    Ok(candidates.into_iter().next())
}

pub fn synthesize_reference<'parser>(
    ctx: &mut ParserContext<'parser>,
    access: &[String],
    symbol: &'parser Symbol,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let name: &'parser str = symbol.name.as_str();
    let variant: Variant = symbol.variant.clone();

    match variant {
        Variant::Function | Variant::CompilerIntrinsic => {
            let qualified_symbol: String = if variant == Variant::Function {
                thrustc_import::synthesis::ensure_qualified_function(
                    &mut ctx.import_context(),
                    access,
                    name,
                    span,
                )?
            } else {
                thrustc_import::synthesis::ensure_qualified_compiler_intrinsic(
                    &mut ctx.import_context(),
                    access,
                    name,
                    span,
                )?
            };

            let Some(signature) = thrustc_import::synthesis::resolve_signature(
                &ctx.import_context(),
                access,
                name,
                variant,
            )
            .cloned()
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0028,
                    format!("'{}::{}' not found.", access.join("::"), name),
                    "The module does not export that symbol.".into(),
                    None,
                    span,
                ));
            };

            let (return_type, parameter_types, has_ignore): (Type, Vec<Type>, bool) =
                match signature {
                    Signature::Function {
                        kind,
                        parameters,
                        attributes,
                        ..
                    }
                    | Signature::CompilerIntrinsic {
                        kind,
                        parameters,
                        attributes,
                        ..
                    } => (
                        kind,
                        parameters.iter().map(|(_, ty, _)| ty.clone()).collect(),
                        attributes.has_ignore_attribute(),
                    ),
                    _ => {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0028,
                            format!("'{}::{}' is not callable.", access.join("::"), name),
                            "A function was expected.".into(),
                            None,
                            span,
                        ));
                    }
                };

            let modificator: FunctionReferenceTypeModificator =
                FunctionReferenceTypeModificator::new(
                    LLVMFunctionReferenceTypeModificator::new(has_ignore),
                    GCCFunctionReferenceTypeModificator::default(),
                );

            Ok(Ast::Reference {
                name: qualified_symbol,
                kind: Type::Fn {
                    return_type: return_type.into(),
                    parameter_types,
                    modificator,
                    span,
                },
                span,
                metadata: ReferenceMetadata::new(
                    true,
                    false,
                    ReferenceType::default(),
                    false,
                    None,
                ),
                id: NodeId::new(),
            })
        }

        Variant::Constant => {
            let qualified_symbol: String =
                thrustc_import::synthesis::ensure_qualified_constant(
                    &mut ctx.import_context(),
                    access,
                    name,
                    span,
                )?;

            let Some(Signature::Constant { kind, .. }) =
                thrustc_import::synthesis::resolve_signature(
                    &ctx.import_context(),
                    access,
                    name,
                    Variant::Constant,
                )
                .cloned()
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0028,
                    format!("'{}::{}' not found.", access.join("::"), name),
                    "The module does not export that constant.".into(),
                    None,
                    span,
                ));
            };

            Ok(Ast::Reference {
                name: qualified_symbol,
                kind,
                span,
                metadata: ReferenceMetadata::new(
                    true,
                    false,
                    ReferenceType::Constant,
                    false,
                    None,
                ),
                id: NodeId::new(),
            })
        }

        Variant::Static => {
            let qualified_symbol: String =
                thrustc_import::synthesis::ensure_qualified_static(
                    &mut ctx.import_context(),
                    access,
                    name,
                    span,
                )?;

            let Some(Signature::Static {
                kind,
                is_mutable,
                attributes,
                modificators,
                ..
            }) = thrustc_import::synthesis::resolve_signature(
                &ctx.import_context(),
                access,
                name,
                Variant::Static,
            )
            .cloned()
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0028,
                    format!("'{}::{}' not found.", access.join("::"), name),
                    "The module does not export that static.".into(),
                    None,
                    span,
                ));
            };

            let metadata: StaticMetadata =
                thrustc_import::synthesis::build_static_metadata(
                    is_mutable,
                    &attributes,
                    &modificators,
                );

            Ok(Ast::Reference {
                name: qualified_symbol,
                kind,
                span,
                metadata: ReferenceMetadata::new(
                    true,
                    metadata.is_mutable(),
                    ReferenceType::Static,
                    metadata.is_unitialized(),
                    metadata.get_llvm_metadata().atomic_ord,
                ),
                id: NodeId::new(),
            })
        }

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!(
                "'{}::{}' cannot be used as an expression.",
                access.join("::"),
                name
            ),
            "Use a function, constant or static.".into(),
            None,
            span,
        )),
    }
}

pub fn synthesize_type<'parser>(
    ctx: &mut ParserContext<'parser>,
    access: &[String],
    symbol: &'parser Symbol,
    span: Span,
) -> Result<Type, CompilationIssue> {
    let name: &'parser str = symbol.name.as_str();

    let _qualified_symbol: String = match symbol.variant {
        Variant::Struct => thrustc_import::synthesis::ensure_qualified_struct(
            &mut ctx.import_context(),
            access,
            name,
            span,
        )?,
        Variant::CustomType => thrustc_import::synthesis::ensure_qualified_custom_type(
            &mut ctx.import_context(),
            access,
            name,
            span,
        )?,
        Variant::Enum => thrustc_import::synthesis::ensure_qualified_enum(
            &mut ctx.import_context(),
            access,
            name,
            span,
        )?,
        _ => {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0042,
                format!("'{}::{}' is not a type.", access.join("::"), name),
                "The module does not export that type.".into(),
                None,
                span,
            ));
        }
    };

    match symbol.variant {
        Variant::Struct => {
            let Some(Signature::Struct { kind, .. }) =
                thrustc_import::synthesis::resolve_signature(
                    &ctx.import_context(),
                    access,
                    name,
                    Variant::Struct,
                )
                .cloned()
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0042,
                    format!("'{}::{}' was not resolved.", access.join("::"), name),
                    "The imported structure is unavailable.".into(),
                    None,
                    span,
                ));
            };

            Ok(kind)
        }
        Variant::CustomType => {
            let Some(Signature::CustomType { kind, .. }) =
                thrustc_import::synthesis::resolve_signature(
                    &ctx.import_context(),
                    access,
                    name,
                    Variant::CustomType,
                )
                .cloned()
            else {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0042,
                    format!("'{}::{}' was not resolved.", access.join("::"), name),
                    "The imported type is unavailable.".into(),
                    None,
                    span,
                ));
            };

            Ok(kind)
        }
        Variant::Enum => Ok(Type::Void { span }),
        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0042,
            format!("'{}::{}' is not a type.", access.join("::"), name),
            "The module does not export that type.".into(),
            None,
            span,
        )),
    }
}
