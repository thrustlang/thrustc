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

#![allow(clippy::explicit_counter_loop)]
#![allow(clippy::too_many_arguments)]

use thrustc_ast::{
    Ast, NodeId,
    ast_metadata::{ConstantMetadata, FunctionParameterMetadata, StaticMetadata},
};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_atomic_ordering::ThrustAtomicOrdering;
use thrustc_attributes::{ThrustAttribute, ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::{FunctionParameterNames, FunctionParametersTypes};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_external_table::ExternalSymbolTable;
use thrustc_parser_table::{GenericCustomTypeEntry, GenericFunctionEntry, GenericStructEntry};
use thrustc_preprocessor::module::Module;
use thrustc_preprocessor::signatures::{Signature, Variant};
use thrustc_thread_mode::ThrustThreadMode;

use thrustc_typesystem::Type;
use thrustc_typesystem::traits::{TypeCodeLocation, TypePointerExtensions};
use thrustc_typesystem::type_metadata::StructTypeMetadata;

use crate::context::ImportContext;

pub fn synthesize_only_import<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    access: &[String],
    names: &[String],
    span: Span,
) -> Result<(), CompilationIssue> {
    let table: ExternalSymbolTable<'parser> = ExternalSymbolTable::new(ctx.get_modules());

    let Some(module) = table.resolve(access) else {
        return Ok(());
    };

    let origin: std::path::PathBuf = module.get_path().to_path_buf();

    for symbol in module.get_symbols() {
        if !names.contains(&symbol.name) || !symbol.public {
            continue;
        }

        match &symbol.signature {
            Signature::Function {
                kind,
                demangling_name,
                parameters,
                attributes,
                type_params,
                ..
            } => {
                if ctx.get_symbols().has_generic_function(&symbol.name)
                    || ctx.get_symbols().has_function(&symbol.name)
                {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                if let Some(type_params) = type_params {
                    let return_type: Type = kind.clone();
                    let parameter_types: Vec<Type> =
                        parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
                    let parameter_names: Vec<String> =
                        parameters.iter().map(|(name, ..)| name.clone()).collect();
                    let has_ignore: bool = attributes.has_ignore_attribute();

                    ctx.get_mut_symbols().new_generic_function(
                        symbol.name.clone(),
                        GenericFunctionEntry {
                            name: symbol.name.clone(),
                            type_params: type_params.clone(),
                            parameter_types,
                            parameter_names,
                            return_type,
                            attributes: attributes.clone(),
                            has_local_template: false,
                            has_varargs: has_ignore,
                            span,
                        },
                    );

                    ctx.get_mut_symbols()
                        .record_import_origin(symbol.name.clone(), origin.clone());

                    continue;
                }

                let return_type: Type = kind.clone();
                let parameter_types: Vec<Type> =
                    parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
                let parameter_names: Vec<String> =
                    parameters.iter().map(|(name, ..)| name.clone()).collect();
                let has_ignore: bool = attributes.has_ignore_attribute();

                let _ = ctx.get_mut_symbols().new_function(
                    symbol.name.clone(),
                    (
                        return_type.clone(),
                        FunctionParametersTypes(parameter_types.clone()),
                        FunctionParameterNames(parameter_names.clone()),
                        has_ignore,
                    ),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                self::synthesize_function(
                    ctx,
                    &symbol.name,
                    &symbol.name,
                    return_type,
                    parameter_types,
                    parameter_names,
                    attributes.clone(),
                    demangling_name.clone(),
                    span,
                );
            }
            Signature::CompilerIntrinsic {
                kind,
                external_name,
                parameters,
                attributes,
                ..
            } => {
                if ctx.get_symbols().has_compiler_intrinsic(&symbol.name)
                    || ctx.get_symbols().has_function(&symbol.name)
                {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                let parameter_types: Vec<Type> =
                    parameters.iter().map(|(_, ty, _)| ty.clone()).collect();

                let _ = ctx.get_mut_symbols().new_compiler_intrinsic(
                    symbol.name.clone(),
                    (
                        kind.clone(),
                        thrustc_entities::parser_entities::IntrinsicParametersTypes(
                            parameter_types.clone(),
                        ),
                        attributes.has_ignore_attribute(),
                    ),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                self::synthesize_compiler_intrinsic(
                    ctx,
                    &symbol.name,
                    external_name,
                    kind.clone(),
                    parameter_types,
                    attributes.clone(),
                    span,
                );
            }
            Signature::Constant {
                kind,
                attributes,
                modificators,
                value,
                ..
            } => {
                if ctx.get_symbols().has_global_constant(&symbol.name) {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                let folded_value: Option<Ast> =
                    value.as_ref().map(|v| v.to_ast(kind.clone(), span));

                let _ = ctx.get_mut_symbols().new_global_constant(
                    symbol.name.clone(),
                    (kind.clone(), attributes.clone(), folded_value.clone()),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                let Some(value_ast) = folded_value else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0028,
                        format!(
                            "'{}::{}' cannot be resolved to a compile-time value.",
                            access.join("::"),
                            symbol.name
                        ),
                        "The referenced constant cannot be resolved to a compile-time value; it must be resolvable to a literal."
                            .into(),
                        None,
                        span,
                    ));
                };

                self::synthesize_constant(
                    ctx,
                    &symbol.name,
                    kind.clone(),
                    &value_ast,
                    attributes,
                    modificators,
                    span,
                );
            }
            Signature::Static {
                kind,
                is_mutable,
                attributes,
                modificators,
                ..
            } => {
                if ctx.get_symbols().has_global_static(&symbol.name) {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                let metadata: StaticMetadata =
                    self::build_static_metadata(*is_mutable, attributes, modificators);

                let _ = ctx.get_mut_symbols().new_global_static(
                    symbol.name.clone(),
                    (kind.clone(), metadata, attributes.clone()),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                self::synthesize_global(
                    ctx,
                    &symbol.name,
                    kind.clone(),
                    attributes,
                    *is_mutable,
                    modificators,
                    span,
                );
            }
            Signature::CustomType {
                kind,
                attributes,
                type_params,
                ..
            } => {
                if ctx.get_symbols().has_generic_custom_type(&symbol.name)
                    || ctx.get_symbols().has_global_custom_type(&symbol.name)
                {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                if let Some(type_params) = type_params {
                    ctx.get_mut_symbols().new_generic_custom_type(
                        symbol.name.clone(),
                        GenericCustomTypeEntry {
                            type_params: type_params.clone(),
                            kind: kind.clone(),
                        },
                    );

                    ctx.get_mut_symbols()
                        .record_import_origin(symbol.name.clone(), origin.clone());

                    continue;
                }

                let _ = ctx.get_mut_symbols().new_global_custom_type(
                    symbol.name.clone(),
                    (kind.clone(), attributes.clone()),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                ctx.add_ast_node(Ast::CustomType {
                    name: symbol.name.clone(),
                    kind: kind.clone(),
                    span,
                    id: NodeId::new(),
                });
            }
            Signature::Struct {
                kind,
                fields,
                type_params,
                ..
            } => {
                if ctx.get_symbols().has_generic_struct(&symbol.name)
                    || ctx.get_symbols().has_global_struct(&symbol.name)
                {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                let metadata: StructTypeMetadata = match kind {
                    Type::Struct { metadata, .. } => *metadata,
                    _ => continue,
                };

                if let Some(type_params) = type_params {
                    ctx.get_mut_symbols().new_generic_struct(
                        symbol.name.clone(),
                        GenericStructEntry {
                            type_params: type_params.clone(),
                            field_names: fields.iter().map(|(name, _, _)| name.clone()).collect(),
                            field_types: fields.iter().map(|(_, ty, _)| ty.clone()).collect(),
                            metadata,
                            span: kind.get_span(),
                        },
                    );

                    ctx.get_mut_symbols()
                        .record_import_origin(symbol.name.clone(), origin.clone());

                    continue;
                }

                let mut data: Vec<(String, Type, u32, Span)> = Vec::with_capacity(fields.len());

                let mut position: u32 = 0;

                for (field_name, field_type, field_span) in fields.iter() {
                    data.push((
                        field_name.clone(),
                        field_type.clone(),
                        position,
                        *field_span,
                    ));

                    position = position.saturating_add(1);
                }

                let _ = ctx.get_mut_symbols().new_global_struct(
                    symbol.name.clone(),
                    (
                        symbol.name.clone(),
                        data.clone(),
                        ThrustAttributes::new(),
                        metadata,
                        span,
                    ),
                );

                ctx.get_mut_symbols()
                    .record_import_origin(symbol.name.clone(), origin.clone());

                ctx.add_ast_node(Ast::Struct {
                    name: symbol.name.clone(),
                    data: (symbol.name.clone(), data, metadata, span),
                    kind: kind.clone(),
                    attributes: ThrustAttributes::new(),
                    span,
                    id: NodeId::new(),
                });
            }
            Signature::Enum {
                fields, attributes, ..
            } => {
                if ctx.get_symbols().has_global_enum(&symbol.name) {
                    self::check_only_collision(ctx, &symbol.name, access, &origin, span)?;
                    continue;
                }

                let enum_name: String = symbol.name.clone();
                let data: thrustc_ast::ast_logic_data::EnumData<'parser> =
                    self::build_enum_data(fields, span)?;

                let _ = ctx
                    .get_mut_symbols()
                    .new_global_enum(enum_name.clone(), (data.clone(), attributes.clone()));

                ctx.get_mut_symbols()
                    .record_import_origin(enum_name.clone(), origin.clone());

                ctx.add_ast_node(Ast::Enum {
                    name: enum_name,
                    data,
                    attributes: attributes.clone(),
                    kind: Type::Void { span },
                    span,
                    id: NodeId::new(),
                });
            }
        }
    }

    Ok(())
}

pub fn check_qualified_collision<'parser>(
    ctx: &ImportContext<'parser, '_>,
    symbol: &'parser str,
    access: &[String],
    origin: Option<&std::path::PathBuf>,
    span: Span,
) -> Result<(), CompilationIssue> {
    if let (Some(path), Some(imported)) = (origin, ctx.get_symbols().get_import_origin(symbol)) {
        if path != imported {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0043,
                format!(
                    "The symbol '{}' is exported by more than one imported module (e.g. '{}').",
                    symbol,
                    access.join("::")
                ),
                "Rename or disambiguate one of the imports to avoid an ambiguous name.".into(),
                None,
                span,
            ));
        }
    }

    Ok(())
}

pub fn ensure_qualified_function<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    access: &[String],
    symbol: &'parser str,
    span: Span,
) -> Result<String, CompilationIssue> {
    let qualified_symbol: String = self::qualified_symbol_name(access, symbol);

    let Some(Signature::Function {
        kind,
        demangling_name,
        parameters,
        attributes,
        type_params,
        ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::Function)
    else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("'{}::{}' not found.", access.join("::"), symbol),
            "The module does not export a function with that name.".into(),
            None,
            span,
        ));
    };

    let return_type: Type = kind.clone();
    let parameter_types: Vec<Type> = parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
    let parameter_names: Vec<String> = parameters.iter().map(|(name, ..)| name.clone()).collect();

    if type_params.is_some() {
        return Ok(qualified_symbol);
    }

    if self::has_any_synthetized_function(ctx, &qualified_symbol) {
        return Ok(qualified_symbol);
    }

    self::synthesize_function(
        ctx,
        &qualified_symbol,
        symbol,
        return_type,
        parameter_types,
        parameter_names,
        attributes.clone(),
        demangling_name.clone(),
        span,
    );

    Ok(qualified_symbol)
}

pub fn ensure_deallocator_for_type<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    kind: &Type,
    span: Span,
) -> Result<Option<(String, Vec<Type>)>, CompilationIssue> {
    let found: Option<(Vec<String>, &'parser str, Vec<Type>)> = {
        let mut found: Option<(Vec<String>, &'parser str, Vec<Type>)> = None;

        for module in ctx.get_modules() {
            let access: Vec<String> = if let Some(alias) = module.get_alias() {
                if alias.is_empty() {
                    vec![module.get_name().to_string()]
                } else {
                    alias.to_vec()
                }
            } else {
                vec![module.get_name().to_string()]
            };

            if let Some(candidate) = self::find_deallocator_in_module(module, access, kind) {
                found = Some(candidate);
                break;
            }
        }

        found
    };

    let Some((access, symbol, generic_args)) = found else {
        return Ok(None);
    };

    let deallocator_name: String = if generic_args.is_empty() {
        self::ensure_qualified_function(ctx, &access, symbol, span)?
    } else {
        self::ensure_concrete_qualified_function(ctx, &access, symbol, &generic_args, span)?
    };

    Ok(Some((deallocator_name, Vec::with_capacity(0))))
}

fn ensure_concrete_qualified_function<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    access: &[String],
    symbol: &'parser str,
    generic_args: &[Type],
    span: Span,
) -> Result<String, CompilationIssue> {
    let origin: Option<std::path::PathBuf> = ExternalSymbolTable::new(ctx.get_modules())
        .resolve(access)
        .map(|module| module.get_path().to_path_buf());

    let Some(Signature::Function {
        kind,
        parameters,
        attributes,
        type_params: Some(type_params),
        ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::Function)
    else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("'{}::{}' not found.", access.join("::"), symbol),
            "The module does not export a function with that name.".into(),
            None,
            span,
        ));
    };

    let mut env: thrustc_generics::TypeEnv =
        thrustc_generics::TypeEnv::with_capacity(type_params.len());

    for (parameter, argument) in type_params.iter().zip(generic_args.iter()) {
        env.insert(parameter.clone(), argument.clone());
    }

    let origin_key: Option<String> = origin
        .as_ref()
        .map(|path| path.to_string_lossy().to_string());
    let key: String = thrustc_generics::instantiation_key(origin_key.as_deref(), symbol, &env);

    if !self::has_any_synthetized_function(ctx, &key) {
        let parameter_types: Vec<Type> = parameters
            .iter()
            .map(|(_, ty, _)| thrustc_generics::substitute(ty, &env))
            .collect();
        let parameter_names: Vec<String> =
            parameters.iter().map(|(name, ..)| name.clone()).collect();
        let return_type: Type = thrustc_generics::substitute(kind, &env);
        let concrete_demangling_name: String = origin
            .as_ref()
            .and_then(|path| path.file_stem())
            .map_or_else(
                || format!("{}.{key}", access.join("_")),
                |module| format!("{}.{}", module.to_string_lossy(), key),
            );

        self::synthesize_function(
            ctx,
            &key,
            symbol,
            return_type,
            parameter_types,
            parameter_names,
            attributes.clone(),
            concrete_demangling_name,
            span,
        );
    }

    if let Some(origin) = origin {
        thrustc_generics::record_pending(origin, symbol.to_string(), env);
    }

    Ok(key)
}

fn find_deallocator_in_module<'parser>(
    module: &'parser Module,
    access: Vec<String>,
    kind: &Type,
) -> Option<(Vec<String>, &'parser str, Vec<Type>)> {
    fn deallocator_type_matches(declared: &Type, provided: &Type) -> bool {
        match (declared, provided) {
            (Type::Struct { name: left, .. }, Type::Struct { name: right, .. }) => left == right,
            (Type::Const(left, _), Type::Const(right, _)) => deallocator_type_matches(left, right),
            (
                Type::Ptr {
                    subtype: Some(left),
                    ..
                },
                Type::Ptr {
                    subtype: Some(right),
                    ..
                },
            ) => deallocator_type_matches(left, right),
            _ => true,
        }
    }

    for symbol in module.get_symbols() {
        if symbol.variant != Variant::Function {
            continue;
        }

        let Signature::Function {
            parameters,
            attributes,
            type_params,
            ..
        } = &symbol.signature
        else {
            continue;
        };

        if !attributes.has_deallocator_attribute() {
            continue;
        }

        if parameters.len() != 1 {
            continue;
        }

        let expected_arg: Type = Type::Ptr {
            subtype: Some(std::boxed::Box::new(kind.clone())),
            address_space: None,
            span: kind.get_span(),
        };

        if let Some(type_params) = type_params {
            let Type::Ptr {
                subtype: Some(subtype),
                ..
            } = &parameters[0].1
            else {
                continue;
            };

            if !deallocator_type_matches(subtype, kind) {
                continue;
            }

            let parameter_types: Vec<Type> =
                parameters.iter().map(|(_, ty, _)| ty.clone()).collect();
            let argument_types: Vec<Type> = vec![expected_arg];

            if let Ok(result) = thrustc_generics::solve(
                type_params,
                &[],
                &parameter_types,
                &argument_types,
                &Type::Void {
                    span: kind.get_span(),
                },
                false,
                kind.get_span(),
            ) {
                let generic_args: Vec<Type> = type_params
                    .iter()
                    .filter_map(|parameter| result.env.get(parameter).cloned())
                    .collect();

                if generic_args.len() == type_params.len() {
                    return Some((access, symbol.name.as_str(), generic_args));
                }
            }

            continue;
        }

        let Type::Ptr {
            subtype: Some(subtype),
            ..
        } = &parameters[0].1
        else {
            continue;
        };

        if subtype.as_ref() == kind {
            return Some((access, symbol.name.as_str(), Vec::with_capacity(0)));
        }
    }

    for submodule in module.get_submodules() {
        let mut submodule_access: Vec<String> = access.clone();

        submodule_access.push(submodule.get_name().to_string());

        if let Some(candidate) = self::find_deallocator_in_module(submodule, submodule_access, kind)
        {
            return Some(candidate);
        }
    }

    None
}

pub fn resolve_qualified_generic<'parser>(
    ctx: &ImportContext<'parser, '_>,
    access: &[String],
    symbol: &str,
) -> Option<(thrustc_typesystem::Type, Option<Vec<String>>)> {
    if let Some(Signature::Struct {
        kind, type_params, ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::Struct)
    {
        return Some((kind.clone(), type_params.clone()));
    }

    if let Some(Signature::CustomType {
        kind, type_params, ..
    }) = self::resolve_signature(ctx, access, symbol, Variant::CustomType)
    {
        return Some((kind.clone(), type_params.clone()));
    }

    None
}

pub fn synthesize_function<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    symbol: &str,
    original_name: &str,
    return_type: thrustc_typesystem::Type,
    parameter_types: Vec<thrustc_typesystem::Type>,
    parameter_names: Vec<String>,
    mut attributes: ThrustAttributes,
    demangling_name: String,
    span: Span,
) {
    let mut parameters: Vec<Ast> = Vec::with_capacity(parameter_types.len());
    let mut position: u32 = 0;

    for (kind, name) in parameter_types.iter().zip(parameter_names.iter()) {
        parameters.push(Ast::FunctionParameter {
            name: name.clone(),
            ascii_name: name.clone(),
            kind: kind.clone(),
            position,
            metadata: FunctionParameterMetadata::new(kind.is_ptr_like_type()),
            span,
            id: NodeId::new(),
        });

        position += 1;
    }

    if !attributes.has_extern_attribute() {
        attributes.push(ThrustAttribute::Extern(demangling_name.clone(), span));
    }

    let declaration: Ast = Ast::Function {
        name: symbol.to_string(),
        ascii_name: symbol.to_string(),
        demangling_name,
        original_name: Some(original_name.to_string()),
        parameters,
        parameter_types,
        body: None,
        return_type,
        attributes,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

pub fn synthesize_compiler_intrinsic<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    symbol: &str,
    external_name: &str,
    return_type: thrustc_typesystem::Type,
    parameter_types: Vec<thrustc_typesystem::Type>,
    attributes: ThrustAttributes,
    span: Span,
) {
    let parameters: Vec<Ast> = parameter_types
        .iter()
        .map(|kind| Ast::CompilerIntrinsicParameter {
            kind: kind.clone(),
            span,
            id: NodeId::new(),
        })
        .collect();

    let declaration: Ast = Ast::CompilerIntrinsic {
        name: symbol.to_string(),
        external_name: external_name.to_string(),
        parameters,
        parameters_types: parameter_types,
        return_type,
        attributes,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

fn build_enum_data<'parser>(
    fields: &'parser [(
        String,
        Type,
        Option<thrustc_compile_time::BuiltinValue>,
        Span,
    )],
    span: Span,
) -> Result<thrustc_ast::ast_logic_data::EnumData<'parser>, CompilationIssue> {
    let mut data: thrustc_ast::ast_logic_data::EnumData<'parser> = Vec::with_capacity(fields.len());

    for (field_name, field_type, value, field_span) in fields {
        let Some(value) = value else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!(
                    "Enum field '{}' cannot be resolved to a compile-time value.",
                    field_name
                ),
                "Enum fields imported from modules must be resolvable to literals.".into(),
                None,
                span,
            ));
        };

        data.push((
            field_name.clone(),
            field_type.clone(),
            value.to_ast(field_type.clone(), *field_span),
        ));
    }

    Ok(data)
}

pub fn synthesize_global<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    symbol: &str,
    kind: thrustc_typesystem::Type,
    attributes: &ThrustAttributes,
    is_mutable: bool,
    modificators: &Modificators,
    span: Span,
) {
    let metadata: StaticMetadata =
        self::build_static_metadata(is_mutable, attributes, modificators);

    let declaration: Ast = Ast::Static {
        name: symbol.to_string(),
        ascii_name: symbol.to_string(),
        kind,
        value: None,
        attributes: attributes.clone(),
        modificators: modificators.clone(),
        metadata,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

pub fn synthesize_constant<'parser>(
    ctx: &mut ImportContext<'parser, '_>,
    symbol: &str,
    kind: thrustc_typesystem::Type,
    value: &Ast<'parser>,
    attributes: &ThrustAttributes,
    modificators: &Modificators,
    span: Span,
) {
    let thread_local: bool = modificators.has_lazythread_modificator();
    let is_volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();

    let metadata: ConstantMetadata =
        ConstantMetadata::new(true, thread_local, is_volatile, atomic_ord);

    let private_attributes: ThrustAttributes = attributes
        .iter()
        .filter(|attribute| !attribute.is_public_attribute() && !attribute.is_extern_attribute())
        .cloned()
        .collect();

    let declaration: Ast = Ast::Const {
        name: symbol.to_string(),
        ascii_name: symbol.to_string(),
        kind,
        value: Box::new(value.clone()),
        attributes: private_attributes,
        modificators: modificators.clone(),
        metadata,
        span,
        id: NodeId::new(),
    };

    ctx.add_ast_node(declaration);
}

pub fn build_static_metadata(
    is_mutable: bool,
    attributes: &ThrustAttributes,
    modificators: &Modificators,
) -> StaticMetadata {
    let thread_local: bool = modificators.has_lazythread_modificator();
    let volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();
    let thread_mode: Option<ThrustThreadMode> = modificators.get_thread_mode_modificator();
    let external: bool = attributes.has_extern_attribute();

    StaticMetadata::new(
        true,
        is_mutable,
        true,
        thread_local,
        volatile,
        external,
        atomic_ord,
        thread_mode,
    )
}

fn check_only_collision<'parser>(
    ctx: &ImportContext<'parser, '_>,
    symbol: &'parser str,
    access: &[String],
    origin: &std::path::PathBuf,
    span: Span,
) -> Result<(), CompilationIssue> {
    match ctx.get_symbols().get_import_origin(symbol) {
        Some(imported) if imported == origin => Ok(()),
        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0043,
            format!(
                "The symbol '{}' is exported by more than one imported module (e.g. '{}').",
                symbol,
                access.join("::")
            ),
            "Rename or disambiguate one of the imports to avoid an ambiguous name.".into(),
            None,
            span,
        )),
    }
}

pub fn qualified_symbol_name(access: &[String], symbol: &str) -> String {
    let mut name: String = String::with_capacity(symbol.len() + 32);

    name.push_str("__qualified_");

    for part in access {
        for ch in part.chars() {
            if ch == '_' || ch.is_ascii_alphanumeric() {
                name.push(ch);
            } else {
                name.push('_');
            }
        }

        name.push('_');
    }

    name.push_str(symbol);

    name
}

pub fn has_any_synthetized_function<'parser>(ctx: &ImportContext<'parser, '_>, name: &str) -> bool {
    for node in ctx.get_ast() {
        let Ast::Function {
            name: function_name,
            ..
        } = node
        else {
            continue;
        };

        if function_name == name {
            return true;
        }
    }

    false
}

pub fn resolve_signature<'parser>(
    ctx: &ImportContext<'parser, '_>,
    access: &[String],
    symbol: &str,
    variant: Variant,
) -> Option<&'parser Signature> {
    let table: ExternalSymbolTable<'parser> = ExternalSymbolTable::new(ctx.get_modules());

    table.search_signature(access, symbol, variant)
}
