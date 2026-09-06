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

#![allow(unused_assignments)]

use thrustc_ast::{
    Ast,
    ast_logic_data::StructureData,
    traits::{AstGetType, AstStructFieldsDataExtensions},
};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_constants::COMPILER_TOO_MANY_EXPRESSION_DEPTH;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::{
    Type,
    type_metadata::{ArrayTypeMetadata, FixedArrayTypeMetadata},
    type_modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
};

use thrustc_entities::parser_entities::{
    ConstantSymbol, CustomTypeSymbol, FoundSymbolId, LocalSymbol, ParameterSymbol, StaticSymbol,
    Struct,
};

use thrustc_parser_table::traits::{
    ConstantSymbolExtensions, FoundSymbolEitherExtensions, FoundSymbolExtensions,
    StructSymbolExtensions,
};

use crate::{ParserContext, attributes, expressions};

pub fn build_type<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_expr: bool,
) -> Result<Type, CompilationIssue> {
    ctx.enter_type()?;

    let ty: Result<Type, CompilationIssue> = self::build_type_inner(ctx, parse_expr);

    ctx.leave_type();

    ty
}

fn build_type_inner<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_expr: bool,
) -> Result<Type, CompilationIssue> {
    match ctx.peek().get_type() {
        tk_kind if tk_kind.is_type() => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            match tk_kind {
                _ if tk_kind.is_array() => self::parse_array_type(ctx, span),
                _ if tk_kind.is_const() => self::parse_constant_type(ctx, span),
                _ if tk_kind.is_fn_ref() => self::parse_anonymous_function_type(ctx, span),

                _ => match tk_kind {
                    ty if ty.is_ptr() && ctx.check(TokenType::LBracket) => {
                        let ptr_type: Type = Type::Ptr {
                            subtype: None,
                            address_space: None,
                            span,
                        };

                        self::parse_pointer_type(ctx, ptr_type, span)
                    }

                    TokenType::Char => Ok(Type::Char { span }),
                    TokenType::S8 => Ok(Type::S8 { span }),
                    TokenType::S16 => Ok(Type::S16 { span }),
                    TokenType::S32 => Ok(Type::S32 { span }),
                    TokenType::S64 => Ok(Type::S64 { span }),
                    TokenType::Ssize => Ok(Type::SSize { span }),

                    TokenType::U8 => Ok(Type::U8 { span }),
                    TokenType::U16 => Ok(Type::U16 { span }),
                    TokenType::U32 => Ok(Type::U32 { span }),
                    TokenType::U64 => Ok(Type::U64 { span }),
                    TokenType::U128 => Ok(Type::U128 { span }),
                    TokenType::Usize => Ok(Type::USize { span }),

                    TokenType::Bool => Ok(Type::Bool { span }),

                    TokenType::F32 => Ok(Type::F32 { span }),
                    TokenType::F64 => Ok(Type::F64 { span }),
                    TokenType::F128 => Ok(Type::F128 { span }),

                    TokenType::FX8680 => Ok(Type::FX8680 { span }),
                    TokenType::FPPC128 => Ok(Type::FPPC128 { span }),

                    TokenType::Ptr => Ok(Type::Ptr {
                        subtype: None,
                        address_space: None,
                        span,
                    }),

                    TokenType::Void => Ok(Type::Void { span }),

                    any => Err(CompilationIssue::Error(
                        CompilationIssueCode::E0001,
                        format!("Unknown type '{}'.", any),
                        "You should make sure that it exist at this scope.".into(),
                        None,
                        span,
                    )),
                },
            }
        }

        TokenType::Identifier => {
            let identifier_tk: &Token = ctx.advance()?;

            let name: &'parser str = identifier_tk.get_lexeme();
            let span: Span = identifier_tk.get_span();

            if ctx.match_token(TokenType::ColonColon)? {
                let mut access: Vec<String> = vec![name.to_string()];

                let mut symbol_span: Span = span;

                let symbol: &'parser str = loop {
                    let part_tk: &Token = ctx.consume(
                        TokenType::Identifier,
                        CompilationIssueCode::E0001,
                        "Expected identifier after '::'.".into(),
                    )?;

                    let part: &'parser str = part_tk.get_lexeme();

                    symbol_span = part_tk.get_span();

                    if ctx.match_token(TokenType::ColonColon)? {
                        access.push(part.to_string());
                    } else {
                        break part;
                    }
                };

                if let Some((qualified_type, type_params)) =
                    crate::module_import::resolve_qualified_generic(ctx, &access, symbol)
                {
                    if let Some(type_params) = type_params {
                        if ctx.check(TokenType::LBracket) {
                            let env: thrustc_generics::TypeEnv =
                                self::parse_generic_type_arguments(ctx, &type_params, symbol_span)?;

                            return Ok(thrustc_generics::substitute(&qualified_type, &env));
                        }
                    }

                    return Ok(qualified_type);
                }

                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0042,
                    format!(
                        "Type '{}::{}' could not be determined.",
                        access.join("::"),
                        symbol
                    ),
                    "The module does not export that type.".into(),
                    None,
                    symbol_span,
                ));
            }

            if let Some(parameter_span) = ctx.get_symbols().resolve_type_parameter(name) {
                return Ok(Type::Unresolved {
                    hint: name.to_string(),
                    span: parameter_span,
                });
            }

            if ctx.check(TokenType::LBracket) {
                if let Some(generic) = ctx.get_symbols().get_generic_struct(name).cloned() {
                    let env: thrustc_generics::TypeEnv =
                        self::parse_generic_type_arguments(ctx, &generic.type_params, span)?;

                    let fields: Vec<Type> = generic
                        .field_types
                        .iter()
                        .map(|field| thrustc_generics::substitute(field, &env))
                        .collect();

                    let ty = Type::Struct {
                        name: name.to_string(),
                        fields,
                        metadata: generic.metadata,
                        span,
                    };

                    return Ok(ty);
                }

                if let Some(generic) = ctx.get_symbols().get_generic_custom_type(name).cloned() {
                    let env: thrustc_generics::TypeEnv =
                        self::parse_generic_type_arguments(ctx, &generic.type_params, span)?;

                    return Ok(thrustc_generics::substitute(&generic.kind, &env));
                }
            }

            let object: Result<FoundSymbolId, CompilationIssue> =
                ctx.get_symbols().get_symbols_id(name, span);

            match object {
                Err(_) => self::resolve_builtin_type_or_unknown(ctx, name, span),
                Ok(object) if object.is_structure() => {
                    let (id, scope_idx) = object.expected_struct(span)?;
                    let reference: Result<Struct, CompilationIssue> =
                        ctx.get_symbols().get_struct_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let data: StructureData = object.get_data();
                    let ty: Type = data.get_struct_type();

                    Ok(ty)
                }
                Ok(object) if object.is_custom_type() => {
                    let (id, scope_idx) = object.expected_custom_type(span)?;
                    let reference: Result<CustomTypeSymbol, CompilationIssue> =
                        ctx.get_symbols().get_custom_type_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }
                Ok(object) if object.is_parameter() => {
                    let parameter_id: &str = object.expected_parameter(span)?;
                    let reference: Result<ParameterSymbol, CompilationIssue> =
                        ctx.get_symbols().get_parameter_by_id(parameter_id, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }
                Ok(object) if object.is_local() => {
                    let (id, scope_idx) = object.expected_local(span)?;
                    let reference: Result<&LocalSymbol, CompilationIssue> =
                        ctx.get_symbols().get_local_by_id(id, scope_idx, span);

                    let Ok(object) = reference.cloned() else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }
                Ok(object) if object.is_static() => {
                    let (id, scope_idx) = object.expected_static(span)?;
                    let reference: Result<StaticSymbol, CompilationIssue> =
                        ctx.get_symbols().get_static_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }
                Ok(object) if object.is_constant() => {
                    let (id, scope_idx) = object.expected_constant(span)?;
                    let reference: Result<ConstantSymbol, CompilationIssue> =
                        ctx.get_symbols().get_const_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Unknown type '{}'.", name),
                            "You should make sure that it exist at this scope.".into(),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }

                _ => self::resolve_builtin_type_or_unknown(ctx, name, span),
            }
        }

        _ if parse_expr => expressions::parse_expr(ctx)?.get_value_type().cloned(),

        any => Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Unknown type '{}'.", any),
            "You should make sure that it exist at this scope.".into(),
            None,
            ctx.peek().get_span(),
        )),
    }
}

fn resolve_builtin_type_or_unknown<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Type, CompilationIssue> {
    if let Some(builtin_type) = ctx.get_builtins().get_type(name) {
        return Ok(builtin_type.clone());
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0001,
        format!("Unknown type '{}'.", name),
        "You should make sure that it exist at this scope.".into(),
        None,
        span,
    ))
}

fn parse_generic_type_arguments<'parser>(
    ctx: &mut ParserContext<'parser>,
    type_params: &[String],
    span: Span,
) -> Result<thrustc_generics::TypeEnv, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    let mut type_args: Vec<Type> = Vec::with_capacity(type_params.len());

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let argument_type: Type = self::build_type(ctx, false)?;

        type_args.push(argument_type);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(
            TokenType::Comma,
            CompilationIssueCode::E0001,
            "Expected ','.".into(),
        )?;
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    if type_args.len() != type_params.len() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0053,
            "The generic type does not receive that many type arguments.".into(),
            "You should provide one type per generic parameter.".into(),
            None,
            span,
        ));
    }

    let generics: thrustc_generics::TypeEnv = type_params
        .iter()
        .zip(type_args)
        .map(|(parameter, argument)| (parameter.clone(), argument))
        .collect();

    Ok(generics)
}

fn parse_anonymous_function_type(
    ctx: &mut ParserContext<'_>,
    span: Span,
) -> Result<Type, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    let mut parameter_types: Vec<Type> = Vec::with_capacity(10);

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let param_type: Type = self::build_type(ctx, false)?;

        parameter_types.push(param_type);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(
            TokenType::Comma,
            CompilationIssueCode::E0001,
            "Expected ','.".into(),
        )?;
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Arrow])?;
    let has_ignore: bool = attributes.has_ignore_attribute();

    ctx.consume(
        TokenType::Arrow,
        CompilationIssueCode::E0001,
        "Expected '->'.".into(),
    )?;

    let return_type: Type = self::build_type(ctx, false)?;

    let function_ty: Type = Type::Fn {
        return_type: return_type.into(),
        parameter_types,
        modificator: FunctionReferenceTypeModificator::new(
            LLVMFunctionReferenceTypeModificator::new(has_ignore),
            GCCFunctionReferenceTypeModificator::default(),
        ),
        span,
    };

    Ok(function_ty)
}

fn parse_constant_type(ctx: &mut ParserContext<'_>, span: Span) -> Result<Type, CompilationIssue> {
    let inner_type: Type = self::build_type(ctx, false)?;

    Ok(Type::Const(inner_type.into(), span))
}

fn parse_array_type(ctx: &mut ParserContext<'_>, span: Span) -> Result<Type, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0019,
        "Expected '['.".into(),
    )?;

    let array_type: Type = self::build_type(ctx, false)?;

    let mut address_space: Option<u16> = None;

    if ctx.check(TokenType::SemiColon) {
        ctx.consume(
            TokenType::SemiColon,
            CompilationIssueCode::E0001,
            "Expected ';'.".into(),
        )?;

        let size: Ast = expressions::parse_expr(ctx)?;

        let size_value: Option<u64> = {
            let ctx_ref: &ParserContext<'_> = &*ctx;
            let mut depth: usize = 0;

            match thrustc_compile_time::fold_resolving(&size, &mut |name, span| {
                self::resolve_constant_value(ctx_ref, name, span, &mut depth)
            }) {
                Some(BuiltinValue::Integer(value)) => Some(value),
                _ => None,
            }
        };

        if ctx.check(TokenType::Comma) {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;

            address_space = self::parse_memory_address_space(ctx, span)?;
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;

        let size: u32 = self::check_fixed_array_size(size_value, span)?;

        return Ok(Type::FixedArray {
            base_type: array_type.into(),
            size,
            metadata: FixedArrayTypeMetadata::new(address_space),
            span,
        });
    }

    if ctx.check(TokenType::Comma) {
        ctx.consume(
            TokenType::Comma,
            CompilationIssueCode::E0001,
            "Expected ','.".into(),
        )?;

        address_space = self::parse_memory_address_space(ctx, span)?;
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    let array_ty: Type = Type::Array {
        base_type: array_type.into(),
        infered_type: None,
        metadata: ArrayTypeMetadata::new(None, address_space),
        span,
    };

    Ok(array_ty)
}

fn parse_pointer_type(
    ctx: &mut ParserContext<'_>,
    mut before_type: Type,
    span: Span,
) -> Result<Type, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    if let Type::Ptr { .. } = &mut before_type {
        let mut inner_type: Type = self::build_type(ctx, false)?;

        while ctx.check(TokenType::LBracket) {
            inner_type = self::parse_pointer_type(ctx, inner_type, span)?;
        }

        let mut address_space: Option<u16> = None;

        if ctx.check(TokenType::Comma) {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;

            address_space = self::parse_memory_address_space(ctx, span)?;
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;

        let ptr_type: Type = Type::Ptr {
            subtype: Some(inner_type.into()),
            address_space,
            span,
        };

        Ok(ptr_type)
    } else {
        Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Expected pointer type, not '{}'", before_type),
            "You should pass a pointer type.".into(),
            None,
            ctx.previous().span,
        ))
    }
}

fn parse_memory_address_space<'parser>(
    ctx: &mut ParserContext<'parser>,
    span: Span,
) -> Result<Option<u16>, CompilationIssue> {
    let memory_address_expr: Ast<'parser> = expressions::parse_expr(ctx)?;

    let value: Option<u64> = {
        let ctx_ref: &ParserContext<'parser> = &*ctx;
        let mut depth: usize = 0;

        match thrustc_compile_time::fold_resolving(&memory_address_expr, &mut |name, span| {
            self::resolve_constant_value(ctx_ref, name, span, &mut depth)
        }) {
            Some(BuiltinValue::Integer(value)) => Some(value),
            _ => None,
        }
    };

    let value: u16 = value
        .and_then(|value| u16::try_from(value).ok())
        .ok_or_else(|| {
            CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected constant integer value as memory address.".into(),
                "You should pass a constant integer expression.".into(),
                None,
                span,
            )
        })?;

    Ok(Some(value))
}

fn resolve_constant_value(
    ctx: &ParserContext<'_>,
    name: &str,
    span: Span,
    depth: &mut usize,
) -> Option<BuiltinValue> {
    if *depth >= COMPILER_TOO_MANY_EXPRESSION_DEPTH as usize {
        return None;
    }

    let symbol: FoundSymbolId = ctx.get_symbols().get_symbols_id(name, span).ok()?;

    if !symbol.is_constant() {
        return None;
    }

    let (id, scope_idx) = symbol.expected_constant(span).ok()?;

    let constant: ConstantSymbol = ctx
        .get_symbols()
        .get_const_by_id(id, scope_idx, span)
        .ok()?;

    let value: Ast = constant.get_value()?;

    *depth = depth.saturating_add(1);

    let result: Option<BuiltinValue> =
        thrustc_compile_time::fold_resolving(&value, &mut |name, span| {
            self::resolve_constant_value(ctx, name, span, &mut *depth)
        });

    *depth = depth.saturating_sub(1);

    result
}

fn check_fixed_array_size(size: Option<u64>, span: Span) -> Result<u32, CompilationIssue> {
    let size: u64 = size.ok_or_else(|| {
        CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Expected constant integer value as array size.".into(),
            "You should pass a constant integer expression.".into(),
            None,
            span,
        )
    })?;

    u32::try_from(size).map_err(|_| {
        CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Array size is too large.".into(),
            "The array size must fit in a unsigned 32-bit integer.".into(),
            None,
            span,
        )
    })
}
