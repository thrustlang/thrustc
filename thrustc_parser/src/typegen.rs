use thrustc_ast::{
    Ast,
    data::StructureData,
    traits::{AstGetType, AstStandardExtensions},
};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::{
    Type,
    modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
    traits::TypeIsExtensions,
};

use thrustc_entities::parser::{
    ConstantSymbol, CustomTypeSymbol, FoundSymbolId, LocalSymbol, ParameterSymbol, StaticSymbol,
    Struct,
};

use crate::{
    ParserContext, attributes, expressions,
    traits::{
        FoundSymbolEitherExtensions, FoundSymbolExtensions, StructFieldsExtensions,
        StructSymbolExtensions,
    },
};

pub fn build_type(ctx: &mut ParserContext<'_>, parse_expr: bool) -> Result<Type, CompilationIssue> {
    match ctx.peek().kind {
        tk_kind if tk_kind.is_type() => {
            let tk: &Token = ctx.advance()?;
            let span: Span = tk.get_span();

            match tk_kind {
                _ if tk_kind.is_array() => self::build_array_type(ctx, span),
                _ if tk_kind.is_const() => self::build_const_type(ctx, span),
                _ if tk_kind.is_fn_ref() => self::build_fn_ref_type(ctx, span),
                _ => match tk_kind {
                    ty if ty.is_ptr() && ctx.check(TokenType::LBracket) => {
                        self::build_recursive_type(ctx, Type::Ptr(None, span), span)
                    }
                    TokenType::Char => Ok(Type::Char(span)),

                    TokenType::S8 => Ok(Type::S8(span)),
                    TokenType::S16 => Ok(Type::S16(span)),
                    TokenType::S32 => Ok(Type::S32(span)),
                    TokenType::S64 => Ok(Type::S64(span)),
                    TokenType::Ssize => Ok(Type::SSize(span)),

                    TokenType::U8 => Ok(Type::U8(span)),
                    TokenType::U16 => Ok(Type::U16(span)),
                    TokenType::U32 => Ok(Type::U32(span)),
                    TokenType::U64 => Ok(Type::U64(span)),
                    TokenType::U128 => Ok(Type::U128(span)),
                    TokenType::Usize => Ok(Type::USize(span)),

                    TokenType::Bool => Ok(Type::Bool(span)),

                    TokenType::F32 => Ok(Type::F32(span)),
                    TokenType::F64 => Ok(Type::F64(span)),
                    TokenType::F128 => Ok(Type::F128(span)),

                    TokenType::FX8680 => Ok(Type::FX8680(span)),
                    TokenType::FPPC128 => Ok(Type::FPPC128(span)),

                    TokenType::Ptr => Ok(Type::Ptr(None, span)),
                    TokenType::Addr => Ok(Type::Addr(span)),
                    TokenType::Void => Ok(Type::Void(span)),

                    any => Err(CompilationIssue::Error(
                        CompilationIssueCode::E0001,
                        format!("{} isn't a valid type.", any),
                        None,
                        span,
                    )),
                },
            }
        }

        TokenType::Identifier => {
            let identifier_tk: &Token = ctx.advance()?;

            let name: &str = identifier_tk.get_lexeme();
            let span: Span = identifier_tk.get_span();

            let object: Result<FoundSymbolId, CompilationIssue> =
                ctx.get_symbols().get_symbols_id(name, span);

            match object {
                Ok(object) if object.is_structure() => {
                    let (id, scope_idx) = object.expected_struct(span)?;
                    let reference: Result<Struct, CompilationIssue> =
                        ctx.get_symbols().get_struct_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Not found type '{}'.", name),
                            None,
                            span,
                        ));
                    };

                    let data: StructureData = object.get_data();

                    Ok(data.get_type())
                }
                Ok(object) if object.is_custom_type() => {
                    let (id, scope_idx) = object.expected_custom_type(span)?;
                    let reference: Result<CustomTypeSymbol, CompilationIssue> =
                        ctx.get_symbols().get_custom_type_by_id(id, scope_idx, span);

                    let Ok(object) = reference else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0001,
                            format!("Not found type '{}'.", name),
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
                            format!("Not found type '{}'.", name),
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
                            format!("Not found type '{}'.", name),
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
                            format!("Not found type '{}'.", name),
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
                            format!("Not found type '{}'.", name),
                            None,
                            span,
                        ));
                    };

                    let object_type: Type = object.0;

                    Ok(object_type)
                }

                _ => Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    format!("Not found type '{}'.", name),
                    None,
                    span,
                )),
            }
        }

        _ if parse_expr => expressions::build_expr(ctx)?.get_value_type().cloned(),

        what_heck => Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Expected type, not '{}'", what_heck),
            None,
            ctx.previous().span,
        )),
    }
}

fn build_fn_ref_type(ctx: &mut ParserContext<'_>, span: Span) -> Result<Type, CompilationIssue> {
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

    Ok(Type::Fn(
        parameter_types,
        return_type.into(),
        FunctionReferenceTypeModificator::new(
            LLVMFunctionReferenceTypeModificator::new(has_ignore),
            GCCFunctionReferenceTypeModificator::default(),
        ),
        span,
    ))
}

fn build_const_type(ctx: &mut ParserContext<'_>, span: Span) -> Result<Type, CompilationIssue> {
    let inner_type: Type = self::build_type(ctx, false)?;

    Ok(Type::Const(inner_type.into(), span))
}

fn build_array_type(ctx: &mut ParserContext<'_>, span: Span) -> Result<Type, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0019,
        "Expected '['.".into(),
    )?;

    let array_type: Type = self::build_type(ctx, false)?;

    if ctx.check(TokenType::SemiColon) {
        ctx.consume(
            TokenType::SemiColon,
            CompilationIssueCode::E0001,
            "Expected ';'.".into(),
        )?;

        let size: Ast = expressions::build_expr(ctx)?;
        let size_type: &Type = size.get_value_type()?;

        if !size.is_integer() {
            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected literal integer value as a max size indicator.".into(),
                None,
                span,
            ));
        }

        if !size_type.is_unsigned_integer_type() || !size_type.is_lesseq_unsigned32bit_integer() {
            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected unsigned integer value less than or equal to 32 bits.".into(),
                None,
                span,
            ));
        }

        let size: u64 = if let Ast::Integer { value, .. } = size {
            value
        } else {
            0
        };

        let array_size: Result<u32, std::num::TryFromIntError> = u32::try_from(size);

        if array_size.is_err() {
            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected any unsigned 32 bits integer value.".into(),
                None,
                span,
            ));
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;

        return Ok(Type::FixedArray(
            array_type.into(),
            array_size.unwrap_or_default(),
            span,
        ));
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    Ok(Type::Array {
        base_type: array_type.into(),
        infered_type: None,
        span,
    })
}

fn build_recursive_type(
    ctx: &mut ParserContext<'_>,
    mut before_type: Type,
    span: Span,
) -> Result<Type, CompilationIssue> {
    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    if let Type::Ptr(..) = &mut before_type {
        let mut inner_type: Type = self::build_type(ctx, false)?;

        while ctx.check(TokenType::LBracket) {
            inner_type = self::build_recursive_type(ctx, inner_type, span)?;
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;

        Ok(Type::Ptr(Some(inner_type.into()), span))
    } else {
        Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Expected pointer type, not '{}'", before_type),
            None,
            ctx.previous().span,
        ))
    }
}
