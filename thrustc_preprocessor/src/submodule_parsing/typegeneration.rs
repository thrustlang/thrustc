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
    Ast,
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
    traits::TypeIsExtensions,
    type_modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
};

use crate::{
    parser::ModuleParser,
    signatures::{Signature, Symbol, Variant},
    submodule_parsing::{attributes, expressions},
};

pub fn build_type(ctx: &mut ModuleParser<'_>) -> Result<Type, ()> {
    match ctx.peek().kind {
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
                    TokenType::Char => Ok(Type::Char(span)),

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
                    TokenType::Addr => Ok(Type::Addr(span)),
                    TokenType::Void => Ok(Type::Void(span)),

                    _ => Err(()),
                },
            }
        }

        TokenType::Identifier => {
            let identifier_tk: &Token = ctx.advance()?;

            let name: String = identifier_tk.get_lexeme().to_string();

            let symbol: Option<&crate::signatures::Symbol> =
                ctx.get_module().search_symbol(name, Variant::CustomType);

            match symbol {
                Some(symbol) => {
                    if let Symbol {
                        signature: Signature::CustomType { kind, .. },
                        variant: Variant::CustomType,
                        ..
                    } = symbol
                    {
                        Ok(kind.clone())
                    } else {
                        Err(())
                    }
                }

                None => Err(()),
            }
        }

        _ => Err(()),
    }
}

fn parse_anonymous_function_type(ctx: &mut ModuleParser<'_>, span: Span) -> Result<Type, ()> {
    ctx.consume(TokenType::LBracket)?;

    let mut parameter_types: Vec<Type> = Vec::with_capacity(10);

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let param_type: Type = self::build_type(ctx)?;

        parameter_types.push(param_type);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(TokenType::Comma)?;
    }

    ctx.consume(TokenType::RBracket)?;

    let attributes: ThrustAttributes = attributes::build_attributes(ctx, &[TokenType::Arrow])?;
    let has_ignore: bool = attributes.has_ignore_attribute();

    ctx.consume(TokenType::Arrow)?;

    let return_type: Type = self::build_type(ctx)?;

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

fn parse_constant_type(ctx: &mut ModuleParser<'_>, span: Span) -> Result<Type, ()> {
    let inner_type: Type = self::build_type(ctx)?;

    Ok(Type::Const(inner_type.into(), span))
}

fn parse_array_type(ctx: &mut ModuleParser<'_>, span: Span) -> Result<Type, ()> {
    ctx.consume(TokenType::LBracket)?;

    let array_type: Type = self::build_type(ctx)?;

    if ctx.check(TokenType::SemiColon) {
        ctx.consume(TokenType::SemiColon)?;

        let size: Ast = expressions::parse_expr(ctx)?;
        let size_type: &Type = size.get_value_type().map_err(|_| ())?;

        if !size.is_integer() {
            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected literal integer value as a size indicator.".into(),
                "You should pass an integer expression.".into(),
                None,
                span,
            ));
        }

        if !size_type.is_unsigned_integer_type() || !size_type.is_lesseq_unsigned32bit_integer() {
            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected unsigned integer value less than or equal to 32 bits.".into(),
                "You should pass an integer expression.".into(),
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
            return Err(());
        }

        ctx.consume(TokenType::RBracket)?;

        return Ok(Type::FixedArray(
            array_type.into(),
            array_size.unwrap_or_default(),
            span,
        ));
    }

    ctx.consume(TokenType::RBracket)?;

    Ok(Type::Array {
        base_type: array_type.into(),
        infered_type: None,
        span,
    })
}

fn parse_pointer_type(
    ctx: &mut ModuleParser<'_>,
    mut before_type: Type,
    span: Span,
) -> Result<Type, ()> {
    ctx.consume(TokenType::LBracket)?;

    if let Type::Ptr { .. } = &mut before_type {
        let mut inner_type: Type = self::build_type(ctx)?;

        while ctx.check(TokenType::LBracket) {
            inner_type = self::parse_pointer_type(ctx, inner_type, span)?;
        }

        let mut address_space: Option<u16> = None;

        if ctx.check(TokenType::Comma) {
            ctx.consume(TokenType::Comma)?;

            let memory_address_expr: Ast<'_> = expressions::parse_expr(ctx)?;
            let memory_address_type: &Type =
                memory_address_expr.get_value_type().map_err(|_| ())?;

            if !memory_address_expr.is_integer() {
                ctx.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected literal integer value".into(),
                    "You should pass an integer expression.".into(),
                    None,
                    span,
                ));
            }

            if !memory_address_type.is_unsigned_integer_type()
                || !memory_address_type.is_lesseq_unsigned32bit_integer()
            {
                ctx.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected unsigned integer value.".into(),
                    "You should pass a unsigned integer value less than or equal to 32 bits."
                        .into(),
                    None,
                    span,
                ));
            }

            let memery_address_unprocessed: u64 =
                if let Ast::Integer { value, .. } = memory_address_expr {
                    value
                } else {
                    0
                };

            let memory_address_value: Result<u16, std::num::TryFromIntError> =
                u16::try_from(memery_address_unprocessed);

            if memory_address_value.is_err() {
                ctx.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected literal integer value".into(),
                    "You should pass an integer expression.".into(),
                    None,
                    span,
                ));
            }

            address_space = Some(memory_address_value.unwrap_or_default());
        }

        ctx.consume(TokenType::RBracket)?;

        let ptr_type: Type = Type::Ptr {
            subtype: Some(inner_type.into()),
            address_space,
            span,
        };

        Ok(ptr_type)
    } else {
        Err(())
    }
}
