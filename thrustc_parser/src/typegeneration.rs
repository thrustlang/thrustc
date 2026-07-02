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
    ast_logic_data::StructureData,
    traits::{AstGetType, AstStandardExtensions, AstStructFieldsDataExtensions},
};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_llvm_target_triple::traits::LLVMTargetTripleSupport;
use thrustc_span::Span;

use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::{
    Type,
    traits::TypeIsExtensions,
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
    FoundSymbolEitherExtensions, FoundSymbolExtensions, StructSymbolExtensions,
};

use crate::{ParserContext, attributes, expressions};

pub fn build_type(ctx: &mut ParserContext<'_>, parse_expr: bool) -> Result<Type, CompilationIssue> {
    let llvm: bool = ctx.get_options().llvm();

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

                    TokenType::FX8680 => {
                        if llvm {
                            let compiler_options: &thrustc_options::CompilerOptions =
                                ctx.get_options();
                            let llvm_backend: &thrustc_backends::llvm::LLVMBackend =
                                compiler_options.get_llvm_backend();

                            let normalized_target_triple: &thrustc_llvm_target_triple::LLVMTargetTriple =
                                llvm_backend.get_target().get_normalized_target_triple();

                            let support: bool =
                                normalized_target_triple.support_80_bits_floating_point();

                            if !support {
                                ctx.add_error_report(CompilationIssue::Error(
                                    CompilationIssueCode::E0039,
                                    "Unsupported type".into(),
                                    "Type isn't supported on the current target architecture."
                                        .into(),
                                    None,
                                    span,
                                ));
                            }
                        }

                        Ok(Type::FX8680 { span })
                    }
                    TokenType::FPPC128 => {
                        if llvm {
                            let compiler_options: &thrustc_options::CompilerOptions =
                                ctx.get_options();
                            let llvm_backend: &thrustc_backends::llvm::LLVMBackend =
                                compiler_options.get_llvm_backend();

                            let normalized_target_triple: &thrustc_llvm_target_triple::LLVMTargetTriple =
                                llvm_backend.get_target().get_normalized_target_triple();

                            let support: bool =
                                normalized_target_triple.support_128_bits_ppc_floating_point();

                            if !support {
                                ctx.add_error_report(CompilationIssue::Error(
                                    CompilationIssueCode::E0039,
                                    "Unsupported type".into(),
                                    "Type isn't supported on the current target architecture."
                                        .into(),
                                    None,
                                    span,
                                ));
                            }
                        }

                        Ok(Type::FPPC128 { span })
                    }

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

                _ => Err(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    format!("Unknown type '{}'.", name),
                    "You should make sure that it exist at this scope.".into(),
                    None,
                    span,
                )),
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
        let size_type: &Type = size.get_value_type()?;

        if !size.is_integer() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected literal integer value".into(),
                "You should pass an integer expression.".into(),
                None,
                span,
            ));
        }

        if !size_type.is_unsigned_integer_type() || !size_type.is_lesseq_unsigned32bit_integer() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected unsigned integer value.".into(),
                "You should pass a unsigned integer value less than or equal to 32 bits.".into(),
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
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected literal integer value".into(),
                "You should pass an integer expression.".into(),
                None,
                span,
            ));
        }

        if ctx.check(TokenType::Comma) {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;

            let memory_address_expr: Ast<'_> = expressions::parse_expr(ctx)?;
            let memory_address_type: &Type = memory_address_expr.get_value_type()?;

            if !memory_address_expr.is_integer() {
                ctx.add_error_report(CompilationIssue::Error(
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
                ctx.add_error_report(CompilationIssue::Error(
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
                ctx.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected literal integer value".into(),
                    "You should pass an integer expression.".into(),
                    None,
                    span,
                ));
            }

            address_space = Some(memory_address_value.unwrap_or_default());
        }

        ctx.consume(
            TokenType::RBracket,
            CompilationIssueCode::E0001,
            "Expected ']'.".into(),
        )?;

        return Ok(Type::FixedArray {
            base_type: array_type.into(),
            size: array_size.unwrap_or_default(),
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

        let memory_address_expr: Ast<'_> = expressions::parse_expr(ctx)?;
        let memory_address_type: &Type = memory_address_expr.get_value_type()?;

        if !memory_address_expr.is_integer() {
            ctx.add_error_report(CompilationIssue::Error(
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
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected unsigned integer value.".into(),
                "You should pass a unsigned integer value less than or equal to 32 bits.".into(),
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
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected literal integer value".into(),
                "You should pass an integer expression.".into(),
                None,
                span,
            ));
        }

        address_space = Some(memory_address_value.unwrap_or_default());
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

            let memory_address_expr: Ast<'_> = expressions::parse_expr(ctx)?;
            let memory_address_type: &Type = memory_address_expr.get_value_type()?;

            if !memory_address_expr.is_integer() {
                ctx.add_error_report(CompilationIssue::Error(
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
                ctx.add_error_report(CompilationIssue::Error(
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
                ctx.add_error_report(CompilationIssue::Error(
                    CompilationIssueCode::E0001,
                    "Expected literal integer value".into(),
                    "You should pass an integer expression.".into(),
                    None,
                    span,
                ));
            }

            address_space = Some(memory_address_value.unwrap_or_default());
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
