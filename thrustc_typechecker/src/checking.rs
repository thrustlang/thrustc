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

use thrustc_ast::{Ast, ast_metadata::CastingMetadata};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;

use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{context::TypeCheckerControlContext, metadata::TypeCheckerNodeMetadata};

pub fn check_type_together(
    target: &Type,
    provided: &Type,
    node: Option<&Ast>,
    operator: Option<&TokenType>,
    metadata: TypeCheckerNodeMetadata,
    span: Span,

    control_context: &mut TypeCheckerControlContext,
) -> Result<(), CompilationIssue> {
    control_context.increase_checking_depth();

    if control_context.get_checking_depth() >= thrustc_constants::COMPILER_TOO_MANY_TYPE_DEPTH {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0037,
            "Too many type depth, the expression exceeds type checking bounds!".into(),
            "You should remove nested types.".into(),
            None,
            span,
        ));
    }

    let error: CompilationIssue = CompilationIssue::Error(
        CompilationIssueCode::E0020,
        format!("Expected '{}' type, got '{}' type.", target, provided),
        "You should make the type match.".into(),
        None,
        span,
    );

    if let Some(Ast::BinaryOp {
        operator,
        kind: provided,
        ..
    }) = node
    {
        self::check_type_together(
            target,
            provided,
            None,
            Some(operator),
            metadata,
            span,
            control_context,
        )?;

        return Ok(());
    }

    if let Some(Ast::UnaryOp {
        operator,
        kind: provided,
        ..
    }) = node
    {
        self::check_type_together(
            target,
            provided,
            None,
            Some(operator),
            metadata,
            span,
            control_context,
        )?;

        return Ok(());
    }

    if let Some(Ast::Group {
        node,
        kind: provided,
        ..
    }) = node
    {
        self::check_type_together(
            target,
            provided,
            Some(node),
            None,
            metadata,
            span,
            control_context,
        )?;

        return Ok(());
    }

    match (target, provided, operator) {
        (Type::Const(ty_1, ..), Type::Const(ty_2, ..), None) => {
            self::check_type_together(ty_1, ty_2, None, None, metadata, span, control_context)?;
            Ok(())
        }

        (ty_1, Type::Const(ty_2, ..), None) => {
            self::check_type_together(ty_1, ty_2, None, None, metadata, span, control_context)?;
            Ok(())
        }

        (Type::Const(ty_1, ..), ty_2, .., None) => {
            self::check_type_together(ty_1, ty_2, None, None, metadata, span, control_context)?;
            Ok(())
        }

        (Type::Char { .. }, Type::Char { .. }, None) => Ok(()),

        (
            Type::Struct {
                fields: fields_1,
                metadata: metadata_1,
                ..
            },
            Type::Struct {
                fields: fields_2,
                metadata: metadata_2,
                ..
            },
            None,
        ) => {
            if fields_1.len() != fields_2.len() {
                return Err(error);
            }

            if metadata_1 != metadata_2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0021,
                    format!(
                        "Expected structure type with '{}' attributes but found '{}'.",
                        metadata_1, metadata_2
                    ),
                    "You should compare them and see which specific one doesn't follow the expected."
                        .into(),
                    None,
                    span,
                ));
            }

            {
                for (target, provided) in fields_1.iter().zip(fields_2) {
                    self::check_type_together(
                        target,
                        provided,
                        None,
                        None,
                        metadata,
                        span,
                        control_context,
                    )?;
                }
            }

            Ok(())
        }

        (
            Type::Fn {
                return_type: return_type_1,
                parameter_types: parameters_types_1,
                modificator: modificator_1,
                ..
            },
            Type::Fn {
                return_type: return_type_2,
                parameter_types: parameters_types_2,
                modificator: modificator_2,
                ..
            },
            None,
        ) => {
            if parameters_types_1.len() != parameters_types_2.len() {
                return Err(error);
            }

            if return_type_1 != return_type_2 {
                return Err(error);
            }

            if modificator_1 != modificator_2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0021,
                    format!(
                        "Expected function reference type with '{}' attributes but found '{}'.",
                        modificator_1, modificator_2
                    ),
                    "You should compare them and see which specific one doesn't follow the expected.".into(),
                    None,
                    span,
                ));
            }

            {
                for (target, provided) in parameters_types_1.iter().zip(parameters_types_2) {
                    self::check_type_together(
                        target,
                        provided,
                        None,
                        None,
                        metadata,
                        span,
                        control_context,
                    )?;
                }
            }

            Ok(())
        }

        (
            Type::FixedArray {
                base_type: target,
                size: target_size,
                ..
            },
            Type::FixedArray {
                base_type: provided,
                size: provided_size,
                ..
            },
            None,
        ) => {
            if target_size == provided_size {
                self::check_type_together(
                    target,
                    provided,
                    None,
                    None,
                    metadata,
                    span,
                    control_context,
                )?;
                return Ok(());
            }

            Err(error)
        }

        (
            Type::Array {
                base_type: target, ..
            },
            Type::Array {
                base_type: provided,
                ..
            },
            None,
        ) => {
            self::check_type_together(
                target,
                provided,
                None,
                None,
                metadata,
                span,
                control_context,
            )?;
            Ok(())
        }

        (
            Type::Ptr { subtype: None, .. },
            Type::Ptr { subtype: None, .. },
            Some(TokenType::EqEq | TokenType::BangEq) | None,
        ) => Ok(()),

        (
            Type::Ptr {
                subtype: Some(target),
                ..
            },
            Type::Ptr {
                subtype: Some(provided),
                ..
            },
            Some(TokenType::EqEq | TokenType::BangEq) | None,
        ) => {
            self::check_type_together(
                target,
                provided,
                node,
                operator,
                metadata,
                span,
                control_context,
            )?;
            Ok(())
        }

        (Type::Ptr { .. }, Type::Ptr { .. }, Some(TokenType::EqEq | TokenType::BangEq) | None) => {
            Ok(())
        }

        (
            Type::Bool { .. },
            Type::Bool { .. },
            Some(
                TokenType::BangEq
                | TokenType::EqEq
                | TokenType::LessEq
                | TokenType::Less
                | TokenType::Greater
                | TokenType::GreaterEq
                | TokenType::And
                | TokenType::Or
                | TokenType::Bang,
            )
            | None,
        ) => Ok(()),

        (
            Type::S8 { .. },
            Type::S8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::S16 { .. },
            Type::S16 { .. } | Type::S8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::S32 { .. },
            Type::S32 { .. } | Type::S16 { .. } | Type::S8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::S64 { .. },
            Type::S64 { .. } | Type::S32 { .. } | Type::S16 { .. } | Type::S8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Arith
                | TokenType::Star
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::SSize { .. },
            Type::SSize { .. }
            | Type::S64 { .. }
            | Type::S32 { .. }
            | Type::S16 { .. }
            | Type::S8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Arith
                | TokenType::Star
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::U8 { .. },
            Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::U16 { .. },
            Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::U32 { .. },
            Type::U32 { .. } | Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::U64 { .. },
            Type::U64 { .. } | Type::U32 { .. } | Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::U128 { .. },
            Type::U128 { .. }
            | Type::U64 { .. }
            | Type::U32 { .. }
            | Type::U16 { .. }
            | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::USize { .. },
            Type::USize { .. }
            | Type::U64 { .. }
            | Type::U32 { .. }
            | Type::U16 { .. }
            | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) => Ok(()),

        (
            Type::F32 { .. },
            Type::F32 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq,
            )
            | None,
        ) => Ok(()),

        (
            Type::F64 { .. },
            Type::F64 { .. } | Type::F32 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq,
            )
            | None,
        ) => Ok(()),

        (
            Type::F128 { .. },
            Type::F128 { .. } | Type::F64 { .. } | Type::F32 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq,
            )
            | None,
        ) => Ok(()),

        (
            Type::FX8680 { .. },
            Type::FX8680 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq,
            )
            | None,
        ) => Ok(()),

        (
            Type::FPPC128 { .. },
            Type::FPPC128 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq,
            )
            | None,
        ) => Ok(()),

        (
            Type::S8 { .. },
            Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) if metadata.is_literal_value() => Ok(()),

        (
            Type::S16 { .. },
            Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::Arith
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) if metadata.is_literal_value() => Ok(()),

        (
            Type::S32 { .. },
            Type::U32 { .. } | Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::Arith
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) if metadata.is_literal_value() => Ok(()),

        (
            Type::S64 { .. },
            Type::U64 { .. } | Type::U32 { .. } | Type::U16 { .. } | Type::U8 { .. },
            Some(
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star
                | TokenType::Arith
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::PlusPlus
                | TokenType::MinusMinus
                | TokenType::MinusEq
                | TokenType::PlusEq
                | TokenType::Xor
                | TokenType::Bor
                | TokenType::Not
                | TokenType::BAnd,
            )
            | None,
        ) if metadata.is_literal_value() => Ok(()),

        (Type::Void { .. }, Type::Void { .. }, None) => Ok(()),

        _ => Err(error),
    }
}

pub fn check_type_cast(
    cast_type: &Type,
    from_type: &Type,
    metadata: &CastingMetadata,
    span: &Span,

    control_context: &mut TypeCheckerControlContext,
) -> Result<(), CompilationIssue> {
    let is_allocated: bool = metadata.is_allocated();

    control_context.increase_type_cast_depth();

    if control_context.get_type_cast_depth() >= thrustc_constants::COMPILER_TOO_MANY_TYPE_DEPTH {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0037,
            "Too many type depth, the type exceeds type checking bounds!".into(),
            "You should remove nested types.".into(),
            None,
            *span,
        ));
    }

    match (from_type, cast_type) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. },
        ) => Ok(()),

        (Type::FX8680 { .. }, Type::FX8680 { .. }) => Ok(()),
        (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => Ok(()),

        (
            Type::Ptr { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
        ) => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
            Type::Bool { .. },
        ) => Ok(()),

        (
            Type::Bool { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
        ) => Ok(()),

        (Type::Ptr { .. }, Type::Ptr { .. }) => Ok(()),
        (Type::Ptr { .. }, Type::Array { .. }) if is_allocated => Ok(()),
        (Type::Ptr { subtype: None, .. }, Type::Fn { .. }) if is_allocated => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. }
            | Type::FX8680 { .. }
            | Type::FPPC128 { .. }
            | Type::Bool { .. }
            | Type::Struct { .. }
            | Type::Array { .. }
            | Type::FixedArray { .. }
            | Type::Fn { .. },
            Type::Ptr { .. },
        ) if is_allocated => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
            Type::Ptr { .. },
        ) => Ok(()),

        (Type::Const(from_type, ..), cast_type) => {
            self::check_type_cast(from_type, cast_type, metadata, span, control_context)
        }

        (from_type, Type::Const(cast_type, ..)) => {
            self::check_type_cast(cast_type, from_type, metadata, span, control_context)
        }

        (
            Type::Array {
                base_type: from_type,
                ..
            },
            Type::Array {
                base_type: target_type,
                ..
            },
        ) if from_type == target_type => Ok(()),

        (
            Type::FixedArray {
                base_type: provided_type,
                ..
            },
            Type::Array {
                base_type: target_type,
                ..
            },
        ) if provided_type == target_type && is_allocated => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0032,
            format!(
                "Cannot cast type '{}' to '{}'. Types are incompatible for cast.",
                from_type, cast_type
            ),
            "You should try other approach.".into(),
            None,
            *span,
        )),
    }
}
