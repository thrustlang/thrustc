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

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;

use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::TypeIsExtensions};

#[inline]
pub fn validate_binary_node(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match *operator {
        TokenType::Arith
        | TokenType::Star
        | TokenType::Slash
        | TokenType::Minus
        | TokenType::MinusEq
        | TokenType::Plus
        | TokenType::PlusEq => {
            self::validate_binary_arithmetic_expression(operator, left, right, span)
        }
        TokenType::Xor => self::validate_xor_expression(operator, left, right, span),
        TokenType::Bor => self::validate_bor_expression(operator, left, right, span),
        TokenType::BAnd => self::validate_band_expression(operator, left, right, span),
        TokenType::BangEq | TokenType::EqEq => {
            self::validate_binary_equality_expression(operator, left, right, span)
        }
        TokenType::LessEq | TokenType::Less | TokenType::GreaterEq | TokenType::Greater => {
            self::validate_binary_comparasion_expression(operator, left, right, span)
        }
        TokenType::LShift | TokenType::RShift => {
            self::validate_binary_shift_expression(operator, left, right, span)
        }
        TokenType::And | TokenType::Or => {
            self::validate_binary_gate_expression(operator, left, right, span)
        }

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0031,
            format!(
                "'{}{}{}' isn't a valid arithmetic or logical operation.",
                right, operator, left
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_band_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
        ) => Ok(()),
        (Type::SSize { .. }, Type::SSize { .. }) => Ok(()),
        (Type::USize { .. }, Type::USize { .. }) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid bit operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_bor_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
        ) => Ok(()),
        (Type::SSize { .. }, Type::SSize { .. }) => Ok(()),
        (Type::USize { .. }, Type::USize { .. }) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid bit operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_xor_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. },
        ) => Ok(()),
        (Type::SSize { .. }, Type::SSize { .. }) => Ok(()),
        (Type::USize { .. }, Type::USize { .. }) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid bit operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_binary_gate_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (Type::Bool { .. }, Type::Bool { .. }) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid logical operation.",
                right, operator, left
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_binary_shift_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::SSize { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::SSize { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. },
        ) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid arithmetic operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_binary_comparasion_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
        ) => Ok(()),
        (Type::SSize { .. }, Type::SSize { .. }) => Ok(()),
        (Type::USize { .. }, Type::USize { .. }) => Ok(()),
        (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => Ok(()),
        (Type::FX8680 { .. }, Type::FX8680 { .. }) => Ok(()),
        (
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
        ) => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid relational operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_binary_equality_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. },
        ) => Ok(()),
        (Type::SSize { .. }, Type::SSize { .. }) => Ok(()),
        (Type::USize { .. }, Type::USize { .. }) => Ok(()),
        (
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
        ) => Ok(()),
        (Type::Bool { .. }, Type::Bool { .. }) => Ok(()),
        (Type::Char { .. }, Type::Char { .. }) => Ok(()),
        (Type::FX8680 { .. }, Type::FX8680 { .. }) => Ok(()),
        (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => Ok(()),

        _ if left.is_ptr_type() && right.is_ptr_type() => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid relational operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_binary_arithmetic_expression(
    operator: &TokenType,
    left: &Type,
    right: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match (left, right) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::SSize { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::USize { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::SSize { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::USize { .. },
        ) => Ok(()),
        (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => Ok(()),
        (Type::FX8680 { .. }, Type::FX8680 { .. }) => Ok(()),
        (
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
            Type::F32 { .. } | Type::F64 { .. } | Type::F128 { .. },
        ) => Ok(()),
        (Type::Ptr { .. }, Type::Ptr { .. }) if left == right && *operator == TokenType::Minus => {
            Ok(())
        }

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0030,
            format!(
                "'{} {} {}' isn't a valid arithmetic operation.",
                left, operator, right
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}
