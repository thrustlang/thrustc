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

use arbitrary::Unstructured;
use thrustc_ast::ast_metadata::{
    ConstantMetadata, FunctionParameterMetadata, LocalMetadata, ReferenceMetadata, ReferenceType,
    StaticMetadata,
};
use thrustc_token_type::TokenType;
use thrustc_typesystem::traits::{
    CastTypeExtensions, PrecedenceTypeExtensions, TypePointerExtensions,
};
use thrustc_typesystem::Type;

pub mod literal {
    use super::*;

    pub fn integer(u: &mut Unstructured<'_>) -> arbitrary::Result<(Type, u64)> {
        let value: u64 = u.arbitrary()?;
        let span = u.arbitrary()?;

        let kind: Type = if value <= u8::MAX as u64 {
            Type::U8 { span }
        } else if value <= u16::MAX as u64 {
            Type::U16 { span }
        } else if value <= u32::MAX as u64 {
            Type::U32 { span }
        } else {
            Type::U64 { span }
        };

        Ok((kind, value))
    }

    pub fn floating_point(u: &mut Unstructured<'_>) -> arbitrary::Result<(Type, f64)> {
        let mantissa: i64 = u.int_in_range(-1_000_000_000_000_000..=1_000_000_000_000_000)?;
        let scale: u32 = u.int_in_range(0..=6)?;

        let value: f64 = (mantissa as f64) / 10f64.powi(scale as i32);

        let span = u.arbitrary()?;

        let kind: Type = if (value as f32) as f64 == value {
            Type::F32 { span }
        } else {
            Type::F64 { span }
        };

        Ok((kind, value))
    }

    /// Replicates the `true`/`false` literal values.
    pub fn boolean(u: &mut Unstructured<'_>) -> arbitrary::Result<u64> {
        u.int_in_range(0..=1)
    }
}

/// Replicates the parser's binary operation type decision.
///
/// Mirrors `thrustc_parser::src/expressions/precedences/factor.rs`, `term.rs`,
/// `cmp.rs`, `equality.rs`, `and.rs` and `or.rs`.
pub fn binary_op_kind<'ast>(
    u: &mut Unstructured<'ast>,
    left: &Type,
    right: &Type,
    operator: TokenType,
) -> arbitrary::Result<Type> {
    match operator {
        TokenType::Slash | TokenType::Star | TokenType::SlashEq | TokenType::StarEq => {
            Ok(left.get_factor_precedence_type(right))
        }

        TokenType::Plus
        | TokenType::Minus
        | TokenType::PlusEq
        | TokenType::MinusEq
        | TokenType::Arith
        | TokenType::ArithEq
        | TokenType::LShift
        | TokenType::LShiftEq
        | TokenType::RShift
        | TokenType::RShiftEq
        | TokenType::Xor
        | TokenType::XorEq
        | TokenType::Bor
        | TokenType::BorEq
        | TokenType::BAnd
        | TokenType::BAndEq => Ok(left.get_term_precedence_type(right, operator)),

        TokenType::Greater
        | TokenType::GreaterEq
        | TokenType::Less
        | TokenType::LessEq
        | TokenType::BangEq
        | TokenType::EqEq
        | TokenType::And
        | TokenType::Or => Ok(Type::Bool {
            span: u.arbitrary()?,
        }),

        _ => Ok(left.clone()),
    }
}

/// Replicates the parser's unary operation type decision.
///
/// Mirrors `thrustc_parser::src/expressions/precedences/unary.rs`.
pub fn unary_op_kind<'ast>(
    u: &mut Unstructured<'ast>,
    operator: TokenType,
    operand: &Type,
) -> arbitrary::Result<Type> {
    match operator {
        TokenType::Bang => Ok(Type::Bool {
            span: u.arbitrary()?,
        }),

        TokenType::Minus => Ok(operand.narrowing_cast()),

        _ => Ok(operand.clone()),
    }
}

/// Replicates the function parameter metadata decision
/// (`thrustc_parser::src/toplevel/global_function.rs`).
#[inline]
pub fn function_parameter_metadata(kind: &Type) -> FunctionParameterMetadata {
    FunctionParameterMetadata::new(kind.is_ptr_like_type())
}

/// Replicates the local variable metadata decision
/// (`thrustc_parser::src/statements/variable.rs`).
#[inline]
pub fn local_metadata(is_unitialized: bool) -> LocalMetadata {
    LocalMetadata::new(is_unitialized, true, false, None)
}

/// Replicates the local static metadata decision
/// (`thrustc_parser::src/statements/local_static.rs`). The fuzzer never emits
/// the `mut` modifier, so the parser would decide `is_mutable = false`.
#[inline]
pub fn static_metadata(is_unitialized: bool) -> StaticMetadata {
    StaticMetadata::new(true, false, is_unitialized, false, false, false, None, None)
}

/// Replicates the local constant metadata decision
/// (`thrustc_parser::src/statements/local_constant.rs`).
#[inline]
pub fn constant_metadata() -> ConstantMetadata {
    ConstantMetadata::new(false, false, false, None)
}

/// Replicates the reference metadata decision
/// (`thrustc_parser::src/expressions/reference.rs`), derived from the declared
/// symbol state tracked by the fuzzer.
#[inline]
pub fn reference_metadata(
    kind: &Type,
    reference_type: ReferenceType,
    is_unitialized: bool,
) -> ReferenceMetadata {
    match reference_type {
        ReferenceType::Parameter => ReferenceMetadata::new(
            kind.is_ptr_like_type(),
            kind.is_ptr_like_type(),
            ReferenceType::Parameter,
            false,
        ),

        ReferenceType::Static => {
            ReferenceMetadata::new(true, false, ReferenceType::Static, is_unitialized)
        }

        ReferenceType::Constant => {
            ReferenceMetadata::new(true, false, ReferenceType::Constant, false)
        }

        _ => ReferenceMetadata::new(true, true, ReferenceType::Local, is_unitialized),
    }
}
