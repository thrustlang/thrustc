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
pub fn validate_unary_node(
    operator: &TokenType,
    ty: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    match operator {
        TokenType::Minus | TokenType::PlusPlus | TokenType::MinusMinus => {
            self::validate_general_unary(operator, ty, span)
        }

        TokenType::Not => self::validate_not_unary(operator, ty, span),
        TokenType::Bang => self::validate_bang_unary(operator, ty, span),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0031,
            format!(
                "'{}{}' isn't a valid arithmetic or logical operation.",
                operator, ty
            ),
            "It doesn't follow any rule, you should remove or change it.".into(),
            None,
            span,
        )),
    }
}

#[inline]
fn validate_not_unary(operator: &TokenType, ty: &Type, span: Span) -> Result<(), CompilationIssue> {
    if ty.is_integer_type() || ty.is_ptr_type() {
        return Ok(());
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0030,
        format!("'{}{}' isn't a logical valid operation.", operator, ty),
        "It doesn't follow any rule, you should remove or change it.".into(),
        None,
        span,
    ))
}

#[inline]
fn validate_general_unary(
    operator: &TokenType,
    ty: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    if ty.is_integer_type() || ty.is_float_type() {
        return Ok(());
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0030,
        format!("'{}{}' isn't a valid arithmetic operation.", operator, ty),
        "It doesn't follow any rule, you should remove or change it.".into(),
        None,
        span,
    ))
}

#[inline]
fn validate_bang_unary(
    operator: &TokenType,
    ty: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    if ty.is_bool_type() || ty.is_ptr_type() {
        return Ok(());
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0030,
        format!("'{}{}' isn't a valid logical operation.", operator, ty),
        "It doesn't follow any rule, you should remove or change it.".into(),
        None,
        span,
    ))
}
