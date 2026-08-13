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

use inkwell::FloatPredicate;
use inkwell::IntPredicate;

use thrustc_code_location::Span;
use thrustc_token_type::TokenType;

use crate::abort;
use crate::context::LLVMCodeGenContext;

#[must_use]
#[inline]
pub fn get_integer_predicate(
    context: &mut LLVMCodeGenContext<'_, '_>,
    operator: &TokenType,
    lhs_signed: bool,
    rhs_signed: bool,
    span: Span,
) -> IntPredicate {
    match operator {
        TokenType::EqEq => IntPredicate::EQ,
        TokenType::BangEq => IntPredicate::NE,

        TokenType::Greater if !lhs_signed && !rhs_signed => IntPredicate::UGT,
        TokenType::Greater if lhs_signed && !rhs_signed => IntPredicate::SGT,
        TokenType::Greater if !lhs_signed && rhs_signed => IntPredicate::SGT,
        TokenType::Greater if lhs_signed && rhs_signed => IntPredicate::SGT,
        TokenType::GreaterEq if !lhs_signed && !rhs_signed => IntPredicate::UGE,
        TokenType::GreaterEq if lhs_signed && !rhs_signed => IntPredicate::SGE,
        TokenType::GreaterEq if !lhs_signed && rhs_signed => IntPredicate::SGE,
        TokenType::GreaterEq if lhs_signed && rhs_signed => IntPredicate::SGE,
        TokenType::Less if !lhs_signed && !rhs_signed => IntPredicate::ULT,
        TokenType::Less if lhs_signed && !rhs_signed => IntPredicate::SLT,
        TokenType::Less if !lhs_signed && rhs_signed => IntPredicate::SLT,
        TokenType::Less if lhs_signed && rhs_signed => IntPredicate::SLT,
        TokenType::LessEq if !lhs_signed && !rhs_signed => IntPredicate::ULE,
        TokenType::LessEq if lhs_signed && !rhs_signed => IntPredicate::SLE,
        TokenType::LessEq if !lhs_signed && rhs_signed => IntPredicate::SLE,
        TokenType::LessEq if lhs_signed && rhs_signed => IntPredicate::SLE,

        _ => abort::abort_codegen(
            context,
            "Failed to determinate integer predicate!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}

#[must_use]
#[inline]
pub fn get_pointer_predicate(
    context: &mut LLVMCodeGenContext<'_, '_>,
    operator: &TokenType,
    span: Span,
) -> IntPredicate {
    match operator {
        TokenType::EqEq => IntPredicate::EQ,
        TokenType::BangEq => IntPredicate::NE,

        _ => abort::abort_codegen(
            context,
            "Failed to determinate pointer predicate!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}

#[must_use]
#[inline]
pub fn get_float_predicate(
    context: &mut LLVMCodeGenContext<'_, '_>,
    operator: &TokenType,
    span: Span,
) -> FloatPredicate {
    match operator {
        TokenType::EqEq => FloatPredicate::OEQ,
        TokenType::BangEq => FloatPredicate::ONE,
        TokenType::Greater => FloatPredicate::OGT,
        TokenType::GreaterEq => FloatPredicate::OGE,
        TokenType::Less => FloatPredicate::OLT,
        TokenType::LessEq => FloatPredicate::OLE,

        _ => abort::abort_codegen(
            context,
            "Failed to determinate floating-point predicate!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        ),
    }
}
