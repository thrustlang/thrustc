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

use thrustc_code_location::Span;
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use thrustc_ast::Ast;

#[derive(Debug, Clone)]
pub enum BuiltinValue {
    Integer(u64),
    Float(f64),
    Bool(bool),
    Char(u8),
    CString(std::vec::Vec<u8>),
    CNString(std::vec::Vec<u8>),
    NullPtr,
    Void,
}

#[derive(Debug, Clone)]
pub enum BuiltinArgument {
    Value { value: BuiltinValue, span: Span },
    Type { ty: Type, span: Span },
}

impl BuiltinValue {
    pub fn to_ast<'ast>(&self, kind: Type, span: Span) -> Ast<'ast> {
        match self {
            BuiltinValue::Integer(value) => Ast::new_integer(kind, *value, span),
            BuiltinValue::Float(value) => Ast::new_float(kind, *value, span),
            BuiltinValue::Bool(value) => Ast::new_boolean(kind, (*value) as u64, span),
            BuiltinValue::Char(value) => Ast::new_char(kind, (*value) as u64, span),
            BuiltinValue::CString(bytes) => Ast::new_cstring(bytes.clone(), kind, span),
            BuiltinValue::CNString(bytes) => Ast::new_cnstring(bytes.clone(), kind, span),
            BuiltinValue::NullPtr => Ast::new_nullptr(span),
            BuiltinValue::Void => Ast::new_nullptr(span),
        }
    }
}

pub fn fold(ast: &Ast) -> Option<BuiltinValue> {
    self::fold_resolving(ast, &mut |_, _| None)
}

pub fn fold_resolving(
    ast: &Ast,
    resolve: &mut dyn FnMut(&str, Span) -> Option<BuiltinValue>,
) -> Option<BuiltinValue> {
    match ast {
        Ast::Integer { value, .. } => Some(BuiltinValue::Integer(*value)),
        Ast::Float { value, .. } => Some(BuiltinValue::Float(*value)),
        Ast::Boolean { value, .. } => Some(BuiltinValue::Bool(*value != 0)),
        Ast::Char { byte, .. } => Some(BuiltinValue::Char(*byte as u8)),
        Ast::CString { bytes, .. } => Some(BuiltinValue::CString(bytes.clone())),
        Ast::CNString { bytes, .. } => Some(BuiltinValue::CNString(bytes.clone())),
        Ast::NullPtr { .. } => Some(BuiltinValue::NullPtr),
        Ast::Group { node, .. } => self::fold_resolving(node, resolve),
        Ast::BinaryOp {
            left,
            operator,
            right,
            ..
        } => {
            let left: BuiltinValue = self::fold_resolving(left, resolve)?;
            let right: BuiltinValue = self::fold_resolving(right, resolve)?;

            fold_binary(*operator, left, right)
        }
        Ast::UnaryOp {
            operator, node, before, ..
        } => {
            let value: BuiltinValue = self::fold_resolving(node, resolve)?;

            fold_unary(*operator, value, *before)
        }
        Ast::Reference { name, span, .. } => resolve(name, *span),
        _ => None,
    }
}

fn fold_binary(operator: TokenType, left: BuiltinValue, right: BuiltinValue) -> Option<BuiltinValue> {
    match (left, right) {
        (BuiltinValue::Integer(left), BuiltinValue::Integer(right)) => match operator {
            TokenType::Plus => Some(BuiltinValue::Integer(left.saturating_add(right))),
            TokenType::Minus => Some(BuiltinValue::Integer(left.saturating_sub(right))),
            TokenType::Star => Some(BuiltinValue::Integer(left.saturating_mul(right))),
            TokenType::Slash => Some(BuiltinValue::Integer(left.checked_div(right)?)),
            TokenType::Arith => Some(BuiltinValue::Integer(left.checked_rem(right)?)),
            TokenType::LShift => Some(BuiltinValue::Integer(left.wrapping_shl(right as u32))),
            TokenType::RShift => Some(BuiltinValue::Integer(left.wrapping_shr(right as u32))),
            TokenType::BAnd => Some(BuiltinValue::Integer(left & right)),
            TokenType::Bor => Some(BuiltinValue::Integer(left | right)),
            TokenType::Xor => Some(BuiltinValue::Integer(left ^ right)),
            TokenType::EqEq => Some(BuiltinValue::Bool(left == right)),
            TokenType::BangEq => Some(BuiltinValue::Bool(left != right)),
            TokenType::Less => Some(BuiltinValue::Bool(left < right)),
            TokenType::LessEq => Some(BuiltinValue::Bool(left <= right)),
            TokenType::Greater => Some(BuiltinValue::Bool(left > right)),
            TokenType::GreaterEq => Some(BuiltinValue::Bool(left >= right)),
            TokenType::And => Some(BuiltinValue::Bool(left != 0 && right != 0)),
            TokenType::Or => Some(BuiltinValue::Bool(left != 0 || right != 0)),
            _ => None,
        },
        (BuiltinValue::Bool(left), BuiltinValue::Bool(right)) => match operator {
            TokenType::EqEq => Some(BuiltinValue::Bool(left == right)),
            TokenType::BangEq => Some(BuiltinValue::Bool(left != right)),
            TokenType::And => Some(BuiltinValue::Bool(left && right)),
            TokenType::Or => Some(BuiltinValue::Bool(left || right)),
            _ => None,
        },
        (BuiltinValue::Float(left), BuiltinValue::Float(right)) => match operator {
            TokenType::Plus => Some(BuiltinValue::Float(left + right)),
            TokenType::Minus => Some(BuiltinValue::Float(left - right)),
            TokenType::Star => Some(BuiltinValue::Float(left * right)),
            TokenType::Slash => Some(BuiltinValue::Float(left / right)),
            TokenType::EqEq => Some(BuiltinValue::Bool(left == right)),
            TokenType::BangEq => Some(BuiltinValue::Bool(left != right)),
            TokenType::Less => Some(BuiltinValue::Bool(left < right)),
            TokenType::LessEq => Some(BuiltinValue::Bool(left <= right)),
            TokenType::Greater => Some(BuiltinValue::Bool(left > right)),
            TokenType::GreaterEq => Some(BuiltinValue::Bool(left >= right)),
            _ => None,
        },
        _ => None,
    }
}

fn fold_unary(operator: TokenType, value: BuiltinValue, before: bool) -> Option<BuiltinValue> {
    if !before {
        return None;
    }

    match (operator, value) {
        (TokenType::Bang, BuiltinValue::Bool(value)) => Some(BuiltinValue::Bool(!value)),
        (TokenType::Not, BuiltinValue::Integer(value)) => Some(BuiltinValue::Integer(!value)),
        (TokenType::Minus, BuiltinValue::Integer(value)) => Some(BuiltinValue::Integer(value.wrapping_neg())),
        (TokenType::Minus, BuiltinValue::Float(value)) => Some(BuiltinValue::Float(-value)),
        (TokenType::Plus, BuiltinValue::Integer(value)) => Some(BuiltinValue::Integer(value)),
        (TokenType::Plus, BuiltinValue::Float(value)) => Some(BuiltinValue::Float(value)),
        _ => None,
    }
}