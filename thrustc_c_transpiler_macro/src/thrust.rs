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

use crate::macro_ast::{MacroDesignator, MacroExpr, MacroPostOp, MacroUnOp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ThrustType {
    Named(String),
    Pointer(Box<ThrustType>),
    Array(Box<ThrustType>, Option<usize>),
}

impl ThrustType {
    #[inline]
    pub fn from_text(text: &str) -> Self {
        Self::Named(text.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variadic {
    name: String,
    gnu_named: bool,
}

impl Variadic {
    #[inline]
    pub fn new(name: String, gnu_named: bool) -> Self {
        Self { name, gnu_named }
    }
}

impl Variadic {
    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn is_gnu_named(&self) -> bool {
        self.gnu_named
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroAst {
    Ident(String),
    Literal(String),
    Null,
    Paren(Box<MacroAst>),
    Unary {
        op: String,
        arg: Box<MacroAst>,
    },
    Postfix {
        op: String,
        arg: Box<MacroAst>,
    },
    Binary {
        op: String,
        left: Box<MacroAst>,
        right: Box<MacroAst>,
    },
    Comma(Vec<MacroAst>),
    Ternary {
        cond: Box<MacroAst>,
        then_branch: Box<MacroAst>,
        else_branch: Box<MacroAst>,
    },
    Call {
        callee: Box<MacroAst>,
        args: Vec<MacroAst>,
    },
    Index {
        base: Box<MacroAst>,
        index: Box<MacroAst>,
    },
    Member {
        base: Box<MacroAst>,
        field: String,
    },
    Cast {
        target: ThrustType,
        arg: Box<MacroAst>,
    },
    Assign {
        op: String,
        target: Box<MacroAst>,
        value: Box<MacroAst>,
    },
    SizeOf(ThrustType),
    SizeOfExpr(Box<MacroAst>),
    InitList(Vec<MacroAst>),
    InitField {
        field: String,
        value: Box<MacroAst>,
    },
    InitIndex {
        index: Box<MacroAst>,
        value: Box<MacroAst>,
    },
    InitValue(Box<MacroAst>),
    CompoundLiteral {
        ty: ThrustType,
        init: Box<MacroAst>,
    },
}

impl MacroAst {
    pub fn from_expr(expr: &MacroExpr) -> Self {
        match expr {
            MacroExpr::Ident(name) => Self::Ident(name.clone()),
            MacroExpr::Literal(text) => Self::Literal(text.clone()),
            MacroExpr::Null => Self::Null,
            MacroExpr::Paren(inner) => Self::Paren(Box::new(Self::from_expr(inner))),
            MacroExpr::Unary { op, arg } => Self::Unary {
                op: match op {
                    MacroUnOp::Ref => "&".to_string(),
                    MacroUnOp::Deref => "*".to_string(),
                    MacroUnOp::Not => "!".to_string(),
                    MacroUnOp::Invert => "~".to_string(),
                    MacroUnOp::Negate => "-".to_string(),
                    MacroUnOp::Positive => "+".to_string(),
                    MacroUnOp::PreIncrement => "++".to_string(),
                    MacroUnOp::PreDecrement => "--".to_string(),
                },
                arg: Box::new(Self::from_expr(arg)),
            },
            MacroExpr::Postfix { op, arg } => Self::Postfix {
                op: match op {
                    MacroPostOp::Increment => "++".to_string(),
                    MacroPostOp::Decrement => "--".to_string(),
                },
                arg: Box::new(Self::from_expr(arg)),
            },
            MacroExpr::Binary { op, left, right } => Self::Binary {
                op: op.clone(),
                left: Box::new(Self::from_expr(left)),
                right: Box::new(Self::from_expr(right)),
            },
            MacroExpr::Comma(items) => Self::Comma(items.iter().map(Self::from_expr).collect()),
            MacroExpr::Ternary {
                cond,
                then_branch,
                else_branch,
            } => Self::Ternary {
                cond: Box::new(Self::from_expr(cond)),
                then_branch: Box::new(Self::from_expr(then_branch)),
                else_branch: Box::new(Self::from_expr(else_branch)),
            },
            MacroExpr::Call { callee, args } => Self::Call {
                callee: Box::new(Self::from_expr(callee)),
                args: args.iter().map(Self::from_expr).collect(),
            },
            MacroExpr::Index { base, index } => Self::Index {
                base: Box::new(Self::from_expr(base)),
                index: Box::new(Self::from_expr(index)),
            },
            MacroExpr::Member { base, field } => Self::Member {
                base: Box::new(Self::from_expr(base)),
                field: field.clone(),
            },
            MacroExpr::Cast { target, arg } => Self::Cast {
                target: ThrustType::from_text(target),
                arg: Box::new(Self::from_expr(arg)),
            },
            MacroExpr::Assign { op, target, value } => Self::Assign {
                op: op.clone(),
                target: Box::new(Self::from_expr(target)),
                value: Box::new(Self::from_expr(value)),
            },
            MacroExpr::SizeOf(target) => Self::SizeOf(ThrustType::from_text(target)),
            MacroExpr::SizeOfExpr(inner) => Self::SizeOfExpr(Box::new(Self::from_expr(inner))),
            MacroExpr::InitList(items) => Self::InitList(
                items
                    .iter()
                    .map(|item| match item.get_designator() {
                        Some(MacroDesignator::Field(field)) => Self::InitField {
                            field: field.clone(),
                            value: Box::new(Self::from_expr(item.get_value())),
                        },
                        Some(MacroDesignator::Index(index)) => Self::InitIndex {
                            index: Box::new(Self::from_expr(index)),
                            value: Box::new(Self::from_expr(item.get_value())),
                        },
                        None => Self::InitValue(Box::new(Self::from_expr(item.get_value()))),
                    })
                    .collect(),
            ),
            MacroExpr::CompoundLiteral { ty, init } => Self::CompoundLiteral {
                ty: ThrustType::from_text(ty),
                init: Box::new(Self::from_expr(init)),
            },
        }
    }
}
