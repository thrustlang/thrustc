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
use thrustc_typesystem::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroUnOp {
    Ref,
    Deref,
    Not,
    Invert,
    Negate,
    Positive,
    PreIncrement,
    PreDecrement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroPostOp {
    Increment,
    Decrement,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroExpr {
    Ident(String),
    Literal(String),
    Null,
    Paren(Box<MacroExpr>),
    Unary {
        op: MacroUnOp,
        arg: Box<MacroExpr>,
    },
    Postfix {
        op: MacroPostOp,
        arg: Box<MacroExpr>,
    },
    Binary {
        op: String,
        left: Box<MacroExpr>,
        right: Box<MacroExpr>,
    },
    Comma(Vec<MacroExpr>),
    Ternary {
        cond: Box<MacroExpr>,
        then_branch: Box<MacroExpr>,
        else_branch: Box<MacroExpr>,
    },
    Call {
        callee: Box<MacroExpr>,
        args: Vec<MacroExpr>,
    },
    Index {
        base: Box<MacroExpr>,
        index: Box<MacroExpr>,
    },
    Member {
        base: Box<MacroExpr>,
        field: String,
    },
    Cast {
        target: String,
        arg: Box<MacroExpr>,
    },
    Assign {
        op: String,
        target: Box<MacroExpr>,
        value: Box<MacroExpr>,
    },
    SizeOf(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForDecl {
    ty: String,
    name: String,
    init: Option<MacroExpr>,
}

impl ForDecl {
    pub fn new(ty: String, name: String, init: Option<MacroExpr>) -> Self {
        Self { ty, name, init }
    }
}

impl ForDecl {
    #[inline]
    pub fn get_ty(&self) -> &str {
        &self.ty
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_init(&self) -> Option<&MacroExpr> {
        self.init.as_ref()
    }
}

impl ForDecl {
    #[inline]
    pub fn get_mut_ty(&mut self) -> &mut String {
        &mut self.ty
    }

    #[inline]
    pub fn get_mut_name(&mut self) -> &mut String {
        &mut self.name
    }

    #[inline]
    pub fn get_mut_init(&mut self) -> &mut Option<MacroExpr> {
        &mut self.init
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForInit {
    Decl {
        ty: String,
        name: String,
        init: Option<MacroExpr>,
    },
    Decls(Vec<ForDecl>),
    Expr(MacroExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroStmt {
    VarDecl {
        ty: String,
        name: String,
        init: Option<MacroExpr>,
    },
    Expr(MacroExpr),
    If {
        cond: MacroExpr,
        then_branch: Vec<MacroStmt>,
        else_branch: Vec<MacroStmt>,
    },
    While {
        cond: MacroExpr,
        body: Vec<MacroStmt>,
    },
    DoWhile {
        body: Vec<MacroStmt>,
        cond: MacroExpr,
    },
    For {
        init: Option<ForInit>,
        cond: Option<MacroExpr>,
        inc: Option<MacroExpr>,
        body: Vec<MacroStmt>,
    },
    Compound(Vec<MacroStmt>),
}

#[derive(Debug, Clone)]
pub struct MacroNodeMeta {
    kind: Type,
    span: Span,
}

impl MacroNodeMeta {
    pub fn new(kind: Type, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn unresolved(hint: &str, span: Span) -> Self {
        Self {
            kind: Type::Unresolved {
                hint: hint.to_string(),
                span,
            },
            span,
        }
    }
}

impl MacroNodeMeta {
    #[inline]
    pub fn get_kind(&self) -> &Type {
        &self.kind
    }

    #[inline]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl MacroNodeMeta {
    #[inline]
    pub fn get_mut_kind(&mut self) -> &mut Type {
        &mut self.kind
    }

    #[inline]
    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

#[derive(Debug, Clone)]
pub struct MacroExprNode {
    expr: MacroExpr,
    meta: MacroNodeMeta,
}

impl MacroExprNode {
    pub fn new(expr: MacroExpr, meta: MacroNodeMeta) -> Self {
        Self { expr, meta }
    }
}

impl MacroExprNode {
    #[inline]
    pub fn get_expr(&self) -> &MacroExpr {
        &self.expr
    }

    #[inline]
    pub fn get_meta(&self) -> &MacroNodeMeta {
        &self.meta
    }
}

impl MacroExprNode {
    #[inline]
    pub fn get_mut_expr(&mut self) -> &mut MacroExpr {
        &mut self.expr
    }

    #[inline]
    pub fn get_mut_meta(&mut self) -> &mut MacroNodeMeta {
        &mut self.meta
    }
}

#[derive(Debug, Clone)]
pub struct MacroStmtNode {
    stmt: MacroStmt,
    meta: MacroNodeMeta,
}

impl MacroStmtNode {
    pub fn new(stmt: MacroStmt, meta: MacroNodeMeta) -> Self {
        Self { stmt, meta }
    }
}

impl MacroStmtNode {
    #[inline]
    pub fn get_stmt(&self) -> &MacroStmt {
        &self.stmt
    }

    #[inline]
    pub fn get_meta(&self) -> &MacroNodeMeta {
        &self.meta
    }
}

impl MacroStmtNode {
    #[inline]
    pub fn get_mut_stmt(&mut self) -> &mut MacroStmt {
        &mut self.stmt
    }

    #[inline]
    pub fn get_mut_meta(&mut self) -> &mut MacroNodeMeta {
        &mut self.meta
    }
}

#[derive(Debug, Clone)]
pub enum MacroAst {
    Expr(MacroExprNode),
    Stmt(MacroStmtNode),
}
