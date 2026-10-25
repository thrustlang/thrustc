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

/*
 * This enum is named after the storage class because it controls how a macro
 * local is lowered, but it intentionally groups three C concepts: storage
 * classes (Static, Extern), the const qualifier (Const), and an ordinary
 * Thrust local (Var). It is therefore a lowering category, not a strict C
 * storage-class classification.
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroStorageClass {
    Var,
    Const,
    Static,
    Extern,
}

/*
 * A C initializer designator is the optional prefix inside a braced
 * initializer that says which member or element receives the value. Without a
 * designator, values are assigned by position, for example `{1, 2}`. With a
 * designator, the target is explicit: `.x = 1` assigns the struct member `x`,
 * and `[2] = 3` assigns array element 2. This enum represents those two forms
 * so macro lowering can build Thrust struct fields or indexed fixed-array
 * values from braced initializers.
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroDesignator {
    Field(String),
    Index(MacroExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroInitItem {
    designator: Option<MacroDesignator>,
    value: MacroExpr,
}

impl MacroInitItem {
    #[inline]
    pub fn new(designator: Option<MacroDesignator>, value: MacroExpr) -> Self {
        Self { designator, value }
    }
}

impl MacroInitItem {
    #[inline]
    pub fn get_designator(&self) -> Option<&MacroDesignator> {
        self.designator.as_ref()
    }

    #[inline]
    pub fn get_value(&self) -> &MacroExpr {
        &self.value
    }
}

impl MacroInitItem {
    #[inline]
    pub fn get_mut_designator(&mut self) -> &mut Option<MacroDesignator> {
        &mut self.designator
    }

    #[inline]
    pub fn get_mut_value(&mut self) -> &mut MacroExpr {
        &mut self.value
    }
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
    SizeOfExpr(Box<MacroExpr>),
    InitList(Vec<MacroInitItem>),
    CompoundLiteral {
        ty: String,
        init: Box<MacroExpr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForDecl {
    ty: String,
    name: String,
    init: Option<MacroExpr>,
}

impl ForDecl {
    #[inline]
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
pub struct MacroSwitchArms {
    labels: Vec<Option<MacroExpr>>,
    body: Vec<MacroStmt>,
}

impl MacroSwitchArms {
    #[inline]
    pub fn new(labels: Vec<Option<MacroExpr>>, body: Vec<MacroStmt>) -> Self {
        Self { labels, body }
    }
}

impl MacroSwitchArms {
    #[inline]
    pub fn get_labels(&self) -> &[Option<MacroExpr>] {
        &self.labels
    }

    #[inline]
    pub fn get_body(&self) -> &[MacroStmt] {
        &self.body
    }
}

impl MacroSwitchArms {
    #[inline]
    pub fn get_mut_labels(&mut self) -> &mut Vec<Option<MacroExpr>> {
        &mut self.labels
    }

    #[inline]
    pub fn get_mut_body(&mut self) -> &mut Vec<MacroStmt> {
        &mut self.body
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroStmt {
    Empty,
    VarDecl {
        ty: String,
        name: String,
        init: Option<MacroExpr>,
        storage: MacroStorageClass,
        is_const: bool,
        is_volatile: bool,
    },
    Expr(MacroExpr),
    Break,
    Continue,
    Return(Option<MacroExpr>),
    Goto(String),
    Label(String),
    If {
        cond: MacroExpr,
        then_branch: Vec<MacroStmt>,
        else_branch: Vec<MacroStmt>,
    },
    Switch {
        cond: MacroExpr,
        arms: Vec<MacroSwitchArms>,
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
pub struct MacroNodeMetadata {
    kind: Type,
    span: Span,
}

impl MacroNodeMetadata {
    #[inline]
    pub fn new(kind: Type, span: Span) -> Self {
        Self { kind, span }
    }
}

impl MacroNodeMetadata {
    #[inline]
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

impl MacroNodeMetadata {
    #[inline]
    pub fn get_kind(&self) -> &Type {
        &self.kind
    }

    #[inline]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl MacroNodeMetadata {
    #[inline]
    pub fn get_mut_kind(&mut self) -> &mut Type {
        &mut self.kind
    }
}

impl MacroNodeMetadata {
    #[inline]
    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

#[derive(Debug, Clone)]
pub struct MacroExprNode {
    expr: MacroExpr,
    meta: MacroNodeMetadata,
}

impl MacroExprNode {
    #[inline]
    pub fn new(expr: MacroExpr, meta: MacroNodeMetadata) -> Self {
        Self { expr, meta }
    }
}

impl MacroExprNode {
    #[inline]
    pub fn get_expr(&self) -> &MacroExpr {
        &self.expr
    }

    #[inline]
    pub fn get_meta(&self) -> &MacroNodeMetadata {
        &self.meta
    }
}

impl MacroExprNode {
    #[inline]
    pub fn get_mut_expr(&mut self) -> &mut MacroExpr {
        &mut self.expr
    }

    #[inline]
    pub fn get_mut_meta(&mut self) -> &mut MacroNodeMetadata {
        &mut self.meta
    }
}

#[derive(Debug, Clone)]
pub struct MacroStmtNode {
    stmt: MacroStmt,
    meta: MacroNodeMetadata,
}

impl MacroStmtNode {
    #[inline]
    pub fn new(stmt: MacroStmt, meta: MacroNodeMetadata) -> Self {
        Self { stmt, meta }
    }
}

impl MacroStmtNode {
    #[inline]
    pub fn get_stmt(&self) -> &MacroStmt {
        &self.stmt
    }

    #[inline]
    pub fn get_meta(&self) -> &MacroNodeMetadata {
        &self.meta
    }
}

impl MacroStmtNode {
    #[inline]
    pub fn get_mut_stmt(&mut self) -> &mut MacroStmt {
        &mut self.stmt
    }

    #[inline]
    pub fn get_mut_meta(&mut self) -> &mut MacroNodeMetadata {
        &mut self.meta
    }
}

#[derive(Debug, Clone)]
pub enum MacroAst {
    Expr(MacroExprNode),
    Stmt(MacroStmtNode),
}
