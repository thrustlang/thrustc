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

use crate::macro_lex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MacroExpr {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MacroUnOp {
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
pub(crate) enum MacroPostOp {
    Increment,
    Decrement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MacroLimit {
    TokenPasting,
    VariadicArguments,
    Stringizing,
    CallBodied,
    NotComputable,
}

impl MacroExpr {
    pub(crate) fn is_atomic(&self) -> bool {
        matches!(
            self,
            MacroExpr::Ident(_)
                | MacroExpr::Literal(_)
                | MacroExpr::Null
                | MacroExpr::Paren(_)
                | MacroExpr::Call { .. }
                | MacroExpr::Index { .. }
                | MacroExpr::Member { .. }
                | MacroExpr::Postfix { .. }
        )
    }
}

pub(crate) struct MacroCursor<'tokens> {
    tokens: &'tokens [String],
    position: usize,
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn new(tokens: &'tokens [String]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse(body: &[String]) -> Result<MacroExpr, MacroLimit> {
        let mut cursor: MacroCursor<'_> = MacroCursor::new(body);

        let parsed: MacroExpr = cursor.parse_assign()?;

        if !cursor.at_end() {
            return Err(MacroLimit::NotComputable);
        }

        Ok(parsed)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn peek(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|token| token.as_str())
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn eat(&mut self, expected: &str) -> bool {
        if self.peek() == Some(expected) {
            self.position += 1;

            return true;
        }

        false
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn expect(&mut self, expected: &str) -> Result<(), MacroLimit> {
        if self.eat(expected) {
            return Ok(());
        }

        Err(MacroLimit::NotComputable)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_assign(&mut self) -> Result<MacroExpr, MacroLimit> {
        let target: MacroExpr = self.parse_ternary()?;

        if self.peek().is_some_and(|token| {
            matches!(
                token,
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
            )
        }) {
            let op: String = self.tokens[self.position].to_string();

            self.position += 1;

            let value: MacroExpr = self.parse_assign()?;

            return Ok(MacroExpr::Assign {
                op,
                target: Box::new(target),
                value: Box::new(value),
            });
        }

        Ok(target)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_ternary(&mut self) -> Result<MacroExpr, MacroLimit> {
        let cond: MacroExpr = self.parse_lor()?;

        if !self.eat("?") {
            return Ok(cond);
        }

        let then_branch: MacroExpr = self.parse_ternary()?;

        self.expect(":")?;

        let else_branch: MacroExpr = self.parse_ternary()?;

        Ok(MacroExpr::Ternary {
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_lor(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_land()?;

        while self.eat("||") {
            let right: MacroExpr = self.parse_land()?;

            left = MacroExpr::Binary {
                op: "||".to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_land(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_bor()?;

        while self.eat("&&") {
            let right: MacroExpr = self.parse_bor()?;

            left = MacroExpr::Binary {
                op: "&&".to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_bor(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_bxor()?;

        while self.eat("|") {
            let right: MacroExpr = self.parse_bxor()?;

            left = MacroExpr::Binary {
                op: "|".to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_bxor(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_band()?;

        while self.eat("^") {
            let right: MacroExpr = self.parse_band()?;

            left = MacroExpr::Binary {
                op: "^".to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_band(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_equality()?;

        while self.eat("&") {
            let right: MacroExpr = self.parse_equality()?;

            left = MacroExpr::Binary {
                op: "&".to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_equality(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_relational()?;

        loop {
            if self.eat("==") {
                let right: MacroExpr = self.parse_relational()?;

                left = MacroExpr::Binary {
                    op: "==".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat("!=") {
                let right: MacroExpr = self.parse_relational()?;

                left = MacroExpr::Binary {
                    op: "!=".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_relational(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_shift()?;

        loop {
            if self.eat("<=") {
                let right: MacroExpr = self.parse_shift()?;

                left = MacroExpr::Binary {
                    op: "<=".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat(">=") {
                let right: MacroExpr = self.parse_shift()?;

                left = MacroExpr::Binary {
                    op: ">=".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat("<") {
                let right: MacroExpr = self.parse_shift()?;

                left = MacroExpr::Binary {
                    op: "<".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat(">") {
                let right: MacroExpr = self.parse_shift()?;

                left = MacroExpr::Binary {
                    op: ">".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_shift(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_additive()?;

        loop {
            if self.eat("<<") {
                let right: MacroExpr = self.parse_additive()?;

                left = MacroExpr::Binary {
                    op: "<<".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat(">>") {
                let right: MacroExpr = self.parse_additive()?;

                left = MacroExpr::Binary {
                    op: ">>".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_additive(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_multiplicative()?;

        loop {
            if self.eat("+") {
                let right: MacroExpr = self.parse_multiplicative()?;

                left = MacroExpr::Binary {
                    op: "+".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat("-") {
                let right: MacroExpr = self.parse_multiplicative()?;

                left = MacroExpr::Binary {
                    op: "-".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_multiplicative(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut left: MacroExpr = self.parse_cast()?;

        loop {
            if self.eat("*") {
                let right: MacroExpr = self.parse_cast()?;

                left = MacroExpr::Binary {
                    op: "*".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat("/") {
                let right: MacroExpr = self.parse_cast()?;

                left = MacroExpr::Binary {
                    op: "/".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if self.eat("%") {
                let right: MacroExpr = self.parse_cast()?;

                left = MacroExpr::Binary {
                    op: "%".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_cast(&mut self) -> Result<MacroExpr, MacroLimit> {
        if self.peek() == Some("(") {
            let saved: usize = self.position;

            self.position += 1;

            if let Some(target) = self.parse_cast_target() {
                if self.eat(")") {
                    if target == "void" {
                        return self.parse_unary();
                    }

                    let arg: MacroExpr = self.parse_cast()?;

                    return Ok(MacroExpr::Cast {
                        target,
                        arg: Box::new(arg),
                    });
                }
            }

            self.position = saved;
        }

        self.parse_unary()
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_cast_target(&mut self) -> Option<String> {
        let mut words: Vec<String> = Vec::new();
        let mut stars: usize = 0;

        loop {
            match self.peek() {
                Some("*") => {
                    stars += 1;
                    self.position += 1;
                }
                Some("int") | Some("unsigned") | Some("long") | Some("short") | Some("char")
                | Some("float") | Some("double") | Some("signed") | Some("void")
                | Some("const") | Some("volatile") | Some("_Bool") => {
                    let word: String = self.tokens[self.position].to_string();

                    if word != "const" && word != "volatile" {
                        words.push(word);
                    }

                    self.position += 1;
                }
                _ => break,
            }
        }

        if words.is_empty() {
            return None;
        }

        let key: String = words.join(" ");
        let base: String = macro_lex::macro_type_name(&key)?;

        if base == "void" {
            if stars > 0 {
                return None;
            }

            return Some("void".to_string());
        }

        let mut out: String = base;

        for _ in 0..stars {
            out = format!("ptr[{out}]");
        }

        Some(out)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_unary(&mut self) -> Result<MacroExpr, MacroLimit> {
        if self.eat("++") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::PreIncrement,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("--") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::PreDecrement,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("!") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Not,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("~") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Invert,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("-") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Negate,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("+") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Positive,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("*") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Deref,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("&") {
            return Ok(MacroExpr::Unary {
                op: MacroUnOp::Ref,
                arg: Box::new(self.parse_cast()?),
            });
        }

        if self.eat("sizeof") {
            if self.eat("(") {
                let saved: usize = self.position;

                if let Some(target) = self.parse_cast_target() {
                    if self.eat(")") {
                        return Ok(MacroExpr::SizeOf(target));
                    }

                    return Err(MacroLimit::NotComputable);
                }

                self.position = saved;

                if let Some(target) = self.parse_sizeof_user_target() {
                    return Ok(MacroExpr::SizeOf(target));
                }
            }

            return Err(MacroLimit::NotComputable);
        }

        self.parse_postfix()
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_sizeof_user_target(&mut self) -> Option<String> {
        if matches!(self.peek(), Some("struct") | Some("enum") | Some("union")) {
            self.position += 1;
        }

        let name: String = match self.peek() {
            Some(word)
                if word
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_') =>
            {
                let name: String = word.to_string();

                self.position += 1;

                name
            }
            _ => return None,
        };

        if matches!(
            name.as_str(),
            "int"
                | "unsigned"
                | "signed"
                | "long"
                | "short"
                | "char"
                | "float"
                | "double"
                | "void"
                | "const"
                | "volatile"
                | "_Bool"
                | "struct"
                | "enum"
                | "union"
        ) {
            return None;
        }

        let mut out: String = name;

        while self.eat("*") {
            out = format!("ptr[{out}]");
        }

        if !self.eat(")") {
            return None;
        }

        Some(out)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_postfix(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut node: MacroExpr = self.parse_primary()?;

        loop {
            if self.eat("(") {
                let mut args: Vec<MacroExpr> = Vec::new();

                if !self.eat(")") {
                    loop {
                        args.push(self.parse_assign()?);

                        if self.eat(",") {
                            continue;
                        }

                        self.expect(")")?;
                        break;
                    }
                }

                node = MacroExpr::Call {
                    callee: Box::new(node),
                    args,
                };
            } else if self.eat("[") {
                let index: MacroExpr = self.parse_assign()?;

                self.expect("]")?;

                node = MacroExpr::Index {
                    base: Box::new(node),
                    index: Box::new(index),
                };
            } else if self.eat(".") || self.eat("->") {
                let field: String = match self.peek() {
                    Some(name)
                        if name
                            .chars()
                            .next()
                            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_') =>
                    {
                        let field: String = name.to_string();

                        self.position += 1;

                        field
                    }
                    _ => return Err(MacroLimit::NotComputable),
                };

                node = MacroExpr::Member {
                    base: Box::new(node),
                    field,
                };
            } else if self.eat("++") {
                node = MacroExpr::Postfix {
                    op: MacroPostOp::Increment,
                    arg: Box::new(node),
                };
            } else if self.eat("--") {
                node = MacroExpr::Postfix {
                    op: MacroPostOp::Decrement,
                    arg: Box::new(node),
                };
            } else {
                break;
            }
        }

        Ok(node)
    }
}

impl<'tokens> MacroCursor<'tokens> {
    pub(crate) fn parse_primary(&mut self) -> Result<MacroExpr, MacroLimit> {
        if self.eat("(") {
            let inner: MacroExpr = self.parse_assign()?;

            self.expect(")")?;

            return Ok(MacroExpr::Paren(Box::new(inner)));
        }

        if self.eat("NULL") {
            return Ok(MacroExpr::Null);
        }

        let Some(token) = self.peek() else {
            return Err(MacroLimit::NotComputable);
        };

        if token.chars().next().is_some_and(|ch| ch.is_ascii_digit())
            || token.starts_with('"')
            || token.starts_with('\'')
        {
            let literal: String = macro_lex::normalize_literal_token_spelling(token);

            self.position += 1;

            return Ok(MacroExpr::Literal(literal));
        }

        if token
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        {
            let name: String = token.to_string();

            self.position += 1;

            return Ok(MacroExpr::Ident(name));
        }

        Err(MacroLimit::NotComputable)
    }
}

pub(crate) fn lower(expr: &MacroExpr, ctx: crate::location::Location) -> String {
    match expr {
        MacroExpr::Ident(name) => name.to_string(),
        MacroExpr::Literal(text) => text.to_string(),
        MacroExpr::Null => "nullptr".to_string(),

        MacroExpr::Paren(inner) => {
            let inner_text: String = self::lower(inner, ctx);

            format!("({inner_text})")
        }

        MacroExpr::Unary { op, arg } => {
            if matches!(op, MacroUnOp::Ref) {
                let arg_text: String = self::lower(arg, crate::location::Location::AddressOf);

                let grouped: String = if arg.is_atomic() {
                    arg_text
                } else {
                    format!("({arg_text})")
                };

                return format!("ref {grouped}");
            }

            let arg_text: String = self::lower(arg, crate::location::Location::RValue);

            let grouped: String = if arg.is_atomic() {
                arg_text
            } else {
                format!("({arg_text})")
            };

            match op {
                MacroUnOp::Ref => format!("ref {grouped}"),
                MacroUnOp::Deref => format!("deref {grouped}"),
                MacroUnOp::Not => format!("!{grouped}"),
                MacroUnOp::Invert => format!("~{grouped}"),
                MacroUnOp::Negate => format!("-{grouped}"),
                MacroUnOp::Positive => format!("+{grouped}"),
                MacroUnOp::PreIncrement => format!("++{grouped}"),
                MacroUnOp::PreDecrement => format!("--{grouped}"),
            }
        }

        MacroExpr::Postfix { op, arg } => {
            let arg_text: String = self::lower(arg, crate::location::Location::RValue);

            let grouped: String = if arg.is_atomic() {
                arg_text
            } else {
                format!("({arg_text})")
            };

            match op {
                MacroPostOp::Increment => format!("{grouped}++"),
                MacroPostOp::Decrement => format!("{grouped}--"),
            }
        }

        MacroExpr::Binary { op, left, right } => {
            let left_text: String = self::lower(left, crate::location::Location::RValue);
            let right_text: String = self::lower(right, crate::location::Location::RValue);

            format!("{left_text} {op} {right_text}")
        }

        MacroExpr::Assign { op, target, value } => {
            let target_text: String = self::lower(target, crate::location::Location::LValue);
            let value_text: String = self::lower(value, crate::location::Location::RValue);

            format!("{target_text} {op} {value_text}")
        }

        MacroExpr::Ternary {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_text: String = self::lower(cond, crate::location::Location::RValue);
            let then_text: String = self::lower(then_branch, ctx);
            let else_text: String = self::lower(else_branch, ctx);

            format!("if {cond_text} {{ {then_text} }} else {{ {else_text} }}")
        }

        MacroExpr::Call { callee, args } => {
            let callee_text: String = self::lower(callee, crate::location::Location::RValue);

            let grouped_callee: String = if matches!(callee.as_ref(), MacroExpr::Ident(_)) {
                callee_text
            } else {
                format!("({callee_text})")
            };

            let argument_texts: Vec<String> = args
                .iter()
                .map(|arg| self::lower(arg, crate::location::Location::RValue))
                .collect();

            format!("{grouped_callee}({})", argument_texts.join(", "))
        }

        MacroExpr::Index { base, index } => {
            let index_text: String = self::lower(index, crate::location::Location::RValue);

            if ctx.is_address_of() {
                let base_text: String = self::lower(base, ctx);

                return format!("{base_text}[{index_text}]");
            }

            let base_text: String = self::lower_place_base(base);

            format!("{base_text}->[{index_text}]")
        }

        MacroExpr::Member { base, field } => {
            let base_text: String = self::lower(base, crate::location::Location::RValue);

            let grouped_base: String = if matches!(base.as_ref(), MacroExpr::Ident(_)) {
                base_text
            } else {
                format!("({base_text})")
            };

            if ctx.is_address_of() {
                format!("{grouped_base}.{field}")
            } else {
                format!("{grouped_base}->{field}")
            }
        }

        MacroExpr::Cast { target, arg } => {
            let arg_text: String = self::lower(arg, crate::location::Location::RValue);

            format!("({arg_text}) as {target}")
        }

        MacroExpr::SizeOf(target) => {
            format!("abiSizeOf({target})")
        }
    }
}

pub(crate) fn lower_function_body(expr: &MacroExpr, return_type: &str) -> String {
    if let MacroExpr::Ternary {
        cond,
        then_branch,
        else_branch,
    } = expr
    {
        let cond_text: String = self::lower(cond, crate::location::Location::RValue);
        let then_text: String = self::lower(then_branch, crate::location::Location::RValue);
        let else_text: String = self::lower(else_branch, crate::location::Location::RValue);

        return format!(
            "if {cond_text} {{ ({then_text}) as {return_type} }} else {{ ({else_text}) as {return_type} }}"
        );
    }

    let body_text: String = self::lower(expr, crate::location::Location::RValue);

    format!("({body_text}) as {return_type}")
}

pub(crate) fn lower_place_base(expr: &MacroExpr) -> String {
    match expr {
        MacroExpr::Ident(name) => name.to_string(),

        MacroExpr::Paren(inner) => self::lower_place_base(inner),

        MacroExpr::Member { base, field } => {
            let base_text: String = self::lower_place_base(base);

            format!("{base_text}.{field}")
        }

        _ => format!("({})", self::lower(expr, crate::location::Location::RValue)),
    }
}
