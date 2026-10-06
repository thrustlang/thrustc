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

use crate::macro_expr::{MacroCursor, MacroExpr, MacroLimit, MacroUnOp};

type SplitDecl<'tokens> = Result<Option<(String, String, &'tokens [String])>, MacroLimit>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MacroStmt {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ForInit {
    Decl {
        ty: String,
        name: String,
        init: Option<MacroExpr>,
    },
    Expr(MacroExpr),
}

pub(crate) struct MacroStmtCursor<'tokens> {
    tokens: &'tokens [String],
    position: usize,
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn new(tokens: &'tokens [String]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn peek(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|token| token.as_str())
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn eat(&mut self, expected: &str) -> bool {
        if self.peek() == Some(expected) {
            self.position += 1;

            return true;
        }

        false
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn expect(&mut self, expected: &str) -> Result<(), MacroLimit> {
        if self.eat(expected) {
            return Ok(());
        }

        Err(MacroLimit::NotComputable)
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_body(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        let mut out: Vec<MacroStmt> = Vec::new();

        while !self.at_end() && self.peek() != Some("}") {
            if self.eat(";") {
                continue;
            }

            out.push(self.parse_statement()?);
        }

        Ok(out)
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_statement(&mut self) -> Result<MacroStmt, MacroLimit> {
        match self.peek() {
            Some("for") => self.parse_for(),
            Some("while") => self.parse_while(),
            Some("do") => self.parse_do(),
            Some("if") => self.parse_if(),
            Some("{") => {
                self.position += 1;

                let inner: Vec<MacroStmt> = self.parse_body()?;

                self.expect("}")?;

                Ok(MacroStmt::Compound(inner))
            }
            Some(
                "return" | "continue" | "break" | "goto" | "switch" | "case" | "default"
                | "typedef",
            ) => Err(MacroLimit::NotComputable),
            Some(_) => self.parse_decl_or_expr(),
            None => Err(MacroLimit::NotComputable),
        }
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_decl_or_expr(&mut self) -> Result<MacroStmt, MacroLimit> {
        let start: usize = self.position;

        let end: usize = self::find_semicolon(self.tokens, start)?;

        let stmt_tokens: &[String] = &self.tokens[start..end];

        self.position = end + 1;

        if let Some(decl) = self::split_var_decl(stmt_tokens)? {
            let init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                Some(MacroCursor::parse(decl.2)?)
            };

            return Ok(MacroStmt::VarDecl {
                ty: decl.0,
                name: decl.1,
                init,
            });
        }

        Ok(MacroStmt::Expr(MacroCursor::parse(stmt_tokens)?))
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_for(&mut self) -> Result<MacroStmt, MacroLimit> {
        self.expect("for")?;
        self.expect("(")?;

        let header_start: usize = self.position;

        let header_end: usize = self::find_matching_paren(self.tokens, header_start)?;

        let header: &[String] = &self.tokens[header_start..header_end];

        self.position = header_end + 1;

        let parts: Vec<&[String]> = self::split_top_level(header, ";")?;

        if parts.len() != 3 {
            return Err(MacroLimit::NotComputable);
        }

        let init: Option<ForInit> = if parts[0].is_empty() {
            None
        } else if let Some(decl) = self::split_var_decl(parts[0])? {
            let decl_init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                Some(MacroCursor::parse(decl.2)?)
            };

            Some(ForInit::Decl {
                ty: decl.0,
                name: decl.1,
                init: decl_init,
            })
        } else {
            Some(ForInit::Expr(MacroCursor::parse(parts[0])?))
        };

        let cond: Option<MacroExpr> = if parts[1].is_empty() {
            None
        } else {
            Some(MacroCursor::parse(parts[1])?)
        };

        let inc: Option<MacroExpr> = if parts[2].is_empty() {
            None
        } else {
            Some(MacroCursor::parse(parts[2])?)
        };

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        Ok(MacroStmt::For {
            init,
            cond,
            inc,
            body,
        })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_while(&mut self) -> Result<MacroStmt, MacroLimit> {
        self.expect("while")?;
        self.expect("(")?;

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let cond: MacroExpr = MacroCursor::parse(&self.tokens[start..end])?;

        self.position = end + 1;

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        Ok(MacroStmt::While { cond, body })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_do(&mut self) -> Result<MacroStmt, MacroLimit> {
        self.expect("do")?;

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        self.expect("while")?;
        self.expect("(")?;

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let cond: MacroExpr = MacroCursor::parse(&self.tokens[start..end])?;

        self.position = end + 1;

        self.eat(";");

        Ok(MacroStmt::DoWhile { body, cond })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_if(&mut self) -> Result<MacroStmt, MacroLimit> {
        self.expect("if")?;
        self.expect("(")?;

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let cond: MacroExpr = MacroCursor::parse(&self.tokens[start..end])?;

        self.position = end + 1;

        let then_branch: Vec<MacroStmt> = self.parse_single_or_compound()?;

        let else_branch: Vec<MacroStmt> = if self.eat("else") {
            self.parse_single_or_compound()?
        } else {
            Vec::new()
        };

        Ok(MacroStmt::If {
            cond,
            then_branch,
            else_branch,
        })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub(crate) fn parse_single_or_compound(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        if self.peek() == Some("{") {
            self.position += 1;

            let inner: Vec<MacroStmt> = self.parse_body()?;

            self.expect("}")?;

            return Ok(inner);
        }

        Ok(vec![self.parse_statement()?])
    }
}

impl MacroStmt {
    pub(crate) fn lower(&self, indent: usize) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);

        match self {
            MacroStmt::VarDecl { ty, name, init } => {
                let clean: String = crate::util::sanitize_thrust_identifier(name);

                if let Some(init) = init {
                    let init_text: String =
                        crate::macro_expr::lower(init, crate::location::Location::RValue);

                    Ok(vec![format!("{pad}var {clean}: {ty} = {init_text};")])
                } else {
                    Ok(vec![format!("{pad}var {clean}: {ty};")])
                }
            }

            MacroStmt::Expr(expr) => {
                let text: String = self.lower_inc_dec(expr);

                Ok(vec![format!("{pad}{text};")])
            }

            MacroStmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_text: String = self.lower_condition(cond);

                let mut out: Vec<String> = vec![format!("{pad}if {cond_text} {{")];

                for stmt in then_branch.iter() {
                    out.extend(stmt.lower(indent + 1)?);
                }

                if else_branch.is_empty() {
                    out.push(format!("{pad}}}"));
                } else {
                    out.push(format!("{pad}}} else {{"));

                    for stmt in else_branch.iter() {
                        out.extend(stmt.lower(indent + 1)?);
                    }

                    out.push(format!("{pad}}}"));
                }

                Ok(out)
            }

            MacroStmt::While { cond, body } => {
                let cond_text: String = self.lower_condition(cond);

                let mut out: Vec<String> = vec![format!("{pad}while {cond_text} {{")];

                for stmt in body.iter() {
                    out.extend(stmt.lower(indent + 1)?);
                }

                out.push(format!("{pad}}}"));

                Ok(out)
            }

            MacroStmt::DoWhile { body, cond } => {
                if matches!(cond, MacroExpr::Literal(text) if text == "0") {
                    let mut out: Vec<String> = Vec::new();

                    for stmt in body.iter() {
                        out.extend(stmt.lower(indent)?);
                    }

                    return Ok(out);
                }

                let cond_text: String = self.lower_condition(cond);

                let mut out: Vec<String> = Vec::new();

                for stmt in body.iter() {
                    out.extend(stmt.lower(indent)?);
                }

                out.push(format!("{pad}while {cond_text} {{"));

                for stmt in body.iter() {
                    out.extend(stmt.lower(indent + 1)?);
                }

                out.push(format!("{pad}}}"));

                Ok(out)
            }

            MacroStmt::For {
                init,
                cond,
                inc,
                body,
            } => self.lower_for(indent, init.as_ref(), cond.as_ref(), inc.as_ref(), body),

            MacroStmt::Compound(inner) => {
                let mut out: Vec<String> = Vec::new();

                for stmt in inner.iter() {
                    out.extend(stmt.lower(indent)?);
                }

                Ok(out)
            }
        }
    }
}

impl MacroStmt {
    fn lower_for(
        &self,
        indent: usize,
        init: Option<&ForInit>,
        cond: Option<&MacroExpr>,
        inc: Option<&MacroExpr>,
        body: &[MacroStmt],
    ) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);

        if init.is_none() && cond.is_none() && inc.is_none() {
            let mut out: Vec<String> = vec![format!("{pad}for ; ; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(indent + 1)?);
            }

            out.push(format!("{pad}}}"));

            return Ok(out);
        }

        if let Some(ForInit::Decl { ty, name, init }) = init {
            let clean: String = crate::util::sanitize_thrust_identifier(name);

            let init_text: String = if let Some(init) = init {
                let value: String =
                    crate::macro_expr::lower(init, crate::location::Location::RValue);

                format!("var {clean}: {ty} = {value}")
            } else {
                format!("var {clean}: {ty}")
            };

            let cond_text: String = cond
                .map(|cond| self.lower_condition(cond))
                .unwrap_or_else(|| "true".to_string());

            let inc_text: String = inc.map(|inc| self.lower_inc_dec(inc)).unwrap_or_default();

            let mut out: Vec<String> =
                vec![format!("{pad}for {init_text}; {cond_text}; {inc_text}; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(indent + 1)?);
            }

            out.push(format!("{pad}}}"));

            return Ok(out);
        }

        let mut out: Vec<String> = Vec::new();

        if let Some(ForInit::Expr(init)) = init {
            let init_text: String = self.lower_inc_dec(init);

            out.push(format!("{pad}{init_text};"));
        }

        let cond_text: String = cond
            .map(|cond| self.lower_condition(cond))
            .unwrap_or_else(|| "true".to_string());

        out.push(format!("{pad}while {cond_text} {{"));

        for stmt in body.iter() {
            out.extend(stmt.lower(indent + 1)?);
        }

        if let Some(inc) = inc {
            let inc_text: String = self.lower_inc_dec(inc);

            if !inc_text.is_empty() {
                out.push(format!("{}    {inc_text};", pad));
            }
        }

        out.push(format!("{pad}}}"));

        Ok(out)
    }
}

impl MacroStmt {
    fn lower_condition(&self, cond: &MacroExpr) -> String {
        if let MacroExpr::Binary { op, .. } = cond {
            if op == "=="
                || op == "!="
                || op == "<"
                || op == "<="
                || op == ">"
                || op == ">="
                || op == "&&"
                || op == "||"
            {
                return crate::macro_expr::lower(cond, crate::location::Location::RValue);
            }
        }

        format!(
            "({}) != 0",
            crate::macro_expr::lower(cond, crate::location::Location::RValue)
        )
    }
}

impl MacroStmt {
    fn lower_inc_dec(&self, expr: &MacroExpr) -> String {
        match expr {
            MacroExpr::Unary { op, arg }
                if matches!(op, MacroUnOp::PreIncrement | MacroUnOp::PreDecrement) =>
            {
                let arg_text: String =
                    crate::macro_expr::lower(arg, crate::location::Location::LValue);

                if matches!(op, MacroUnOp::PreIncrement) {
                    format!("{arg_text} += 1")
                } else {
                    format!("{arg_text} -= 1")
                }
            }

            MacroExpr::Postfix { op, arg } => {
                let arg_text: String =
                    crate::macro_expr::lower(arg, crate::location::Location::LValue);

                match op {
                    crate::macro_expr::MacroPostOp::Increment => format!("{arg_text} += 1"),
                    crate::macro_expr::MacroPostOp::Decrement => format!("{arg_text} -= 1"),
                }
            }

            _ => crate::macro_expr::lower(expr, crate::location::Location::RValue),
        }
    }
}

fn split_var_decl(tokens: &[String]) -> SplitDecl<'_> {
    let mut depth: i32 = 0;

    let mut assign: Option<usize> = None;

    let mut index: usize = 0;

    while index < tokens.len() {
        let token: &str = tokens[index].as_str();

        if token == "(" || token == "[" {
            depth += 1;
        } else if token == ")" || token == "]" {
            depth -= 1;
        } else if token == "=" && depth == 0 {
            assign = Some(index);
            break;
        }

        index += 1;
    }

    let decl_part: &[String] = match assign {
        Some(eq) => &tokens[..eq],
        None => tokens,
    };

    let init_part: &[String] = match assign {
        Some(eq) => &tokens[eq + 1..],
        None => &[],
    };

    if decl_part.is_empty() {
        return Ok(None);
    }

    let mut core: &[String] = decl_part;

    let mut array_suffix: Option<&[String]> = None;

    if core.len() > 2
        && core[core.len() - 1].as_str() == "]"
        && core[core.len() - 3].as_str() == "["
    {
        array_suffix = Some(&core[core.len() - 2..core.len() - 1]);
        core = &core[..core.len() - 3];
    }

    if core.is_empty() {
        return Ok(None);
    }

    let last: &str = core[core.len() - 1].as_str();

    if !last
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
    {
        return Ok(None);
    }

    if matches!(
        last,
        "return"
            | "continue"
            | "break"
            | "goto"
            | "switch"
            | "case"
            | "default"
            | "typedef"
            | "for"
            | "while"
            | "do"
            | "if"
            | "else"
            | "sizeof"
    ) {
        return Ok(None);
    }

    let type_tokens: &[String] = &core[..core.len() - 1];

    if type_tokens.is_empty() {
        return Ok(None);
    }

    let mut stars: usize = 0;

    let mut words: Vec<String> = Vec::new();

    for token in type_tokens.iter() {
        if token == "*" {
            stars += 1;
        } else if matches!(
            token.as_str(),
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
            if token != "const" && token != "volatile" {
                words.push(token.to_string());
            }
        } else if token
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        {
            words.push(token.to_string());
        } else {
            return Ok(None);
        }
    }

    if words.is_empty() {
        return Ok(None);
    }

    let base: Option<String> = if words.len() == 1
        && !matches!(
            words[0].as_str(),
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
        Some(words[0].to_string())
    } else if words.len() == 2
        && matches!(words[0].as_str(), "struct" | "enum" | "union")
        && words[1]
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
    {
        Some(words[1].to_string())
    } else {
        crate::macro_lex::macro_type_name(&words.join(" "))
    };

    let Some(mut out) = base else {
        return Err(MacroLimit::NotComputable);
    };

    for _ in 0..stars {
        out = format!("ptr[{out}]");
    }

    if let Some(size_tokens) = array_suffix {
        if size_tokens.len() != 1 {
            return Err(MacroLimit::NotComputable);
        }

        let size_text: String =
            crate::macro_lex::normalize_literal_token_spelling(size_tokens[0].as_str());

        if !size_text
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_digit())
        {
            return Err(MacroLimit::NotComputable);
        }

        out = format!("array[{out}; {size_text}]");
    }

    Ok(Some((out, last.to_string(), init_part)))
}

fn find_semicolon(tokens: &[String], start: usize) -> Result<usize, MacroLimit> {
    let mut depth: i32 = 0;

    let mut index: usize = start;

    while index < tokens.len() {
        let token: &str = tokens[index].as_str();

        if token == "(" || token == "[" {
            depth += 1;
        } else if token == ")" || token == "]" {
            depth -= 1;
        } else if token == ";" && depth == 0 {
            return Ok(index);
        } else if token == "}" && depth == 0 {
            break;
        }

        index += 1;
    }

    Err(MacroLimit::NotComputable)
}

fn find_matching_paren(tokens: &[String], start: usize) -> Result<usize, MacroLimit> {
    let mut depth: i32 = 1;

    let mut index: usize = start;

    while index < tokens.len() {
        let token: &str = tokens[index].as_str();

        if token == "(" {
            depth += 1;
        } else if token == ")" {
            depth -= 1;

            if depth == 0 {
                return Ok(index);
            }
        }

        index += 1;
    }

    Err(MacroLimit::NotComputable)
}

fn split_top_level<'tokens>(
    tokens: &'tokens [String],
    sep: &str,
) -> Result<Vec<&'tokens [String]>, MacroLimit> {
    let mut parts: Vec<&[String]> = Vec::new();

    let mut depth: i32 = 0;

    let mut start: usize = 0;

    let mut index: usize = 0;

    while index < tokens.len() {
        let token: &str = tokens[index].as_str();

        if token == "(" || token == "[" {
            depth += 1;
        } else if token == ")" || token == "]" {
            depth -= 1;
        } else if token == sep && depth == 0 {
            parts.push(&tokens[start..index]);
            start = index + 1;
        }

        index += 1;
    }

    parts.push(&tokens[start..]);

    Ok(parts)
}

pub(crate) fn parse_statement_body(tokens: &[String]) -> Result<Vec<MacroStmt>, MacroLimit> {
    let mut cursor: MacroStmtCursor<'_> = MacroStmtCursor::new(tokens);

    let parsed: Vec<MacroStmt> = cursor.parse_body()?;

    if !cursor.at_end() {
        return Err(MacroLimit::NotComputable);
    }

    Ok(parsed)
}
