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

use crate::macro_ast::{ForDecl, ForInit, MacroExpr, MacroStmt, MacroUnOp};
use crate::macro_error::MacroLimit;
use crate::macro_expr::MacroCursor;
use crate::macro_token::{MacroToken, MacroTokenKind, MacroTokenOrigin};

type SplitDecl<'tokens> = Result<Option<(String, String, &'tokens [MacroToken])>, MacroLimit>;
type MultiDeclEntry = (String, String, Option<MacroExpr>);

#[derive(Debug)]
pub struct MacroStmtCursor<'tokens> {
    tokens: &'tokens [MacroToken],
    position: usize,
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn new(tokens: &'tokens [MacroToken]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    fn advance(&mut self) -> Option<&'tokens MacroToken> {
        if self.position >= self.tokens.len() {
            return None;
        }

        let token: &'tokens MacroToken = &self.tokens[self.position];

        self.position = self.position.saturating_add(1);

        Some(token)
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_body(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        let mut out: Vec<MacroStmt> = Vec::new();

        while self.position < self.tokens.len() && self.tokens[self.position].get_text() != "}" {
            if self.tokens[self.position].get_text() == ";" {
                let _ignored: Option<&'tokens MacroToken> = self.advance();
                continue;
            }

            out.push(self.parse_statement()?);
        }

        Ok(out)
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_statement(&mut self) -> Result<MacroStmt, MacroLimit> {
        match self.tokens.get(self.position).map(|token| token.get_text()) {
            Some("for") => self.parse_for(),
            Some("while") => self.parse_while(),
            Some("do") => self.parse_do(),
            Some("if") => self.parse_if(),
            Some("{") => {
                let _ignored: Option<&'tokens MacroToken> = self.advance();

                let inner: Vec<MacroStmt> = self.parse_body()?;

                if self
                    .tokens
                    .get(self.position)
                    .is_none_or(|token| token.get_text() != "}")
                {
                    return Err(MacroLimit::ExpectedToken);
                }

                let _ignored: Option<&'tokens MacroToken> = self.advance();

                Ok(MacroStmt::Compound(inner))
            }
            Some(
                "return" | "continue" | "break" | "goto" | "switch" | "case" | "default"
                | "typedef",
            ) => Err(MacroLimit::UnsupportedStatementKeyword),
            Some(_) => self.parse_decl_or_expr(),
            None => Err(MacroLimit::UnexpectedEndOfTokens),
        }
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_decl_or_expr(&mut self) -> Result<MacroStmt, MacroLimit> {
        let start: usize = self.position;

        let end: usize = self::find_semicolon(self.tokens, start)?;

        let stmt_tokens: &[MacroToken] = &self.tokens[start..end];

        self.position = end + 1;

        if let Some(decl) = self::split_var_decl(stmt_tokens)? {
            let init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                let spellings: Vec<String> = crate::macro_token::texts(decl.2);

                Some(MacroCursor::parse(&spellings)?)
            };

            return Ok(MacroStmt::VarDecl {
                ty: decl.0,
                name: decl.1,
                init,
            });
        }

        if let Some(multidecl) = Self::parse_multi_var_decl_simple(stmt_tokens)? {
            if multidecl.len() == 1 {
                return Ok(multidecl
                    .into_iter()
                    .next()
                    .unwrap_or(MacroStmt::Compound(Vec::new())));
            }

            return Ok(MacroStmt::Compound(multidecl));
        }

        let spellings: Vec<String> = crate::macro_token::texts(stmt_tokens);

        Ok(MacroStmt::Expr(MacroCursor::parse(&spellings)?))
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    fn parse_multi_var_decl_simple(
        tokens: &[MacroToken],
    ) -> Result<Option<Vec<MacroStmt>>, MacroLimit> {
        let Some(entries) = Self::parse_multi_var_decl_entries(tokens)? else {
            return Ok(None);
        };

        Ok(Some(
            entries
                .into_iter()
                .map(|(ty, name, init)| MacroStmt::VarDecl { ty, name, init })
                .collect(),
        ))
    }

    fn parse_multi_var_decl_entries(
        tokens: &[MacroToken],
    ) -> Result<Option<Vec<MultiDeclEntry>>, MacroLimit> {
        let parts: Vec<&[MacroToken]> = self::split_top_level(tokens, ",")?;

        if parts.len() < 2 {
            return Ok(None);
        }

        let Some(first_decl) = self::split_var_decl(parts[0])? else {
            return Ok(None);
        };

        let first_decl_part: &[MacroToken] = if let Some(eq_index) = {
            let mut depth: i32 = 0;
            let mut out_index: Option<usize> = None;

            for (index, token) in parts[0].iter().enumerate() {
                if token.get_text() == "(" || token.get_text() == "[" {
                    depth += 1;
                } else if token.get_text() == ")" || token.get_text() == "]" {
                    depth -= 1;
                } else if token.get_text() == "=" && depth == 0 {
                    out_index = Some(index);
                    break;
                }
            }

            out_index
        } {
            &parts[0][..eq_index]
        } else {
            parts[0]
        };

        let first_shape: DeclaratorShape =
            Self::parse_simple_declarator_shape(first_decl_part, true)?;
        let base_ty: String = Self::peel_type_by_shape(&first_decl.0, &first_shape)?;

        let first_init: Option<MacroExpr> = if first_decl.2.is_empty() {
            None
        } else {
            let spellings: Vec<String> = crate::macro_token::texts(first_decl.2);

            Some(MacroCursor::parse(&spellings)?)
        };

        let mut out: Vec<MultiDeclEntry> =
            vec![(first_decl.0.clone(), first_decl.1.clone(), first_init)];

        for part in parts.iter().skip(1) {
            if part.is_empty() {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            if let Some(explicit_decl) = self::split_var_decl(part)? {
                let init: Option<MacroExpr> = if explicit_decl.2.is_empty() {
                    None
                } else {
                    let spellings: Vec<String> = crate::macro_token::texts(explicit_decl.2);

                    Some(MacroCursor::parse(&spellings)?)
                };

                out.push((explicit_decl.0, explicit_decl.1, init));
                continue;
            }

            let decl_part: &[MacroToken] = if let Some(eq_index) = {
                let mut depth: i32 = 0;
                let mut out_index: Option<usize> = None;

                for (index, token) in part.iter().enumerate() {
                    if token.get_text() == "(" || token.get_text() == "[" {
                        depth += 1;
                    } else if token.get_text() == ")" || token.get_text() == "]" {
                        depth -= 1;
                    } else if token.get_text() == "=" && depth == 0 {
                        out_index = Some(index);
                        break;
                    }
                }

                out_index
            } {
                &part[..eq_index]
            } else {
                part
            };

            let shape: DeclaratorShape = Self::parse_simple_declarator_shape(decl_part, false)?;
            let ty: String = Self::apply_shape_to_type(&base_ty, &shape)?;

            let init: Option<MacroExpr> = if let Some(eq_index) = {
                let mut depth: i32 = 0;
                let mut out_index: Option<usize> = None;

                for (index, token) in part.iter().enumerate() {
                    if token.get_text() == "(" || token.get_text() == "[" {
                        depth += 1;
                    } else if token.get_text() == ")" || token.get_text() == "]" {
                        depth -= 1;
                    } else if token.get_text() == "=" && depth == 0 {
                        out_index = Some(index);
                        break;
                    }
                }

                out_index
            } {
                let rhs: &[MacroToken] = &part[eq_index + 1..];

                if rhs.is_empty() {
                    return Err(MacroLimit::DeclaratorMalformed);
                }

                let spellings: Vec<String> = crate::macro_token::texts(rhs);

                Some(MacroCursor::parse(&spellings)?)
            } else {
                None
            };

            out.push((ty, shape.name, init));
        }

        Ok(Some(out))
    }

    fn parse_simple_declarator_shape(
        decl_part: &[MacroToken],
        allow_specifier_prefix: bool,
    ) -> Result<DeclaratorShape, MacroLimit> {
        if decl_part.is_empty() {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        let mut core: &[MacroToken] = decl_part;
        let mut array_size: Option<String> = None;

        if core.len() > 2
            && core[core.len() - 3].get_text() == "["
            && core[core.len() - 1].get_text() == "]"
        {
            let normalized: String =
                crate::macro_lex::normalize_literal_token_spelling(core[core.len() - 2].get_text());

            if !normalized
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_digit())
            {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            array_size = Some(normalized);
            core = &core[..core.len() - 3];
        }

        if core.is_empty() {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        let name_index: usize = core.len() - 1;
        let name: &str = core[name_index].get_text();

        if !name
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
            || Self::is_reserved_word(name)
        {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        let mut stars: usize = 0;
        let mut index: usize = name_index;

        while index > 0 && core[index - 1].get_text() == "*" {
            stars += 1;
            index -= 1;
        }

        let specifier_prefix: &[MacroToken] = &core[..index];

        if !allow_specifier_prefix && !specifier_prefix.is_empty() {
            return Err(MacroLimit::DeclaratorUnsupported);
        }

        Ok(DeclaratorShape {
            stars,
            name: name.to_string(),
            array_size,
        })
    }

    fn peel_type_by_shape(full_type: &str, shape: &DeclaratorShape) -> Result<String, MacroLimit> {
        let mut current: String = full_type.to_string();

        if let Some(size_text) = &shape.array_size {
            if !current.starts_with("array[") || !current.ends_with(']') {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            let inner_text: &str = &current[6..current.len() - 1];
            let separator: &str = "; ";

            let Some(split_at) = inner_text.rfind(separator) else {
                return Err(MacroLimit::DeclaratorMalformed);
            };

            let inner: String = inner_text[..split_at].to_string();
            let parsed_size: String = inner_text[split_at + separator.len()..].to_string();

            if parsed_size.is_empty() {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            if parsed_size != *size_text {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            current = inner;
        }

        for _ in 0..shape.stars {
            if !current.starts_with("ptr[") || !current.ends_with(']') {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            let inner: String = current[4..current.len() - 1].to_string();

            current = inner;
        }

        Ok(current)
    }

    fn apply_shape_to_type(base_type: &str, shape: &DeclaratorShape) -> Result<String, MacroLimit> {
        let mut ty: String = base_type.to_string();

        for _ in 0..shape.stars {
            ty = format!("ptr[{ty}]");
        }

        if let Some(size_text) = &shape.array_size {
            if size_text.is_empty() {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            ty = format!("array[{ty}; {size_text}]");
        }

        Ok(ty)
    }

    fn is_reserved_word(word: &str) -> bool {
        matches!(
            word,
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
        )
    }
}

#[derive(Debug, Clone)]
struct DeclaratorShape {
    stars: usize,
    name: String,
    array_size: Option<String>,
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_for(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "for")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        let header_start: usize = self.position;

        let header_end: usize = self::find_matching_paren(self.tokens, header_start)?;

        let header: &[MacroToken] = &self.tokens[header_start..header_end];

        self.position = header_end + 1;

        let parts: Vec<&[MacroToken]> = self::split_top_level(header, ";")?;

        if parts.len() != 3 {
            return Err(MacroLimit::ForHeaderUnsupported);
        }

        let init: Option<ForInit> = if parts[0].is_empty() {
            None
        } else if let Some(decl) = self::split_var_decl(parts[0])? {
            let decl_init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                let spellings: Vec<String> = crate::macro_token::texts(decl.2);

                Some(MacroCursor::parse(&spellings)?)
            };

            Some(ForInit::Decl {
                ty: decl.0,
                name: decl.1,
                init: decl_init,
            })
        } else if let Some(multi_decls) = Self::parse_multi_var_decl_entries(parts[0])? {
            Some(ForInit::Decls(
                multi_decls
                    .into_iter()
                    .map(|(ty, name, init)| ForDecl::new(ty, name, init))
                    .collect(),
            ))
        } else {
            let spellings: Vec<String> = crate::macro_token::texts(parts[0]);

            Some(ForInit::Expr(MacroCursor::parse(&spellings)?))
        };

        let cond: Option<MacroExpr> = if parts[1].is_empty() {
            None
        } else {
            let spellings: Vec<String> = crate::macro_token::texts(parts[1]);

            Some(MacroCursor::parse(&spellings)?)
        };

        let inc: Option<MacroExpr> = if parts[2].is_empty() {
            None
        } else {
            let spellings: Vec<String> = crate::macro_token::texts(parts[2]);

            Some(MacroCursor::parse(&spellings)?)
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
    pub fn parse_while(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "while")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let spellings: Vec<String> = crate::macro_token::texts(&self.tokens[start..end]);

        let cond: MacroExpr = MacroCursor::parse(&spellings)?;

        self.position = end + 1;

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        Ok(MacroStmt::While { cond, body })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_do(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "do")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "while")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let spellings: Vec<String> = crate::macro_token::texts(&self.tokens[start..end]);

        let cond: MacroExpr = MacroCursor::parse(&spellings)?;

        self.position = end + 1;

        if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.get_text() == ";")
        {
            let _ignored: Option<&'tokens MacroToken> = self.advance();
        }

        Ok(MacroStmt::DoWhile { body, cond })
    }
}

impl<'tokens> MacroStmtCursor<'tokens> {
    pub fn parse_if(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "if")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.get_text() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens MacroToken> = self.advance();

        let start: usize = self.position;

        let end: usize = self::find_matching_paren(self.tokens, start)?;

        let spellings: Vec<String> = crate::macro_token::texts(&self.tokens[start..end]);

        let cond: MacroExpr = MacroCursor::parse(&spellings)?;

        self.position = end + 1;

        let then_branch: Vec<MacroStmt> = self.parse_single_or_compound()?;

        let else_branch: Vec<MacroStmt> = if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.get_text() == "else")
        {
            let _ignored: Option<&'tokens MacroToken> = self.advance();
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
    pub fn parse_single_or_compound(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.get_text() == "{")
        {
            let _ignored: Option<&'tokens MacroToken> = self.advance();

            let inner: Vec<MacroStmt> = self.parse_body()?;

            if self
                .tokens
                .get(self.position)
                .is_none_or(|token| token.get_text() != "}")
            {
                return Err(MacroLimit::ExpectedToken);
            }

            let _ignored: Option<&'tokens MacroToken> = self.advance();

            return Ok(inner);
        }

        Ok(vec![self.parse_statement()?])
    }
}

impl MacroStmt {
    pub fn lower(&self, indent: usize) -> Result<Vec<String>, MacroLimit> {
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

        if let Some(ForInit::Decls(decls)) = init {
            let mut out: Vec<String> = Vec::new();

            for decl in decls.iter() {
                let clean: String = crate::util::sanitize_thrust_identifier(decl.get_name());

                if let Some(init) = decl.get_init() {
                    let init_text: String =
                        crate::macro_expr::lower(init, crate::location::Location::RValue);

                    out.push(format!(
                        "{pad}var {clean}: {} = {init_text};",
                        decl.get_ty()
                    ));
                } else {
                    out.push(format!("{pad}var {clean}: {};", decl.get_ty()));
                }
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
                    crate::macro_ast::MacroPostOp::Increment => format!("{arg_text} += 1"),
                    crate::macro_ast::MacroPostOp::Decrement => format!("{arg_text} -= 1"),
                }
            }

            _ => crate::macro_expr::lower(expr, crate::location::Location::RValue),
        }
    }
}

fn split_var_decl(tokens: &[MacroToken]) -> SplitDecl<'_> {
    let mut depth: i32 = 0;

    let mut assign: Option<usize> = None;

    let mut index: usize = 0;

    while index < tokens.len() {
        let token: &str = tokens[index].get_text();

        if tokens[index].get_kind() == MacroTokenKind::Punctuation && (token == "(" || token == "[")
        {
            depth += 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && (token == ")" || token == "]")
        {
            depth -= 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && token == "="
            && depth == 0
        {
            assign = Some(index);
            break;
        }

        index += 1;
    }

    let decl_part: &[MacroToken] = match assign {
        Some(eq) => &tokens[..eq],
        None => tokens,
    };

    let init_part: &[MacroToken] = match assign {
        Some(eq) => &tokens[eq + 1..],
        None => &[],
    };

    if decl_part.is_empty() {
        return Ok(None);
    }

    let mut core: &[MacroToken] = decl_part;

    let mut array_suffix: Option<&[MacroToken]> = None;

    if core.len() > 2
        && core[core.len() - 1].get_text() == "]"
        && core[core.len() - 3].get_text() == "["
    {
        array_suffix = Some(&core[core.len() - 2..core.len() - 1]);
        core = &core[..core.len() - 3];
    }

    if core.is_empty() {
        return Ok(None);
    }

    let last: &str = core[core.len() - 1].get_text();

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

    let type_tokens: &[MacroToken] = &core[..core.len() - 1];

    if type_tokens.is_empty() {
        return Ok(None);
    }

    let mut stars: usize = 0;

    let mut words: Vec<String> = Vec::new();

    for token in type_tokens.iter() {
        if token.get_text() == "*" {
            stars += 1;
        } else if matches!(
            token.get_text(),
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
            if token.get_text() != "const" && token.get_text() != "volatile" {
                words.push(token.get_text().to_string());
            }
        } else if token
            .get_text()
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        {
            words.push(token.get_text().to_string());
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
        return Err(MacroLimit::DeclaratorMalformed);
    };

    for _ in 0..stars {
        out = format!("ptr[{out}]");
    }

    if let Some(size_tokens) = array_suffix {
        if size_tokens.len() != 1 {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        let size_text: String =
            crate::macro_lex::normalize_literal_token_spelling(size_tokens[0].get_text());

        if !size_text
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_digit())
        {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        out = format!("array[{out}; {size_text}]");
    }

    Ok(Some((out, last.to_string(), init_part)))
}

fn find_semicolon(tokens: &[MacroToken], start: usize) -> Result<usize, MacroLimit> {
    let mut depth: i32 = 0;

    let mut index: usize = start;

    while index < tokens.len() {
        let token: &str = tokens[index].get_text();

        if tokens[index].get_kind() == MacroTokenKind::Punctuation && (token == "(" || token == "[")
        {
            depth += 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && (token == ")" || token == "]")
        {
            depth -= 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && token == ";"
            && depth == 0
        {
            return Ok(index);
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && token == "}"
            && depth == 0
        {
            break;
        }

        index += 1;
    }

    Err(MacroLimit::TokenBalanceError)
}

fn find_matching_paren(tokens: &[MacroToken], start: usize) -> Result<usize, MacroLimit> {
    let mut depth: i32 = 1;

    let mut index: usize = start;

    while index < tokens.len() {
        let token: &str = tokens[index].get_text();

        if tokens[index].get_kind() == MacroTokenKind::Punctuation && token == "(" {
            depth += 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation && token == ")" {
            depth -= 1;

            if depth == 0 {
                return Ok(index);
            }
        }

        index += 1;
    }

    Err(MacroLimit::TokenBalanceError)
}

fn split_top_level<'tokens>(
    tokens: &'tokens [MacroToken],
    sep: &str,
) -> Result<Vec<&'tokens [MacroToken]>, MacroLimit> {
    let mut parts: Vec<&[MacroToken]> = Vec::new();

    let mut depth: i32 = 0;

    let mut start: usize = 0;

    let mut index: usize = 0;

    while index < tokens.len() {
        let token: &str = tokens[index].get_text();

        if tokens[index].get_kind() == MacroTokenKind::Punctuation && (token == "(" || token == "[")
        {
            depth += 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && (token == ")" || token == "]")
        {
            depth -= 1;
        } else if tokens[index].get_kind() == MacroTokenKind::Punctuation
            && token == sep
            && depth == 0
        {
            parts.push(&tokens[start..index]);
            start = index + 1;
        }

        index += 1;
    }

    parts.push(&tokens[start..]);

    Ok(parts)
}

pub fn parse_statement_body_tokens(tokens: &[MacroToken]) -> Result<Vec<MacroStmt>, MacroLimit> {
    let mut cursor: MacroStmtCursor<'_> = MacroStmtCursor::new(tokens);

    let parsed: Vec<MacroStmt> = cursor.parse_body()?;

    if cursor.position < cursor.tokens.len() {
        return Err(MacroLimit::TrailingTokens);
    }

    Ok(parsed)
}

pub fn parse_statement_body(spellings: &[String]) -> Result<Vec<MacroStmt>, MacroLimit> {
    let tokens: Vec<MacroToken> =
        crate::macro_token::from_spellings(spellings, MacroTokenOrigin::FallbackLexed);

    self::parse_statement_body_tokens(&tokens)
}
