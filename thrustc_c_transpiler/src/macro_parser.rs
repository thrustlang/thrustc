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

use crate::macro_ast::{ForDecl, ForInit, MacroExpr, MacroPostOp, MacroStmt, MacroUnOp};
use crate::macro_error::MacroLimit;
use crate::macro_lex;

type SplitDecl<'tokens> = Result<Option<(String, String, &'tokens [String])>, MacroLimit>;
type MultiDeclEntry = (String, String, Option<MacroExpr>);

#[derive(Debug)]
pub struct MacroParser<'tokens> {
    tokens: &'tokens [String],
    position: usize,
}

impl<'tokens> MacroParser<'tokens> {
    pub fn new(tokens: &'tokens [String]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_body(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        let mut out: Vec<MacroStmt> = Vec::new();

        while self.position < self.tokens.len() && self.tokens[self.position].as_str() != "}" {
            if self.tokens[self.position].as_str() == ";" {
                let _ignored: Option<&'tokens str> = self.advance();
                continue;
            }

            out.push(self.parse_statement()?);
        }

        Ok(out)
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_statement_body(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        let parsed: Vec<MacroStmt> = self.parse_body()?;

        if self.position < self.tokens.len() {
            return Err(MacroLimit::TrailingTokens);
        }

        Ok(parsed)
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_statement(&mut self) -> Result<MacroStmt, MacroLimit> {
        match self.tokens.get(self.position).map(|token| token.as_str()) {
            Some("for") => self.parse_for(),
            Some("while") => self.parse_while(),
            Some("do") => self.parse_do(),
            Some("if") => self.parse_if(),
            Some("{") => {
                let _ignored: Option<&'tokens str> = self.advance();

                let inner: Vec<MacroStmt> = self.parse_body()?;

                if self
                    .tokens
                    .get(self.position)
                    .is_none_or(|token| token.as_str() != "}")
                {
                    return Err(MacroLimit::ExpectedToken);
                }

                let _ignored: Option<&'tokens str> = self.advance();

                Ok(MacroStmt::Compound(inner))
            }
            Some(text) if text != "else" && text != "sizeof" && Self::is_reserved_word(text) => {
                Err(MacroLimit::UnsupportedStatementKeyword)
            }
            Some(_) => self.parse_decl_or_expr(),
            None => Err(MacroLimit::UnexpectedEndOfTokens),
        }
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_decl_or_expr(&mut self) -> Result<MacroStmt, MacroLimit> {
        let start: usize = self.position;

        let end: usize = self.find_semicolon(start)?;

        let stmt_tokens: &[String] = &self.tokens[start..end];

        self.position = end + 1;

        if let Some(multidecl) = Self::parse_multi_var_decl_simple(stmt_tokens)? {
            if multidecl.len() == 1 {
                return Ok(multidecl
                    .into_iter()
                    .next()
                    .unwrap_or(MacroStmt::Compound(Vec::new())));
            }

            return Ok(MacroStmt::Compound(multidecl));
        }

        if let Some(decl) = Self::split_var_decl(stmt_tokens)? {
            let init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                let spellings: Vec<String> = decl.2.to_vec();

                Some(MacroParser::parse(&spellings)?)
            };

            return Ok(MacroStmt::VarDecl {
                ty: decl.0,
                name: decl.1,
                init,
            });
        }

        let spellings: Vec<String> = stmt_tokens.to_vec();

        Ok(MacroStmt::Expr(MacroParser::parse(&spellings)?))
    }
}

impl<'tokens> MacroParser<'tokens> {
    fn parse_multi_var_decl_simple(
        tokens: &[String],
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
        tokens: &[String],
    ) -> Result<Option<Vec<MultiDeclEntry>>, MacroLimit> {
        let parts: Vec<&[String]> = Self::split_top_level(tokens, ",")?;

        if parts.len() < 2 {
            return Ok(None);
        }

        let Some(first_decl) = Self::split_var_decl(parts[0])? else {
            return Ok(None);
        };

        let first_decl_part: &[String] = if first_decl.2.is_empty() {
            parts[0]
        } else {
            &parts[0][..parts[0].len() - first_decl.2.len() - 1]
        };

        let first_shape: DeclaratorShape =
            Self::parse_simple_declarator_shape(first_decl_part, true)?;
        let base_ty: String = Self::peel_type_by_shape(&first_decl.0, &first_shape)?;

        let first_init: Option<MacroExpr> = if first_decl.2.is_empty() {
            None
        } else {
            let spellings: Vec<String> = first_decl.2.to_vec();

            Some(MacroParser::parse(&spellings)?)
        };

        let mut out: Vec<MultiDeclEntry> =
            vec![(first_decl.0.clone(), first_decl.1.clone(), first_init)];

        for part in parts.iter().skip(1) {
            if part.is_empty() {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            if let Some(explicit_decl) = Self::split_var_decl(part)? {
                let init: Option<MacroExpr> = if explicit_decl.2.is_empty() {
                    None
                } else {
                    let spellings: Vec<String> = explicit_decl.2.to_vec();

                    Some(MacroParser::parse(&spellings)?)
                };

                out.push((explicit_decl.0, explicit_decl.1, init));
                continue;
            }

            let part_texts: Vec<String> = part.to_vec();

            let parsed_part: crate::macro_ast::MacroExpr =
                MacroParser::parse(&part_texts).map_err(|_| MacroLimit::DeclaratorMalformed)?;

            if let crate::macro_ast::MacroExpr::Ident(bare_name) = parsed_part {
                let shape: DeclaratorShape = DeclaratorShape {
                    stars: 0,
                    name: bare_name,
                    array_size: None,
                };

                let ty: String = Self::apply_shape_to_type(&base_ty, &shape)?;

                out.push((ty, shape.name, None));
                continue;
            }

            let crate::macro_ast::MacroExpr::Assign { op, target, value } = parsed_part else {
                return Err(MacroLimit::DeclaratorMalformed);
            };

            if op != "=" {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            let crate::macro_ast::MacroExpr::Ident(target_name) = *target else {
                return Err(MacroLimit::DeclaratorMalformed);
            };

            let shape: DeclaratorShape = DeclaratorShape {
                stars: 0,
                name: target_name,
                array_size: None,
            };

            let ty: String = Self::apply_shape_to_type(&base_ty, &shape)?;

            out.push((ty, shape.name, Some(*value)));
        }

        Ok(Some(out))
    }

    fn parse_simple_declarator_shape(
        decl_part: &[String],
        allow_specifier_prefix: bool,
    ) -> Result<DeclaratorShape, MacroLimit> {
        if decl_part.is_empty() {
            return Err(MacroLimit::DeclaratorMalformed);
        }

        let mut core: &[String] = decl_part;
        let mut array_size: Option<String> = None;

        if core.len() > 2
            && core[core.len() - 3].as_str() == "["
            && core[core.len() - 1].as_str() == "]"
        {
            let normalized: String =
                crate::macro_lex::normalize_literal_token_spelling(core[core.len() - 2].as_str());

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
        let name: &str = core[name_index].as_str();

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

        while index > 0 && core[index - 1].as_str() == "*" {
            stars += 1;
            index -= 1;
        }

        let specifier_prefix: &[String] = &core[..index];

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
}

impl<'tokens> MacroParser<'tokens> {
    fn is_reserved_word(word: &str) -> bool {
        crate::macro_token::is_keyword(word)
            && word != "const"
            && word != "volatile"
            && word != "struct"
            && word != "enum"
            && word != "union"
    }
}

#[derive(Debug, Clone)]
struct DeclaratorShape {
    stars: usize,
    name: String,
    array_size: Option<String>,
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_for(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "for")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        let header_start: usize = self.position;
        let header_end: usize = self.find_matching_paren(header_start)?;

        let header: &[String] = &self.tokens[header_start..header_end];

        self.position = header_end + 1;

        let parts: Vec<&[String]> = Self::split_top_level(header, ";")?;

        if parts.len() != 3 {
            return Err(MacroLimit::ForHeaderUnsupported);
        }

        let init: Option<ForInit> = if parts[0].is_empty() {
            None
        } else if let Some(decl) = Self::split_var_decl(parts[0])? {
            let decl_init: Option<MacroExpr> = if decl.2.is_empty() {
                None
            } else {
                let spellings: Vec<String> = decl.2.to_vec();

                Some(MacroParser::parse(&spellings)?)
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
            let spellings: Vec<String> = parts[0].to_vec();

            Some(ForInit::Expr(MacroParser::parse(&spellings)?))
        };

        let cond: Option<MacroExpr> = if parts[1].is_empty() {
            None
        } else {
            let spellings: Vec<String> = parts[1].to_vec();

            Some(MacroParser::parse(&spellings)?)
        };

        let inc: Option<MacroExpr> = if parts[2].is_empty() {
            None
        } else {
            let spellings: Vec<String> = parts[2].to_vec();

            Some(MacroParser::parse(&spellings)?)
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_while(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "while")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        let start: usize = self.position;

        let end: usize = self.find_matching_paren(start)?;

        let spellings: Vec<String> = self.tokens[start..end].to_vec();

        let cond: MacroExpr = MacroParser::parse(&spellings)?;

        self.position = end + 1;

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        Ok(MacroStmt::While { cond, body })
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_do(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "do")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        let body: Vec<MacroStmt> = self.parse_single_or_compound()?;

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "while")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        let start: usize = self.position;

        let end: usize = self.find_matching_paren(start)?;

        let spellings: Vec<String> = self.tokens[start..end].to_vec();

        let cond: MacroExpr = MacroParser::parse(&spellings)?;

        self.position = end + 1;

        if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.as_str() == ";")
        {
            let _ignored: Option<&'tokens str> = self.advance();
        }

        Ok(MacroStmt::DoWhile { body, cond })
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_if(&mut self) -> Result<MacroStmt, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "if")
        {
            return Err(MacroLimit::StatementMalformed);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        if self
            .tokens
            .get(self.position)
            .is_none_or(|token| token.as_str() != "(")
        {
            return Err(MacroLimit::ExpectedToken);
        }

        let _ignored: Option<&'tokens str> = self.advance();

        let start: usize = self.position;

        let end: usize = self.find_matching_paren(start)?;

        let spellings: Vec<String> = self.tokens[start..end].to_vec();

        let cond: MacroExpr = MacroParser::parse(&spellings)?;

        self.position = end + 1;

        let then_branch: Vec<MacroStmt> = self.parse_single_or_compound()?;

        let else_branch: Vec<MacroStmt> = if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.as_str() == "else")
        {
            let _ignored: Option<&'tokens str> = self.advance();
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_single_or_compound(&mut self) -> Result<Vec<MacroStmt>, MacroLimit> {
        if self
            .tokens
            .get(self.position)
            .is_some_and(|token| token.as_str() == "{")
        {
            let _ignored: Option<&'tokens str> = self.advance();

            let inner: Vec<MacroStmt> = self.parse_body()?;

            if self
                .tokens
                .get(self.position)
                .is_none_or(|token| token.as_str() != "}")
            {
                return Err(MacroLimit::ExpectedToken);
            }

            let _ignored: Option<&'tokens str> = self.advance();

            return Ok(inner);
        }

        Ok(vec![self.parse_statement()?])
    }
}

impl<'tokens> MacroParser<'tokens> {
    fn find_semicolon(&self, start: usize) -> Result<usize, MacroLimit> {
        let mut depth: i32 = 0;

        let mut index: usize = start;

        while index < self.tokens.len() {
            let token: &str = self.tokens[index].as_str();

            if crate::macro_token::is_punctuation(self.tokens[index].as_str())
                && (token == "(" || token == "[")
            {
                depth += 1;
            } else if crate::macro_token::is_punctuation(self.tokens[index].as_str())
                && (token == ")" || token == "]")
            {
                depth -= 1;
            } else if crate::macro_token::is_punctuation(self.tokens[index].as_str())
                && token == ";"
                && depth == 0
            {
                return Ok(index);
            } else if crate::macro_token::is_punctuation(self.tokens[index].as_str())
                && token == "}"
                && depth == 0
            {
                break;
            }

            index += 1;
        }

        Err(MacroLimit::TokenBalanceError)
    }
}

impl<'tokens> MacroParser<'tokens> {
    fn find_matching_paren(&self, start: usize) -> Result<usize, MacroLimit> {
        let mut depth: i32 = 1;

        let mut index: usize = start;

        while index < self.tokens.len() {
            let token: &str = self.tokens[index].as_str();

            if crate::macro_token::is_punctuation(self.tokens[index].as_str()) && token == "(" {
                depth += 1;
            } else if crate::macro_token::is_punctuation(self.tokens[index].as_str())
                && token == ")"
            {
                depth -= 1;

                if depth == 0 {
                    return Ok(index);
                }
            }

            index += 1;
        }

        Err(MacroLimit::TokenBalanceError)
    }
}

impl<'tokens> MacroParser<'tokens> {
    fn split_var_decl(tokens: &[String]) -> SplitDecl<'_> {
        let assign: Option<usize> = tokens
            .iter()
            .scan(0i32, |depth, token| {
                let text: &str = token.as_str();

                let is_punct: bool = crate::macro_token::is_punctuation(token.as_str());

                if is_punct && (text == "(" || text == "[") {
                    *depth += 1;
                } else if is_punct && (text == ")" || text == "]") {
                    *depth -= 1;
                }

                Some(is_punct && text == "=" && *depth == 0)
            })
            .position(|is_assign| is_assign);

        let (decl_part, init_part): (&[String], &[String]) = match assign {
            Some(eq) => {
                let (head, tail): (&[String], &[String]) = tokens.split_at(eq);

                (head, tail.get(1..).unwrap_or_default())
            }
            None => (tokens, &[]),
        };

        if decl_part.is_empty() {
            return Ok(None);
        }

        let mut core: &[String] = decl_part;

        let mut array_suffix: Option<&[String]> = None;

        if let Some((close, without_close)) = core.split_last() {
            if close.as_str() == "]" {
                if let Some((size_token, without_size)) = without_close.split_last() {
                    if let Some((open, head)) = without_size.split_last() {
                        if open.as_str() == "[" {
                            array_suffix = Some(std::slice::from_ref(size_token));
                            core = head;
                        }
                    }
                }
            }
        }

        if core.is_empty() {
            return Ok(None);
        }

        let Some((name_token, type_tokens)) = core.split_last() else {
            return Ok(None);
        };

        let last: &str = name_token.as_str();

        if !last
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        {
            return Ok(None);
        }

        if MacroParser::is_reserved_word(last) {
            return Ok(None);
        }

        if type_tokens.is_empty() {
            return Ok(None);
        }

        let stars: usize = type_tokens
            .iter()
            .filter(|token| token.as_str() == "*")
            .count();

        let has_invalid_word: bool = type_tokens
            .iter()
            .filter(|token| token.as_str() != "*")
            .any(|token| {
                let text: &str = token.as_str();

                text != "const"
                    && text != "volatile"
                    && !text
                        .chars()
                        .next()
                        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
            });

        if has_invalid_word {
            return Ok(None);
        }

        let words: Vec<String> = type_tokens
            .iter()
            .filter(|token| {
                token.as_str() != "*" && token.as_str() != "const" && token.as_str() != "volatile"
            })
            .map(|token| token.as_str().to_string())
            .collect();

        if words.is_empty() {
            return Ok(None);
        }

        let joined: String = words.join(" ");

        let base: Option<String> = if let Some(mapped) = crate::macro_lex::macro_type_name(&joined)
        {
            Some(mapped)
        } else {
            match words.as_slice() {
                [single] if !crate::macro_token::is_keyword(single) => Some(single.clone()),
                [head, tail]
                    if (head == "struct" || head == "enum" || head == "union")
                        && tail
                            .chars()
                            .next()
                            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_') =>
                {
                    Some(tail.clone())
                }
                _ => None,
            }
        };

        let Some(base_text) = base else {
            return Err(MacroLimit::DeclaratorMalformed);
        };

        let mut out: String = (0..stars).fold(base_text, |acc, _| format!("ptr[{acc}]"));

        if let Some(size_tokens) = array_suffix {
            let Some((size_token, rest)) = size_tokens.split_first() else {
                return Err(MacroLimit::DeclaratorMalformed);
            };

            if !rest.is_empty() {
                return Err(MacroLimit::DeclaratorMalformed);
            }

            let size_text: String =
                crate::macro_lex::normalize_literal_token_spelling(size_token.as_str());

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
}

impl<'tokens> MacroParser<'tokens> {
    fn split_top_level<'a>(
        tokens: &'a [String],
        sep: &str,
    ) -> Result<Vec<&'a [String]>, MacroLimit> {
        let mut parts: Vec<&[String]> = Vec::new();

        let mut depth: i32 = 0;

        let mut start: usize = 0;

        let mut index: usize = 0;

        while index < tokens.len() {
            let token: &str = tokens[index].as_str();

            if crate::macro_token::is_punctuation(tokens[index].as_str())
                && (token == "(" || token == "[")
            {
                depth += 1;
            } else if crate::macro_token::is_punctuation(tokens[index].as_str())
                && (token == ")" || token == "]")
            {
                depth -= 1;
            } else if crate::macro_token::is_punctuation(tokens[index].as_str())
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
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse(body: &[String]) -> Result<MacroExpr, MacroLimit> {
        let mut cursor: MacroParser<'_> = MacroParser::new(body);

        let parsed: MacroExpr = cursor.parse_comma()?;

        if !cursor.at_end() {
            return Err(MacroLimit::TrailingTokens);
        }

        Ok(parsed)
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_comma(&mut self) -> Result<MacroExpr, MacroLimit> {
        let mut items: Vec<MacroExpr> = vec![self.parse_assign()?];

        while self.eat(",") {
            items.push(self.parse_assign()?);
        }

        if items.len() == 1 {
            return Ok(items.remove(0));
        }

        Ok(MacroExpr::Comma(items))
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn advance(&mut self) -> Option<&'tokens str> {
        if self.position >= self.tokens.len() {
            return None;
        }

        let token: &'tokens str = self.tokens[self.position].as_str();
        self.position = self.position.saturating_add(1);

        Some(token)
    }
}

impl<'tokens> MacroParser<'tokens> {
    #[inline]
    pub fn peek(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|token| token.as_str())
    }
}

impl<'tokens> MacroParser<'tokens> {
    #[inline]
    pub fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn eat(&mut self, expected: &str) -> bool {
        if self.peek() == Some(expected) {
            let _ignored: Option<&'tokens str> = self.advance();

            return true;
        }

        false
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn expect(&mut self, expected: &str) -> Result<(), MacroLimit> {
        if self.eat(expected) {
            return Ok(());
        }

        Err(MacroLimit::ExpectedToken)
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_assign(&mut self) -> Result<MacroExpr, MacroLimit> {
        let target: MacroExpr = self.parse_ternary()?;

        if self.peek().is_some_and(|token| {
            matches!(
                token,
                "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
            )
        }) {
            let Some(op_token) = self.advance() else {
                return Err(MacroLimit::UnexpectedEndOfTokens);
            };
            let op: String = op_token.to_string();

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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_ternary(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_lor(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_land(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_bor(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_bxor(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_band(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_equality(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_relational(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_shift(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_additive(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_multiplicative(&mut self) -> Result<MacroExpr, MacroLimit> {
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_cast(&mut self) -> Result<MacroExpr, MacroLimit> {
        if self.peek() == Some("(") {
            let saved: usize = self.position;

            let _ignored: Option<&'tokens str> = self.advance();

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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_cast_target(&mut self) -> Option<String> {
        let mut words: Vec<String> = Vec::new();
        let mut stars: usize = 0;

        loop {
            match self.peek() {
                Some("*") => {
                    stars += 1;
                    let _ignored: Option<&'tokens str> = self.advance();
                }
                Some("int") | Some("unsigned") | Some("long") | Some("short") | Some("char")
                | Some("float") | Some("double") | Some("signed") | Some("void")
                | Some("const") | Some("volatile") | Some("_Bool") => {
                    let word_token: &'tokens str = self.advance()?;
                    let word: String = word_token.to_string();

                    if word != "const" && word != "volatile" {
                        words.push(word);
                    }
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_unary(&mut self) -> Result<MacroExpr, MacroLimit> {
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

                    return Err(MacroLimit::SizeOfMalformed);
                }

                self.position = saved;

                if let Some(target) = self.parse_sizeof_user_target() {
                    return Ok(MacroExpr::SizeOf(target));
                }

                self.position = saved;

                if self.parse_comma().is_ok() && self.eat(")") {
                    return Err(MacroLimit::SizeOfExpression);
                }
            }

            return Err(MacroLimit::SizeOfMalformed);
        }

        self.parse_postfix()
    }
}

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_sizeof_user_target(&mut self) -> Option<String> {
        if matches!(self.peek(), Some("struct") | Some("enum") | Some("union")) {
            let _ignored: Option<&'tokens str> = self.advance();
        }

        let name: String = match self.peek() {
            Some(word)
                if word
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_') =>
            {
                let name: String = word.to_string();

                let _ignored: Option<&'tokens str> = self.advance();

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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_postfix(&mut self) -> Result<MacroExpr, MacroLimit> {
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

                        let _ignored: Option<&'tokens str> = self.advance();

                        field
                    }
                    _ => return Err(MacroLimit::ExpressionMalformed),
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

impl<'tokens> MacroParser<'tokens> {
    pub fn parse_primary(&mut self) -> Result<MacroExpr, MacroLimit> {
        if self.eat("(") {
            let inner: MacroExpr = self.parse_assign()?;

            self.expect(")")?;

            return Ok(MacroExpr::Paren(Box::new(inner)));
        }

        if self.eat("NULL") {
            return Ok(MacroExpr::Null);
        }

        let Some(token) = self.peek() else {
            return Err(MacroLimit::ExpressionMalformed);
        };

        if token.chars().next().is_some_and(|ch| ch.is_ascii_digit())
            || token.starts_with('"')
            || token.starts_with('\'')
        {
            let literal: String = macro_lex::normalize_literal_token_spelling(token);

            let _ignored: Option<&'tokens str> = self.advance();

            return Ok(MacroExpr::Literal(literal));
        }

        if token
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        {
            let name: String = token.to_string();

            let _ignored: Option<&'tokens str> = self.advance();

            return Ok(MacroExpr::Ident(name));
        }

        Err(MacroLimit::ExpressionMalformed)
    }
}
