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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroTokenKind {
    Identifier,
    Literal,
    Punctuation,
    Keyword,
    Whitespace,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroTokenOrigin {
    DefinitionBody,
    InvocationArgument,
    ExpansionSpelling,
    FallbackLexed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroSpan {
    file: Option<String>,
    line: u32,
    column: u32,
    offset: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroToken {
    kind: MacroTokenKind,
    text: String,
    span: Option<MacroSpan>,
    origin: MacroTokenOrigin,
}

impl MacroSpan {
    #[inline]
    pub fn new(file: Option<String>, line: u32, column: u32, offset: u32) -> Self {
        Self {
            file,
            line,
            column,
            offset,
        }
    }
}

impl MacroSpan {
    #[inline]
    pub fn get_file(&self) -> Option<&String> {
        self.file.as_ref()
    }

    #[inline]
    pub fn get_line(&self) -> u32 {
        self.line
    }

    #[inline]
    pub fn get_column(&self) -> u32 {
        self.column
    }

    #[inline]
    pub fn get_offset(&self) -> u32 {
        self.offset
    }
}

impl MacroSpan {
    #[inline]
    pub fn get_mut_file(&mut self) -> &mut Option<String> {
        &mut self.file
    }

    #[inline]
    pub fn get_mut_line(&mut self) -> &mut u32 {
        &mut self.line
    }

    #[inline]
    pub fn get_mut_column(&mut self) -> &mut u32 {
        &mut self.column
    }

    #[inline]
    pub fn get_mut_offset(&mut self) -> &mut u32 {
        &mut self.offset
    }
}

impl MacroToken {
    #[inline]
    pub fn new(kind: MacroTokenKind, text: String, origin: MacroTokenOrigin) -> Self {
        Self {
            kind,
            text,
            span: None,
            origin,
        }
    }
}

impl MacroToken {
    #[inline]
    pub fn get_kind(&self) -> MacroTokenKind {
        self.kind
    }

    #[inline]
    pub fn get_text(&self) -> &str {
        &self.text
    }

    #[inline]
    pub fn get_span(&self) -> Option<&MacroSpan> {
        self.span.as_ref()
    }

    #[inline]
    pub fn get_origin(&self) -> MacroTokenOrigin {
        self.origin
    }
}

impl MacroToken {
    #[inline]
    pub fn set_span(&mut self, span: Option<MacroSpan>) {
        self.span = span;
    }
}

impl MacroToken {
    #[inline]
    pub fn get_mut_kind(&mut self) -> &mut MacroTokenKind {
        &mut self.kind
    }

    #[inline]
    pub fn get_mut_text(&mut self) -> &mut String {
        &mut self.text
    }

    #[inline]
    pub fn get_mut_span(&mut self) -> &mut Option<MacroSpan> {
        &mut self.span
    }

    #[inline]
    pub fn get_mut_origin(&mut self) -> &mut MacroTokenOrigin {
        &mut self.origin
    }
}

#[inline]
pub fn texts(tokens: &[MacroToken]) -> Vec<String> {
    tokens
        .iter()
        .map(|token| token.get_text().to_string())
        .collect()
}

#[inline]
pub fn from_spellings(spellings: &[String], origin: MacroTokenOrigin) -> Vec<MacroToken> {
    spellings
        .iter()
        .map(|text| MacroToken::new(classify_text(text), text.clone(), origin))
        .collect()
}

#[inline]
pub fn classify_text(text: &str) -> MacroTokenKind {
    if text.trim().is_empty() {
        return MacroTokenKind::Whitespace;
    }

    if text
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
    {
        return if is_keyword(text) {
            MacroTokenKind::Keyword
        } else {
            MacroTokenKind::Identifier
        };
    }

    if text.starts_with('"')
        || text.starts_with('\'')
        || text.chars().next().is_some_and(|ch| ch.is_ascii_digit())
    {
        return MacroTokenKind::Literal;
    }

    if is_punctuation(text) {
        return MacroTokenKind::Punctuation;
    }

    MacroTokenKind::Unknown
}

#[inline]
fn is_keyword(text: &str) -> bool {
    matches!(
        text,
        "if" | "else"
            | "for"
            | "while"
            | "do"
            | "switch"
            | "case"
            | "default"
            | "return"
            | "break"
            | "continue"
            | "goto"
            | "sizeof"
            | "typedef"
            | "const"
            | "volatile"
            | "struct"
            | "enum"
            | "union"
    )
}

#[inline]
fn is_punctuation(text: &str) -> bool {
    matches!(
        text,
        "(" | ")"
            | "["
            | "]"
            | "{"
            | "}"
            | ","
            | ";"
            | ":"
            | "."
            | "->"
            | "?"
            | "="
            | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "&="
            | "|="
            | "^="
            | "<<="
            | ">>="
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "&&"
            | "||"
            | "+"
            | "-"
            | "*"
            | "/"
            | "%"
            | "&"
            | "|"
            | "^"
            | "<<"
            | ">>"
            | "!"
            | "~"
            | "++"
            | "--"
            | "#"
            | "##"
    )
}
