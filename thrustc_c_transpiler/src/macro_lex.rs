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

pub fn extract_binary_operator(
    entity: &clang::Entity<'_>,
    left: &clang::Entity<'_>,
    right: &clang::Entity<'_>,
) -> Option<String> {
    // The operator always lives in the source gap between the left and right
    // operands. Resolve that gap directly so every operator (`+`, `<<`, `==`,
    // `&&`, compound assignments, ...) is covered without hardcoding a list.
    //
    // When the entity comes from a macro expansion, the spelling locations
    // point into the macro definition (the only reliable text), so we prefer
    // them over the expansion locations which collapse onto the call site.
    let entity_range: clang::source::SourceRange<'_> = entity.get_range()?;
    let left_range: clang::source::SourceRange<'_> = left.get_range()?;
    let right_range: clang::source::SourceRange<'_> = right.get_range()?;

    let entity_start_spelling: clang::source::Location<'_> =
        entity_range.get_start().get_spelling_location();

    let entity_start_expansion: clang::source::Location<'_> =
        entity_range.get_start().get_expansion_location();

    let use_spelling: bool = entity_start_spelling.file != entity_start_expansion.file
        || entity_start_spelling.offset != entity_start_expansion.offset;

    let (operand_end, operand_start): (clang::source::Location<'_>, clang::source::Location<'_>) =
        if use_spelling {
            (
                left_range.get_end().get_spelling_location(),
                right_range.get_start().get_spelling_location(),
            )
        } else {
            (
                left_range.get_end().get_expansion_location(),
                right_range.get_start().get_expansion_location(),
            )
        };

    if let (Some(end_file), Some(start_file)) = (operand_end.file, operand_start.file)
        && end_file == start_file
        && operand_end.offset <= operand_start.offset
    {
        let gap: clang::source::SourceRange<'_> = clang::source::SourceRange::new(
            start_file.get_offset_location(operand_end.offset),
            start_file.get_offset_location(operand_start.offset),
        );

        let operator: Option<String> = gap
            .tokenize()
            .into_iter()
            .map(|token| token.get_spelling())
            .find(|spelling| !spelling.trim().is_empty());

        if operator.is_some() {
            return operator;
        }
    }

    // Fallback for the rare cases where the gap cannot be tokenized: recover
    // the operator from the entity text via the same gap reasoning.
    let left_end: clang::source::Location<'_> = if use_spelling {
        left_range.get_end().get_spelling_location()
    } else {
        left_range.get_end().get_expansion_location()
    };

    let right_start: clang::source::Location<'_> = if use_spelling {
        right_range.get_start().get_spelling_location()
    } else {
        right_range.get_start().get_expansion_location()
    };

    if left_end.file != right_start.file {
        return None;
    }

    let source_tokens: Vec<clang::token::Token<'_>> = if use_spelling {
        self::spelling_range_tokens(entity).unwrap_or_default()
    } else {
        entity_range.tokenize()
    };

    let operator_tokens: Vec<String> = source_tokens
        .iter()
        .filter_map(|token| {
            let location: clang::source::Location<'_> = if use_spelling {
                token.get_location().get_spelling_location()
            } else {
                token.get_location().get_expansion_location()
            };

            if location.file != left_end.file
                || location.offset < left_end.offset
                || location.offset >= right_start.offset
            {
                return None;
            }

            Some(token.get_spelling())
        })
        .collect();

    self::extract_binary_operator_from_tokens(&operator_tokens).map(str::to_string)
}

pub fn needs_space_between_tokens(prev: &str, current: &str) -> bool {
    if current == ")" || current == "]" || current == ";" || current == "," {
        return false;
    }

    if prev == "(" || prev == "[" || prev == "!" || prev == "~" {
        return false;
    }

    let prev_ident: bool = prev
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

    let current_ident: bool = current
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

    if prev_ident && current_ident {
        return true;
    }

    let prev_is_operator: bool = matches!(
        prev,
        "=" | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "&="
            | "|="
            | "^="
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
            | "<<="
            | ">>="
    );

    let current_is_operator: bool = matches!(
        current,
        "=" | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "&="
            | "|="
            | "^="
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
            | "<<="
            | ">>="
    );

    if prev_is_operator || current_is_operator {
        return true;
    }

    false
}

pub fn extract_binary_operator_from_tokens(tokens: &[String]) -> Option<&'static str> {
    let mut depth: i32 = 0;

    let token_iter: std::slice::Iter<'_, String> = tokens.iter();

    for t in token_iter {
        match t.as_str() {
            "(" | "[" | "{" => {
                depth += 1;
            }
            ")" | "]" | "}" => {
                depth -= 1;
            }
            _ if depth == 0 => {
                let op: Option<&'static str> = match t.as_str() {
                    "=" => Some("="),
                    "+=" => Some("+="),
                    "-=" => Some("-="),
                    "*=" => Some("*="),
                    "/=" => Some("/="),
                    "%=" => Some("%="),
                    "&=" => Some("&="),
                    "|=" => Some("|="),
                    "^=" => Some("^="),
                    "==" => Some("=="),
                    "!=" => Some("!="),
                    "<" => Some("<"),
                    "<=" => Some("<="),
                    ">" => Some(">"),
                    ">=" => Some(">="),
                    "&&" => Some("&&"),
                    "||" => Some("||"),
                    "+" => Some("+"),
                    "-" => Some("-"),
                    "*" => Some("*"),
                    "/" => Some("/"),
                    "%" => Some("%"),
                    "&" => Some("&"),
                    "|" => Some("|"),
                    "^" => Some("^"),
                    "<<" => Some("<<"),
                    ">>" => Some(">>"),
                    "<<=" => Some("<<="),
                    ">>=" => Some(">>="),
                    "," => Some(","),
                    _ => None,
                };

                if op.is_some() {
                    return op;
                }
            }
            _ => {}
        }
    }

    None
}

pub fn tokens_to_thrust_source(tokens: &[clang::token::Token<'_>]) -> String {
    let mut out: String = String::new();
    let mut previous: Option<String> = None;

    for tk in tokens.iter() {
        let raw: String = tk.get_spelling();
        let s: String = match tk.get_kind() {
            clang::token::TokenKind::Identifier => crate::util::sanitize_thrust_identifier(&raw),
            clang::token::TokenKind::Literal => self::normalize_literal_token_spelling(&raw),
            clang::token::TokenKind::Punctuation if raw == "." => "->".into(),
            _ => raw,
        };

        if previous
            .as_deref()
            .is_some_and(|prev| self::needs_space_between_tokens(prev, &s))
        {
            out.push(' ');
        }

        out.push_str(&s);
        previous = Some(s);
    }

    out
}

pub fn is_assignment_expression(entity: &clang::Entity<'_>) -> bool {
    if entity.get_kind() == clang::EntityKind::UnaryOperator {
        return self::entity_spellings(entity)
            .iter()
            .any(|s| s == "++" || s == "--");
    }

    if !matches!(
        entity.get_kind(),
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator
    ) {
        return false;
    }

    let tokens: Vec<String> = self::entity_spellings(entity);

    matches!(
        self::extract_binary_operator_from_tokens(&tokens),
        Some("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=")
    )
}

pub fn entity_spellings(entity: &clang::Entity<'_>) -> Vec<String> {
    if self::is_from_macro_expansion(entity) {
        if let Some(tokens) = self::spelling_range_tokens(entity) {
            return tokens
                .into_iter()
                .map(|token| token.get_spelling())
                .collect();
        }
    }

    entity
        .get_range()
        .map(|range| {
            range
                .tokenize()
                .into_iter()
                .map(|token| token.get_spelling())
                .collect()
        })
        .unwrap_or_default()
}

pub fn macro_type_name(key: &str) -> Option<String> {
    match key {
        "int" | "signed" | "signed int" => Some("s32".to_string()),
        "unsigned" | "unsigned int" => Some("u32".to_string()),
        "long" | "long int" | "signed long" | "signed long int" => Some("ssize".to_string()),
        "unsigned long" | "unsigned long int" => Some("usize".to_string()),
        "long long" | "signed long long" | "signed long long int" | "long long int" => {
            Some("s64".to_string())
        }
        "unsigned long long" | "unsigned long long int" => Some("u64".to_string()),
        "short" | "short int" | "signed short" | "signed short int" => Some("s16".to_string()),
        "unsigned short" | "unsigned short int" => Some("u16".to_string()),
        "char" => Some("char".to_string()),
        "signed char" => Some("s8".to_string()),
        "unsigned char" => Some("u8".to_string()),
        "float" => Some("f32".to_string()),
        "double" => Some("f64".to_string()),
        "void" => Some("void".to_string()),
        _ => None,
    }
}

pub fn spelling_range_tokens<'clang>(
    entity: &clang::Entity<'clang>,
) -> Option<Vec<clang::token::Token<'clang>>> {
    let range: clang::source::SourceRange<'_> = entity.get_range()?;

    let spelling_start: clang::source::Location<'_> = range.get_start().get_spelling_location();
    let spelling_end: clang::source::Location<'_> = range.get_end().get_spelling_location();

    let start_file: clang::source::File<'_> = spelling_start.file?;
    let end_file: clang::source::File<'_> = spelling_end.file?;

    if start_file != end_file {
        return None;
    }

    let start: clang::source::SourceLocation<'_> =
        start_file.get_offset_location(spelling_start.offset);
    let end: clang::source::SourceLocation<'_> = end_file.get_offset_location(spelling_end.offset);

    Some(clang::source::SourceRange::new(start, end).tokenize())
}

pub fn range_spellings(
    range: &clang::source::SourceRange<'_>,
    entity: &clang::Entity<'_>,
) -> Vec<String> {
    if self::is_from_macro_expansion(entity) {
        if let Some(tokens) = self::spelling_range_tokens(entity) {
            return tokens
                .into_iter()
                .map(|token| token.get_spelling())
                .collect();
        }
    }

    range
        .tokenize()
        .into_iter()
        .map(|token| token.get_spelling())
        .collect()
}

pub fn normalize_literal_token_spelling(spelling: &str) -> String {
    if spelling.starts_with('"') || spelling.starts_with('\'') {
        return spelling.to_string();
    }

    if spelling.starts_with("0x") || spelling.starts_with("0X") {
        return spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string();
    }

    if spelling.contains('.') || spelling.contains('e') || spelling.contains('E') {
        return spelling.trim_end_matches(['f', 'F', 'l', 'L']).to_string();
    }

    spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string()
}

pub fn is_from_macro_expansion(entity: &clang::Entity<'_>) -> bool {
    let Some(range) = entity.get_range() else {
        return false;
    };

    let expansion: clang::source::Location<'_> = range.get_start().get_expansion_location();
    let spelling: clang::source::Location<'_> = range.get_start().get_spelling_location();

    match (expansion.file, spelling.file) {
        (Some(expansion_file), Some(spelling_file)) => expansion_file != spelling_file,
        _ => false,
    }
}

pub fn normalize_line_continuations(text: &str) -> String {
    let mut out: String = String::with_capacity(text.len());

    let mut chars: std::iter::Peekable<std::str::Chars<'_>> = text.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if chars.peek() == Some(&'\n') {
                chars.next();
                continue;
            }

            if chars.peek() == Some(&'\r') {
                chars.next();

                if chars.peek() == Some(&'\n') {
                    chars.next();
                }

                continue;
            }
        }

        out.push(ch);
    }

    out
}

pub fn to_macro_tokens(
    tokens: &[clang::token::Token<'_>],
    origin: crate::macro_token::MacroTokenOrigin,
) -> Vec<crate::macro_token::MacroToken> {
    tokens
        .iter()
        .map(|token| {
            let raw: String = token.get_spelling();

            let had_continuation: bool = raw.contains("\\\n") || raw.contains("\\\r\n");

            let raw: String = self::normalize_line_continuations(&raw);

            let text: String = match token.get_kind() {
                clang::token::TokenKind::Identifier => {
                    crate::util::sanitize_thrust_identifier(&raw)
                }
                clang::token::TokenKind::Literal => self::normalize_literal_token_spelling(&raw),
                clang::token::TokenKind::Punctuation if raw == "." => "->".to_string(),
                _ => raw,
            };

            let kind: crate::macro_token::MacroTokenKind = if had_continuation {
                crate::macro_token::classify_text(&text)
            } else {
                match token.get_kind() {
                    clang::token::TokenKind::Identifier => crate::macro_token::classify_text(&text),
                    clang::token::TokenKind::Literal => crate::macro_token::MacroTokenKind::Literal,
                    clang::token::TokenKind::Punctuation => {
                        crate::macro_token::MacroTokenKind::Punctuation
                    }
                    clang::token::TokenKind::Keyword => crate::macro_token::MacroTokenKind::Keyword,
                    _ => crate::macro_token::MacroTokenKind::Unknown,
                }
            };

            let location: clang::source::Location<'_> =
                token.get_location().get_spelling_location();

            let span: crate::macro_token::MacroSpan = crate::macro_token::MacroSpan::new(
                location
                    .file
                    .map(|file| file.get_path().display().to_string()),
                location.line,
                location.column,
                location.offset,
            );

            let mut token: crate::macro_token::MacroToken =
                crate::macro_token::MacroToken::new(kind, text, origin);

            token.set_span(Some(span));

            token
        })
        .collect()
}

pub fn lex_text_to_macro_tokens(
    source: &str,
    origin: crate::macro_token::MacroTokenOrigin,
) -> Vec<crate::macro_token::MacroToken> {
    let bytes: &[u8] = source.as_bytes();
    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();
    let mut index: usize = 0;

    while index < bytes.len() {
        let ch: u8 = bytes[index];

        if ch == b'\\' {
            if bytes.get(index + 1) == Some(&b'\n') {
                index += 2;
                continue;
            }

            if bytes.get(index + 1) == Some(&b'\r') && bytes.get(index + 2) == Some(&b'\n') {
                index += 3;
                continue;
            }
        }

        if ch.is_ascii_whitespace() {
            let start: usize = index;

            while index < bytes.len() && bytes[index].is_ascii_whitespace() {
                index += 1;
            }

            out.push(crate::macro_token::MacroToken::new(
                crate::macro_token::MacroTokenKind::Whitespace,
                source[start..index].to_string(),
                origin,
            ));
            continue;
        }

        if ch.is_ascii_alphabetic() || ch == b'_' {
            let start: usize = index;

            index += 1;

            while index < bytes.len()
                && (bytes[index].is_ascii_alphanumeric() || bytes[index] == b'_')
            {
                index += 1;
            }

            let text: String = source[start..index].to_string();
            out.push(crate::macro_token::MacroToken::new(
                crate::macro_token::classify_text(&text),
                text,
                origin,
            ));
            continue;
        }

        if ch.is_ascii_digit() {
            let start: usize = index;

            index += 1;

            while index < bytes.len()
                && (bytes[index].is_ascii_alphanumeric()
                    || bytes[index] == b'_'
                    || bytes[index] == b'.')
            {
                index += 1;
            }

            let text: String = source[start..index].to_string();
            out.push(crate::macro_token::MacroToken::new(
                crate::macro_token::MacroTokenKind::Literal,
                text,
                origin,
            ));
            continue;
        }

        if ch == b'"' || ch == b'\'' {
            let quote: u8 = ch;
            let start: usize = index;

            index += 1;

            while index < bytes.len() {
                let inner: u8 = bytes[index];

                index += 1;

                if inner == b'\\' {
                    index += 1;
                    continue;
                }

                if inner == quote {
                    break;
                }
            }

            out.push(crate::macro_token::MacroToken::new(
                crate::macro_token::MacroTokenKind::Literal,
                source[start..index.min(bytes.len())].to_string(),
                origin,
            ));
            continue;
        }

        if index + 1 < bytes.len() {
            let two: &str = &source[index..index + 2];
            if crate::macro_token::classify_text(two)
                == crate::macro_token::MacroTokenKind::Punctuation
            {
                out.push(crate::macro_token::MacroToken::new(
                    crate::macro_token::MacroTokenKind::Punctuation,
                    two.to_string(),
                    origin,
                ));
                index += 2;
                continue;
            }
        }

        let one: &str = &source[index..index + 1];
        out.push(crate::macro_token::MacroToken::new(
            crate::macro_token::classify_text(one),
            one.to_string(),
            origin,
        ));
        index += 1;
    }

    out
}

pub fn detokenize_macro_tokens(tokens: &[crate::macro_token::MacroToken]) -> String {
    let mut out: String = String::new();

    for (index, token) in tokens.iter().enumerate() {
        if token.get_kind() == crate::macro_token::MacroTokenKind::Whitespace {
            out.push_str(token.get_text());
            continue;
        }

        if index > 0 {
            let previous: &crate::macro_token::MacroToken = &tokens[index - 1];

            if previous.get_kind() != crate::macro_token::MacroTokenKind::Whitespace
                && self::needs_space_between_tokens(previous.get_text(), token.get_text())
            {
                out.push(' ');
            }
        }

        out.push_str(token.get_text());
    }

    out
}
