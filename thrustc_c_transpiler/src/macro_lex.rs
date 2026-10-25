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

use crate::macro_token::MacroToken;

pub fn lex_binary_operator(
    entity: &clang::Entity<'_>,
    left: &clang::Entity<'_>,
    right: &clang::Entity<'_>,
) -> Option<String> {
    // The operator always lives in the source gap between the left and right
    // operands. Resolve that gap directly so every operator (`+`, `<<`, `==`,
    // `&&`, compound assignments, ...) is covered without hardcoding a list.
    //
    // When the entity comes from a macro expansion, the lexeme locations
    // point into the macro definition (the only reliable text), so we prefer
    // them over the expansion locations which collapse onto the call site.
    let entity_range: clang::source::SourceRange<'_> = entity.get_range()?;
    let left_range: clang::source::SourceRange<'_> = left.get_range()?;
    let right_range: clang::source::SourceRange<'_> = right.get_range()?;

    let entity_start_lexeme: clang::source::Location<'_> =
        entity_range.get_start().get_spelling_location();

    let entity_start_expansion: clang::source::Location<'_> =
        entity_range.get_start().get_expansion_location();

    let use_lexeme: bool = entity_start_lexeme.file != entity_start_expansion.file
        || entity_start_lexeme.offset != entity_start_expansion.offset;

    let (operand_end, operand_start): (clang::source::Location<'_>, clang::source::Location<'_>) =
        if use_lexeme {
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
        && operand_end.line == operand_start.line
    {
        let gap: clang::source::SourceRange<'_> = clang::source::SourceRange::new(
            start_file.get_offset_location(operand_end.offset),
            start_file.get_offset_location(operand_start.offset),
        );

        let operator: Option<String> = gap
            .tokenize()
            .into_iter()
            .map(|token| token.get_spelling())
            .find(|lexeme| !lexeme.trim().is_empty());

        if operator.is_some() {
            return operator;
        }
    }

    // Fallback for the rare cases where the gap cannot be tokenized: recover
    // the operator from the entity text via the same gap reasoning.
    if use_lexeme {
        let lexeme_tokens: Vec<String> = self::lexeme_range_tokens(entity)
            .unwrap_or_default()
            .iter()
            .map(|token| token.get_spelling())
            .collect();

        return self::extract_binary_operator_from_tokens(&lexeme_tokens).map(str::to_string);
    }

    let left_end: clang::source::Location<'_> = left_range.get_end().get_expansion_location();

    let right_start: clang::source::Location<'_> = right_range.get_start().get_expansion_location();

    if left_end.file != right_start.file {
        return None;
    }

    let source_tokens: Vec<clang::token::Token<'_>> = entity_range.tokenize();

    let operator_tokens: Vec<String> = source_tokens
        .iter()
        .filter_map(|token| {
            let location: clang::source::Location<'_> =
                token.get_location().get_expansion_location();

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
            clang::token::TokenKind::Identifier => {
                crate::util::normalize_to_thrust_identifier(&raw)
            }
            clang::token::TokenKind::Literal => self::normalize_literal_token_lexeme(&raw),
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
        return self::entity_lexemes(entity)
            .iter()
            .any(|s| s == "++" || s == "--");
    }

    if !matches!(
        entity.get_kind(),
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator
    ) {
        return false;
    }

    let tokens: Vec<String> = self::entity_lexemes(entity);

    matches!(
        self::extract_binary_operator_from_tokens(&tokens),
        Some("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=")
    )
}

pub fn entity_lexemes(entity: &clang::Entity<'_>) -> Vec<String> {
    if self::is_lexeme_from_macro_expansion(entity) {
        if let Some(tokens) = self::lexeme_range_tokens(entity) {
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

pub fn lex_macro_type(key: &str) -> Option<String> {
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
        "_Bool" => Some("bool".to_string()),
        "float" => Some("f32".to_string()),
        "double" => Some("f64".to_string()),
        "void" => Some("void".to_string()),
        _ => None,
    }
}

pub fn lexeme_range_tokens<'clang>(
    entity: &clang::Entity<'clang>,
) -> Option<Vec<clang::token::Token<'clang>>> {
    let range: clang::source::SourceRange<'_> = entity.get_range()?;

    let lexeme_start: clang::source::Location<'_> = range.get_start().get_spelling_location();
    let lexeme_end: clang::source::Location<'_> = range.get_end().get_spelling_location();

    let start_file: clang::source::File<'_> = lexeme_start.file?;
    let end_file: clang::source::File<'_> = lexeme_end.file?;

    if start_file != end_file {
        return None;
    }

    let start: clang::source::SourceLocation<'_> =
        start_file.get_offset_location(lexeme_start.offset);
    let end: clang::source::SourceLocation<'_> = end_file.get_offset_location(lexeme_end.offset);

    Some(clang::source::SourceRange::new(start, end).tokenize())
}

pub fn range_lexemes(
    range: &clang::source::SourceRange<'_>,
    entity: &clang::Entity<'_>,
) -> Vec<String> {
    if self::is_lexeme_from_macro_expansion(entity) {
        if let Some(tokens) = self::lexeme_range_tokens(entity) {
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

pub fn normalize_literal_token_lexeme(lexeme: &str) -> String {
    if lexeme.starts_with('"') || lexeme.starts_with('\'') {
        return lexeme.to_string();
    }

    if lexeme.starts_with("0x") || lexeme.starts_with("0X") {
        return lexeme.trim_end_matches(['u', 'U', 'l', 'L']).to_string();
    }

    if lexeme.contains('.') || lexeme.contains('e') || lexeme.contains('E') {
        return lexeme.trim_end_matches(['f', 'F', 'l', 'L']).to_string();
    }

    lexeme.trim_end_matches(['u', 'U', 'l', 'L']).to_string()
}

pub fn is_lexeme_from_macro_expansion(entity: &clang::Entity<'_>) -> bool {
    let Some(range) = entity.get_range() else {
        return false;
    };

    let expansion: clang::source::Location<'_> = range.get_start().get_expansion_location();
    let lexeme: clang::source::Location<'_> = range.get_start().get_spelling_location();

    match (expansion.file, lexeme.file) {
        (Some(expansion_file), Some(lexeme_file)) => expansion_file != lexeme_file,
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
                    crate::util::normalize_to_thrust_identifier(&raw)
                }
                clang::token::TokenKind::Literal => self::normalize_literal_token_lexeme(&raw),
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

pub type SiteCallArguments = (
    Vec<Vec<crate::macro_token::MacroToken>>,
    Vec<(u32, u32)>,
    u32,
);

pub fn lex_site_call_arguments(site: &clang::Entity<'_>, name: &str) -> Option<SiteCallArguments> {
    let range: clang::source::SourceRange<'_> = site.get_range()?;
    let location: clang::source::Location<'_> = range.get_start().get_expansion_location();
    let file: clang::source::File<'_> = location.file?;
    let contents: String = std::fs::read_to_string(file.get_path()).ok()?;
    let bytes: &[u8] = contents.as_bytes();

    let mut offset: usize = location.offset as usize;

    if bytes.get(offset..offset + name.len()) != Some(name.as_bytes()) {
        return None;
    }

    offset += name.len();

    while bytes
        .get(offset)
        .is_some_and(|byte| byte.is_ascii_whitespace())
    {
        offset += 1;
    }

    if bytes.get(offset) != Some(&b'(') {
        return None;
    }

    offset += 1;

    let mut arg_ranges: Vec<(usize, usize)> = Vec::new();
    let mut argument_start: usize = offset;
    let mut depth: i32 = 1;

    while offset < bytes.len() {
        let byte: u8 = bytes[offset];

        if byte == b'"' || byte == b'\'' {
            offset = self::skip_c_string(bytes, offset);
            continue;
        }

        if byte == b'/' && bytes.get(offset + 1) == Some(&b'/') {
            while offset < bytes.len() && bytes[offset] != b'\n' {
                offset += 1;
            }

            continue;
        }

        if byte == b'/' && bytes.get(offset + 1) == Some(&b'*') {
            offset += 2;

            while offset + 1 < bytes.len() && !(bytes[offset] == b'*' && bytes[offset + 1] == b'/')
            {
                offset += 1;
            }

            offset += 2;

            continue;
        }

        if byte == b'(' || byte == b'[' || byte == b'{' {
            depth += 1;
            offset += 1;

            continue;
        }

        if byte == b')' || byte == b']' || byte == b'}' {
            depth -= 1;

            if depth == 0 && byte == b')' {
                offset += 1;
                break;
            }

            if depth < 1 {
                return None;
            }

            offset += 1;
            continue;
        }

        if byte == b',' && depth == 1 {
            arg_ranges.push((argument_start, offset));
            offset += 1;
            argument_start = offset;
            continue;
        }

        offset += 1;
    }

    if depth != 0 {
        return None;
    }

    if argument_start < offset - 1 {
        arg_ranges.push((argument_start, offset - 1));
    }

    let mut args: Vec<Vec<crate::macro_token::MacroToken>> = Vec::new();

    for (start, end) in arg_ranges.iter() {
        if start >= end {
            args.push(Vec::new());
            continue;
        }

        let start_offset: u32 = u32::try_from(*start).ok()?;
        let end_offset: u32 = u32::try_from(*end).ok()?;

        let start_location: clang::source::SourceLocation<'_> =
            file.get_offset_location(start_offset);
        let end_location: clang::source::SourceLocation<'_> = file.get_offset_location(end_offset);

        let arg_range: clang::source::SourceRange<'_> =
            clang::source::SourceRange::new(start_location, end_location);

        let tokens: Vec<clang::token::Token<'_>> = arg_range.tokenize();
        let macro_tokens: Vec<crate::macro_token::MacroToken> = self::to_macro_tokens(
            &tokens,
            crate::macro_token::MacroTokenOrigin::InvocationArgument,
        );

        args.push(macro_tokens);
    }

    let ranges: Vec<(u32, u32)> = arg_ranges
        .iter()
        .map(|(start, end)| Some((u32::try_from(*start).ok()?, u32::try_from(*end).ok()?)))
        .collect::<Option<Vec<(u32, u32)>>>()?;

    Some((args, ranges, u32::try_from(offset).ok()?))
}

pub fn get_argument_clang_types<'clang>(
    site: &clang::Entity<'clang>,
    arg_ranges: &[(u32, u32)],
) -> Vec<Option<clang::Type<'clang>>> {
    let Some(range) = site.get_range() else {
        return vec![None; arg_ranges.len()];
    };

    let Some(file) = range.get_start().get_expansion_location().file else {
        return vec![None; arg_ranges.len()];
    };

    let path: std::path::PathBuf = file.get_path();

    let mut best: Vec<(u32, Option<clang::Type<'clang>>)> = vec![(0, None); arg_ranges.len()];

    self::collect_clang_argument_types(site, &path, arg_ranges, &mut best);

    best.into_iter().map(|(_, ty)| ty).collect()
}

fn collect_clang_argument_types<'clang>(
    entity: &clang::Entity<'clang>,
    path: &std::path::Path,
    arg_ranges: &[(u32, u32)],
    best: &mut [(u32, Option<clang::Type<'clang>>)],
) {
    if let Some(range) = entity.get_range() {
        let start: clang::source::Location<'_> = range.get_start().get_spelling_location();
        let end: clang::source::Location<'_> = range.get_end().get_spelling_location();

        let same_file: bool =
            start
                .file
                .as_ref()
                .zip(end.file.as_ref())
                .is_some_and(|(start_file, end_file)| {
                    start_file.get_path() == path && end_file.get_path() == path
                });

        if same_file {
            let span: u32 = end.offset.saturating_sub(start.offset);

            for (index, (arg_start, arg_end)) in arg_ranges.iter().enumerate() {
                let inside: bool = start.offset >= *arg_start && end.offset <= *arg_end;

                if inside && span > best[index].0 {
                    best[index] = (span, entity.get_type());
                }
            }
        }
    }

    for child in entity.get_children() {
        self::collect_clang_argument_types(&child, path, arg_ranges, best);
    }
}

pub fn is_parameter_token_written(tokens: &[MacroToken], index: usize) -> bool {
    let assign_ops: [&str; 11] = [
        "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "|=", "^=",
    ];

    let next = tokens
        .get(index.saturating_add(1))
        .map(|token| token.get_text());

    if next.is_some_and(|text| text == "++" || text == "--" || assign_ops.contains(&text)) {
        return true;
    }

    let prev = index
        .checked_sub(1)
        .and_then(|prev_index| tokens.get(prev_index))
        .map(|token| token.get_text());

    if prev.is_some_and(|text| text == "++" || text == "--") {
        return true;
    }

    if prev == Some("(") && next == Some(")") {
        let assign_after_paren: Option<&str> = tokens
            .get(index.saturating_add(2))
            .map(|token| token.get_text());

        if assign_after_paren.is_some_and(|text| assign_ops.contains(&text)) {
            return true;
        }
    }

    false
}

pub fn is_macro_calling_other_statements(
    body: &[MacroToken],
    statement_names: &[String],
    own_name: &str,
) -> bool {
    let mut index: usize = 0;

    while index < body.len() {
        let token: &str = body[index].get_text();

        if token
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
            && body
                .get(index + 1)
                .is_some_and(|next| next.get_text() == "(")
            && token != own_name
            && !matches!(
                token,
                "if" | "for" | "while" | "switch" | "return" | "sizeof" | "do"
            )
            && statement_names.iter().any(|name| name == token)
        {
            return true;
        }

        index += 1;
    }

    false
}

fn skip_c_string(bytes: &[u8], start: usize) -> usize {
    let quote: u8 = bytes[start];
    let mut offset: usize = start + 1;

    while offset < bytes.len() {
        let inner: u8 = bytes[offset];

        offset += 1;

        if inner == b'\\' {
            offset += 1;
            continue;
        }

        if inner == quote {
            break;
        }
    }

    offset
}
