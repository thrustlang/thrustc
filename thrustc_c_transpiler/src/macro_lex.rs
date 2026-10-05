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

pub(crate) fn extract_binary_operator(
    entity: &clang::Entity<'_>,
    left: &clang::Entity<'_>,
    right: &clang::Entity<'_>,
) -> Option<&'static str> {
    let range: clang::source::SourceRange<'_> = entity.get_range()?;
    let left_range: clang::source::SourceRange<'_> = left.get_range()?;
    let right_range: clang::source::SourceRange<'_> = right.get_range()?;

    let from_spelling: bool =
        self::is_from_macro_expansion(entity) && self::spelling_range_tokens(entity).is_some();

    let source_tokens: Vec<clang::token::Token<'_>> = if from_spelling {
        self::spelling_range_tokens(entity).unwrap_or_default()
    } else {
        range.tokenize()
    };

    let left_end: clang::source::Location<'_> = if from_spelling {
        left_range.get_end().get_spelling_location()
    } else {
        left_range.get_end().get_expansion_location()
    };

    let right_start: clang::source::Location<'_> = if from_spelling {
        right_range.get_start().get_spelling_location()
    } else {
        right_range.get_start().get_expansion_location()
    };

    if left_end.file == right_start.file {
        let operator_tokens: Vec<String> = source_tokens
            .iter()
            .filter_map(|token| {
                let location: clang::source::Location<'_> = if from_spelling {
                    token.get_location().get_spelling_location()
                } else {
                    token.get_location().get_expansion_location()
                };

                if location.file != left_end.file {
                    return None;
                }

                if location.offset < left_end.offset || location.offset >= right_start.offset {
                    return None;
                }

                Some(token.get_spelling())
            })
            .collect();

        if let Some(operator) = self::extract_binary_operator_from_tokens(&operator_tokens) {
            return Some(operator);
        }
    }

    let source_token_texts: Vec<String> = source_tokens
        .into_iter()
        .map(|token| token.get_spelling())
        .collect();

    let left_token_texts: Vec<String> = if from_spelling {
        self::spelling_range_tokens(left).map(|tokens| {
            tokens
                .into_iter()
                .map(|token| token.get_spelling())
                .collect()
        })
    } else {
        None
    }
    .unwrap_or_else(|| {
        left_range
            .tokenize()
            .into_iter()
            .map(|t| t.get_spelling())
            .collect()
    });

    let right_token_texts: Vec<String> = if from_spelling {
        self::spelling_range_tokens(right).map(|tokens| {
            tokens
                .into_iter()
                .map(|token| token.get_spelling())
                .collect()
        })
    } else {
        None
    }
    .unwrap_or_else(|| {
        right_range
            .tokenize()
            .into_iter()
            .map(|t| t.get_spelling())
            .collect()
    });

    if left_token_texts.is_empty() || source_token_texts.len() < left_token_texts.len() {
        return None;
    }

    let left_index: usize =
        (0..=source_token_texts.len() - left_token_texts.len()).find(|&index| {
            source_token_texts[index..index + left_token_texts.len()] == *left_token_texts
        })?;

    let start: usize = left_index + left_token_texts.len();

    if right_token_texts.is_empty()
        || source_token_texts.len() < right_token_texts.len()
        || start >= source_token_texts.len()
    {
        return None;
    }

    let end: usize =
        (start..=source_token_texts.len() - right_token_texts.len()).find(|&index| {
            source_token_texts[index..index + right_token_texts.len()] == *right_token_texts
        })?;

    self::extract_binary_operator_from_tokens(&source_token_texts[start..end])
}

pub(crate) fn needs_space_between_tokens(prev: &str, current: &str) -> bool {
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

pub(crate) fn extract_binary_operator_from_tokens(tokens: &[String]) -> Option<&'static str> {
    let mut depth: i32 = 0;

    let token_iter = tokens.iter();

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

pub(crate) fn tokens_to_thrust_source(tokens: &[clang::token::Token<'_>]) -> String {
    let mut out: String = String::new();
    let mut previous: Option<String> = None;

    for tk in tokens.iter() {
        let raw: String = tk.get_spelling();
        let s: String = match tk.get_kind() {
            clang::token::TokenKind::Identifier => {
                let __sanitized: String = raw.to_string();

                match __sanitized.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{__sanitized}_")
                    }

                    _ => __sanitized,
                }
            }
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

pub(crate) fn is_assignment_expression(entity: &clang::Entity<'_>) -> bool {
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

pub(crate) fn entity_spellings(entity: &clang::Entity<'_>) -> Vec<String> {
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

pub(crate) fn macro_type_name(key: &str) -> Option<String> {
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

pub(crate) fn spelling_range_tokens<'clang>(
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

pub(crate) fn range_spellings(
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

pub(crate) fn normalize_literal_token_spelling(spelling: &str) -> String {
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

pub(crate) fn is_from_macro_expansion(entity: &clang::Entity<'_>) -> bool {
    let Some(range) = entity.get_range() else {
        return false;
    };

    let expansion = range.get_start().get_expansion_location();
    let spelling = range.get_start().get_spelling_location();

    match (expansion.file, spelling.file) {
        (Some(expansion_file), Some(spelling_file)) => expansion_file != spelling_file,
        _ => false,
    }
}
