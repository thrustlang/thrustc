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

pub fn validate_variadic_body(
    tokens: &[crate::macro_token::MacroToken],
    variadic_parameter: &str,
) -> Result<(), crate::macro_error::MacroLimit> {
    let mut index: usize = 0;

    while index < tokens.len() {
        if tokens[index].get_text() == "__VA_OPT__" {
            let mut open: usize = index + 1;

            while open < tokens.len()
                && tokens[open].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
            {
                open += 1;
            }

            if tokens.get(open).is_none_or(|token| token.get_text() != "(") {
                return Err(crate::macro_error::MacroLimit::VariadicOptionMalformed);
            }

            let mut depth: i32 = 1;
            let mut close: usize = open + 1;

            while close < tokens.len() {
                if tokens[close].get_text() == "(" {
                    depth += 1;
                } else if tokens[close].get_text() == ")" {
                    depth -= 1;

                    if depth == 0 {
                        break;
                    }
                }

                close += 1;
            }

            if close >= tokens.len() {
                return Err(crate::macro_error::MacroLimit::VariadicOptionMalformed);
            }

            index = close + 1;

            continue;
        }

        if tokens[index].get_text() != "##" {
            index += 1;

            continue;
        }

        let mut previous: Option<usize> = None;

        if index > 0 {
            let mut probe: usize = index - 1;

            loop {
                if tokens[probe].get_kind() != crate::macro_token::MacroTokenKind::Whitespace {
                    previous = Some(probe);
                    break;
                }

                if probe == 0 {
                    break;
                }

                probe -= 1;
            }
        }

        let mut next: usize = index + 1;

        while next < tokens.len()
            && tokens[next].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
        {
            next += 1;
        }

        let previous_is_comma: bool =
            previous.is_some_and(|position| tokens[position].get_text() == ",");
        let next_is_variadic: bool = tokens
            .get(next)
            .is_some_and(|token| token.get_text() == variadic_parameter);

        if !previous_is_comma || !next_is_variadic {
            return Err(crate::macro_error::MacroLimit::TokenPasting);
        }

        index += 1;
    }

    Ok(())
}

pub fn normalize_variadic_body_for_classification(
    tokens: &[crate::macro_token::MacroToken],
) -> Vec<crate::macro_token::MacroToken> {
    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();
    let mut index: usize = 0;

    while index < tokens.len() {
        let token: &crate::macro_token::MacroToken = &tokens[index];

        if token.get_text() == "##" {
            index += 1;

            continue;
        }

        if token.get_text() != "__VA_OPT__" {
            out.push(token.clone());
            index += 1;

            continue;
        }

        let mut open: usize = index + 1;

        while open < tokens.len()
            && tokens[open].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
        {
            open += 1;
        }

        if tokens.get(open).is_none_or(|token| token.get_text() != "(") {
            out.push(token.clone());
            index += 1;

            continue;
        }

        let mut depth: i32 = 1;
        let mut close: usize = open + 1;

        while close < tokens.len() {
            if tokens[close].get_text() == "(" {
                depth += 1;
            } else if tokens[close].get_text() == ")" {
                depth -= 1;

                if depth == 0 {
                    break;
                }
            }

            close += 1;
        }

        if close >= tokens.len() {
            out.push(token.clone());
            index += 1;

            continue;
        }

        out.extend(tokens[open + 1..close].iter().cloned());

        index = close + 1;
    }

    out
}

fn apply_gnu_comma_swallow(
    tokens: &[crate::macro_token::MacroToken],
    variadic_parameter: &str,
    has_variadic_content: bool,
) -> Vec<crate::macro_token::MacroToken> {
    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();
    let mut index: usize = 0;

    while index < tokens.len() {
        if tokens[index].get_text() != "," {
            out.push(tokens[index].clone());
            index += 1;

            continue;
        }

        let mut hash_index: usize = index + 1;

        while hash_index < tokens.len()
            && tokens[hash_index].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
        {
            hash_index += 1;
        }

        if tokens
            .get(hash_index)
            .is_none_or(|token| token.get_text() != "##")
        {
            out.push(tokens[index].clone());
            index += 1;

            continue;
        }

        let mut name_index: usize = hash_index + 1;

        while name_index < tokens.len()
            && tokens[name_index].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
        {
            name_index += 1;
        }

        if tokens
            .get(name_index)
            .is_none_or(|token| token.get_text() != variadic_parameter)
        {
            out.push(tokens[index].clone());
            index += 1;

            continue;
        }

        if has_variadic_content {
            out.push(tokens[index].clone());
        }

        out.extend(tokens[index + 1..hash_index].iter().cloned());

        index = hash_index + 1;
    }

    out
}

fn expand_va_opt(
    tokens: &[crate::macro_token::MacroToken],
    has_variadic_content: bool,
) -> Option<Vec<crate::macro_token::MacroToken>> {
    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();
    let mut index: usize = 0;

    while index < tokens.len() {
        if tokens[index].get_text() != "__VA_OPT__" {
            out.push(tokens[index].clone());
            index += 1;

            continue;
        }

        let mut open: usize = index + 1;

        while open < tokens.len()
            && tokens[open].get_kind() == crate::macro_token::MacroTokenKind::Whitespace
        {
            open += 1;
        }

        if tokens.get(open).is_none_or(|token| token.get_text() != "(") {
            return None;
        }

        let mut depth: i32 = 1;
        let mut close: usize = open + 1;

        while close < tokens.len() {
            if tokens[close].get_text() == "(" {
                depth += 1;
            } else if tokens[close].get_text() == ")" {
                depth -= 1;

                if depth == 0 {
                    break;
                }
            }

            close += 1;
        }

        if close >= tokens.len() {
            return None;
        }

        if has_variadic_content {
            out.extend(tokens[open + 1..close].iter().cloned());
        }

        index = close + 1;
    }

    Some(out)
}

pub fn substitute_variadic_parameter_tokens(
    tokens: &[crate::macro_token::MacroToken],
    fixed_parameters: &[String],
    variadic_parameter: &str,
    fixed_args: &[Vec<crate::macro_token::MacroToken>],
    variadic_args: &[Vec<crate::macro_token::MacroToken>],
    has_variadic_content: bool,
) -> Option<Vec<crate::macro_token::MacroToken>> {
    if fixed_parameters.len() != fixed_args.len() {
        return None;
    }

    let expanded_variadic: Vec<crate::macro_token::MacroToken> =
        self::join_variadic_arguments(variadic_args);

    let mut expanded_tokens: Vec<crate::macro_token::MacroToken> =
        self::expand_va_opt(tokens, has_variadic_content)?;

    expanded_tokens =
        self::apply_gnu_comma_swallow(&expanded_tokens, variadic_parameter, has_variadic_content);

    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();

    for token in expanded_tokens.iter() {
        if token.get_text() == "##" {
            return None;
        }

        if token.get_kind() == crate::macro_token::MacroTokenKind::Identifier {
            if let Some(index) = fixed_parameters
                .iter()
                .position(|parameter| parameter == token.get_text())
            {
                out.extend(fixed_args[index].iter().cloned());

                continue;
            }

            if token.get_text() == variadic_parameter {
                out.extend(expanded_variadic.iter().cloned());

                continue;
            }
        }

        out.push(token.clone());
    }

    Some(out)
}

pub fn join_variadic_arguments(
    args: &[Vec<crate::macro_token::MacroToken>],
) -> Vec<crate::macro_token::MacroToken> {
    let mut out: Vec<crate::macro_token::MacroToken> = Vec::new();

    for (index, argument) in args.iter().enumerate() {
        if index != 0 {
            out.push(crate::macro_token::MacroToken::new(
                crate::macro_token::MacroTokenKind::Punctuation,
                ",".to_string(),
                crate::macro_token::MacroTokenOrigin::DefinitionBody,
            ));
        }

        out.extend(argument.iter().cloned());
    }

    out
}
