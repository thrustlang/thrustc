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

use crate::macro_error::MacroLimit;
use crate::macro_table::{MacroFunctionLikeKind, MacroTable};
use crate::macro_token::{MacroToken, MacroTokenKind};

#[derive(Debug)]
pub struct MacroExpansionContext {
    active_expansions: Vec<String>,
    depth: usize,
    max_depth: usize,
    max_rescan_passes: usize,
}

impl MacroExpansionContext {
    #[inline]
    pub fn new(max_depth: usize, max_rescan_passes: usize) -> Self {
        Self {
            active_expansions: Vec::new(),
            depth: 0,
            max_depth,
            max_rescan_passes,
        }
    }
}

impl MacroExpansionContext {
    #[inline]
    pub fn new_default() -> Self {
        Self::new(64, 128)
    }

    fn try_enter(&mut self, macro_name: &str) -> Result<(), MacroLimit> {
        if self.depth >= self.max_depth {
            return Err(MacroLimit::ExpansionDepthExceeded);
        }

        if self
            .active_expansions
            .iter()
            .any(|active| active.as_str() == macro_name)
        {
            return Err(MacroLimit::RecursiveExpansion);
        }

        self.depth = self.depth.saturating_add(1);
        self.active_expansions.push(macro_name.to_string());

        Ok(())
    }
}

impl MacroExpansionContext {
    fn leave(&mut self) {
        self.active_expansions.pop();

        self.depth = self.depth.saturating_sub(1);
    }
}

pub fn substitute_parameter_tokens(
    tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Option<Vec<MacroToken>> {
    if parameters.len() != args.len() {
        return None;
    }

    let mut out: Vec<MacroToken> = Vec::new();

    for token in tokens.iter() {
        if token.get_kind() == MacroTokenKind::Identifier {
            if let Some(index) = parameters
                .iter()
                .position(|parameter| parameter == token.get_text())
            {
                out.extend(args[index].iter().cloned());
                continue;
            }
        }

        out.push(token.clone());
    }

    Some(out)
}

pub fn parse_invocation_arguments(
    tokens: &[MacroToken],
    open_paren_index: usize,
) -> Result<(Vec<Vec<MacroToken>>, usize), MacroLimit> {
    if tokens
        .get(open_paren_index)
        .is_none_or(|token| token.get_text() != "(")
    {
        return Err(MacroLimit::ExpectedToken);
    }

    let mut depth: i32 = 1;
    let mut index: usize = open_paren_index + 1;
    let mut args: Vec<Vec<MacroToken>> = Vec::new();
    let mut current: Vec<MacroToken> = Vec::new();
    let mut saw_any_content: bool = false;

    while index < tokens.len() {
        let token: &MacroToken = &tokens[index];

        if token.get_kind() == MacroTokenKind::Punctuation && token.get_text() == "(" {
            depth += 1;
            current.push(token.clone());
            saw_any_content = true;
            index += 1;
            continue;
        }

        if token.get_kind() == MacroTokenKind::Punctuation && token.get_text() == ")" {
            depth -= 1;

            if depth == 0 {
                if saw_any_content || !current.is_empty() || !args.is_empty() {
                    args.push(current);
                }

                return Ok((args, index));
            }

            if depth < 0 {
                return Err(MacroLimit::TokenBalanceError);
            }

            current.push(token.clone());
            saw_any_content = true;
            index += 1;
            continue;
        }

        if token.get_kind() == MacroTokenKind::Punctuation && token.get_text() == "," && depth == 1
        {
            args.push(current);
            current = Vec::new();
            saw_any_content = true;
            index += 1;
            continue;
        }

        current.push(token.clone());

        if token.get_kind() != MacroTokenKind::Whitespace {
            saw_any_content = true;
        }

        index += 1;
    }

    Err(MacroLimit::UnexpectedEndOfTokens)
}

#[inline]
pub fn expand_function_tokens(
    tokens: &[MacroToken],
    table: &MacroTable,
    context: &mut MacroExpansionContext,
) -> Result<Vec<MacroToken>, MacroLimit> {
    self::rescan_tokens(tokens, table, context)
}

pub fn rescan_tokens(
    tokens: &[MacroToken],
    table: &MacroTable,
    context: &mut MacroExpansionContext,
) -> Result<Vec<MacroToken>, MacroLimit> {
    let mut current: Vec<MacroToken> = tokens.to_vec();

    let mut pass: usize = 0;

    while pass < context.max_rescan_passes {
        pass += 1;

        let mut changed: bool = false;
        let mut output: Vec<MacroToken> = Vec::new();
        let mut index: usize = 0;

        while index < current.len() {
            let token: &MacroToken = &current[index];

            if token.get_kind() != MacroTokenKind::Identifier {
                output.push(token.clone());
                index += 1;
                continue;
            }

            let Some(definition) = table.get_function_like_definition_cloned(token.get_text())
            else {
                if let Some(object_body) = table.get_object_macro_definition(token.get_text()) {
                    context.try_enter(token.get_text())?;

                    let rescanned_object: Result<Vec<MacroToken>, MacroLimit> =
                        self::rescan_tokens(&object_body, table, context);

                    context.leave();

                    output.extend(rescanned_object?);

                    index += 1;
                    changed = true;

                    continue;
                }

                output.push(token.clone());
                index += 1;
                continue;
            };

            let mut open_paren_index: usize = index + 1;

            while current
                .get(open_paren_index)
                .is_some_and(|next| next.get_kind() == MacroTokenKind::Whitespace)
            {
                open_paren_index += 1;
            }

            if current.get(open_paren_index).is_none_or(|next| {
                next.get_kind() != MacroTokenKind::Punctuation || next.get_text() != "("
            }) {
                output.push(token.clone());
                index += 1;
                continue;
            }

            if let MacroFunctionLikeKind::Unsupported(limit) = definition.get_kind() {
                return Err(limit);
            }

            let (raw_args, close_paren_index): (Vec<Vec<MacroToken>>, usize) =
                self::parse_invocation_arguments(&current, open_paren_index)?;

            if raw_args.len() != definition.get_parameters().len() {
                return Err(MacroLimit::ArgumentCountMismatch);
            }

            context.try_enter(definition.get_name())?;

            let rescanned_result: Result<Vec<MacroToken>, MacroLimit> = {
                let mut expanded_args: Vec<Vec<MacroToken>> = Vec::new();

                for arg in raw_args.iter() {
                    expanded_args.push(self::rescan_tokens(arg, table, context)?);
                }

                let substituted: Vec<MacroToken> = self::substitute_parameter_tokens(
                    definition.get_body(),
                    definition.get_parameters(),
                    &expanded_args,
                )
                .ok_or(MacroLimit::ArgumentCountMismatch)?;

                self::rescan_tokens(&substituted, table, context)
            };

            context.leave();

            let rescanned: Vec<MacroToken> = rescanned_result?;

            output.extend(rescanned);

            index = close_paren_index + 1;
            changed = true;
        }

        if !changed {
            return Ok(output);
        }

        current = output;
    }

    Err(MacroLimit::RescanLimitExceeded)
}
