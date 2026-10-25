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

use std::path::PathBuf;

use thrustc_c_transpiler_macro::macro_context::MacroContext;
use thrustc_c_transpiler_macro::macro_error::MacroLimit;
use thrustc_c_transpiler_macro::macro_expand::MacroExpansionContext;
use thrustc_c_transpiler_macro::macro_parser::MacroParser;
use thrustc_c_transpiler_macro::macro_table::{
    MacroFunctionLikeDefinition, MacroFunctionLikeKind, MacroTable,
};
use thrustc_c_transpiler_macro::macro_token::{MacroToken, MacroTokenOrigin};

use crate::error::MacroExpansionError;
use crate::options::Options;
use crate::types::{Macro, MacroExpansion, MacroType, Variadic};

pub fn expand_expression(
    macros: &[Macro],
    name: &str,
    arguments: &[String],
    options: &Options,
) -> Result<MacroExpansion, MacroExpansionError> {
    let table: MacroTable = self::build_macro_table(macros);

    let Some(definition): Option<MacroFunctionLikeDefinition> =
        table.get_function_like_definition_cloned(name)
    else {
        return Err(MacroExpansionError::UnknownMacro(name.to_string()));
    };

    if definition.get_kind() != MacroFunctionLikeKind::PureFunction {
        return Err(MacroExpansionError::Malformed(format!(
            "The C macro '{name}' is not a pure expression macro."
        )));
    }

    let substituted: Vec<MacroToken> = self::substitute_arguments(&definition, arguments)?;

    let mut expansion_context: MacroExpansionContext = MacroExpansionContext::new(
        options.get_max_expansion_depth(),
        options.get_max_rescan_passes(),
    );

    let expanded: Vec<MacroToken> = thrustc_c_transpiler_macro::macro_expand::expand_function_tokens(
        &substituted,
        &table,
        &mut expansion_context,
    )
    .map_err(|limit: MacroLimit| {
        let (detail, help) =
            thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

        MacroExpansionError::ExpansionFailed(format!("{detail}\nhelp: {help}"))
    })?;

    let lexemes: Vec<String> = thrustc_c_transpiler_macro::macro_token::texts(&expanded);

    let parsed: thrustc_c_transpiler_macro::macro_ast::MacroExpr =
        MacroParser::parse(&lexemes).map_err(|limit: MacroLimit| {
            let (detail, help) =
                thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

            MacroExpansionError::Malformed(format!("{detail}\nhelp: {help}"))
        })?;

    let mut context: MacroContext<'_> = MacroContext::new(PathBuf::new());

    let pending_base: usize = context.pending_statements_len();

    let source: String = thrustc_c_transpiler_macro::macro_expr::lower(
        &mut context,
        &parsed,
        thrustc_c_transpiler_macro::location::Location::RValue,
        "T1",
    );

    if context.pending_statements_len() > pending_base {
        return Err(MacroExpansionError::NeedsStatements(name.to_string()));
    }

    Ok(self::restore_arguments(source, arguments))
}

pub fn expand_object(
    macros: &[Macro],
    name: &str,
    options: &Options,
) -> Result<MacroExpansion, MacroExpansionError> {
    let Some(macro_definition): Option<&Macro> = macros
        .iter()
        .find(|macro_definition| {
            macro_definition.get_name() == name
                && macro_definition.get_kind() == MacroType::Object
        })
    else {
        return Err(MacroExpansionError::UnknownMacro(name.to_string()));
    };

    let table: MacroTable = self::build_macro_table(macros);

    let mut expansion_context: MacroExpansionContext = MacroExpansionContext::new(
        options.get_max_expansion_depth(),
        options.get_max_rescan_passes(),
    );

    let expanded: Vec<MacroToken> = thrustc_c_transpiler_macro::macro_expand::expand_function_tokens(
        macro_definition.get_body(),
        &table,
        &mut expansion_context,
    )
    .map_err(|limit: MacroLimit| {
        let (detail, help) =
            thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

        MacroExpansionError::ExpansionFailed(format!("{detail}\nhelp: {help}"))
    })?;

    let lexemes: Vec<String> = thrustc_c_transpiler_macro::macro_token::texts(&expanded);

    let parsed: thrustc_c_transpiler_macro::macro_ast::MacroExpr =
        MacroParser::parse(&lexemes).map_err(|limit: MacroLimit| {
            let (detail, help) =
                thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

            MacroExpansionError::Malformed(format!("{detail}\nhelp: {help}"))
        })?;

    let mut context: MacroContext<'_> = MacroContext::new(PathBuf::new());

    let pending_base: usize = context.pending_statements_len();

    let source: String = thrustc_c_transpiler_macro::macro_expr::lower(
        &mut context,
        &parsed,
        thrustc_c_transpiler_macro::location::Location::RValue,
        "T1",
    );

    if context.pending_statements_len() > pending_base {
        return Err(MacroExpansionError::NeedsStatements(name.to_string()));
    }

    Ok(MacroExpansion::new(source, Vec::new()))
}

pub fn expand_type(
    macros: &[Macro],
    name: &str,
) -> Result<MacroExpansion, MacroExpansionError> {
    let Some(macro_definition): Option<&Macro> = macros
        .iter()
        .find(|macro_definition| macro_definition.get_name() == name)
    else {
        return Err(MacroExpansionError::UnknownMacro(name.to_string()));
    };

    if macro_definition.get_kind() != MacroType::Type {
        return Err(MacroExpansionError::Malformed(format!(
            "The C macro '{name}' is not a type macro."
        )));
    }

    let lexemes: Vec<String> =
        thrustc_c_transpiler_macro::macro_token::texts(macro_definition.get_body())
            .into_iter()
            .filter(|lexeme| !lexeme.trim().is_empty())
            .collect();

    let joined: String = lexemes.join(" ");

    if let Some(mapped) = thrustc_c_transpiler_macro::macro_lex::lex_macro_type(&joined) {
        return Ok(MacroExpansion::new(mapped, Vec::new()));
    }

    if joined.contains("ptr[") || joined.contains("array[") || joined.contains("fixed[") {
        return Ok(MacroExpansion::new(joined, Vec::new()));
    }

    Err(MacroExpansionError::Malformed(format!(
        "The C macro '{name}' does not describe a Thrust type."
    )))
}

pub fn expand_statement(
    macros: &[Macro],
    name: &str,
    arguments: &[String],
    options: &Options,
) -> Result<MacroExpansion, MacroExpansionError> {
    let table: MacroTable = self::build_macro_table(macros);

    let Some(definition): Option<MacroFunctionLikeDefinition> =
        table.get_function_like_definition_cloned(name)
    else {
        return Err(MacroExpansionError::UnknownMacro(name.to_string()));
    };

    if definition.get_kind() != MacroFunctionLikeKind::Statement {
        return Err(MacroExpansionError::Malformed(format!(
            "The C macro '{name}' is not a statement macro."
        )));
    }

    let substituted: Vec<MacroToken> = self::substitute_arguments(&definition, arguments)?;

    let mut expansion_context: MacroExpansionContext = MacroExpansionContext::new(
        options.get_max_expansion_depth(),
        options.get_max_rescan_passes(),
    );

    let expanded: Vec<MacroToken> = thrustc_c_transpiler_macro::macro_expand::expand_function_tokens(
        &substituted,
        &table,
        &mut expansion_context,
    )
    .map_err(|limit: MacroLimit| {
        let (detail, help) =
            thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

        MacroExpansionError::ExpansionFailed(format!("{detail}\nhelp: {help}"))
    })?;

    let lexemes: Vec<String> = thrustc_c_transpiler_macro::macro_token::texts(&expanded);

    let mut parser: MacroParser<'_> = MacroParser::new(&lexemes);

    let statements: Vec<thrustc_c_transpiler_macro::macro_ast::MacroStmt> =
        parser.parse_statement_body().map_err(|limit: MacroLimit| {
            let (detail, help) =
                thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

            MacroExpansionError::Malformed(format!("{detail}\nhelp: {help}"))
        })?;

    let mut context: MacroContext<'_> = MacroContext::new(PathBuf::new());

    let mut lines: Vec<String> = Vec::new();

    for statement in statements.iter() {
        let lowered: Vec<String> = statement
            .lower(&mut context, 1)
            .map_err(|limit: MacroLimit| {
                let (detail, help) =
                    thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(name, limit);

                MacroExpansionError::Malformed(format!("{detail}\nhelp: {help}"))
            })?;

        lines.extend(lowered);
    }

    Ok(self::restore_arguments(
        format!("{{\n{}\n}}", lines.join("\n")),
        arguments,
    ))
}

fn build_macro_table(macros: &[Macro]) -> MacroTable {
    let mut table: MacroTable = MacroTable::new(PathBuf::new());

    for macro_definition in macros.iter() {
        let variadic: Option<thrustc_c_transpiler_macro::macros::MacroVariadic> =
            macro_definition.get_variadic().map(|variadic: &Variadic| {
                thrustc_c_transpiler_macro::macros::MacroVariadic::new(
                    variadic.get_name().to_string(),
                    variadic.is_gnu_named(),
                )
            });

        let kind: MacroFunctionLikeKind = match macro_definition.get_kind() {
            MacroType::Function => MacroFunctionLikeKind::PureFunction,
            MacroType::Statement => MacroFunctionLikeKind::Statement,
            MacroType::Type | MacroType::Object => continue,
        };

        table.register_neutral_function_macro(MacroFunctionLikeDefinition::new(
            macro_definition.get_name().to_string(),
            macro_definition.get_parameters().to_vec(),
            variadic,
            macro_definition.get_body().to_vec(),
            kind,
        ));
    }

    for macro_definition in macros.iter() {
        if macro_definition.get_kind() != MacroType::Object {
            continue;
        }

        table.register_object_macro(
            macro_definition.get_name().to_string(),
            macro_definition.get_body().to_vec(),
        );
    }

    table
}

fn substitute_arguments(
    definition: &MacroFunctionLikeDefinition,
    arguments: &[String],
) -> Result<Vec<MacroToken>, MacroExpansionError> {
    let name: &str = definition.get_name();

    let parameters: Vec<String> = definition.get_parameters().to_vec();
    let body: Vec<MacroToken> = definition.get_body().to_vec();

    if definition.get_variadic().is_none() && arguments.len() != parameters.len() {
        return Err(MacroExpansionError::ArgumentCountMismatch(name.to_string()));
    }

    if definition.get_variadic().is_some() && arguments.len() < parameters.len() {
        return Err(MacroExpansionError::ArgumentCountMismatch(name.to_string()));
    }

    let placeholder_arguments: Vec<Vec<MacroToken>> = (0..arguments.len())
        .map(|index| {
            vec![MacroToken::new(
                thrustc_c_transpiler_macro::macro_token::MacroTokenKind::Identifier,
                format!("__thrustc_macro_arg_{index}__"),
                MacroTokenOrigin::InvocationArgument,
            )]
        })
        .collect();

    if let Some(variadic) = definition.get_variadic() {
        let fixed_arguments: &[Vec<MacroToken>] = &placeholder_arguments[..parameters.len()];
        let variadic_arguments: &[Vec<MacroToken>] = &placeholder_arguments[parameters.len()..];

        thrustc_c_transpiler_macro::macro_expand_variadic::substitute_variadic_parameter_tokens(
            &body,
            &parameters,
            variadic.get_name(),
            fixed_arguments,
            variadic_arguments,
            arguments[parameters.len()..]
                .iter()
                .any(|argument| !argument.trim().is_empty()),
        )
        .ok_or_else(|| MacroExpansionError::ArgumentCountMismatch(name.to_string()))
    } else {
        thrustc_c_transpiler_macro::macro_expand::substitute_parameter_tokens(
            &body,
            &parameters,
            &placeholder_arguments,
        )
        .ok_or_else(|| MacroExpansionError::ArgumentCountMismatch(name.to_string()))
    }
}

fn restore_arguments(source: String, arguments: &[String]) -> MacroExpansion {
    let mut source: String = source;
    let mut argument_ranges: Vec<(u32, u32)> = Vec::new();

    for (index, argument) in arguments.iter().enumerate() {
        let placeholder: String = format!("__thrustc_macro_arg_{index}__");
        let mut search_from: usize = 0;

        while let Some(relative) = source[search_from..].find(&placeholder) {
            let start: usize = search_from.saturating_add(relative);
            let end: usize = start.saturating_add(placeholder.len());

            source.replace_range(start..end, argument);

            let argument_end: usize = start.saturating_add(argument.len());

            argument_ranges.push((start as u32, argument_end as u32));

            search_from = argument_end;
        }
    }

    MacroExpansion::new(source, argument_ranges)
}
