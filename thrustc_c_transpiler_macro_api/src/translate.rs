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

use thrustc_c_transpiler_macro::macro_parser::MacroParser;
use thrustc_c_transpiler_macro::macro_token;
use thrustc_c_transpiler_macro::macros::MacroKind as EngineMacroKind;
use thrustc_c_transpiler_macro::thrust::{MacroAst, Variadic};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use crate::options::Options;
use crate::traits::MacroDiagnostic;
use crate::types::{Entity, Macro, MacroType};

pub fn translate_object_macro(macro_decl: &clang::Entity<'_>) -> Option<Entity> {
    let raw_name: String = macro_decl.get_name()?;

    if unsafe { macro_decl.is_function_like_macro_unchecked() } {
        return None;
    }

    let range: clang::source::SourceRange<'_> = macro_decl.get_range()?;

    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

    let name_index: usize = tokens
        .iter()
        .position(|token| token.get_spelling() == raw_name)?;

    let body_text: String = tokens
        .iter()
        .skip(name_index.saturating_add(1))
        .map(|token| token.get_spelling())
        .filter(|lexeme| lexeme != "#")
        .collect::<Vec<String>>()
        .join(" ");

    if body_text.trim().is_empty() {
        return None;
    }

    let body: Vec<thrustc_c_transpiler_macro::macro_token::MacroToken> =
        thrustc_c_transpiler_macro::macro_lex::lex_text_to_macro_tokens(
            &body_text,
            thrustc_c_transpiler_macro::macro_token::MacroTokenOrigin::DefinitionBody,
        )
        .into_iter()
        .filter(|token| {
            token.get_kind() != thrustc_c_transpiler_macro::macro_token::MacroTokenKind::Whitespace
        })
        .collect();

    Some(Entity::Macro(Macro::new(
        raw_name,
        Vec::new(),
        None,
        body,
        None,
        MacroType::Object,
    )))
}

pub fn translate_type_macro(macro_decl: &clang::Entity<'_>, _span: Span) -> Option<Entity> {
    let raw_name: String = macro_decl.get_name()?;

    if unsafe { macro_decl.is_function_like_macro_unchecked() } {
        return None;
    }

    let range: clang::source::SourceRange<'_> = macro_decl.get_range()?;
    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();

    let name_index: usize = tokens
        .iter()
        .position(|token| token.get_spelling() == raw_name)?;

    let body_tokens: Vec<clang::token::Token<'_>> = tokens
        .iter()
        .skip(name_index.saturating_add(1))
        .filter(|token| token.get_spelling() != "#")
        .cloned()
        .collect();

    if body_tokens.is_empty() {
        return None;
    }

    let joined: String = body_tokens
        .iter()
        .map(|token| token.get_spelling())
        .collect::<Vec<String>>()
        .join(" ");

    thrustc_c_transpiler_macro::macro_lex::lex_macro_type(&joined)?;

    let body: Vec<thrustc_c_transpiler_macro::macro_token::MacroToken> =
        thrustc_c_transpiler_macro::macro_lex::lex_text_to_macro_tokens(
            &joined,
            thrustc_c_transpiler_macro::macro_token::MacroTokenOrigin::DefinitionBody,
        )
        .into_iter()
        .filter(|token| {
            token.get_kind() != thrustc_c_transpiler_macro::macro_token::MacroTokenKind::Whitespace
        })
        .collect();

    let macro_: Entity = Entity::Macro(Macro::new(
        raw_name,
        Vec::new(),
        None,
        body,
        None,
        MacroType::Type,
    ));

    Some(macro_)
}

pub fn translate_function_macros(
    macro_decls: &[clang::Entity<'_>],
    span: Span,
    _options: &Options,
    diagnostics: &mut impl MacroDiagnostic,
) -> Vec<Entity> {
    let mut entities: Vec<Entity> = Vec::new();

    for macro_decl in macro_decls.iter() {
        let Some(raw_name): Option<String> = macro_decl.get_name() else {
            continue;
        };

        let Some((name, kind)) =
            thrustc_c_transpiler_macro::macros::classify_macro_definition(macro_decl)
        else {
            continue;
        };

        match kind {
            EngineMacroKind::PureFunction {
                parameters,
                variadic,
                body,
            } => {
                let parse_body: Vec<thrustc_c_transpiler_macro::macro_token::MacroToken> =
                    if variadic.is_some() {
                        thrustc_c_transpiler_macro::macro_expand_variadic::normalize_variadic_body_for_classification(&body)
                    } else {
                        body.clone()
                    };

                let lexemes: Vec<String> = macro_token::texts(&parse_body);

                let parsed: thrustc_c_transpiler_macro::macro_ast::MacroExpr =
                    match MacroParser::parse(&lexemes) {
                        Ok(parsed) => parsed,
                        Err(limit) => {
                            let (detail, _) =
                                thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(
                                    &raw_name, limit,
                                );

                            diagnostics.add_warning(CompilationIssue::Warning(
                                CompilationIssueCode::W0104,
                                format!("macro '{raw_name}' skipped: {detail}"),
                                span,
                            ));

                            continue;
                        }
                    };

                let variadic: Option<Variadic> = variadic.map(|variadic| {
                    Variadic::new(variadic.get_name().to_string(), variadic.is_gnu_named())
                });

                let ast: MacroAst = MacroAst::from_expr(&parsed);

                entities.push(Entity::Macro(Macro::new(
                    name,
                    parameters,
                    variadic,
                    body,
                    Some(ast),
                    MacroType::Function,
                )));
            }
            EngineMacroKind::Statement {
                parameters,
                variadic,
                body,
            } => {
                let variadic: Option<Variadic> = variadic.map(|variadic| {
                    Variadic::new(variadic.get_name().to_string(), variadic.is_gnu_named())
                });

                entities.push(Entity::Macro(Macro::new(
                    name,
                    parameters,
                    variadic,
                    body,
                    None,
                    MacroType::Statement,
                )));
            }
            EngineMacroKind::Object => continue,
            EngineMacroKind::Unsupported(limit) => {
                let (detail, help) =
                    thrustc_c_transpiler_macro::macro_error::get_macro_issue_help(&raw_name, limit);

                diagnostics.add_warning(CompilationIssue::Warning(
                    CompilationIssueCode::W0104,
                    format!("macro '{raw_name}' skipped: {detail}\nhelp: {help}"),
                    span,
                ));
            }
        }
    }

    entities
}
