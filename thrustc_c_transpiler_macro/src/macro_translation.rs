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

use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_typesystem::Type;

use crate::{
    macro_ast::MacroExpr,
    macro_context::MacroContext,
    macro_parser::MacroParser,
    macro_token::{MacroToken, MacroTokenOrigin},
    macros::MacroKind,
};

const C_MACRO_SYMBOL_PREFIX: &str = "__c_macro_";

pub fn append_translated_macro_constants(
    macro_decls: &[clang::Entity<'_>],
    out: &mut String,
    fns_out: &mut String,
    ctx: &mut MacroContext<'_>,
    span: Span,
    emitted_constants: &mut usize,
    emitted_functions: &mut usize,
) {
    {
        let valid_macros = macro_decls.iter().filter_map(|macro_decl| {
            let raw_name: String = macro_decl.get_name()?;
            Some((macro_decl, raw_name))
        });

        let object_macros = valid_macros
            .filter(|(macro_decl, _)| unsafe { !macro_decl.is_function_like_macro_unchecked() });

        for (macro_decl, raw_name) in object_macros {
            let name: String = {
                let base: String = crate::util::normalize_to_thrust_identifier(&raw_name);

                format!("{C_MACRO_SYMBOL_PREFIX}{base}")
            };

            let object_body: Vec<MacroToken> = macro_decl
                .get_range()
                .map(|range| {
                    let tokens: Vec<MacroToken> = crate::macro_lex::to_macro_tokens(
                        &range.tokenize(),
                        MacroTokenOrigin::DefinitionBody,
                    );

                    match tokens.iter().position(|token| token.get_text() == raw_name) {
                        Some(index) => tokens[index + 1..].to_vec(),
                        None => Vec::new(),
                    }
                })
                .unwrap_or_default();

            ctx.get_mut_macro_table()
                .register_object_macro(raw_name.clone(), object_body);

            if let Some(result) = macro_decl.evaluate() {
                let translated: Option<(Type, BuiltinValue)> = match result {
                    clang::EvaluationResult::SignedInteger(value) => {
                        Some((Type::S64 { span }, BuiltinValue::Integer(value as u64)))
                    }
                    clang::EvaluationResult::UnsignedInteger(value) => {
                        Some((Type::U64 { span }, BuiltinValue::Integer(value)))
                    }
                    clang::EvaluationResult::Float(value) => {
                        Some((Type::F64 { span }, BuiltinValue::Float(value)))
                    }
                    clang::EvaluationResult::String(value)
                    | clang::EvaluationResult::ObjCString(value) => Some((
                        Type::Array {
                            base_type: Box::new(Type::Char { span }),
                            infered_type: None,
                            metadata: thrustc_typesystem::type_metadata::ArrayTypeMetadata::new(
                                None, None,
                            ),
                            span,
                        },
                        BuiltinValue::CString(value.to_bytes().to_vec()),
                    )),
                    _ => None,
                };

                if let Some((kind, builtin_value)) = translated {
                    let kind_text: String = crate::type_format::format_type_thrust(&kind);
                    let value_text: String =
                        crate::type_format::format_builtin_value_thrust(&builtin_value, &kind, ctx);

                    out.push_str("const ");
                    out.push_str(&name);
                    out.push_str(": ");
                    out.push_str(&kind_text);
                    out.push_str(" = ");
                    out.push_str(&value_text);
                    out.push_str(";\n");

                    *emitted_constants = emitted_constants.saturating_add(1);

                    continue;
                }
            }
        }
    }

    {
        let valid_macros = macro_decls.iter().filter_map(|macro_decl| {
            let raw_name: String = macro_decl.get_name()?;

            Some((macro_decl, raw_name))
        });

        let function_macros = valid_macros
            .filter(|(macro_decl, _)| unsafe { macro_decl.is_function_like_macro_unchecked() });

        let mut classified_macros = Vec::new();

        for (macro_decl, _) in function_macros {
            if let Some((name, kind)) = crate::macros::classify_macro_definition(macro_decl) {
                classified_macros.push((*macro_decl, name, kind));
            }
        }

        crate::macros::reclassify_macros_calling_statements(&mut classified_macros);

        ctx.get_mut_macro_table()
            .register_function_like_macros(&classified_macros);

        ctx.get_mut_macro_table()
            .register_statement_macros(&classified_macros);

        for (macro_decl, raw_name, kind) in classified_macros.iter() {
            match kind {
                MacroKind::PureFunction {
                    parameters,
                    variadic,
                    body,
                } => {
                    if variadic.is_some() {
                        continue;
                    }

                    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
                        crate::macro_expand::MacroExpansionContext::new(64, 128);

                    let expanded_body: Vec<MacroToken> =
                        match crate::macro_expand::expand_function_tokens(
                            body,
                            ctx.get_macro_table(),
                            &mut expansion_context,
                        ) {
                            Ok(tokens) => tokens,
                            Err(reason) => {
                                let prefix: Option<String> = crate::macro_error::prefix(macro_decl);
                                let (detail, help) =
                                    crate::macro_error::get_macro_issue_help(raw_name, reason);

                                let message: String = match prefix {
                                    Some(prefix) => format!("{prefix}{detail}"),
                                    None => "{detail}".to_string(),
                                };

                                crate::macro_error::add_macro_error(
                                    ctx, macro_decl, raw_name, &message, &help, span,
                                );

                                continue;
                            }
                        };

                    let body_lexemes: Vec<String> = crate::macro_token::texts(&expanded_body);
                    let parsed: MacroExpr = match MacroParser::parse(&body_lexemes) {
                        Ok(parsed) => parsed,
                        Err(reason) => {
                            let prefix: Option<String> = crate::macro_error::prefix(macro_decl);
                            let (detail, help) =
                                crate::macro_error::get_macro_issue_help(raw_name, reason);

                            let message: String = match prefix {
                                Some(prefix) => format!("{prefix}{detail}"),
                                None => "{detail}".to_string(),
                            };

                            crate::macro_error::add_macro_error(
                                ctx, macro_decl, raw_name, &message, &help, span,
                            );

                            continue;
                        }
                    };

                    let pending_base: usize = ctx.pending_statements_len();

                    let body_text: String =
                        crate::macro_expr::lower_function_body(ctx, &parsed, "T1");

                    let mut body_lines: Vec<String> =
                        ctx.take_pending_statements_since(pending_base);

                    body_lines.push(format!("return {body_text};"));

                    fns_out.push_str(&self::emit_function_as_macro_fn(
                        raw_name,
                        parameters,
                        &body_lines,
                    ));

                    *emitted_functions = emitted_functions.saturating_add(1);
                }

                MacroKind::Statement {
                    parameters,
                    variadic,
                    body,
                } => {
                    ctx.get_mut_macro_table().add_statement_macro_definition(
                        raw_name.clone(),
                        parameters.clone(),
                        variadic.clone(),
                        body.clone(),
                        macro_decl,
                    );
                }
                MacroKind::Object => continue,
                MacroKind::Unsupported(limit) => {
                    let prefix: Option<String> = crate::macro_error::prefix(macro_decl);
                    let (detail, help) = crate::macro_error::get_macro_issue_help(raw_name, *limit);

                    let message: String = match prefix {
                        Some(prefix) => format!("{prefix}{detail}"),
                        None => "{detail}".to_string(),
                    };

                    crate::macro_error::add_macro_error(
                        ctx, macro_decl, raw_name, &message, &help, span,
                    );
                }
            }
        }
    }

    if *emitted_constants > 0 {
        out.push('\n');
    }

    if *emitted_functions > 0 {
        fns_out.push('\n');
    }
}

fn emit_function_as_macro_fn(name: &str, parameters: &[String], body_lines: &[String]) -> String {
    let type_names: Vec<String> = if parameters.is_empty() {
        vec!["T1".to_string()]
    } else {
        (1..=parameters.len())
            .map(|index| format!("T{index}"))
            .collect()
    };

    let parameter_texts: Vec<String> = parameters
        .iter()
        .zip(type_names.iter())
        .map(|(parameter, type_name)| {
            let parameter_name: String = { crate::util::normalize_to_thrust_identifier(parameter) };

            format!("{parameter_name}: {type_name}")
        })
        .collect();

    let function_name: String = {
        let base: String = crate::util::normalize_to_thrust_identifier(name);

        format!("{C_MACRO_SYMBOL_PREFIX}{base}")
    };

    let mut out: String = String::new();

    out.push_str("fn ");
    out.push_str(&function_name);
    out.push('[');
    out.push_str(&type_names.join(", "));
    out.push_str("](");
    out.push_str(&parameter_texts.join(", "));
    out.push_str(") T1 @alwaysInline {\n");

    for line in body_lines.iter() {
        out.push_str("    ");
        out.push_str(line);
        out.push('\n');
    }

    out.push_str("}\n");

    out
}
