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

use crate::macro_ast::MacroExpr;
use crate::macro_error::MacroLimit;
use crate::macro_parser::MacroParser;
use crate::macro_token::{MacroToken, MacroTokenOrigin};

use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::path::PathBuf;
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;

use thrustc_typesystem::Type;

const C_MACRO_SYMBOL_PREFIX: &str = "__c_macro_";

#[derive(Debug)]
pub struct MacroContext<'clang> {
    table: crate::macro_table::MacroTable,
    context: crate::context::TranspilerContext,
    expansion_stack: Vec<String>,
    temporary_counter: u64,
    pending_statements: Vec<String>,
    type_environment: HashMap<String, crate::macro_type::InferredType>,
    marker: PhantomData<&'clang ()>,
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn new(main_file: PathBuf) -> Self {
        Self {
            table: crate::macro_table::MacroTable::new(main_file),
            context: crate::context::TranspilerContext::new(),
            expansion_stack: Vec::new(),
            temporary_counter: 0,
            pending_statements: Vec::new(),
            type_environment: HashMap::new(),
            marker: PhantomData,
        }
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_macro_table(&self) -> &crate::macro_table::MacroTable {
        &self.table
    }

    #[inline]
    pub fn get_transpiler_context(&self) -> &crate::context::TranspilerContext {
        &self.context
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_mut_macro_table(&mut self) -> &mut crate::macro_table::MacroTable {
        &mut self.table
    }

    #[inline]
    pub fn get_mut_transpiler_context(&mut self) -> &mut crate::context::TranspilerContext {
        &mut self.context
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_expansion(&mut self, name: &str) -> bool {
        if self.expansion_stack.iter().any(|entry| entry == name) {
            return true;
        }

        self.expansion_stack.push(name.to_string());

        false
    }

    #[inline]
    pub fn pop_expansion(&mut self) {
        self.expansion_stack.pop();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn next_temporary_name(&mut self) -> String {
        let index: u64 = self.temporary_counter;

        self.temporary_counter = self.temporary_counter.saturating_add(1);

        format!("thrust_temporary_{index}")
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_pending_statement(&mut self, statement: String) {
        self.pending_statements.push(statement);
    }

    #[inline]
    pub fn pending_statements_len(&self) -> usize {
        self.pending_statements.len()
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn take_pending_statements_since(&mut self, base: usize) -> Vec<String> {
        if base >= self.pending_statements.len() {
            return Vec::new();
        }

        self.pending_statements.split_off(base)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_type_environment(&self) -> &HashMap<String, crate::macro_type::InferredType> {
        &self.type_environment
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_type_environment(
        &mut self,
        environment: HashMap<String, crate::macro_type::InferredType>,
    ) {
        self.type_environment = environment;
    }

    #[inline]
    pub fn clear_type_environment(&mut self) {
        self.type_environment.clear();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroKind {
    Object,
    PureFunction {
        parameters: Vec<String>,
        body: Vec<MacroToken>,
    },
    Statement {
        parameters: Vec<String>,
        body: Vec<MacroToken>,
    },
    Unsupported(MacroLimit),
}

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
            if let Some((name, kind)) = self::classify_macro_definition(macro_decl) {
                classified_macros.push((*macro_decl, name, kind));
            }
        }

        self::reclassify_macros_calling_statements(&mut classified_macros);

        ctx.get_mut_macro_table()
            .register_function_like_macros(&classified_macros);

        ctx.get_mut_macro_table()
            .register_statement_macros(&classified_macros);

        for (macro_decl, raw_name, kind) in classified_macros.iter() {
            match kind {
                MacroKind::PureFunction { parameters, body } => {
                    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
                        crate::macro_expand::MacroExpansionContext::new_default();

                    let expanded_body: Vec<MacroToken> =
                        match crate::macro_expand::expand_function_tokens(
                            body,
                            ctx.get_macro_table(),
                            &mut expansion_context,
                        ) {
                            Ok(tokens) => tokens,
                            Err(reason) => {
                                let prefix: String = self::expansion_prefix(macro_decl);
                                let (detail, help) =
                                    crate::macro_error::get_macro_issue_help(raw_name, reason);

                                crate::macro_error::add_macro_error(
                                    ctx,
                                    macro_decl,
                                    raw_name,
                                    &format!("{prefix}{detail}"),
                                    &help,
                                    span,
                                );

                                continue;
                            }
                        };

                    let body_spellings: Vec<String> = crate::macro_token::texts(&expanded_body);
                    let parsed: MacroExpr = match MacroParser::parse(&body_spellings) {
                        Ok(parsed) => parsed,
                        Err(reason) => {
                            let prefix: String = self::expansion_prefix(macro_decl);
                            let (detail, help) =
                                crate::macro_error::get_macro_issue_help(raw_name, reason);

                            crate::macro_error::add_macro_error(
                                ctx,
                                macro_decl,
                                raw_name,
                                &format!("{prefix}{detail}"),
                                &help,
                                span,
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

                MacroKind::Statement { parameters, body } => {
                    ctx.get_mut_macro_table().add_statement_macro_definition(
                        raw_name.clone(),
                        parameters.clone(),
                        body.clone(),
                        macro_decl,
                    );
                }
                MacroKind::Object => continue,
                MacroKind::Unsupported(limit) => {
                    let prefix: String = self::expansion_prefix(macro_decl);
                    let (detail, help) = crate::macro_error::get_macro_issue_help(raw_name, *limit);

                    crate::macro_error::add_macro_error(
                        ctx,
                        macro_decl,
                        raw_name,
                        &format!("{prefix}{detail}"),
                        &help,
                        span,
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

#[derive(Debug)]
struct MacroParameterFlags {
    mutable: HashSet<String>,
    pointer: HashSet<String>,
}

fn build_statement_macro_function(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    name: &str,
    parameters: &[String],
    args: &[Vec<MacroToken>],
    flags: &MacroParameterFlags,
    span: Span,
) -> Option<String> {
    let function_name: String = {
        let base: String = crate::util::normalize_to_thrust_identifier(name);

        format!("{C_MACRO_SYMBOL_PREFIX}{base}")
    };

    let body_tokens: Vec<MacroToken> = ctx.get_macro_table().get_body_macro_tokens(name)?;

    if body_tokens.iter().any(|token| {
        token.get_text() == "return"
            || token.get_text() == "continue"
            || token.get_text() == "break"
            || token.get_text() == "goto"
    }) {
        return None;
    }

    let template_args: Vec<Vec<MacroToken>> = parameters
        .iter()
        .map(|parameter| {
            vec![MacroToken::new(
                crate::macro_token::MacroTokenKind::Identifier,
                parameter.clone(),
                MacroTokenOrigin::DefinitionBody,
            )]
        })
        .collect();

    let mut joined_lines: String =
        match self::lowered_statement_lines(ctx, &body_tokens, parameters, &template_args) {
            Some(lines) => lines.join("\n"),
            None => self::translated_expanded_lines(ctx, site, name, parameters, args, span)?,
        };

    if !flags.mutable.is_empty() {
        joined_lines = self::rewrite_mutable_parameter_uses(&joined_lines, &flags.mutable);
        joined_lines = self::strip_parenthesized_mutable_lhs(&joined_lines, &flags.mutable);
        joined_lines =
            self::cast_mutable_parameter_assignments(&joined_lines, parameters, &flags.mutable);
    }

    let type_names: Vec<String> = (1..=parameters.len())
        .map(|index| format!("T{index}"))
        .collect();

    let parameter_texts: Vec<String> = parameters
        .iter()
        .zip(type_names.iter())
        .map(|(parameter, type_name)| {
            let parameter_name: String = { crate::util::normalize_to_thrust_identifier(parameter) };

            if flags.pointer.contains(parameter) {
                format!("{parameter_name}: ptr[{type_name}]")
            } else {
                format!("{parameter_name}: {type_name}")
            }
        })
        .collect();

    let mut text: String = String::new();

    text.push_str("fn ");
    text.push_str(&function_name);
    text.push('[');
    text.push_str(&type_names.join(", "));
    text.push_str("](");
    text.push_str(&parameter_texts.join(", "));
    text.push_str(") void @alwaysInline {\n");
    text.push_str(&joined_lines);
    text.push_str("\n}\n");

    Some(text)
}

fn lowered_statement_lines(
    ctx: &mut MacroContext<'_>,
    body_tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Option<Vec<String>> {
    let renamed: Vec<MacroToken> = self::substitute_tokens(body_tokens, parameters, args)?;

    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
        crate::macro_expand::MacroExpansionContext::new_default();

    let expanded_tokens: Vec<MacroToken> = crate::macro_expand::expand_function_tokens(
        &renamed,
        ctx.get_macro_table(),
        &mut expansion_context,
    )
    .ok()?;

    let statement_spellings: Vec<String> = crate::macro_token::texts(&expanded_tokens);

    let mut statement_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&statement_spellings);

    let parsed: Vec<crate::macro_ast::MacroStmt> = statement_parser.parse_statement_body().ok()?;

    let mut lines: Vec<String> = Vec::new();

    for stmt in parsed.iter() {
        lines.extend(stmt.lower(ctx, 1).ok()?);
    }

    Some(lines)
}

fn translated_expanded_lines(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    macro_name: &str,
    parameters: &[String],
    args: &[Vec<MacroToken>],
    span: Span,
) -> Option<String> {
    let children: Vec<clang::Entity<'_>> = site.get_children();

    let body_nodes: Vec<clang::Entity<'_>> = if site.get_kind() == clang::EntityKind::CompoundStmt {
        children
    } else {
        match children.first() {
            Some(first) if first.get_kind() == clang::EntityKind::CompoundStmt => {
                first.get_children()
            }
            _ => {
                if children.len() > 1 {
                    children[..children.len() - 1].to_vec()
                } else {
                    children
                }
            }
        }
    };

    let mut body_lines: Vec<String> = Vec::new();

    for child in body_nodes.iter() {
        let base_error_count: usize = ctx.get_transpiler_context().error_count();

        let lines: Vec<String> =
            crate::stmt::translate_stmt(child, 1, span, Some("void"), None, ctx).ok()?;

        if ctx.get_transpiler_context().error_count() > base_error_count {
            crate::macro_error::collapse_macro_site_errors_with_failed_at(
                ctx,
                site,
                macro_name,
                child,
                base_error_count,
            );
        }

        body_lines.extend(lines);
    }

    let mut joined_lines: String = body_lines.join("\n");

    let line_tokens: Vec<MacroToken> =
        crate::macro_lex::lex_text_to_macro_tokens(&joined_lines, MacroTokenOrigin::FallbackLexed);
    let generalized_tokens: Vec<MacroToken> =
        self::replace_argument_token_sequences(&line_tokens, parameters, args);
    let template_args: Vec<Vec<MacroToken>> = parameters
        .iter()
        .map(|parameter| {
            vec![MacroToken::new(
                crate::macro_token::MacroTokenKind::Identifier,
                parameter.clone(),
                MacroTokenOrigin::DefinitionBody,
            )]
        })
        .collect();
    let substituted_tokens: Vec<MacroToken> =
        self::substitute_tokens(&generalized_tokens, parameters, &template_args)?;

    joined_lines = crate::macro_lex::detokenize_macro_tokens(&substituted_tokens);

    Some(joined_lines)
}

fn substitute_tokens(
    tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Option<Vec<MacroToken>> {
    crate::macro_expand::substitute_parameter_tokens(tokens, parameters, args)
}

fn replace_argument_token_sequences(
    tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Vec<MacroToken> {
    if parameters.len() != args.len() {
        return tokens.to_vec();
    }

    let mut parameter_indices: Vec<usize> = (0..parameters.len()).collect();

    parameter_indices.sort_by(|left, right| args[*right].len().cmp(&args[*left].len()));

    let mut out: Vec<MacroToken> = Vec::new();
    let mut index: usize = 0;

    while index < tokens.len() {
        let mut matched: bool = false;

        for parameter_index in parameter_indices.iter().copied() {
            let arg_tokens: &[MacroToken] = &args[parameter_index];

            if arg_tokens.is_empty() {
                continue;
            }

            let end: usize = index.saturating_add(arg_tokens.len());

            if end > tokens.len() {
                continue;
            }

            let token_slice: &[MacroToken] = &tokens[index..end];

            if !token_slice
                .iter()
                .zip(arg_tokens.iter())
                .all(|(left, right)| left.get_text() == right.get_text())
            {
                continue;
            }

            out.push(MacroToken::new(
                crate::macro_token::MacroTokenKind::Identifier,
                parameters[parameter_index].clone(),
                MacroTokenOrigin::DefinitionBody,
            ));

            index = end;
            matched = true;
            break;
        }

        if matched {
            continue;
        }

        out.push(tokens[index].clone());
        index = index.saturating_add(1);
    }

    out
}

fn detect_mutated_parameters(tokens: &[MacroToken], parameters: &[String]) -> HashSet<String> {
    let mut mutable_parameters: HashSet<String> = HashSet::new();

    for parameter in parameters.iter() {
        for (index, token) in tokens.iter().enumerate() {
            if token.get_text() != *parameter {
                continue;
            }

            if self::parameter_token_is_written(tokens, index) {
                mutable_parameters.insert(parameter.clone());
                break;
            }
        }
    }

    mutable_parameters
}

fn parameter_token_is_written(tokens: &[MacroToken], index: usize) -> bool {
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

fn detect_indexed_parameters(tokens: &[MacroToken], parameters: &[String]) -> HashSet<String> {
    let mut indexed_parameters: HashSet<String> = HashSet::new();

    for parameter in parameters.iter() {
        for (index, token) in tokens.iter().enumerate() {
            if token.get_text() != *parameter {
                continue;
            }

            let mut probe: usize = index.saturating_add(1);

            while tokens
                .get(probe)
                .is_some_and(|token| token.get_text() == ")")
            {
                probe = probe.saturating_add(1);
            }

            if tokens
                .get(probe)
                .is_some_and(|token| token.get_text() == "[")
            {
                indexed_parameters.insert(parameter.clone());
                break;
            }
        }
    }

    indexed_parameters
}

fn detect_pointer_parameters(tokens: &[MacroToken], parameters: &[String]) -> HashSet<String> {
    let mut pointer_parameters: HashSet<String> = HashSet::new();

    for parameter in parameters.iter() {
        for (index, token) in tokens.iter().enumerate() {
            if token.get_text() != *parameter {
                continue;
            }

            let mut probe: usize = index.saturating_add(1);

            while tokens
                .get(probe)
                .is_some_and(|token| token.get_text() == ")")
            {
                probe = probe.saturating_add(1);
            }

            let next: Option<&str> = tokens.get(probe).map(|token| token.get_text());

            if next == Some("[") || next == Some("->") {
                pointer_parameters.insert(parameter.clone());
                break;
            }
        }
    }

    pointer_parameters
}

fn rewrite_mutable_parameter_uses(lines: &str, mutable_parameters: &HashSet<String>) -> String {
    let tokens: Vec<MacroToken> =
        crate::macro_lex::lex_text_to_macro_tokens(lines, MacroTokenOrigin::FallbackLexed);

    let mut out: Vec<MacroToken> = Vec::new();

    for token in tokens.iter() {
        if !mutable_parameters.contains(token.get_text()) {
            out.push(token.clone());
            continue;
        }

        out.push(MacroToken::new(
            crate::macro_token::MacroTokenKind::Identifier,
            token.get_text().to_string(),
            MacroTokenOrigin::FallbackLexed,
        ));
        out.push(MacroToken::new(
            crate::macro_token::MacroTokenKind::Punctuation,
            "->".to_string(),
            MacroTokenOrigin::FallbackLexed,
        ));
        out.push(MacroToken::new(
            crate::macro_token::MacroTokenKind::Punctuation,
            "[".to_string(),
            MacroTokenOrigin::FallbackLexed,
        ));
        out.push(MacroToken::new(
            crate::macro_token::MacroTokenKind::Literal,
            "0".to_string(),
            MacroTokenOrigin::FallbackLexed,
        ));
        out.push(MacroToken::new(
            crate::macro_token::MacroTokenKind::Punctuation,
            "]".to_string(),
            MacroTokenOrigin::FallbackLexed,
        ));
    }

    crate::macro_lex::detokenize_macro_tokens(&out)
}

fn strip_parenthesized_mutable_lhs(lines: &str, mutable_parameters: &HashSet<String>) -> String {
    let mut out: String = lines.to_string();

    for parameter in mutable_parameters.iter() {
        let with_parens: String = format!("({parameter}->[0]) =");
        let without_parens: String = format!("{parameter}->[0] =");

        out = out.replace(&with_parens, &without_parens);
    }

    out
}

fn cast_mutable_parameter_assignments(
    lines: &str,
    parameters: &[String],
    mutable_parameters: &HashSet<String>,
) -> String {
    let mut out_lines: Vec<String> = Vec::new();

    for line in lines.lines() {
        let mut rewritten_line: String = line.to_string();

        for (index, parameter) in parameters.iter().enumerate() {
            if !mutable_parameters.contains(parameter) {
                continue;
            }

            let lhs: String = format!("{parameter}->[0] =");

            if !rewritten_line.contains(&lhs) || !rewritten_line.trim_end().ends_with(';') {
                continue;
            }

            let Some(eq_pos) = rewritten_line.find(&lhs) else {
                continue;
            };

            let rhs_start: usize = eq_pos.saturating_add(lhs.len());
            let rhs_with_semicolon: &str = &rewritten_line[rhs_start..];
            let rhs_expression: &str = rhs_with_semicolon.trim().trim_end_matches(';').trim();
            let type_name: String = format!("T{}", index.saturating_add(1));

            rewritten_line = format!(
                "{}{} ({}) as {};",
                &rewritten_line[..rhs_start],
                " ",
                rhs_expression,
                type_name
            );
        }

        out_lines.push(rewritten_line);
    }

    out_lines.join("\n")
}

fn collect_argument_types<'clang>(
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
        self::collect_argument_types(&child, path, arg_ranges, best);
    }
}

fn argument_clang_types<'clang>(
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

    self::collect_argument_types(site, &path, arg_ranges, &mut best);

    best.into_iter().map(|(_, ty)| ty).collect()
}

pub fn reclassify_macros_calling_statements(
    items: &mut Vec<(clang::Entity<'_>, String, MacroKind)>,
) {
    loop {
        let statement_names: Vec<String> = items
            .iter()
            .filter(|(_, _, kind)| matches!(kind, MacroKind::Statement { .. }))
            .map(|(_, name, _)| name.clone())
            .collect();

        if statement_names.is_empty() {
            break;
        }

        let mut changed: bool = false;

        for (_, name, kind) in items.iter_mut() {
            if let MacroKind::PureFunction { parameters, body } = kind {
                if self::macro_body_calls(body, &statement_names, name) {
                    *kind = MacroKind::Statement {
                        parameters: std::mem::take(parameters),
                        body: std::mem::take(body),
                    };

                    changed = true;
                }
            }
        }

        if !changed {
            break;
        }
    }
}

pub fn try_extract_statement_macro_call(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    span: Span,
) -> Option<String> {
    if !matches!(
        site.get_kind(),
        clang::EntityKind::DoStmt | clang::EntityKind::CompoundStmt
    ) {
        return None;
    }

    let key: String = crate::macro_table::MacroTable::make_location_key(site)?;

    let name: String = ctx.get_macro_table().find_macro_name(&key)?;

    let Some(parameters) = ctx.get_macro_table().get_parameter_names(&name) else {
        crate::macro_error::add_macro_error(
            ctx,
            site,
            &name,
            "Missing macro parameter metadata for statement macro function extraction.",
            "Ensure this macro is captured by the transpiler macro table before expansion.",
            span,
        );

        return None;
    };

    let Some((args, arg_ranges, _end)) = crate::macro_lex::lex_site_call_arguments(site, &name)
    else {
        crate::macro_error::add_macro_error(
            ctx,
            site,
            &name,
            "Could not extract invocation arguments for statement macro expansion.",
            "Use a macro call form that can be tokenized unambiguously.",
            span,
        );

        return None;
    };

    if args.len() != parameters.len() {
        crate::macro_error::add_macro_error(
            ctx,
            site,
            &name,
            &format!(
                "Macro invocation argument count mismatch: expected {}, got {}.",
                parameters.len(),
                args.len()
            ),
            "Adjust macro invocation arguments to match the macro definition.",
            span,
        );

        return None;
    }

    let function_name: String = {
        let base: String = crate::util::normalize_to_thrust_identifier(&name);

        format!("{C_MACRO_SYMBOL_PREFIX}{base}")
    };

    let mut parsed_args: Vec<MacroExpr> = Vec::new();

    for arg_tokens in args.iter() {
        let expanded_arg: Vec<MacroToken> = {
            let mut expansion_context: crate::macro_expand::MacroExpansionContext =
                crate::macro_expand::MacroExpansionContext::new_default();

            crate::macro_expand::expand_function_tokens(
                arg_tokens,
                ctx.get_macro_table(),
                &mut expansion_context,
            )
            .unwrap_or_else(|_| arg_tokens.clone())
        };

        let arg_spellings: Vec<String> = crate::macro_token::texts(&expanded_arg);

        let parsed: MacroExpr = match MacroParser::parse(&arg_spellings) {
            Ok(parsed) => parsed,
            Err(limit) => {
                crate::macro_error::add_macro_error(
                    ctx,
                    site,
                    &name,
                    &format!(
                        "Could not parse macro invocation argument expression ({:?}).",
                        limit
                    ),
                    "Simplify the macro argument expression or extend macro parser coverage.",
                    span,
                );

                return None;
            }
        };

        parsed_args.push(parsed);
    }

    let clang_types: Vec<Option<clang::Type<'_>>> = self::argument_clang_types(site, &arg_ranges);

    let body_tokens: Vec<MacroToken> = ctx
        .get_macro_table()
        .get_body_macro_tokens(&name)
        .unwrap_or_default();

    let mutable_parameters: HashSet<String> =
        self::detect_mutated_parameters(&body_tokens, &parameters);

    let mut pointer_parameters: HashSet<String> = {
        let mut combined: HashSet<String> = mutable_parameters.clone();

        combined.extend(self::detect_pointer_parameters(&body_tokens, &parameters));

        combined
    };

    for (index, clang_type) in clang_types.iter().enumerate() {
        let is_pointer: bool = clang_type.as_ref().is_some_and(|ty| {
            matches!(
                ty.get_canonical_type().get_kind(),
                clang::TypeKind::Pointer
                    | clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
            )
        });

        if is_pointer {
            pointer_parameters.insert(parameters[index].clone());
        }
    }

    let indexed_parameters: HashSet<String> =
        self::detect_indexed_parameters(&body_tokens, &parameters);

    let mut arg_texts: Vec<String> = parsed_args
        .iter()
        .map(|parsed| {
            crate::macro_expr::lower(ctx, parsed, crate::location::Location::RValue, "T1")
        })
        .collect();

    let expanded_body_tokens: Vec<MacroToken> = {
        let mut expansion_context: crate::macro_expand::MacroExpansionContext =
            crate::macro_expand::MacroExpansionContext::new_default();

        crate::macro_expand::expand_function_tokens(
            &body_tokens,
            ctx.get_macro_table(),
            &mut expansion_context,
        )
        .unwrap_or(body_tokens.clone())
    };

    let body_spellings: Vec<String> = crate::macro_token::texts(&expanded_body_tokens);

    let mut body_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&body_spellings);

    let body_statements: Vec<crate::macro_ast::MacroStmt> = match body_parser.parse_statement_body()
    {
        Ok(statements) => statements,
        Err(limit) => {
            crate::macro_error::add_macro_error(
                ctx,
                site,
                &name,
                &format!(
                    "Could not parse macro body for parameter type inference ({:?}).",
                    limit
                ),
                "Simplify the macro body or extend macro parser coverage.",
                span,
            );

            return None;
        }
    };

    let seeds: Vec<Option<crate::macro_type::InferredType>> = (0..parameters.len())
        .map(|index| {
            clang_types
                .get(index)
                .and_then(|clang_type| clang_type.as_ref())
                .and_then(|clang_type| {
                    crate::macro_type::MacroType::from_clang_type(ctx, clang_type, span)
                })
                .or_else(|| {
                    parsed_args
                        .get(index)
                        .and_then(crate::macro_type::MacroType::literal_argument_type)
                })
        })
        .collect();

    let type_args: Vec<String> = match crate::macro_type::MacroType::infer_parameter_types(
        &parameters,
        &body_statements,
        &seeds,
    ) {
        Ok(type_args) => type_args,
        Err(parameter) => {
            crate::macro_error::add_macro_parameter_type_error(ctx, site, &name, &parameter, span);

            return None;
        }
    };

    for (index, argument_text) in arg_texts.iter_mut().enumerate() {
        let parameter: &String = &parameters[index];

        if !pointer_parameters.contains(parameter) {
            continue;
        }

        if indexed_parameters.contains(parameter) {
            *argument_text = format!("{argument_text} as ptr[{}]", type_args[index]);
            continue;
        }

        let is_place_argument: bool = mutable_parameters.contains(parameter)
            && matches!(
                parsed_args.get(index),
                Some(
                    MacroExpr::Index { .. }
                        | MacroExpr::Member { .. }
                        | MacroExpr::Unary {
                            op: crate::macro_ast::MacroUnOp::Deref,
                            ..
                        }
                )
            );

        if is_place_argument {
            *argument_text = crate::macro_expr::lower(
                ctx,
                &parsed_args[index],
                crate::location::Location::AddressOf,
                "T1",
            );

            continue;
        }

        if argument_text.starts_with("ref ") {
            continue;
        }

        *argument_text = format!("ref ({argument_text})");
    }

    let call_text: String = format!(
        "{function_name}[{}]({})",
        type_args.join(", "),
        arg_texts.join(", ")
    );

    let lowered_args: Vec<Vec<MacroToken>> = arg_texts
        .iter()
        .map(|arg_text| {
            crate::macro_lex::lex_text_to_macro_tokens(
                arg_text,
                MacroTokenOrigin::InvocationArgument,
            )
        })
        .collect();

    if ctx
        .get_macro_table()
        .has_emitted_statement_macro_function(&name)
    {
        return Some(call_text);
    }

    if ctx.push_expansion(&name) {
        crate::macro_error::add_macro_error(
            ctx,
            site,
            &name,
            "Detected recursive statement macro expansion.",
            "Refactor recursive macro expansion to avoid infinite expansion chains.",
            span,
        );

        return None;
    }

    let function_environment: HashMap<String, crate::macro_type::InferredType> = parameters
        .iter()
        .zip(type_args.iter())
        .map(|(parameter, type_arg)| {
            (
                parameter.clone(),
                crate::macro_type::InferredType::Known(type_arg.clone()),
            )
        })
        .collect();

    ctx.set_type_environment(function_environment);

    let statement_macro_function: Option<String> = self::build_statement_macro_function(
        ctx,
        site,
        &name,
        &parameters,
        &lowered_args,
        &MacroParameterFlags {
            mutable: mutable_parameters,
            pointer: pointer_parameters,
        },
        span,
    );

    ctx.clear_type_environment();

    ctx.pop_expansion();

    let Some(statement_macro_function_text) = statement_macro_function else {
        crate::macro_error::add_macro_error(
            ctx,
            site,
            &name,
            "Could not lower statement macro body safely.",
            "This macro body currently requires unsupported statement semantics.",
            span,
        );

        return None;
    };

    ctx.get_mut_macro_table()
        .add_pending_statement_macro_function(statement_macro_function_text);
    ctx.get_mut_macro_table()
        .mark_statement_macro_function_emitted(&name);

    Some(call_text)
}

pub fn try_extract_expression_macro_call(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    span: Span,
) -> Option<String> {
    // clang collapses the source extents of every node produced by a
    // function-like macro expansion onto the invocation site, which makes the
    // expanded AST unusable for reconstructing operators and precedence. Route
    // the invocation through the macro engine instead: substitute the
    // invocation arguments into the macro body and lower the parsed body, so
    // every operator is resolved by the internal parser.
    let key: String = crate::macro_table::MacroTable::make_location_key(site)?;

    let name: String = ctx.get_macro_table().find_macro_name(&key)?;

    let definition: &crate::macro_table::MacroFunctionLikeDefinition =
        ctx.get_macro_table().get_function_like_definition(&name)?;

    if definition.get_kind() != crate::macro_table::MacroFunctionLikeKind::PureFunction {
        return None;
    }

    let parameters: Vec<String> = definition.get_parameters().to_vec();
    let body: Vec<MacroToken> = definition.get_body().to_vec();

    let (args, arg_ranges, invocation_end): (Vec<Vec<MacroToken>>, Vec<(u32, u32)>, u32) =
        crate::macro_lex::lex_site_call_arguments(site, &name)?;

    // Only treat the entity as the macro invocation when its expansion extent
    // matches the invocation exactly; otherwise this is a larger expression
    // that merely contains the invocation.
    let entity_end: u32 = site.get_range()?.get_end().get_expansion_location().offset;

    if entity_end != invocation_end {
        return None;
    }

    if args.len() != parameters.len() {
        return None;
    }

    let renamed: Vec<MacroToken> = self::substitute_tokens(&body, &parameters, &args)?;

    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
        crate::macro_expand::MacroExpansionContext::new_default();

    let expanded: Vec<MacroToken> = crate::macro_expand::expand_function_tokens(
        &renamed,
        ctx.get_macro_table(),
        &mut expansion_context,
    )
    .ok()?;

    let spellings: Vec<String> = crate::macro_token::texts(&expanded);

    let parsed: MacroExpr = MacroParser::parse(&spellings).ok()?;

    let prefix: String = self::expansion_prefix(site);

    let result_type: String = match site.get_type() {
        Some(ty) => crate::type_format::format_clang_type_thrust(&ty, ctx, &prefix, span).ok()?,
        None => "s32".to_string(),
    };

    let clang_types: Vec<Option<clang::Type<'_>>> = self::argument_clang_types(site, &arg_ranges);

    let mut type_environment: HashMap<String, crate::macro_type::InferredType> = HashMap::new();

    for (index, parameter) in parameters.iter().enumerate() {
        let inferred: Option<crate::macro_type::InferredType> = clang_types
            .get(index)
            .and_then(|clang_type| clang_type.as_ref())
            .and_then(|clang_type| {
                crate::macro_type::MacroType::from_clang_type(ctx, clang_type, span)
            });

        if let Some(inferred) = inferred {
            type_environment.insert(parameter.clone(), inferred.clone());

            let arg_spelling: Option<String> = args.get(index).and_then(|tokens| {
                let texts: Vec<String> = crate::macro_token::texts(tokens);

                if texts.len() == 1 {
                    Some(texts[0].clone())
                } else {
                    None
                }
            });

            if let Some(arg_spelling) = arg_spelling {
                type_environment.insert(arg_spelling, inferred);
            }
        }
    }

    ctx.set_type_environment(type_environment);

    let lowered: String = crate::macro_expr::lower(
        ctx,
        &parsed,
        crate::location::Location::RValue,
        &result_type,
    );

    ctx.clear_type_environment();

    Some(lowered)
}

pub fn classify_macro_definition(decl: &clang::Entity<'_>) -> Option<(String, MacroKind)> {
    let raw_name: String = decl.get_name()?;

    if unsafe { !decl.is_function_like_macro_unchecked() } {
        return Some((raw_name, MacroKind::Object));
    }

    let tokens: Vec<MacroToken> = decl
        .get_range()
        .map(|range| {
            let clang_tokens: Vec<clang::token::Token<'_>> = range.tokenize();
            crate::macro_lex::to_macro_tokens(&clang_tokens, MacroTokenOrigin::DefinitionBody)
        })
        .unwrap_or_default();

    let token_texts: Vec<String> = crate::macro_token::texts(&tokens);

    let name_index: Option<usize> = token_texts.iter().position(|token| token == &raw_name);

    let Some(name_index) = name_index else {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::StatementMalformed),
        ));
    };

    if token_texts
        .get(name_index + 1)
        .is_some_and(|token| token != "(")
    {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::ExpectedToken)));
    }

    let mut parameters: Vec<String> = Vec::new();
    let mut body_start: usize = token_texts.len();
    let mut depth: i32 = 0;
    let mut variadic: bool = false;
    let mut dot_run: usize = 0;
    let mut saw_parameter_syntax: bool = false;
    let mut saw_parameter_list_close: bool = false;

    for (index, token) in token_texts.iter().enumerate().skip(name_index + 1) {
        if token == "(" {
            depth += 1;
            dot_run = 0;
            continue;
        }

        if token == ")" {
            depth -= 1;

            if depth == 0 {
                body_start = index + 1;
                saw_parameter_list_close = true;
                break;
            }

            dot_run = 0;
            continue;
        }

        if depth == 1 {
            if token == "..." {
                variadic = true;
                saw_parameter_syntax = true;
            } else if token == "." {
                dot_run += 1;
                saw_parameter_syntax = true;

                if dot_run >= 3 {
                    variadic = true;
                }
            } else if token != "," {
                dot_run = 0;
                saw_parameter_syntax = true;
                parameters.push(token.to_string());
            } else {
                dot_run = 0;
                saw_parameter_syntax = true;
            }
        }
    }

    if !saw_parameter_list_close {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::InvocationMalformed),
        ));
    }

    if variadic
        || parameters.iter().any(|parameter| {
            !parameter
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        })
    {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::VariadicArguments),
        ));
    }

    if parameters.is_empty() && saw_parameter_syntax {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::InvocationMalformed),
        ));
    }

    if body_start > token_texts.len() {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::UnexpectedEndOfTokens),
        ));
    }

    let body: Vec<MacroToken> = tokens[body_start..].to_vec();

    if body.iter().any(|token| token.get_text() == "##") {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::TokenPasting)));
    }

    if body.iter().any(|token| token.get_text() == "__VA_ARGS__") {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::VariadicArguments),
        ));
    }

    if body.iter().any(|token| token.get_text() == "#") {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::Stringizing)));
    }

    if body
        .first()
        .is_some_and(|token| token.get_text() == "do" || token.get_text() == "{")
    {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    let body_texts: Vec<String> = crate::macro_token::texts(&body);

    if crate::macro_parser::MacroParser::parse(&body_texts).is_ok() {
        return Some((raw_name, MacroKind::PureFunction { parameters, body }));
    }

    let body_spellings: Vec<String> = crate::macro_token::texts(&body);

    let mut body_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&body_spellings);

    if body_parser.parse_statement_body().is_ok() {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    Some((
        raw_name,
        MacroKind::Unsupported(MacroLimit::StatementMalformed),
    ))
}

pub fn emit_function_as_macro_fn(
    name: &str,
    parameters: &[String],
    body_lines: &[String],
) -> String {
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

pub fn expansion_prefix<'clang>(entity: impl Borrow<clang::Entity<'clang>>) -> String {
    let entity: &clang::Entity<'clang> = entity.borrow();

    let Some(location) = entity.get_location() else {
        return String::new();
    };

    let expansion: clang::source::Location<'_> = location.get_expansion_location();

    let Some(file) = expansion.file else {
        return String::new();
    };

    let path: PathBuf = file.get_path();

    let display: String = path.display().to_string();

    if display.trim().is_empty() {
        return String::new();
    }

    format!("{}:{}:{}: ", display, expansion.line, expansion.column)
}

pub fn origin_note(entity: &clang::Entity<'_>) -> String {
    if !crate::macro_lex::is_from_macro_expansion(entity) {
        return String::new();
    }

    let display: String = entity
        .get_range()
        .map(|range| {
            range
                .get_start()
                .get_spelling_location()
                .file
                .map(|file| file.get_path().display().to_string())
                .unwrap_or_default()
        })
        .unwrap_or_default();

    if display.trim().is_empty() {
        return String::new();
    }

    format!(" (from macro expansion in {display})")
}

fn macro_body_calls(body: &[MacroToken], statement_names: &[String], own_name: &str) -> bool {
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
