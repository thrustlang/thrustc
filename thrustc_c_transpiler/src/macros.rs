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
use crate::macro_expr::MacroCursor;
use crate::macro_token::{MacroToken, MacroTokenOrigin};

use std::borrow::Borrow;
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
}

impl<'clang> MacroContext<'clang> {
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
}

impl<'clang> MacroContext<'clang> {
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

pub fn append_translated_macro_consts(
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
                let base: String = crate::util::sanitize_thrust_identifier(&raw_name);

                format!("{C_MACRO_SYMBOL_PREFIX}{base}")
            };

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

            let body_calls_code: bool = macro_decl
                .get_range()
                .map(|range| {
                    range
                        .tokenize()
                        .into_iter()
                        .map(|token| token.get_spelling())
                        .any(|spelling| spelling == "(")
                })
                .unwrap_or(false);

            let limit: MacroLimit = if body_calls_code {
                MacroLimit::CallBodied
            } else {
                MacroLimit::ExpressionMalformed
            };

            let prefix: String = self::expansion_prefix(macro_decl);
            let (detail, help) = crate::macro_error::get_macro_issue_help(&raw_name, limit);
            crate::macro_error::add_macro_error(
                ctx,
                macro_decl,
                &raw_name,
                &format!("{prefix}{detail}"),
                &help,
                span,
            );
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
                        match crate::macro_expand::expand_function_like_tokens(
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
                    let parsed: MacroExpr = match MacroCursor::parse(&body_spellings) {
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

                    let body_text: String = crate::macro_expr::lower_function_body(&parsed, "T1");

                    fns_out.push_str(&self::emit_function_like_macro_fn(
                        raw_name, parameters, &body_text,
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

fn build_statement_macro_function(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    name: &str,
    parameters: &[String],
    args: &[Vec<MacroToken>],
    mutable_parameters: &HashSet<String>,
    span: Span,
) -> Option<String> {
    let function_name: String = {
        let base: String = crate::util::sanitize_thrust_identifier(name);

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

    if !mutable_parameters.is_empty() {
        joined_lines = self::rewrite_mutable_parameter_uses(&joined_lines, mutable_parameters);
        joined_lines = self::strip_parenthesized_mutable_lhs(&joined_lines, mutable_parameters);
        joined_lines =
            self::cast_mutable_parameter_assignments(&joined_lines, parameters, mutable_parameters);
    }

    let type_names: Vec<String> = (1..=parameters.len())
        .map(|index| format!("T{index}"))
        .collect();

    let parameter_texts: Vec<String> = parameters
        .iter()
        .zip(type_names.iter())
        .map(|(parameter, type_name)| {
            let parameter_name: String = { crate::util::sanitize_thrust_identifier(parameter) };

            if mutable_parameters.contains(parameter) {
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
    ctx: &MacroContext<'_>,
    body_tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Option<Vec<String>> {
    let renamed: Vec<MacroToken> = self::substitute_tokens(body_tokens, parameters, args)?;

    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
        crate::macro_expand::MacroExpansionContext::new_default();

    let expanded_tokens: Vec<MacroToken> = crate::macro_expand::expand_function_like_tokens(
        &renamed,
        ctx.get_macro_table(),
        &mut expansion_context,
    )
    .ok()?;

    let parsed: Vec<crate::macro_ast::MacroStmt> =
        crate::macro_stmt::parse_statement_body_tokens(&expanded_tokens).ok()?;

    let mut lines: Vec<String> = Vec::new();

    for stmt in parsed.iter() {
        lines.extend(stmt.lower(1).ok()?);
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
            crate::stmt::translate_stmt(child, 1, span, Some("void"), None, ctx);

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

fn site_call_arguments(site: &clang::Entity<'_>, name: &str) -> Option<Vec<Vec<MacroToken>>> {
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

    let mut args: Vec<Vec<MacroToken>> = Vec::new();

    for (start, end) in arg_ranges.iter() {
        if start >= end {
            args.push(Vec::new());
            continue;
        }

        let start_offset: u32 = u32::try_from(*start).ok()?;
        let end_offset: u32 = u32::try_from(*end).ok()?;

        let start_location = file.get_offset_location(start_offset);
        let end_location = file.get_offset_location(end_offset);

        let arg_range: clang::source::SourceRange<'_> =
            clang::source::SourceRange::new(start_location, end_location);

        let tokens: Vec<clang::token::Token<'_>> = arg_range.tokenize();
        let macro_tokens: Vec<MacroToken> =
            crate::macro_lex::to_macro_tokens(&tokens, MacroTokenOrigin::InvocationArgument);

        args.push(macro_tokens);
    }

    Some(args)
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

    let Some(args) = self::site_call_arguments(site, &name) else {
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
        let base: String = crate::util::sanitize_thrust_identifier(&name);

        format!("{C_MACRO_SYMBOL_PREFIX}{base}")
    };

    let mut arg_texts: Vec<String> = Vec::new();

    for arg_tokens in args.iter() {
        let arg_spellings: Vec<String> = crate::macro_token::texts(arg_tokens);
        let parsed: MacroExpr = match MacroCursor::parse(&arg_spellings) {
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

        arg_texts.push(crate::macro_expr::lower(
            &parsed,
            crate::location::Location::RValue,
        ));
    }

    let mutable_parameters: HashSet<String> = ctx
        .get_macro_table()
        .get_body_macro_tokens(&name)
        .as_ref()
        .map(|tokens| self::detect_mutated_parameters(tokens, &parameters))
        .unwrap_or_default();

    for (index, argument_text) in arg_texts.iter_mut().enumerate() {
        if !mutable_parameters.contains(&parameters[index]) {
            continue;
        }

        if argument_text.starts_with("ref ") {
            continue;
        }

        *argument_text = format!("ref {argument_text}");
    }

    let call_text: String = format!("{function_name}({})", arg_texts.join(", "));

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

    let statement_macro_function: Option<String> = self::build_statement_macro_function(
        ctx,
        site,
        &name,
        &parameters,
        &lowered_args,
        &mutable_parameters,
        span,
    );

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

    let mut depth: i32 = 0;
    let mut has_top_level_semicolon: bool = false;

    for token in body.iter() {
        if token.get_text() == "(" || token.get_text() == "[" || token.get_text() == "{" {
            depth += 1;
        } else if token.get_text() == ")" || token.get_text() == "]" || token.get_text() == "}" {
            depth -= 1;
        } else if token.get_text() == ";" && depth == 0 {
            has_top_level_semicolon = true;
            break;
        }
    }

    if has_top_level_semicolon {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    Some((raw_name, MacroKind::PureFunction { parameters, body }))
}

pub fn emit_function_like_macro_fn(name: &str, parameters: &[String], body_text: &str) -> String {
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
            let parameter_name: String = { crate::util::sanitize_thrust_identifier(parameter) };

            format!("{parameter_name}: {type_name}")
        })
        .collect();

    let function_name: String = {
        let base: String = crate::util::sanitize_thrust_identifier(name);

        format!("{C_MACRO_SYMBOL_PREFIX}{base}")
    };

    let mut out: String = String::new();

    out.push_str("fn ");
    out.push_str(&function_name);
    out.push('[');
    out.push_str(&type_names.join(", "));
    out.push_str("](");
    out.push_str(&parameter_texts.join(", "));
    out.push_str(") T1 @alwaysInline {\n    return ");
    out.push_str(body_text);
    out.push_str(";\n}\n");

    out
}

pub fn extract_include_spec(entity: &clang::Entity<'_>) -> Option<String> {
    let range: clang::source::SourceRange<'_> = entity.get_range()?;
    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
    let spellings: Vec<String> = tokens.into_iter().map(|t| t.get_spelling()).collect();

    for s in spellings.iter() {
        if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
            return Some(s[1..s.len() - 1].to_string());
        }

        if s.starts_with('<') && s.ends_with('>') && s.len() >= 2 {
            return Some(s[1..s.len() - 1].to_string());
        }
    }

    let angle_start: Option<usize> = spellings.iter().position(|s| s == "<");

    let angle_content: String = angle_start
        .map(|start| {
            spellings[start.saturating_add(1)..]
                .iter()
                .take_while(|s| s.as_str() != ">")
                .fold(String::new(), |mut acc, s| {
                    acc.push_str(s);

                    acc
                })
        })
        .unwrap_or_default();

    if !angle_content.is_empty() {
        return Some(angle_content);
    }

    None
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
