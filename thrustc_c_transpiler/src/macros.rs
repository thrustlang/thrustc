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

use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::path::PathBuf;
use thrustc_code_location::Span;

const C_MACRO_SYMBOL_PREFIX: &str = "__c_macro_";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroLoopContext {
    Loop {
        continue_action: Option<String>,
    },
    Switch {
        continue_flag: String,
    },
    DoWhileZero,
}

impl MacroLoopContext {
    #[inline]
    pub fn is_switch(&self) -> bool {
        matches!(self, Self::Switch { .. })
    }

    #[inline]
    pub fn continue_action(&self) -> Option<&str> {
        match self {
            Self::Loop { continue_action } => continue_action.as_deref(),
            Self::Switch { .. } | Self::DoWhileZero => None,
        }
    }

    #[inline]
    pub fn continue_flag(&self) -> Option<&str> {
        match self {
            Self::Switch { continue_flag } => Some(continue_flag),
            Self::Loop { .. } | Self::DoWhileZero => None,
        }
    }
}

#[derive(Debug)]
pub struct MacroContext<'clang> {
    table: crate::macro_table::MacroTable,
    context: crate::context::TranspilerContext,
    expansion_stack: Vec<String>,
    temporary_counter: u64,
    pending_statements: Vec<String>,
    type_environment: HashMap<String, crate::macro_type::InferredType>,
    inline_bindings: HashMap<String, MacroExpr>,
    value_types: HashMap<String, crate::macro_type::MacroValueType>,
    record_fields:
        HashMap<String, HashMap<String, crate::macro_type::MacroValueType>>,
    return_type: Option<String>,
    loop_contexts: Vec<MacroLoopContext>,
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
            inline_bindings: HashMap::new(),
            value_types: HashMap::new(),
            record_fields: HashMap::new(),
            return_type: None,
            loop_contexts: Vec::new(),
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
    pub fn set_type_binding(
        &mut self,
        name: String,
        kind: crate::macro_type::InferredType,
    ) {
        self.type_environment.insert(name, kind);
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

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_inline_binding(&self, name: &str) -> Option<&MacroExpr> {
        self.inline_bindings.get(name)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_inline_bindings(&mut self, bindings: HashMap<String, MacroExpr>) {
        self.inline_bindings = bindings;
    }

    #[inline]
    pub fn clear_inline_bindings(&mut self) {
        self.inline_bindings.clear();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_return_type(&self) -> Option<&str> {
        self.return_type.as_deref()
    }

    #[inline]
    pub fn set_return_type(&mut self, return_type: Option<String>) {
        self.return_type = return_type;
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_loop_context(&mut self, context: MacroLoopContext) {
        self.loop_contexts.push(context);
    }

    #[inline]
    pub fn pop_loop_context(&mut self) {
        self.loop_contexts.pop();
    }

    #[inline]
    pub fn get_last_loop_context(&self) -> Option<&MacroLoopContext> {
        self.loop_contexts.last()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementMacroExpansion {
    Call(String),
    Inline(Vec<String>),
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_value_type(
        &self,
        name: &str,
    ) -> Option<&crate::macro_type::MacroValueType> {
        self.value_types.get(name)
    }

    #[inline]
    pub fn set_value_type(
        &mut self,
        name: String,
        kind: crate::macro_type::MacroValueType,
    ) {
        self.value_types.insert(name, kind);
    }

    #[inline]
    pub fn clear_value_types(&mut self) {
        self.value_types.clear();
    }

    #[inline]
    pub fn get_record_field(
        &self,
        record: &str,
        field: &str,
    ) -> Option<&crate::macro_type::MacroValueType> {
        self.record_fields
            .get(record)
            .and_then(|fields| fields.get(field))
    }

    #[inline]
    pub fn register_record_fields(
        &mut self,
        record: String,
        fields: HashMap<String, crate::macro_type::MacroValueType>,
    ) {
        self.record_fields.insert(record, fields);
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

    let mut joined_lines: String = match self::translate_statement_body_and_expand(
        ctx,
        &body_tokens,
        parameters,
        &template_args,
    ) {
        Some(lines) => lines.join("\n"),
        None => {
            self::try_to_translate_expanded_statement_body(ctx, site, name, parameters, args, span)?
        }
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

fn translate_statement_body_and_expand(
    ctx: &mut MacroContext<'_>,
    body_tokens: &[MacroToken],
    parameters: &[String],
    args: &[Vec<MacroToken>],
) -> Option<Vec<String>> {
    let renamed: Vec<MacroToken> =
        crate::macro_expand::substitute_parameter_tokens(body_tokens, parameters, args)?;

    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
        crate::macro_expand::MacroExpansionContext::new(64, 128);

    let expanded_tokens: Vec<MacroToken> = crate::macro_expand::expand_function_tokens(
        &renamed,
        ctx.get_macro_table(),
        &mut expansion_context,
    )
    .ok()?;

    let statement_lexemes: Vec<String> = crate::macro_token::texts(&expanded_tokens);

    let mut statement_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&statement_lexemes);

    let parsed: Vec<crate::macro_ast::MacroStmt> = statement_parser.parse_statement_body().ok()?;

    let mut lines: Vec<String> = Vec::new();

    for stmt in parsed.iter() {
        lines.extend(stmt.lower(ctx, 1).ok()?);
    }

    Some(lines)
}

fn try_to_translate_expanded_statement_body(
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
    let substituted_tokens: Vec<MacroToken> = crate::macro_expand::substitute_parameter_tokens(
        &generalized_tokens,
        parameters,
        &template_args,
    )?;

    joined_lines = crate::macro_lex::detokenize_macro_tokens(&substituted_tokens);

    Some(joined_lines)
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

            if crate::macro_lex::is_parameter_token_written(tokens, index) {
                mutable_parameters.insert(parameter.clone());
                break;
            }
        }
    }

    mutable_parameters
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
    let operators: [&str; 11] = [
        "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "|=", "^=",
    ];

    for parameter in mutable_parameters.iter() {
        for operator in operators.iter() {
            let with_parens: String = format!("({parameter}->[0]) {operator}");
            let without_parens: String = format!("{parameter}->[0] {operator}");

            out = out.replace(&with_parens, &without_parens);
        }
    }

    out
}

fn cast_mutable_parameter_assignments(
    lines: &str,
    parameters: &[String],
    mutable_parameters: &HashSet<String>,
) -> String {
    let mut out_lines: Vec<String> = Vec::new();
    let operators: [(&str, Option<&str>); 11] = [
        ("=", None),
        ("+=", Some("+")),
        ("-=", Some("-")),
        ("*=", Some("*")),
        ("/=", Some("/")),
        ("%=", Some("%")),
        ("<<=", Some("<<")),
        (">>=", Some(">>")),
        ("&=", Some("&")),
        ("|=", Some("|")),
        ("^=", Some("^")),
    ];

    for line in lines.lines() {
        let mut rewritten_line: String = line.to_string();

        for (index, parameter) in parameters.iter().enumerate() {
            if !mutable_parameters.contains(parameter) {
                continue;
            }

            for (operator, binary_operator) in operators.iter() {
                let lhs: String = format!("{parameter}->[0] {operator}");

                if !rewritten_line.contains(&lhs) || !rewritten_line.trim_end().ends_with(';') {
                    continue;
                }

                let Some(eq_pos) = rewritten_line.find(&lhs) else {
                    continue;
                };

                let rhs_start: usize = eq_pos.saturating_add(lhs.len());
                let rhs_with_semicolon: &str = &rewritten_line[rhs_start..];
                let rhs_expression: &str =
                    rhs_with_semicolon.trim().trim_end_matches(';').trim();
                let type_name: String = format!("T{}", index.saturating_add(1));
                let lhs_text: String = format!("{parameter}->[0]");

                rewritten_line = match binary_operator {
                    Some(binary_operator) => format!(
                        "{}{} = (({}) {} ({})) as {};",
                        &rewritten_line[..eq_pos],
                        lhs_text,
                        lhs_text,
                        binary_operator,
                        rhs_expression,
                        type_name
                    ),
                    None => format!(
                        "{}{} ({}) as {};",
                        &rewritten_line[..rhs_start],
                        " ",
                        rhs_expression,
                        type_name
                    ),
                };

                break;
            }
        }

        out_lines.push(rewritten_line);
    }

    out_lines.join("\n")
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
                if crate::macro_lex::is_macro_calling_other_statements(body, &statement_names, name)
                {
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
    function_return_type: Option<&str>,
) -> Option<StatementMacroExpansion> {
    if !matches!(
        site.get_kind(),
        clang::EntityKind::DoStmt | clang::EntityKind::CompoundStmt
    ) {
        return None;
    }

    let key: String = crate::macro_table::MacroTable::get_location(site)?;

    let name: String = ctx.get_macro_table().find_macro_name(&key)?;

    let Some(parameters) = ctx.get_macro_table().get_parameter_names(&name) else {
        if let Some(definition) = ctx.get_macro_table().get_function_like_definition(&name)
            && let crate::macro_table::MacroFunctionLikeKind::Unsupported(limit) =
                definition.get_kind()
        {
            let (detail, help) = crate::macro_error::get_macro_issue_help(&name, limit);

            crate::macro_error::add_macro_error(
                ctx, site, &name, &detail, &help, span,
            );
        } else {
            crate::macro_error::add_macro_error(
                ctx,
                site,
                &name,
                "Missing macro parameter metadata for statement macro function extraction.",
                "Ensure this macro is captured by the transpiler macro table before expansion.",
                span,
            );
        }

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
                crate::macro_expand::MacroExpansionContext::new(64, 128);

            crate::macro_expand::expand_function_tokens(
                arg_tokens,
                ctx.get_macro_table(),
                &mut expansion_context,
            )
            .unwrap_or_else(|_| arg_tokens.clone())
        };

        let arg_lexemes: Vec<String> = crate::macro_token::texts(&expanded_arg);

        let parsed: MacroExpr = match MacroParser::parse(&arg_lexemes) {
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

    let clang_types: Vec<Option<clang::Type<'_>>> =
        crate::macro_lex::get_argument_clang_types(site, &arg_ranges);

    let body_tokens: Vec<MacroToken> = ctx
        .get_macro_table()
        .get_body_macro_tokens(&name)
        .unwrap_or_default();

    let expanded_body_tokens: Vec<MacroToken> = {
        let mut expansion_context: crate::macro_expand::MacroExpansionContext =
            crate::macro_expand::MacroExpansionContext::new(64, 128);

        crate::macro_expand::expand_function_tokens(
            &body_tokens,
            ctx.get_macro_table(),
            &mut expansion_context,
        )
        .unwrap_or(body_tokens.clone())
    };

    let mutable_parameters: HashSet<String> =
        self::detect_mutated_parameters(&expanded_body_tokens, &parameters);

    let mut pointer_parameters: HashSet<String> = {
        let mut combined: HashSet<String> = mutable_parameters.clone();

        combined.extend(self::detect_pointer_parameters(
            &expanded_body_tokens,
            &parameters,
        ));

        combined
    };

    let mut array_parameters: HashSet<String> = HashSet::new();

    for (index, clang_type) in clang_types.iter().enumerate() {
        let kind: Option<clang::TypeKind> =
            clang_type.as_ref().map(|ty| ty.get_canonical_type().get_kind());

        let is_pointer: bool = matches!(
            kind,
            Some(
                clang::TypeKind::Pointer
                    | clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
            )
        );

        if is_pointer {
            pointer_parameters.insert(parameters[index].clone());
        }

        if matches!(
            kind,
            Some(clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray)
        ) {
            array_parameters.insert(parameters[index].clone());
        }
    }

    let indexed_parameters: HashSet<String> =
        self::detect_indexed_parameters(&expanded_body_tokens, &parameters);

    let mut arg_texts: Vec<String> = parsed_args
        .iter()
        .map(|parsed| {
            crate::macro_expr::lower(ctx, parsed, crate::location::Location::RValue, "T1")
        })
        .collect();

    let body_lexemes: Vec<String> = crate::macro_token::texts(&expanded_body_tokens);

    let mut body_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&body_lexemes);

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

    let needs_inline: bool = body_statements
        .iter()
        .any(|statement| statement.requires_caller_context(0, 0));

    if needs_inline {
        let bindings: HashMap<String, MacroExpr> = parameters
            .iter()
            .cloned()
            .zip(parsed_args.iter().cloned())
            .collect();

        let environment: HashMap<String, crate::macro_type::InferredType> = parameters
            .iter()
            .enumerate()
            .map(|(index, parameter)| {
                let seed: Option<crate::macro_type::InferredType> =
                    seeds.get(index).cloned().flatten();

                let kind: crate::macro_type::InferredType = match seed {
                    Some(crate::macro_type::InferredType::Pointer(_)) => {
                        seed.unwrap_or_else(|| {
                            crate::macro_type::InferredType::Known(
                                type_args
                                    .get(index)
                                    .cloned()
                                    .unwrap_or_else(|| "s32".to_string()),
                            )
                        })
                    }
                    _ => crate::macro_type::InferredType::Known(
                        type_args
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| "s32".to_string()),
                    ),
                };

                (parameter.clone(), kind)
            })
            .collect();

        let previous_return_type: Option<String> = ctx.get_return_type().map(str::to_string);
        let previous_environment: HashMap<String, crate::macro_type::InferredType> =
            ctx.get_type_environment().clone();

        ctx.set_inline_bindings(bindings);
        ctx.set_type_environment(environment);
        ctx.set_return_type(function_return_type.map(str::to_string));

        for (parameter, kind) in ctx.get_type_environment().clone() {
            ctx.set_value_type(
                parameter,
                crate::macro_type::MacroValueType::from_inferred_type(&kind),
            );
        }

        for (index, clang_type) in clang_types.iter().enumerate() {
            let Some(clang_type) = clang_type.as_ref() else {
                continue;
            };

            let Some(shape) = crate::macro_type::MacroValueType::from_clang_type(clang_type)
            else {
                continue;
            };

            ctx.set_value_type(parameters[index].clone(), shape.clone());

            if let Some(MacroExpr::Ident(argument)) = parsed_args.get(index) {
                ctx.set_value_type(argument.clone(), shape);
            }
        }

        let mut lines: Vec<String> = vec!["{".to_string()];

        for statement in body_statements.iter() {
            let lowered: Result<Vec<String>, MacroLimit> = statement.lower(ctx, 1);

            let Ok(lowered) = lowered else {
                ctx.clear_inline_bindings();
                ctx.clear_value_types();
                ctx.set_type_environment(previous_environment);
                ctx.set_return_type(previous_return_type);

                crate::macro_error::add_macro_error(
                    ctx,
                    site,
                    &name,
                    "Could not lower inline statement macro body.",
                    "Simplify the macro body or extend macro parser coverage.",
                    span,
                );

                return None;
            };

            lines.extend(lowered);
        }

        lines.push("}".to_string());

        ctx.clear_inline_bindings();
        ctx.clear_value_types();
        ctx.set_type_environment(previous_environment);
        ctx.set_return_type(previous_return_type);

        return Some(StatementMacroExpansion::Inline(lines));
    }

    for (index, argument_text) in arg_texts.iter_mut().enumerate() {
        let parameter: &String = &parameters[index];

        if !pointer_parameters.contains(parameter) {
            continue;
        }

        if array_parameters.contains(parameter) {
            *argument_text = format!("{argument_text} as ptr[{}]", type_args[index]);
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
        return Some(StatementMacroExpansion::Call(call_text));
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
        .enumerate()
        .map(|(index, parameter)| {
            let seed: Option<crate::macro_type::InferredType> =
                seeds.get(index).cloned().flatten();

            let inferred: crate::macro_type::InferredType = match seed {
                Some(crate::macro_type::InferredType::Pointer(_)) => seed.unwrap_or_else(|| {
                    crate::macro_type::InferredType::Known(
                        type_args
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| "s32".to_string()),
                    )
                }),
                _ => crate::macro_type::InferredType::Known(
                    type_args
                        .get(index)
                        .cloned()
                        .unwrap_or_else(|| "s32".to_string()),
                ),
            };

            (parameter.clone(), inferred)
        })
        .collect();

    ctx.set_type_environment(function_environment);

    for (parameter, kind) in ctx.get_type_environment().clone() {
        ctx.set_value_type(
            parameter,
            crate::macro_type::MacroValueType::from_inferred_type(&kind),
        );
    }

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
    ctx.clear_value_types();

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

    Some(StatementMacroExpansion::Call(call_text))
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
    let key: String = crate::macro_table::MacroTable::get_location(site)?;
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

    let renamed: Vec<MacroToken> =
        crate::macro_expand::substitute_parameter_tokens(&body, &parameters, &args)?;

    let mut expansion_context: crate::macro_expand::MacroExpansionContext =
        crate::macro_expand::MacroExpansionContext::new(64, 128);

    let expanded: Vec<MacroToken> = crate::macro_expand::expand_function_tokens(
        &renamed,
        ctx.get_macro_table(),
        &mut expansion_context,
    )
    .ok()?;

    let lexemes: Vec<String> = crate::macro_token::texts(&expanded);

    let parsed: MacroExpr = MacroParser::parse(&lexemes).ok()?;

    let prefix: Option<String> = crate::macro_error::prefix(site);

    let result_type: String = match site.get_type() {
        Some(ty) => {
            crate::type_format::format_clang_type_thrust(&ty, ctx, prefix.as_deref(), span).ok()?
        }
        None => "s32".to_string(),
    };

    let clang_types: Vec<Option<clang::Type<'_>>> =
        crate::macro_lex::get_argument_clang_types(site, &arg_ranges);

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

            let arg_lexeme: Option<String> = args.get(index).and_then(|tokens| {
                let texts: Vec<String> = crate::macro_token::texts(tokens);

                if texts.len() == 1 {
                    Some(texts[0].clone())
                } else {
                    None
                }
            });

            if let Some(arg_lexeme) = arg_lexeme {
                type_environment.insert(arg_lexeme, inferred);
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

    let body_lexemes: Vec<String> = crate::macro_token::texts(&body);

    let mut body_parser: crate::macro_parser::MacroParser<'_> =
        crate::macro_parser::MacroParser::new(&body_lexemes);

    if body_parser.parse_statement_body().is_ok() {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    Some((
        raw_name,
        MacroKind::Unsupported(MacroLimit::StatementMalformed),
    ))
}
