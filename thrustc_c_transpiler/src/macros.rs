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

use crate::macro_expr::{MacroCursor, MacroExpr, MacroLimit};
use std::borrow::Borrow;
use std::marker::PhantomData;
use std::path::PathBuf;
use thrustc_code_location::Span;
use thrustc_compile_time::BuiltinValue;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

#[derive(Debug)]
pub(crate) struct MacroContext<'clang> {
    table: crate::macro_table::MacroTable,
    issues: crate::context::TranspilerContext,
    expansion_stack: Vec<String>,
    temporary_counter: u64,
    marker: PhantomData<&'clang ()>,
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn new(main_file: PathBuf) -> Self {
        Self {
            table: crate::macro_table::MacroTable::new(main_file),
            issues: crate::context::TranspilerContext::new(),
            expansion_stack: Vec::new(),
            temporary_counter: 0,
            marker: PhantomData,
        }
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn get_macro_table(&self) -> &crate::macro_table::MacroTable {
        &self.table
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn get_mut_macro_table(&mut self) -> &mut crate::macro_table::MacroTable {
        &mut self.table
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn get_transpiler_context(&self) -> &crate::context::TranspilerContext {
        &self.issues
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn get_mut_transpiler_context(&mut self) -> &mut crate::context::TranspilerContext {
        &mut self.issues
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn push_expansion(&mut self, name: &str) -> bool {
        if self.expansion_stack.iter().any(|entry| entry == name) {
            return true;
        }

        self.expansion_stack.push(name.to_string());

        false
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn pop_expansion(&mut self) {
        self.expansion_stack.pop();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub(crate) fn next_temporary_name(&mut self) -> String {
        let index: u64 = self.temporary_counter;

        self.temporary_counter = self.temporary_counter.saturating_add(1);

        format!("__thrustc_temporary_{index}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MacroKind {
    Object,
    PureFunction {
        parameters: Vec<String>,
        body: Vec<String>,
    },
    Statement {
        parameters: Vec<String>,
        body: Vec<String>,
    },
    Unsupported(MacroLimit),
}

pub(crate) fn append_translated_macro_consts(
    macro_decls: &[clang::Entity<'_>],
    out: &mut String,
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
                let __sanitized: String = raw_name.to_string();

                match __sanitized.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{__sanitized}_")
                    }

                    _ => __sanitized,
                }
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
                MacroLimit::NotComputable
            };

            let prefix: String = self::expansion_prefix(macro_decl);
            let (detail, help) = self::plain_rejection(&raw_name, limit);

            ctx.get_mut_transpiler_context()
                .add_warning(CompilationIssue::Warning(
                    CompilationIssueCode::W0104,
                    format!("{prefix}{detail} {help}"),
                    span,
                ));
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

        self::promote_statement_delegates(&mut classified_macros);
        ctx.get_mut_macro_table()
            .register_statement_macros(&classified_macros);

        for (macro_decl, raw_name, kind) in classified_macros.iter() {
            match kind {
                MacroKind::PureFunction { parameters, body } => {
                    let parsed: Result<MacroExpr, MacroLimit> = MacroCursor::parse(body);

                    let Ok(parsed) = parsed else {
                        let prefix: String = self::expansion_prefix(macro_decl);
                        let (detail, help) =
                            self::plain_rejection(raw_name, MacroLimit::NotComputable);

                        ctx.get_mut_transpiler_context()
                            .add_warning(CompilationIssue::Warning(
                                CompilationIssueCode::W0104,
                                format!("{prefix}{detail} {help}"),
                                span,
                            ));

                        continue;
                    };

                    let body_text: String = crate::macro_expr::lower_function_body(&parsed, "T1");

                    out.push_str(&self::emit_function_like_macro_fn(
                        raw_name, parameters, &body_text,
                    ));

                    *emitted_functions = emitted_functions.saturating_add(1);
                }

                MacroKind::Statement { parameters, body } => {
                    ctx.get_mut_macro_table().add_statement_macro_definition(
                        raw_name.clone(),
                        parameters.clone(),
                        body.clone(),
                    );
                }
                MacroKind::Object => continue,
                MacroKind::Unsupported(limit) => {
                    let prefix: String = self::expansion_prefix(macro_decl);
                    let (detail, help) = self::plain_rejection(raw_name, *limit);

                    ctx.get_mut_transpiler_context()
                        .add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0104,
                            format!("{prefix}{detail} {help}"),
                            span,
                        ));
                }
            }
        }
    }

    if *emitted_constants > 0 || *emitted_functions > 0 {
        out.push('\n');
    }
}

fn outline_statement_body(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    name: &str,
    parameters: &[String],
    arg_texts: &[String],
    function_name: &str,
    span: Span,
) -> Option<String> {
    let body_tokens: Vec<String> = ctx.get_macro_table().get_body_tokens(name)?;

    if body_tokens.iter().any(|token| {
        token == "return" || token == "continue" || token == "break" || token == "goto"
    }) {
        return None;
    }

    let joined_lines: String = match self::lowered_statement_lines(&body_tokens, parameters) {
        Some(lines) => lines.join("\n"),
        None => self::translated_expanded_lines(ctx, site, parameters, arg_texts, span)?,
    };

    let type_names: Vec<String> = (1..=parameters.len())
        .map(|index| format!("T{index}"))
        .collect();

    let parameter_texts: Vec<String> = parameters
        .iter()
        .zip(type_names.iter())
        .map(|(parameter, type_name)| {
            let parameter_name: String = {
                let __sanitized: String = parameter.to_string();

                match __sanitized.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{__sanitized}_")
                    }

                    _ => __sanitized,
                }
            };

            format!("{parameter_name}: {type_name}")
        })
        .collect();

    let mut text: String = String::new();

    text.push_str("fn ");
    text.push_str(function_name);
    text.push('[');
    text.push_str(&type_names.join(", "));
    text.push_str("](");
    text.push_str(&parameter_texts.join(", "));
    text.push_str(") void @alwaysInline {\n");
    text.push_str(&joined_lines);
    text.push_str("\n}\n");

    Some(text)
}

fn lowered_statement_lines(body_tokens: &[String], parameters: &[String]) -> Option<Vec<String>> {
    let mut renamed: Vec<String> = Vec::new();

    for token in body_tokens.iter() {
        let mut clean: Option<String> = None;

        for parameter in parameters.iter() {
            if token == parameter {
                clean = Some(match parameter.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{parameter}_")
                    }
                    _ => parameter.to_string(),
                });
                break;
            }
        }

        renamed.push(clean.unwrap_or_else(|| token.clone()));
    }

    let parsed: Vec<crate::macro_stmt::MacroStmt> =
        crate::macro_stmt::parse_statement_body(&renamed).ok()?;

    let mut lines: Vec<String> = Vec::new();

    for stmt in parsed.iter() {
        lines.extend(stmt.lower(1).ok()?);
    }

    Some(lines)
}

fn translated_expanded_lines(
    ctx: &mut MacroContext<'_>,
    site: &clang::Entity<'_>,
    parameters: &[String],
    arg_texts: &[String],
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
        let lines: Vec<String> =
            crate::stmt::translate_stmt(child, 1, span, Some("void"), None, ctx);

        body_lines.extend(lines);
    }

    let mut pairs: Vec<(String, String)> = parameters
        .iter()
        .cloned()
        .zip(arg_texts.iter().cloned())
        .collect();

    pairs.sort_by_key(|pair| std::cmp::Reverse(pair.0.len()));

    let mut joined_lines: String = body_lines.join("\n");

    for (parameter, argument) in pairs.iter() {
        joined_lines = self::substitute_word(&joined_lines, parameter, argument);
    }

    Some(joined_lines)
}

fn substitute_word(text: &str, from: &str, to: &str) -> String {
    let mut out: String = String::with_capacity(text.len());
    let mut index: usize = 0;

    while index < text.len() {
        if text[index..].starts_with(from) {
            let before_ok: bool = index == 0
                || !text[..index]
                    .chars()
                    .next_back()
                    .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

            let after: usize = index + from.len();

            let after_ok: bool = after >= text.len()
                || !text[after..]
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

            if before_ok && after_ok {
                out.push_str(to);
                index = after;
                continue;
            }
        }

        if let Some(ch) = text[index..].chars().next() {
            out.push(ch);
            index += ch.len_utf8();
        } else {
            break;
        }
    }

    out
}

fn site_call_arguments(site: &clang::Entity<'_>, name: &str) -> Option<Vec<Vec<String>>> {
    let range: clang::source::SourceRange<'_> = site.get_range()?;

    let location = range.get_start().get_expansion_location();

    let file = location.file?;

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

    let mut args: Vec<Vec<String>> = Vec::new();

    for (start, end) in arg_ranges.iter() {
        if start >= end {
            continue;
        }

        let start_offset: u32 = u32::try_from(*start).ok()?;
        let end_offset: u32 = u32::try_from(*end).ok()?;

        let start_location = file.get_offset_location(start_offset);
        let end_location = file.get_offset_location(end_offset);

        let arg_range: clang::source::SourceRange<'_> =
            clang::source::SourceRange::new(start_location, end_location);

        let spellings: Vec<String> = arg_range
            .tokenize()
            .into_iter()
            .map(|token| token.get_spelling())
            .collect();

        if spellings.is_empty() {
            return None;
        }

        args.push(spellings);
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

pub(crate) fn promote_statement_delegates(items: &mut Vec<(clang::Entity<'_>, String, MacroKind)>) {
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

pub(crate) fn try_outline_statement_macro(
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

    let parameters: Vec<String> = ctx.get_macro_table().get_parameter_names(&name)?;

    let args: Vec<Vec<String>> = self::site_call_arguments(site, &name)?;

    if args.len() != parameters.len() {
        return None;
    }

    let function_name: String = {
        let __sanitized: String = name.to_string();

        match __sanitized.as_str() {
            "array" | "asm" | "bool" | "break" | "char" | "const" | "continue" | "deref"
            | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if" | "import"
            | "importC" | "load" | "loop" | "ptr" | "ref" | "return" | "struct" | "true"
            | "type" | "union" | "var" | "void" | "while" => format!("{__sanitized}_"),

            _ => __sanitized,
        }
    };

    let mut arg_texts: Vec<String> = Vec::new();

    for arg_tokens in args.iter() {
        let parsed: MacroExpr = MacroCursor::parse(arg_tokens).ok()?;

        arg_texts.push(crate::macro_expr::lower(
            &parsed,
            crate::location::Location::RValue,
        ));
    }

    let call_text: String = format!("{function_name}({})", arg_texts.join(", "));

    if ctx.get_macro_table().has_emitted_outline(&name) {
        return Some(call_text);
    }

    if ctx.push_expansion(&name) {
        return None;
    }

    let outlined: Option<String> = self::outline_statement_body(
        ctx,
        site,
        &name,
        &parameters,
        &arg_texts,
        &function_name,
        span,
    );

    ctx.pop_expansion();

    let outlined_text: String = outlined?;

    ctx.get_mut_macro_table()
        .add_pending_outline_function(outlined_text);
    ctx.get_mut_macro_table().mark_outline_emitted(&name);

    Some(call_text)
}

pub(crate) fn classify_macro_definition(decl: &clang::Entity<'_>) -> Option<(String, MacroKind)> {
    let raw_name: String = decl.get_name()?;

    if unsafe { !decl.is_function_like_macro_unchecked() } {
        return Some((raw_name, MacroKind::Object));
    }

    let tokens: Vec<String> = decl
        .get_range()
        .map(|range| {
            range
                .tokenize()
                .into_iter()
                .map(|token| token.get_spelling())
                .collect()
        })
        .unwrap_or_default();

    let name_index: Option<usize> = tokens.iter().position(|token| token == &raw_name);

    let Some(name_index) = name_index else {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::NotComputable)));
    };

    if tokens.get(name_index + 1).is_some_and(|token| token != "(") {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::NotComputable)));
    }

    let mut parameters: Vec<String> = Vec::new();
    let mut body_start: usize = tokens.len();
    let mut depth: i32 = 0;
    let mut variadic: bool = false;
    let mut dot_run: usize = 0;

    for (index, token) in tokens.iter().enumerate().skip(name_index + 1) {
        if token == "(" {
            depth += 1;
            dot_run = 0;
            continue;
        }

        if token == ")" {
            depth -= 1;

            if depth == 0 {
                body_start = index + 1;
                break;
            }

            dot_run = 0;
            continue;
        }

        if depth == 1 {
            if token == "..." {
                variadic = true;
            } else if token == "." {
                dot_run += 1;

                if dot_run >= 3 {
                    variadic = true;
                }
            } else if token != "," {
                dot_run = 0;
                parameters.push(token.to_string());
            } else {
                dot_run = 0;
            }
        }
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

    if parameters.is_empty() {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::NotComputable)));
    }

    if body_start > tokens.len() {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::NotComputable)));
    }

    let body: Vec<String> = tokens[body_start..].to_vec();

    if body.iter().any(|token| token == "##") {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::TokenPasting)));
    }

    if body.iter().any(|token| token == "__VA_ARGS__") {
        return Some((
            raw_name,
            MacroKind::Unsupported(MacroLimit::VariadicArguments),
        ));
    }

    if body.iter().any(|token| token == "#") {
        return Some((raw_name, MacroKind::Unsupported(MacroLimit::Stringizing)));
    }

    if body
        .first()
        .is_some_and(|token| token == "do" || token == "{")
    {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    let mut depth: i32 = 0;
    let mut has_top_level_semicolon: bool = false;

    for token in body.iter() {
        if token == "(" || token == "[" || token == "{" {
            depth += 1;
        } else if token == ")" || token == "]" || token == "}" {
            depth -= 1;
        } else if token == ";" && depth == 0 {
            has_top_level_semicolon = true;
            break;
        }
    }

    if has_top_level_semicolon {
        return Some((raw_name, MacroKind::Statement { parameters, body }));
    }

    Some((raw_name, MacroKind::PureFunction { parameters, body }))
}

pub(crate) fn emit_function_like_macro_fn(
    name: &str,
    parameters: &[String],
    body_text: &str,
) -> String {
    let type_names: Vec<String> = (1..=parameters.len())
        .map(|index| format!("T{index}"))
        .collect();

    let parameter_texts: Vec<String> = parameters
        .iter()
        .zip(type_names.iter())
        .map(|(parameter, type_name)| {
            let parameter_name: String = {
                let __sanitized: String = parameter.to_string();

                match __sanitized.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{__sanitized}_")
                    }

                    _ => __sanitized,
                }
            };

            format!("{parameter_name}: {type_name}")
        })
        .collect();

    let function_name: String = {
        let __sanitized: String = name.to_string();

        match __sanitized.as_str() {
            "array" | "asm" | "bool" | "break" | "char" | "const" | "continue" | "deref"
            | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if" | "import"
            | "importC" | "load" | "loop" | "ptr" | "ref" | "return" | "struct" | "true"
            | "type" | "union" | "var" | "void" | "while" => format!("{__sanitized}_"),

            _ => __sanitized,
        }
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

pub(crate) fn plain_rejection(name: &str, limit: MacroLimit) -> (String, String) {
    let detail: String = match limit {
        MacroLimit::TokenPasting => format!(
            "The C macro '{name}' builds new names by gluing words together, which is not supported."
        ),
        MacroLimit::VariadicArguments => format!(
            "The C macro '{name}' takes a variable number of arguments, which is not supported."
        ),
        MacroLimit::Stringizing => {
            format!("The C macro '{name}' turns code into text, which is not supported.")
        }
        MacroLimit::CallBodied => format!(
            "The C macro '{name}' runs code to get its value, so it cannot become a constant."
        ),
        MacroLimit::NotComputable => {
            format!("The C macro '{name}' could not be computed.")
        }
    };

    let help: String = match limit {
        MacroLimit::TokenPasting => {
            "Rewrite it in the C code without gluing names together.".to_string()
        }
        MacroLimit::VariadicArguments => {
            "Give it a fixed number of arguments in the C code.".to_string()
        }
        MacroLimit::Stringizing => "Write the text directly in the C code instead.".to_string(),
        MacroLimit::CallBodied => {
            "Call the underlying function directly in the C code.".to_string()
        }
        MacroLimit::NotComputable => {
            "Give it a plain number or text value in the C code.".to_string()
        }
    };

    (detail, help)
}

pub(crate) fn extract_include_spec(entity: &clang::Entity<'_>) -> Option<String> {
    let range = entity.get_range()?;
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

pub(crate) fn expansion_prefix<'tu>(entity: impl Borrow<clang::Entity<'tu>>) -> String {
    let entity: &clang::Entity<'tu> = entity.borrow();

    let Some(location) = entity.get_location() else {
        return String::new();
    };

    let expansion = location.get_expansion_location();

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

pub(crate) fn origin_note(entity: &clang::Entity<'_>) -> String {
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

fn macro_body_calls(body: &[String], statement_names: &[String], own_name: &str) -> bool {
    let mut index: usize = 0;

    while index < body.len() {
        let token: &str = &body[index];

        if token
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
            && body.get(index + 1).is_some_and(|next| next == "(")
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
