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
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use crate::expr;

pub(crate) fn translate_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
) -> Result<Vec<String>, CompilationIssue> {
    let indent_str: String = "    ".repeat(indent);

    match entity.get_kind() {
        clang::EntityKind::NullStmt => Ok(Vec::new()),
        clang::EntityKind::ReturnStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if let Some(expr) = children.first() {
                if expr.get_kind() == clang::EntityKind::ConditionalOperator {
                    let expected_return_type: &str = function_return_type.unwrap_or("void");
                    return self::translate_conditional_return(
                        expr,
                        indent,
                        span,
                        expected_return_type,
                    );
                }

                let value: String = self::translate_return_expr(
                    expr,
                    span,
                    function_return_type.unwrap_or("void"),
                )?;
                Ok(vec![format!("{indent_str}return {value};")])
            } else {
                Ok(vec![format!("{indent_str}return;")])
            }
        }

        clang::EntityKind::BreakStmt => Ok(vec![format!("{indent_str}break;")]),
        clang::EntityKind::ContinueStmt => Ok(vec![format!("{indent_str}continue;")]),

        clang::EntityKind::DeclStmt => {
            let mut lines: Vec<String> = Vec::new();

            for decl in entity.get_children() {
                if decl.get_kind() != clang::EntityKind::VarDecl {
                    continue;
                }

                let Some(name) = decl.get_name() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Encountered a local variable without a name.".into(),
                        None,
                        span,
                    ));
                };

                let Some(var_ty) = decl.get_type() else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Missing type for local variable '{name}'."),
                        None,
                        span,
                    ));
                };

                let ty_text: String = crate::format_clang_type_thrust(&var_ty).map_err(|msg| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        format!("Unsupported type for local variable '{name}': {msg}"),
                        None,
                        span,
                    )
                })?;

                let children: Vec<clang::Entity<'_>> = decl.get_children();
                let name: String = crate::sanitize_identifier_for_thrust(&name);
                let init_entity: Option<&clang::Entity<'_>> = children
                    .iter()
                    .find(|c| crate::is_supported_expr_kind(c.get_kind()));
                let init: Option<String> =
                    if var_ty.get_canonical_type().get_kind() == clang::TypeKind::ConstantArray {
                        None
                    } else {
                        init_entity
                            .map(|c| crate::expr::translate_expr(c, span))
                            .transpose()?
                    };

                if let Some(init) = init {
                    lines.push(format!("{indent_str}var {name}: {ty_text} = {init};"));
                } else {
                    lines.push(format!("{indent_str}var {name}: {ty_text};"));
                }
            }

            Ok(lines)
        }

        clang::EntityKind::IfStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();
            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed if statement.".into(),
                    None,
                    span,
                ));
            }

            let cond: String = crate::expr::translate_condition_expr(&children[0], span)?;

            let then_lines: Vec<String> = self::translate_stmt_as_block(
                &children[1],
                indent + 1,
                span,
                function_return_type,
            )?;

            let mut lines: Vec<String> = Vec::new();
            lines.push(format!("{indent_str}if {cond} {{"));
            lines.extend(then_lines);

            if children.len() >= 3 {
                if children[2].get_kind() == clang::EntityKind::IfStmt {
                    let nested =
                        self::translate_stmt(&children[2], indent, span, function_return_type)?;
                    if let Some(first) = nested.first() {
                        lines.push(format!("{indent_str}}} else {}", first.trim_start()));
                        lines.extend(nested.into_iter().skip(1));
                    }
                } else {
                    let else_lines: Vec<String> = self::translate_stmt_as_block(
                        &children[2],
                        indent + 1,
                        span,
                        function_return_type,
                    )?;
                    lines.push(format!("{indent_str}}} else {{"));
                    lines.extend(else_lines);
                    lines.push(format!("{indent_str}}}"));
                }
            } else {
                lines.push(format!("{indent_str}}}"));
            }

            Ok(lines)
        }

        clang::EntityKind::WhileStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();
            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed while statement.".into(),
                    None,
                    span,
                ));
            }

            let cond: String = crate::expr::translate_condition_expr(&children[0], span)?;
            let body_lines: Vec<String> = self::translate_stmt_as_block(
                &children[1],
                indent + 1,
                span,
                function_return_type,
            )?;

            let mut lines: Vec<String> = Vec::new();
            lines.push(format!("{indent_str}while {cond} {{"));
            lines.extend(body_lines);
            lines.push(format!("{indent_str}}}"));

            Ok(lines)
        }

        clang::EntityKind::DoStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();
            if children.len() < 2 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Malformed do-while statement.".into(),
                    None,
                    span,
                ));
            }

            let body_lines = self::translate_stmt_as_block(
                &children[0],
                indent + 1,
                span,
                function_return_type,
            )?;
            let cond = crate::expr::translate_condition_expr(&children[1], span)?;

            let mut lines = Vec::new();
            lines.push(format!("{indent_str}loop {{"));
            lines.extend(body_lines);

            lines.push(format!("{}if !({cond}) {{", "    ".repeat(indent + 1)));
            lines.push(format!("{}break;", "    ".repeat(indent + 2)));
            lines.push(format!("{}}}", "    ".repeat(indent + 1)));
            lines.push(format!("{indent_str}}}"));

            Ok(lines)
        }

        clang::EntityKind::SwitchStmt => {
            self::translate_switch_stmt(entity, indent, span, function_return_type)
        }

        clang::EntityKind::ForStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();
            if children.is_empty() {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported for statement shape.".into(),
                    None,
                    span,
                ));
            }

            let body_node: &clang::Entity<'_> = children.last().unwrap();
            let header_nodes: &[clang::Entity<'_>] = &children[..children.len() - 1];

            let (init_node, cond_node, inc_node) = self::classify_for_header(header_nodes);

            let init_text: String = init_node
                .map(|node| self::translate_for_init(node, span))
                .transpose()?
                .unwrap_or_default();

            let cond_text: String = cond_node
                .map(|node| crate::expr::translate_condition_expr(node, span))
                .transpose()?
                .unwrap_or_default();

            let inc_text: String = inc_node
                .map(|node| self::translate_for_inc(node, span))
                .transpose()?
                .unwrap_or_default();

            let body_lines: Vec<String> =
                self::translate_stmt_as_block(body_node, indent + 1, span, function_return_type)?;

            let mut lines: Vec<String> = Vec::new();
            if init_text.is_empty() && cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !init_text.is_empty() && !cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!(
                    "{indent_str}for {init_text}; {cond_text}; {inc_text}; {{"
                ));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if init_text.is_empty() && !cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !init_text.is_empty() && !cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}{init_text};"));
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !init_text.is_empty() && cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}{init_text};"));
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !init_text.is_empty() && cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}{init_text};"));
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if init_text.is_empty() && !cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if init_text.is_empty() && cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            lines.extend(body_lines);
            lines.push(format!("{indent_str}}}"));

            Ok(lines)
        }

        clang::EntityKind::CompoundStmt => {
            let body_lines: Vec<String> =
                self::translate_compound_stmt(entity, indent + 1, span, function_return_type)?;
            let mut lines = vec![format!("{indent_str}{{")];
            lines.extend(body_lines);
            lines.push(format!("{indent_str}}}"));
            Ok(lines)
        }

        _ => {
            if crate::is_supported_expr_kind(entity.get_kind()) {
                let expr: String = crate::expr::translate_expr(entity, span)?;
                Ok(vec![format!("{indent_str}{expr};")])
            } else {
                Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    format!("Unsupported statement kind: {:?}", entity.get_kind()),
                    None,
                    span,
                ))
            }
        }
    }
}

pub(crate) fn translate_stmt_as_block(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
) -> Result<Vec<String>, CompilationIssue> {
    let indent_str: String = "    ".repeat(indent);

    if entity.get_kind() == clang::EntityKind::CompoundStmt {
        return self::translate_compound_stmt(entity, indent, span, function_return_type);
    }

    let mut lines: Vec<String> = Vec::new();
    for line in self::translate_stmt(entity, indent, span, function_return_type)? {
        lines.push(line);
    }

    if lines.is_empty() {
        lines.push(indent_str);
    }

    Ok(lines)
}

pub(crate) fn translate_compound_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
) -> Result<Vec<String>, CompilationIssue> {
    let mut out: Vec<String> = Vec::new();

    for child in entity.get_children() {
        let lines: Vec<String> = self::translate_stmt(&child, indent, span, function_return_type)?;

        out.extend(lines);
    }

    Ok(out)
}

fn translate_switch_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
) -> Result<Vec<String>, CompilationIssue> {
    let indent_str: String = "    ".repeat(indent);
    let children: Vec<clang::Entity<'_>> = entity.get_children();

    if children.len() < 2 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Malformed switch statement.".into(),
            None,
            span,
        ));
    }

    let cond: String = crate::expr::translate_expr(&children[0], span)?;

    let mut branches: Vec<(Option<String>, Vec<String>)> = Vec::new();

    for case in children[1].get_children() {
        match case.get_kind() {
            clang::EntityKind::CaseStmt => {
                let case_children = case.get_children();

                if case_children.len() < 2 {
                    continue;
                }

                let value = crate::expr::translate_expr(&case_children[0], span)?;
                let body = self::translate_stmt(
                    &case_children[1],
                    indent + 1,
                    span,
                    function_return_type,
                )?;

                branches.push((Some(value), body));
            }
            clang::EntityKind::DefaultStmt => {
                let mut body_lines: Vec<String> = Vec::new();

                for child in case.get_children() {
                    let body =
                        self::translate_stmt(&child, indent + 1, span, function_return_type)?;
                    body_lines.extend(body);
                }

                branches.push((None, body_lines));
            }
            _ => {}
        }
    }

    if branches.is_empty() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Switch statement has no translatable branches.".into(),
            None,
            span,
        ));
    }

    let mut lines: Vec<String> = Vec::new();

    for (idx, (value, body)) in branches.into_iter().enumerate() {
        match (idx, value) {
            (0, Some(value)) => lines.push(format!("{indent_str}if {cond} == {value} {{")),
            (0, None) => lines.push(format!("{indent_str}{{")),
            (_, Some(value)) => lines.push(format!("{indent_str}}} else if {cond} == {value} {{")),
            (_, None) => lines.push(format!("{indent_str}}} else {{")),
        }

        lines.extend(body);
    }

    lines.push(format!("{indent_str}}}"));

    Ok(lines)
}

fn translate_conditional_return(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    expected_return_type: &str,
) -> Result<Vec<String>, CompilationIssue> {
    let indent_str: String = "    ".repeat(indent);
    let child_indent_str: String = "    ".repeat(indent + 1);
    let children: Vec<clang::Entity<'_>> = entity.get_children();

    if children.len() < 3 {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Malformed conditional operator.".into(),
            None,
            span,
        ));
    }

    let cond: String = crate::expr::translate_condition_expr(&children[0], span)?;
    let then_expr: String = self::translate_return_expr(&children[1], span, expected_return_type)?;
    let else_expr: String = self::translate_return_expr(&children[2], span, expected_return_type)?;

    Ok(vec![
        format!("{indent_str}if {cond} {{"),
        format!("{child_indent_str}return {then_expr};"),
        format!("{indent_str}}} else {{"),
        format!("{child_indent_str}return {else_expr};"),
        format!("{indent_str}}}"),
    ])
}

fn translate_return_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    expected_return_type: &str,
) -> Result<String, CompilationIssue> {
    if self::return_type_needs_bool_to_int(expected_return_type)
        && self::expression_produces_condition(entity)
    {
        let condition: String = crate::expr::translate_condition_expr(entity, span)?;
        return Ok(format!("({condition}) as {expected_return_type}"));
    }

    let value: String = crate::expr::translate_expr(entity, span)?;

    if expected_return_type.contains("ptr[char]")
        && (value.starts_with('"') || value.starts_with("n#\""))
    {
        return Ok(format!("{value} as {expected_return_type}"));
    }

    if self::return_type_needs_scalar_cast(expected_return_type) {
        return Ok(format!("({value}) as {expected_return_type}"));
    }

    Ok(value)
}

fn expression_produces_condition(entity: &clang::Entity<'_>) -> bool {
    match entity.get_kind() {
        clang::EntityKind::ConditionalOperator => true,
        clang::EntityKind::UnaryOperator => entity.get_range().is_some_and(|range| {
            range
                .tokenize()
                .iter()
                .any(|token| token.get_spelling() == "!")
        }),
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                return false;
            }

            crate::extract_binary_operator(entity, &children[0], &children[1])
                .map(|op| matches!(op, "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="))
                .unwrap_or(false)
        }
        _ => entity
            .get_type()
            .is_some_and(|ty| ty.get_canonical_type().get_kind() == clang::TypeKind::Bool),
    }
}

fn return_type_needs_bool_to_int(expected_return_type: &str) -> bool {
    matches!(
        expected_return_type,
        "s8" | "s16" | "s32" | "s64" | "ssize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
    )
}

fn return_type_needs_scalar_cast(expected_return_type: &str) -> bool {
    self::return_type_needs_bool_to_int(expected_return_type)
        || matches!(expected_return_type, "char" | "f32" | "f64")
}

fn translate_for_init(entity: &clang::Entity<'_>, span: Span) -> Result<String, CompilationIssue> {
    if entity.get_kind() != clang::EntityKind::DeclStmt {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Only for-loops with a variable declaration initializer are supported.".into(),
            None,
            span,
        ));
    }

    let decls: Vec<clang::Entity<'_>> = entity.get_children();
    let Some(var) = decls
        .iter()
        .find(|d| d.get_kind() == clang::EntityKind::VarDecl)
    else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Unsupported for-loop initializer.".into(),
            None,
            span,
        ));
    };

    let Some(name) = var.get_name() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "For-loop initializer variable has no name.".into(),
            None,
            span,
        ));
    };

    let Some(var_ty) = var.get_type() else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "For-loop initializer variable has no type.".into(),
            None,
            span,
        ));
    };

    let name: String = crate::sanitize_identifier_for_thrust(&name);

    let ty_text: String = crate::format_clang_type_thrust(&var_ty).map_err(|msg| {
        CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            format!("Unsupported for-loop initializer type: {msg}"),
            None,
            span,
        )
    })?;

    let init_expr: Option<clang::Entity<'_>> = var
        .get_children()
        .into_iter()
        .find(|c| crate::is_supported_expr_kind(c.get_kind()));

    let Some(init_expr) = init_expr else {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "For-loop initializer is missing an initial value.".into(),
            None,
            span,
        ));
    };

    let init_text: String = crate::expr::translate_expr(&init_expr, span)?;
    Ok(format!("var {name}: {ty_text} = {init_text}"))
}

fn translate_for_inc(entity: &clang::Entity<'_>, span: Span) -> Result<String, CompilationIssue> {
    let kind: clang::EntityKind = entity.get_kind();

    if kind == clang::EntityKind::UnaryOperator {
        let Some(range) = entity.get_range() else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                "Unable to translate for-loop increment.".into(),
                None,
                span,
            ));
        };

        let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
        let spellings: Vec<String> = tokens.into_iter().map(|t| t.get_spelling()).collect();

        if spellings.contains(&"++".to_string()) || spellings.contains(&"--".to_string()) {
            let operand = entity
                .get_children()
                .into_iter()
                .find(|c| crate::is_supported_expr_kind(c.get_kind()))
                .ok_or_else(|| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unsupported for-loop increment.".into(),
                        None,
                        span,
                    )
                })?;

            let name: String = crate::expr::translate_expr(&operand, span)?;

            if spellings.contains(&"++".to_string()) {
                return Ok(format!("{name} += 1"));
            }

            return Ok(format!("{name} -= 1"));
        }
    }

    if crate::is_supported_expr_kind(kind) {
        return expr::translate_expr(entity, span);
    }

    Err(CompilationIssue::Error(
        CompilationIssueCode::E0110,
        "C translation failed.".into(),
        "Unsupported for-loop increment.".into(),
        None,
        span,
    ))
}

fn classify_for_header<'clang>(
    nodes: &'clang [clang::Entity<'clang>],
) -> (
    Option<&'clang clang::Entity<'clang>>,
    Option<&'clang clang::Entity<'clang>>,
    Option<&'clang clang::Entity<'clang>>,
) {
    let mut init_node: Option<&clang::Entity<'_>> = None;
    let mut cond_node: Option<&clang::Entity<'_>> = None;
    let mut inc_node: Option<&clang::Entity<'_>> = None;

    for node in nodes {
        match node.get_kind() {
            clang::EntityKind::NullStmt => {}
            clang::EntityKind::DeclStmt => init_node = Some(node),
            _ if crate::is_assignment_expression(node) => inc_node = Some(node),
            kind if crate::is_supported_expr_kind(kind) => {
                if cond_node.is_none() {
                    cond_node = Some(node);
                } else {
                    inc_node = Some(node);
                }
            }
            _ => {}
        }
    }

    (init_node, cond_node, inc_node)
}
