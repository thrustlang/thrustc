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

type SwitchSegment = (Vec<Option<String>>, Vec<String>, bool);

pub(crate) fn translate_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
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
        clang::EntityKind::ContinueStmt => {
            if let Some(action) = loop_continue_action {
                Ok(vec![
                    format!("{indent_str}{action};"),
                    format!("{indent_str}continue;"),
                ])
            } else {
                Ok(vec![format!("{indent_str}continue;")])
            }
        }

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

                let ty_text: String = {
                    let ty_text: String = crate::format_clang_type_thrust(&var_ty).map_err(|msg| {
                        CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            format!("Unsupported type for local variable '{name}': {msg}"),
                            None,
                            span,
                        )
                    })?;

                    if var_ty.is_const_qualified() {
                        if let Some(stripped) = ty_text.strip_prefix("const ") {
                            stripped.to_string()
                        } else {
                            ty_text
                        }
                    } else {
                        ty_text
                    }
                };

                let name: String = crate::sanitize_identifier_for_thrust(&name);
                let init_entity: Option<clang::Entity<'_>> = crate::top_level::find_var_initializer(&decl);
                let init: Option<String> = init_entity
                    .map(|initializer| {
                        crate::top_level::translate_global_initializer(&initializer, &var_ty, span)
                    })
                    .transpose()?;

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

            let then_lines: Vec<String> =
                if children[1].get_kind() == clang::EntityKind::CompoundStmt {
                    let mut out: Vec<String> = Vec::new();

                    for child in children[1].get_children() {
                        let lines: Vec<String> = self::translate_stmt(
                            &child,
                            indent + 1,
                            span,
                            function_return_type,
                            loop_continue_action,
                        )?;

                        out.extend(lines);
                    }

                    out
                } else {
                    let mut lines: Vec<String> = self::translate_stmt(
                        &children[1],
                        indent + 1,
                        span,
                        function_return_type,
                        loop_continue_action,
                    )?;

                    if lines.is_empty() {
                        lines.push("    ".repeat(indent + 1));
                    }

                    lines
                };

            let mut lines: Vec<String> = Vec::new();

            lines.push(format!("{indent_str}if {cond} {{"));
            lines.extend(then_lines);

            if children.len() >= 3 {
                if children[2].get_kind() == clang::EntityKind::IfStmt {
                    let nested = self::translate_stmt(
                        &children[2],
                        indent,
                        span,
                        function_return_type,
                        loop_continue_action,
                    )?;
                    if let Some(first) = nested.first() {
                        lines.push(format!("{indent_str}}} else {}", first.trim_start()));
                        lines.extend(nested.into_iter().skip(1));
                    }
                } else {
                    let else_lines: Vec<String> =
                        if children[2].get_kind() == clang::EntityKind::CompoundStmt {
                            let mut out: Vec<String> = Vec::new();

                            for child in children[2].get_children() {
                                let body: Vec<String> = self::translate_stmt(
                                    &child,
                                    indent + 1,
                                    span,
                                    function_return_type,
                                    loop_continue_action,
                                )?;

                                out.extend(body);
                            }

                            out
                        } else {
                            let mut body: Vec<String> = self::translate_stmt(
                                &children[2],
                                indent + 1,
                                span,
                                function_return_type,
                                loop_continue_action,
                            )?;

                            if body.is_empty() {
                                body.push("    ".repeat(indent + 1));
                            }

                            body
                        };

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

            let body_lines: Vec<String> = if children[1].get_kind()
                == clang::EntityKind::CompoundStmt
            {
                let mut out: Vec<String> = Vec::new();

                for child in children[1].get_children() {
                    let lines: Vec<String> =
                        self::translate_stmt(&child, indent + 1, span, function_return_type, None)?;

                    out.extend(lines);
                }

                out
            } else {
                let mut lines: Vec<String> = self::translate_stmt(
                    &children[1],
                    indent + 1,
                    span,
                    function_return_type,
                    None,
                )?;

                if lines.is_empty() {
                    lines.push("    ".repeat(indent + 1));
                }

                lines
            };

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

            let body_lines: Vec<String> = if children[0].get_kind()
                == clang::EntityKind::CompoundStmt
            {
                let mut out: Vec<String> = Vec::new();

                for child in children[0].get_children() {
                    let lines: Vec<String> =
                        self::translate_stmt(&child, indent + 1, span, function_return_type, None)?;

                    out.extend(lines);
                }

                out
            } else {
                let mut lines: Vec<String> = self::translate_stmt(
                    &children[0],
                    indent + 1,
                    span,
                    function_return_type,
                    None,
                )?;

                if lines.is_empty() {
                    lines.push("    ".repeat(indent + 1));
                }

                lines
            };

            let cond: String = crate::expr::translate_condition_expr(&children[1], span)?;

            let mut lines: Vec<String> = Vec::new();

            lines.push(format!("{indent_str}loop {{"));
            lines.extend(body_lines);

            lines.push(format!("{}if !({cond}) {{", "    ".repeat(indent + 1)));
            lines.push(format!("{}break;", "    ".repeat(indent + 2)));
            lines.push(format!("{}}}", "    ".repeat(indent + 1)));
            lines.push(format!("{indent_str}}}"));

            Ok(lines)
        }

        clang::EntityKind::SwitchStmt => self::translate_switch_stmt(
            entity,
            indent,
            span,
            function_return_type,
            loop_continue_action,
        ),

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

            let normalized_header_nodes: Vec<Option<&clang::Entity<'_>>> = header_nodes
                .iter()
                .map(|node| {
                    if node.get_kind() == clang::EntityKind::NullStmt {
                        None
                    } else {
                        Some(node)
                    }
                })
                .collect();

            if normalized_header_nodes.len() > 3 {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    "C translation failed.".into(),
                    "Unsupported for statement shape.".into(),
                    None,
                    span,
                ));
            }

            let (init_node, cond_node, inc_node): (
                Option<&clang::Entity<'_>>,
                Option<&clang::Entity<'_>>,
                Option<&clang::Entity<'_>>,
            ) = match normalized_header_nodes.as_slice() {
                [] => (None, None, None),
                [first] => (*first, None, None),
                [first, second] if first.is_some_and(|node| node.get_kind() == clang::EntityKind::DeclStmt) => {
                    (*first, *second, None)
                }
                [first, second] => (None, *first, *second),
                [first, second, third] => (*first, *second, *third),
                _ => unreachable!(),
            };

            let mut init_prefix_lines: Vec<String> = Vec::new();
            let mut init_header_text: Option<String> = None;

            if let Some(node) = init_node {
                if node.get_kind() == clang::EntityKind::DeclStmt {
                    let decls: Vec<clang::Entity<'_>> = node
                        .get_children()
                        .into_iter()
                        .filter(|declaration| declaration.get_kind() == clang::EntityKind::VarDecl)
                        .collect();

                    if decls.is_empty() {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            "Unsupported for-loop initializer.".into(),
                            None,
                            span,
                        ));
                    }

                    if decls.len() > 1 {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            "For-loops with multiple initializer declarations are not supported yet."
                                .into(),
                            None,
                            span,
                        ));
                    }

                    let var: &clang::Entity<'_> = decls.first().unwrap();

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

                    let ty_text: String = {
                        let ty_text: String =
                            crate::format_clang_type_thrust(&var_ty).map_err(|msg| {
                                CompilationIssue::Error(
                                    CompilationIssueCode::E0110,
                                    "C translation failed.".into(),
                                    format!("Unsupported for-loop initializer type: {msg}"),
                                    None,
                                    span,
                                )
                            })?;

                        if var_ty.is_const_qualified() {
                            if let Some(stripped) = ty_text.strip_prefix("const ") {
                                stripped.to_string()
                            } else {
                                ty_text
                            }
                        } else {
                            ty_text
                        }
                    };

                    let init_expr: Option<clang::Entity<'_>> = crate::top_level::find_var_initializer(var);

                    if let Some(init_expr) = init_expr {
                        let init_value: String =
                            crate::top_level::translate_global_initializer(&init_expr, &var_ty, span)?;

                        init_header_text = Some(format!("var {name}: {ty_text} = {init_value}"));
                    } else {
                        init_header_text = Some(format!("var {name}: {ty_text}"));
                    }
                } else if crate::is_supported_expr_kind(node.get_kind()) {
                    let init_text: String = crate::expr::translate_expr(node, span)?;

                    init_prefix_lines.push(format!("{indent_str}{init_text};"));
                } else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unsupported for-loop initializer.".into(),
                        None,
                        span,
                    ));
                }
            }

            let cond_text: String = if let Some(node) = cond_node {
                crate::expr::translate_condition_expr(node, span)?
            } else {
                String::new()
            };

            let inc_text: String = if let Some(node) = inc_node {
                let kind: clang::EntityKind = node.get_kind();

                if kind == clang::EntityKind::UnaryOperator {
                    let Some(range) = node.get_range() else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            "Unable to translate for-loop increment.".into(),
                            None,
                            span,
                        ));
                    };

                    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
                    let spellings: Vec<String> = tokens
                        .into_iter()
                        .map(|token| token.get_spelling())
                        .collect();

                    if spellings.contains(&"++".to_string())
                        || spellings.contains(&"--".to_string())
                    {
                        let operand = node
                            .get_children()
                            .into_iter()
                            .find(|child| crate::is_supported_expr_kind(child.get_kind()))
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
                            format!("{name} += 1")
                        } else {
                            format!("{name} -= 1")
                        }
                    } else if crate::is_supported_expr_kind(kind) {
                        expr::translate_expr(node, span)?
                    } else {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            "C translation failed.".into(),
                            "Unsupported for-loop increment.".into(),
                            None,
                            span,
                        ));
                    }
                } else if crate::is_supported_expr_kind(kind) {
                    expr::translate_expr(node, span)?
                } else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        "C translation failed.".into(),
                        "Unsupported for-loop increment.".into(),
                        None,
                        span,
                    ));
                }
            } else {
                String::new()
            };

            let body_continue_action: Option<&str> = if inc_text.is_empty() {
                None
            } else {
                Some(inc_text.as_str())
            };

            let body_lines: Vec<String> = if body_node.get_kind() == clang::EntityKind::CompoundStmt
            {
                let mut out: Vec<String> = Vec::new();

                for child in body_node.get_children() {
                    let lines: Vec<String> = self::translate_stmt(
                        &child,
                        indent + 1,
                        span,
                        function_return_type,
                        body_continue_action,
                    )?;

                    out.extend(lines);
                }

                out
            } else {
                let mut lines: Vec<String> = self::translate_stmt(
                    body_node,
                    indent + 1,
                    span,
                    function_return_type,
                    body_continue_action,
                )?;

                if lines.is_empty() {
                    lines.push("    ".repeat(indent + 1));
                }

                lines
            };

            let mut lines: Vec<String> = Vec::new();

            if let Some(init_header_text) = init_header_text {
                let condition_text: &str = if cond_text.is_empty() {
                    "true"
                } else {
                    cond_text.as_str()
                };

                if inc_text.is_empty() {
                    lines.push(format!("{indent_str}{init_header_text};"));
                    lines.push(format!("{indent_str}while {condition_text} {{"));
                    lines.extend(body_lines);
                    lines.push(format!("{indent_str}}}"));

                    return Ok(lines);
                }

                lines.push(format!(
                    "{indent_str}for {init_header_text}; {condition_text}; {inc_text}; {{"
                ));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            lines.extend(init_prefix_lines);

            if cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if !cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            if cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return Ok(lines);
            }

            Ok(body_lines)
        }

        clang::EntityKind::CompoundStmt => {
            let mut body_lines: Vec<String> = Vec::new();

            for child in entity.get_children() {
                let lines: Vec<String> = self::translate_stmt(
                    &child,
                    indent + 1,
                    span,
                    function_return_type,
                    loop_continue_action,
                )?;

                body_lines.extend(lines);
            }

            let mut lines: Vec<String> = vec![format!("{indent_str}{{")];

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

fn translate_switch_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
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

    let body_items: Vec<clang::Entity<'_>> =
        if children[1].get_kind() == clang::EntityKind::CompoundStmt {
            children[1].get_children()
        } else {
            vec![children[1]]
        };

    let mut segments: Vec<(Vec<Option<String>>, Vec<String>, bool)> = Vec::new();
    let mut current_labels: Vec<Option<String>> = Vec::new();
    let mut current_body_nodes: Vec<clang::Entity<'_>> = Vec::new();

    for item in body_items.iter() {
        if matches!(
            item.get_kind(),
            clang::EntityKind::CaseStmt | clang::EntityKind::DefaultStmt
        ) {
            if !current_labels.is_empty() {
                self::push_switch_segment(
                    &mut segments,
                    &mut current_labels,
                    &mut current_body_nodes,
                    indent + 1,
                    span,
                    function_return_type,
                    loop_continue_action,
                )?;
            }

            self::collect_switch_labels_and_body(
                item,
                span,
                &mut current_labels,
                &mut current_body_nodes,
            )?;
        } else if current_labels.is_empty() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                "Switch statement contains unlabeled statements before the first case/default."
                    .into(),
                None,
                span,
            ));
        } else {
            current_body_nodes.push(*item);
        }
    }

    if !current_labels.is_empty() {
        self::push_switch_segment(
            &mut segments,
            &mut current_labels,
            &mut current_body_nodes,
            indent + 1,
            span,
            function_return_type,
            loop_continue_action,
        )?;
    }

    if segments.is_empty() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "C translation failed.".into(),
            "Switch statement has no translatable branches.".into(),
            None,
            span,
        ));
    }

    let mut lines: Vec<String> = Vec::new();

    let mut branch_bodies: Vec<(Option<String>, Vec<String>)> = Vec::new();
    let mut default_body: Option<Vec<String>> = None;

    for (segment_index, (labels, _, _)) in segments.iter().enumerate() {
        let mut merged_body: Vec<String> = Vec::new();

        for (_, body, terminates_segment) in segments.iter().skip(segment_index) {
            merged_body.extend(body.iter().cloned());

            if *terminates_segment {
                break;
            }
        }

        for label in labels.iter() {
            if let Some(value) = label {
                branch_bodies.push((Some(value.clone()), merged_body.clone()));
            } else {
                default_body = Some(merged_body.clone());
            }
        }
    }

    for (idx, (_, body)) in branch_bodies.iter().enumerate() {
        if let Some(value) = branch_bodies[idx].0.as_ref() {
            if idx == 0 {
                lines.push(format!("{indent_str}if {cond} == {value} {{"));
            } else {
                lines.push(format!("{indent_str}}} else if {cond} == {value} {{"));
            }

            lines.extend(body.iter().cloned());
        }
    }

    if let Some(default_body) = default_body {
        if branch_bodies.is_empty() {
            lines.push(format!("{indent_str}{{"));
        } else {
            lines.push(format!("{indent_str}}} else {{"));
        }

        lines.extend(default_body);
    }

    lines.push(format!("{indent_str}}}"));

    Ok(lines)
}

fn push_switch_segment<'stmt>(
    segments: &mut Vec<SwitchSegment>,
    current_labels: &mut Vec<Option<String>>,
    current_body_nodes: &mut Vec<clang::Entity<'stmt>>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
) -> Result<(), CompilationIssue> {
    let terminates_segment: bool = current_body_nodes.last().is_some_and(|node| {
        matches!(
            node.get_kind(),
            clang::EntityKind::BreakStmt
                | clang::EntityKind::ContinueStmt
                | clang::EntityKind::ReturnStmt
        )
    });

    let translated_nodes: &[clang::Entity<'_>] = if current_body_nodes
        .last()
        .is_some_and(|node| node.get_kind() == clang::EntityKind::BreakStmt)
    {
        &current_body_nodes[..current_body_nodes.len().saturating_sub(1)]
    } else {
        current_body_nodes.as_slice()
    };

    let mut body_lines: Vec<String> = Vec::new();

    for node in translated_nodes.iter() {
        let translated: Vec<String> = self::translate_stmt(
            node,
            indent,
            span,
            function_return_type,
            loop_continue_action,
        )?;

        body_lines.extend(translated);
    }

    segments.push((
        std::mem::take(current_labels),
        body_lines,
        terminates_segment,
    ));

    current_body_nodes.clear();

    Ok(())
}

fn collect_switch_labels_and_body<'stmt>(
    entity: &clang::Entity<'stmt>,
    span: Span,
    labels: &mut Vec<Option<String>>,
    body_nodes: &mut Vec<clang::Entity<'stmt>>,
) -> Result<(), CompilationIssue> {
    if entity.get_kind() == clang::EntityKind::CaseStmt {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.is_empty() {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                "C translation failed.".into(),
                "Malformed case statement.".into(),
                None,
                span,
            ));
        }

        let value: String = crate::expr::translate_expr(&children[0], span)?;
        labels.push(Some(value));

        for child in children.iter().skip(1) {
            if matches!(
                child.get_kind(),
                clang::EntityKind::CaseStmt | clang::EntityKind::DefaultStmt
            ) {
                self::collect_switch_labels_and_body(child, span, labels, body_nodes)?;
            } else {
                body_nodes.push(*child);
            }
        }

        return Ok(());
    }

    if entity.get_kind() == clang::EntityKind::DefaultStmt {
        labels.push(None);

        for child in entity.get_children() {
            if matches!(
                child.get_kind(),
                clang::EntityKind::CaseStmt | clang::EntityKind::DefaultStmt
            ) {
                self::collect_switch_labels_and_body(&child, span, labels, body_nodes)?;
            } else {
                body_nodes.push(child);
            }
        }

        return Ok(());
    }

    body_nodes.push(*entity);

    Ok(())
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
    if matches!(
        expected_return_type,
        "s8" | "s16" | "s32" | "s64" | "ssize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
    ) && self::expression_produces_condition(entity)
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

    if matches!(
        expected_return_type,
        "s8" | "s16"
            | "s32"
            | "s64"
            | "ssize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "char"
            | "f32"
            | "f64"
    ) {
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
