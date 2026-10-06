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

use crate::location::Location;

type SwitchSegment = (Vec<Option<String>>, Vec<String>, bool);

fn translate_stmt_inner(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
    ctx: &mut crate::macros::MacroContext<'_>,
) -> Vec<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let indent_str: String = "    ".repeat(indent);

    if let Some(call) = crate::macros::try_extract_statement_macro_call(ctx, entity, span) {
        return vec![format!("{indent_str}{call};")];
    }

    match entity.get_kind() {
        clang::EntityKind::NullStmt => Vec::new(),
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
                        ctx,
                    );
                }

                let value: String = self::translate_return_expr(
                    expr,
                    span,
                    function_return_type.unwrap_or("void"),
                    ctx,
                );
                vec![format!("{indent_str}return {value};")]
            } else {
                vec![format!("{indent_str}return;")]
            }
        }

        clang::EntityKind::BreakStmt => vec![format!("{indent_str}break;")],
        clang::EntityKind::ContinueStmt => {
            if let Some(action) = loop_continue_action {
                vec![
                    format!("{indent_str}{action};"),
                    format!("{indent_str}continue;"),
                ]
            } else {
                vec![format!("{indent_str}continue;")]
            }
        }

        clang::EntityKind::DeclStmt => {
            let mut lines: Vec<String> = Vec::new();

            let var_decls: Vec<clang::Entity<'_>> = entity
                .get_children()
                .into_iter()
                .filter(|decl| decl.get_kind() == clang::EntityKind::VarDecl)
                .collect();

            let decl_iter = var_decls.into_iter();

            for decl in decl_iter {
                let Some(name) = decl.get_name() else {
                    ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!(
                            "C translation failed:\n{prefix}Encountered a local variable without a name."
                        ),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                    return Default::default();
                };

                let Some(var_ty) = decl.get_type() else {
                    ctx.get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            {
                                let detail: String =
                                    format!("Missing type for local variable '{name}'.");

                                format!("C translation failed:\n{prefix}{detail}")
                            },
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                };

                let mut ty_text: String =
                    crate::type_format::format_clang_type_thrust(&var_ty, ctx, &prefix, span);

                if var_ty.is_const_qualified() {
                    ty_text = ty_text
                        .strip_prefix("const ")
                        .unwrap_or(&ty_text)
                        .to_string();
                }

                let name: String = { crate::util::sanitize_thrust_identifier(&name) };

                let init_entity: Option<clang::Entity<'_>> =
                    crate::top_level::find_var_initializer(&decl);

                let init: Option<String> = init_entity.map(|initializer| {
                    crate::top_level::translate_global_initializer(&initializer, &var_ty, span, ctx)
                });

                if let Some(init) = init {
                    lines.push(format!("{indent_str}var {name}: {ty_text} = {init};"));
                } else {
                    lines.push(format!("{indent_str}var {name}: {ty_text};"));
                }
            }

            lines
        }

        clang::EntityKind::IfStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                ctx.get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed if statement."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let cond: String =
                crate::expr::translate_condition_expr(&children[0], span, Location::RValue, ctx);

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
                            ctx,
                        );

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
                        ctx,
                    );

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
                        ctx,
                    );
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
                                    ctx,
                                );

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
                                ctx,
                            );

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

            lines
        }

        clang::EntityKind::WhileStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                ctx.get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Malformed while statement."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let cond: String =
                crate::expr::translate_condition_expr(&children[0], span, Location::RValue, ctx);

            let body_lines: Vec<String> =
                if children[1].get_kind() == clang::EntityKind::CompoundStmt {
                    let mut out: Vec<String> = Vec::new();

                    for child in children[1].get_children() {
                        let lines: Vec<String> = self::translate_stmt(
                            &child,
                            indent + 1,
                            span,
                            function_return_type,
                            None,
                            ctx,
                        );

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
                        ctx,
                    );

                    if lines.is_empty() {
                        lines.push("    ".repeat(indent + 1));
                    }

                    lines
                };

            let mut lines: Vec<String> = Vec::new();

            lines.push(format!("{indent_str}while {cond} {{"));
            lines.extend(body_lines);
            lines.push(format!("{indent_str}}}"));

            lines
        }

        clang::EntityKind::DoStmt => {
            let macro_name: Option<String> =
                crate::macro_table::MacroTable::make_location_key(entity)
                    .and_then(|key| ctx.get_macro_table().find_macro_name(&key));

            let base_error_count: usize = ctx.get_transpiler_context().error_count();

            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                let issue: CompilationIssue = CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Malformed do-while statement."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                );

                if let Some(name) = macro_name.as_deref() {
                    ctx.get_mut_transpiler_context()
                        .fail_macro::<Vec<String>>(issue);

                    crate::macro_error::collapse_macro_site_errors(
                        ctx,
                        entity,
                        name,
                        base_error_count,
                    );

                    return Vec::new();
                }

                ctx.get_mut_transpiler_context().add_error_fail(issue);
                return Default::default();
            }

            let body_lines: Vec<String> =
                if children[0].get_kind() == clang::EntityKind::CompoundStmt {
                    let mut out: Vec<String> = Vec::new();

                    for child in children[0].get_children() {
                        let child_base: usize = ctx.get_transpiler_context().error_count();

                        let lines: Vec<String> = self::translate_stmt(
                            &child,
                            indent + 1,
                            span,
                            function_return_type,
                            None,
                            ctx,
                        );

                        if let Some(name) = macro_name.as_deref() {
                            if ctx.get_transpiler_context().error_count() > child_base {
                                crate::macro_error::collapse_macro_site_errors_with_failed_at(
                                    ctx, entity, name, &child, child_base,
                                );
                            }
                        }

                        out.extend(lines);
                    }

                    out
                } else {
                    let child_base: usize = ctx.get_transpiler_context().error_count();

                    let mut lines: Vec<String> = self::translate_stmt(
                        &children[0],
                        indent + 1,
                        span,
                        function_return_type,
                        None,
                        ctx,
                    );

                    if let Some(name) = macro_name.as_deref() {
                        if ctx.get_transpiler_context().error_count() > child_base {
                            crate::macro_error::collapse_macro_site_errors_with_failed_at(
                                ctx,
                                entity,
                                name,
                                &children[0],
                                child_base,
                            );
                        }
                    }

                    if lines.is_empty() {
                        lines.push("    ".repeat(indent + 1));
                    }

                    lines
                };

            let cond_base: usize = ctx.get_transpiler_context().error_count();

            let cond: String =
                crate::expr::translate_condition_expr(&children[1], span, Location::RValue, ctx);

            if let Some(name) = macro_name.as_deref() {
                if ctx.get_transpiler_context().error_count() > cond_base {
                    crate::macro_error::collapse_macro_site_errors_with_failed_at(
                        ctx,
                        entity,
                        name,
                        &children[1],
                        cond_base,
                    );
                }
            }

            let mut lines: Vec<String> = Vec::new();

            lines.push(format!("{indent_str}loop {{"));
            lines.extend(body_lines);

            lines.push(format!("{}if !({cond}) {{", "    ".repeat(indent + 1)));
            lines.push(format!("{}break;", "    ".repeat(indent + 2)));
            lines.push(format!("{}}}", "    ".repeat(indent + 1)));
            lines.push(format!("{indent_str}}}"));

            lines
        }

        clang::EntityKind::SwitchStmt => self::translate_switch_stmt(
            entity,
            indent,
            span,
            function_return_type,
            loop_continue_action,
            ctx,
        ),

        clang::EntityKind::ForStmt => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.is_empty() {
                ctx.get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Unsupported for statement shape."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let body_node: &clang::Entity<'_> = children.last().unwrap_or_else(|| {
                crate::context::TranspilerContext::abort_transpilation(
                    "For statement without body node in clang AST.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            });
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
                ctx.get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        format!("C translation failed:\n{prefix}Unsupported for statement shape."),
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));
                return Default::default();
            }

            let (init_node, cond_node, inc_node) = if normalized_header_nodes.is_empty() {
                (None, None, None)
            } else if normalized_header_nodes.len() == 1 {
                (normalized_header_nodes[0], None, None)
            } else if normalized_header_nodes.len() == 2 {
                let first: Option<&clang::Entity<'_>> = normalized_header_nodes[0];
                let second: Option<&clang::Entity<'_>> = normalized_header_nodes[1];

                if first.is_some_and(|node| node.get_kind() == clang::EntityKind::DeclStmt) {
                    (first, second, None)
                } else {
                    (None, first, second)
                }
            } else if normalized_header_nodes.len() == 3 {
                (
                    normalized_header_nodes[0],
                    normalized_header_nodes[1],
                    normalized_header_nodes[2],
                )
            } else {
                crate::context::TranspilerContext::abort_transpilation(
                    "For statement header exceeds three nodes.",
                    span,
                    std::path::PathBuf::from(file!()),
                    line!(),
                )
            };

            let mut init_prefix_lines: Vec<String> = Vec::new();
            let mut init_header_text: Option<String> = None;

            match init_node {
                Some(node) if node.get_kind() == clang::EntityKind::DeclStmt => {
                    let decls: Vec<clang::Entity<'_>> = node
                        .get_children()
                        .into_iter()
                        .filter(|declaration| declaration.get_kind() == clang::EntityKind::VarDecl)
                        .collect();

                    if decls.is_empty() {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unsupported for-loop initializer."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    }

                    if decls.len() > 1 {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}For-loops with multiple initializer declarations are not supported yet."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    }

                    let var: &clang::Entity<'_> = decls.first().unwrap_or_else(|| {
                        crate::context::TranspilerContext::abort_transpilation(
                            "For-loop initializer without variable declaration.",
                            span,
                            std::path::PathBuf::from(file!()),
                            line!(),
                        )
                    });

                    let Some(name) = var.get_name() else {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}For-loop initializer variable has no name."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    };

                    let Some(var_ty) = var.get_type() else {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}For-loop initializer variable has no type."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    };

                    let name: String = { crate::util::sanitize_thrust_identifier(&name) };

                    let mut ty_text: String =
                        crate::type_format::format_clang_type_thrust(&var_ty, ctx, &prefix, span);

                    if var_ty.is_const_qualified() {
                        ty_text = ty_text
                            .strip_prefix("const ")
                            .unwrap_or(&ty_text)
                            .to_string();
                    }

                    let init_expr: Option<clang::Entity<'_>> =
                        crate::top_level::find_var_initializer(var);

                    if let Some(init_expr) = init_expr {
                        let init_value: String = crate::top_level::translate_global_initializer(
                            &init_expr, &var_ty, span, ctx,
                        );

                        init_header_text = Some(format!("var {name}: {ty_text} = {init_value}"));
                    } else {
                        init_header_text = Some(format!("var {name}: {ty_text}"));
                    }
                }

                Some(node) if crate::clang_util::is_supported_expr_kind(node.get_kind()) => {
                    let init_text: String =
                        crate::expr::translate_expr(node, span, Location::RValue, ctx);

                    init_prefix_lines.push(format!("{indent_str}{init_text};"));
                }

                Some(_) => {
                    ctx.get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unsupported for-loop initializer."
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
                }

                None => {}
            }

            let cond_text: String = cond_node
                .map(|node| {
                    crate::expr::translate_condition_expr(node, span, Location::RValue, ctx)
                })
                .unwrap_or_default();

            let inc_text: String = if let Some(node) = inc_node {
                let kind: clang::EntityKind = node.get_kind();

                if kind == clang::EntityKind::UnaryOperator {
                    let Some(range) = node.get_range() else {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unable to translate for-loop increment{}.",
                                crate::macros::origin_note(node)
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    };

                    let spellings: Vec<String> = crate::macro_lex::range_spellings(&range, node);

                    if spellings.contains(&"++".to_string())
                        || spellings.contains(&"--".to_string())
                    {
                        let Some(operand) = node.get_children().into_iter().find(|child| {
                            crate::clang_util::is_supported_expr_kind(child.get_kind())
                        }) else {
                            ctx.get_mut_transpiler_context().add_error_fail(
                                CompilationIssue::Error(
                                    CompilationIssueCode::E0110,
                                    format!("C translation failed:\n{prefix}Unsupported for-loop increment{}.", crate::macros::origin_note(node)),
                                    "Rewrite the C input to avoid the unsupported construct.".into(),
                                    None,
                                    span,
                                ),
                            );
                            return Default::default();
                        };

                        let name: String =
                            crate::expr::translate_expr(&operand, span, Location::RValue, ctx);

                        if spellings.contains(&"++".to_string()) {
                            format!("{name} += 1")
                        } else {
                            format!("{name} -= 1")
                        }
                    } else if crate::clang_util::is_supported_expr_kind(kind) {
                        crate::expr::translate_expr(node, span, Location::RValue, ctx)
                    } else {
                        ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unsupported for-loop increment{}.",
                                crate::macros::origin_note(node)
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                        return Default::default();
                    }
                } else if crate::clang_util::is_supported_expr_kind(kind) {
                    crate::expr::translate_expr(node, span, Location::RValue, ctx)
                } else {
                    ctx.get_mut_transpiler_context()
                        .add_error_fail(CompilationIssue::Error(
                            CompilationIssueCode::E0110,
                            format!(
                                "C translation failed:\n{prefix}Unsupported for-loop increment{}.",
                                crate::macros::origin_note(node)
                            ),
                            "Rewrite the C input to avoid the unsupported construct.".into(),
                            None,
                            span,
                        ));
                    return Default::default();
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
                        ctx,
                    );

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
                    ctx,
                );

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

                    return lines;
                }

                lines.push(format!(
                    "{indent_str}for {init_header_text}; {condition_text}; {inc_text}; {{"
                ));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return lines;
            }

            lines.extend(init_prefix_lines);

            if cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return lines;
            }

            if !cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return lines;
            }

            if !cond_text.is_empty() && inc_text.is_empty() {
                lines.push(format!("{indent_str}while {cond_text} {{"));
                lines.extend(body_lines);
                lines.push(format!("{indent_str}}}"));

                return lines;
            }

            if cond_text.is_empty() && !inc_text.is_empty() {
                lines.push(format!("{indent_str}for ; ; {{"));
                lines.extend(body_lines);
                lines.push(format!("{}{};", "    ".repeat(indent + 1), inc_text));
                lines.push(format!("{indent_str}}}"));

                return lines;
            }

            body_lines
        }

        clang::EntityKind::CompoundStmt => {
            let macro_name: Option<String> =
                crate::macro_table::MacroTable::make_location_key(entity)
                    .and_then(|key| ctx.get_macro_table().find_macro_name(&key));

            let mut body_lines: Vec<String> = Vec::new();

            for child in entity.get_children() {
                let child_base: usize = ctx.get_transpiler_context().error_count();

                let lines: Vec<String> = self::translate_stmt(
                    &child,
                    indent + 1,
                    span,
                    function_return_type,
                    loop_continue_action,
                    ctx,
                );

                if let Some(name) = macro_name.as_deref() {
                    if ctx.get_transpiler_context().error_count() > child_base {
                        crate::macro_error::collapse_macro_site_errors_with_failed_at(
                            ctx, entity, name, &child, child_base,
                        );
                    }
                }

                body_lines.extend(lines);
            }

            let mut lines: Vec<String> = vec![format!("{indent_str}{{")];

            lines.extend(body_lines);
            lines.push(format!("{indent_str}}}"));

            lines
        }

        _ => {
            if crate::clang_util::is_supported_expr_kind(entity.get_kind()) {
                let expr: String = crate::expr::translate_expr(entity, span, Location::RValue, ctx);

                vec![format!("{indent_str}{expr};")]
            } else {
                ctx.get_mut_transpiler_context()
                    .add_error_fail(CompilationIssue::Error(
                        CompilationIssueCode::E0110,
                        {
                            let detail: String =
                                format!("Unsupported statement kind: {:?}", entity.get_kind());

                            format!("C translation failed:\n{prefix}{detail}")
                        },
                        "Rewrite the C input to avoid the unsupported construct.".into(),
                        None,
                        span,
                    ));

                Default::default()
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
    ctx: &mut crate::macros::MacroContext<'_>,
) -> Vec<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let indent_str: String = "    ".repeat(indent);
    let children: Vec<clang::Entity<'_>> = entity.get_children();

    if children.len() < 2 {
        ctx.get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!("C translation failed:\n{prefix}Malformed switch statement."),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    }

    let cond: String = crate::expr::translate_expr(&children[0], span, Location::RValue, ctx);

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
                    ctx,
                );
            }

            self::collect_switch_labels_and_body(
                item,
                span,
                &mut current_labels,
                &mut current_body_nodes,
                ctx,
            );
        } else if current_labels.is_empty() {
            ctx.get_mut_transpiler_context().add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!(
                    "C translation failed:\n{prefix}Switch statement contains unlabeled statements before the first case/default."
                ),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
            return Default::default();
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
            ctx,
        );
    }

    if segments.is_empty() {
        ctx.get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!(
                    "C translation failed:\n{prefix}Switch statement has no translatable branches."
                ),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    }

    let mut lines: Vec<String> = Vec::new();

    let mut branch_bodies: Vec<(Option<String>, Vec<String>)> = Vec::new();
    let mut default_body: Option<Vec<String>> = None;

    let segment_iter = segments.iter().enumerate();

    for (segment_index, (labels, _, _)) in segment_iter {
        let remaining: usize = segments.len().saturating_sub(segment_index);

        let take_count: usize = segments
            .iter()
            .skip(segment_index)
            .position(|(_, _, terminates_segment)| *terminates_segment)
            .map(|pos| pos.saturating_add(1))
            .unwrap_or(remaining);

        let merged_iter = segments
            .iter()
            .skip(segment_index)
            .take(take_count)
            .flat_map(|(_, body, _)| body.iter().cloned());

        let merged_body: Vec<String> = merged_iter.collect();

        let label_iter: std::slice::Iter<'_, Option<String>> = labels.iter();

        for label in label_iter {
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

    lines
}

fn translate_return_expr(
    entity: &clang::Entity<'_>,
    span: Span,
    expected_return_type: &str,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> String {
    if matches!(
        expected_return_type,
        "s8" | "s16" | "s32" | "s64" | "ssize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
    ) && crate::stmt_analysis::expression_produces_condition(entity)
    {
        let condition: String =
            crate::expr::translate_condition_expr(entity, span, Location::RValue, macro_ctx);

        return format!("({condition}) as {expected_return_type}");
    }

    let value: String = crate::expr::translate_expr(entity, span, Location::RValue, macro_ctx);

    if expected_return_type.contains("ptr[char]")
        && (value.starts_with('"') || value.starts_with("n#\""))
    {
        return format!("{value} as {expected_return_type}");
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
        return format!("({value}) as {expected_return_type}");
    }

    value
}

fn collect_switch_labels_and_body<'stmt>(
    entity: &clang::Entity<'stmt>,
    span: Span,
    labels: &mut Vec<Option<String>>,
    body_nodes: &mut Vec<clang::Entity<'stmt>>,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) {
    let prefix: String = crate::macros::expansion_prefix(entity);

    if entity.get_kind() == clang::EntityKind::CaseStmt {
        let children: Vec<clang::Entity<'_>> = entity.get_children();

        if children.is_empty() {
            macro_ctx
                .get_mut_transpiler_context()
                .add_error_fail(CompilationIssue::Error(
                    CompilationIssueCode::E0110,
                    format!("C translation failed:\n{prefix}Malformed case statement."),
                    "Rewrite the C input to avoid the unsupported construct.".into(),
                    None,
                    span,
                ));
            return Default::default();
        }

        let value: String =
            crate::expr::translate_expr(&children[0], span, Location::RValue, macro_ctx);

        labels.push(Some(value));

        for child in children.iter().skip(1) {
            if matches!(
                child.get_kind(),
                clang::EntityKind::CaseStmt | clang::EntityKind::DefaultStmt
            ) {
                self::collect_switch_labels_and_body(child, span, labels, body_nodes, macro_ctx);
            } else {
                body_nodes.push(*child);
            }
        }

        return;
    }

    if entity.get_kind() == clang::EntityKind::DefaultStmt {
        labels.push(None);

        for child in entity.get_children() {
            if matches!(
                child.get_kind(),
                clang::EntityKind::CaseStmt | clang::EntityKind::DefaultStmt
            ) {
                self::collect_switch_labels_and_body(&child, span, labels, body_nodes, macro_ctx);
            } else {
                body_nodes.push(child);
            }
        }

        return;
    }

    body_nodes.push(*entity);
}

#[allow(clippy::too_many_arguments)]
fn push_switch_segment<'stmt>(
    segments: &mut Vec<SwitchSegment>,
    current_labels: &mut Vec<Option<String>>,
    current_body_nodes: &mut Vec<clang::Entity<'stmt>>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
    ctx: &mut crate::macros::MacroContext<'_>,
) {
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
            ctx,
        );

        body_lines.extend(translated);
    }

    segments.push((
        std::mem::take(current_labels),
        body_lines,
        terminates_segment,
    ));

    current_body_nodes.clear();
}

fn translate_conditional_return(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    expected_return_type: &str,
    macro_ctx: &mut crate::macros::MacroContext<'_>,
) -> Vec<String> {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let indent_str: String = "    ".repeat(indent);
    let child_indent_str: String = "    ".repeat(indent + 1);
    let children: Vec<clang::Entity<'_>> = entity.get_children();

    if children.len() < 3 {
        macro_ctx
            .get_mut_transpiler_context()
            .add_error_fail(CompilationIssue::Error(
                CompilationIssueCode::E0110,
                format!("C translation failed:\n{prefix}Malformed conditional operator."),
                "Rewrite the C input to avoid the unsupported construct.".into(),
                None,
                span,
            ));
        return Default::default();
    }

    let cond: String =
        crate::expr::translate_condition_expr(&children[0], span, Location::RValue, macro_ctx);
    let then_expr: String =
        self::translate_return_expr(&children[1], span, expected_return_type, macro_ctx);
    let else_expr: String =
        self::translate_return_expr(&children[2], span, expected_return_type, macro_ctx);

    vec![
        format!("{indent_str}if {cond} {{"),
        format!("{child_indent_str}return {then_expr};"),
        format!("{indent_str}}} else {{"),
        format!("{child_indent_str}return {else_expr};"),
        format!("{indent_str}}}"),
    ]
}

pub fn translate_stmt(
    entity: &clang::Entity<'_>,
    indent: usize,
    span: Span,
    function_return_type: Option<&str>,
    loop_continue_action: Option<&str>,
    ctx: &mut crate::macros::MacroContext<'_>,
) -> Vec<String> {
    let base_pending: usize = ctx.pending_statements_len();

    let mut lines: Vec<String> = self::translate_stmt_inner(
        entity,
        indent,
        span,
        function_return_type,
        loop_continue_action,
        ctx,
    );

    let pending: Vec<String> = ctx.take_pending_statements_since(base_pending);

    let indent_str: String = "    ".repeat(indent);

    let mut indented_pending: Vec<String> = Vec::with_capacity(pending.len());

    for statement in pending {
        for line in statement.split('\n') {
            if line.is_empty() {
                continue;
            }

            indented_pending.push(format!("{indent_str}{line}"));
        }
    }

    indented_pending.append(&mut lines);

    indented_pending
}
