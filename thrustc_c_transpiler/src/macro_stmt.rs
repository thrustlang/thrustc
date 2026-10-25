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

use crate::macro_ast::{ForInit, MacroExpr, MacroStmt, MacroUnOp};
use crate::macro_error::MacroLimit;

impl MacroStmt {
    pub fn lower(
        &self,
        ctx: &mut crate::macros::MacroContext,
        indent: usize,
    ) -> Result<Vec<String>, MacroLimit> {
        let base: usize = ctx.pending_statements_len();

        let mut out: Vec<String> = self.lower_body(ctx, indent)?;

        let prelude: Vec<String> = ctx.take_pending_statements_since(base);

        if !prelude.is_empty() {
            let pad: String = "    ".repeat(indent);

            let mut prefixed: Vec<String> = prelude
                .into_iter()
                .map(|line| format!("{pad}{line}"))
                .collect();

            prefixed.extend(out);

            out = prefixed;
        }

        Ok(out)
    }
}

impl MacroStmt {
    fn lower_body(
        &self,
        ctx: &mut crate::macros::MacroContext,
        indent: usize,
    ) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);

        match self {
            MacroStmt::Empty => Ok(Vec::new()),

            MacroStmt::VarDecl {
                ty,
                name,
                init,
                storage,
                is_const,
                is_volatile,
            } => {
                let clean: String = crate::util::normalize_to_thrust_identifier(name);

                let init_text: Option<String> = init.as_ref().map(|init| {
                    crate::macro_expr::lower_initializer(ctx, init, ty)
                });

                ctx.set_type_binding(
                    clean.clone(),
                    crate::macro_type::InferredType::Known(ty.clone()),
                );
                ctx.set_value_type(
                    clean.clone(),
                    crate::macro_type::MacroValueType::from_text(ty),
                );

                let init_text: String = match init_text {
                    Some(init_text) => format!(" = {init_text}"),
                    None => String::new(),
                };

                let volatile: &str = if *is_volatile { " volatile" } else { "" };

                match storage {
                    crate::macro_ast::MacroStorageClass::Const => {
                        if init.is_none() {
                            return Err(MacroLimit::StatementMalformed);
                        }

                        Ok(vec![format!(
                            "{pad}const{volatile} {clean}: {ty}{init_text};"
                        )])
                    }

                    crate::macro_ast::MacroStorageClass::Static => {
                        let mutable: &str = if *is_const { "" } else { " mut" };

                        Ok(vec![format!(
                            "{pad}static{mutable}{volatile} {clean}: {ty}{init_text};"
                        )])
                    }

                    crate::macro_ast::MacroStorageClass::Extern => {
                        Err(MacroLimit::DeclaratorUnsupported)
                    }

                    crate::macro_ast::MacroStorageClass::Var => {
                        Ok(vec![format!(
                            "{pad}var{volatile} {clean}: {ty}{init_text};"
                        )])
                    }
                }
            }

            MacroStmt::Expr(expr) => {
                let text: String = self.lower_inc_dec(ctx, expr);

                Ok(vec![format!("{pad}{text};")])
            }

            MacroStmt::Break => Ok(vec![format!("{pad}break;")]),

            MacroStmt::Continue => self.lower_continue(ctx, indent),

            MacroStmt::Return(value) => {
                let return_type: String = ctx
                    .get_return_type()
                    .unwrap_or("void")
                    .to_string();

                let Some(value) = value else {
                    return Ok(vec![format!("{pad}return;")]);
                };

                let value_text: String =
                    crate::macro_expr::lower(ctx, value, crate::location::Location::RValue, &return_type);

                if return_type == "void" {
                    return Ok(vec![format!("{pad}return {value_text};")]);
                }

                let is_scalar: bool = matches!(
                    return_type.as_str(),
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
                        | "bool"
                );

                if is_scalar {
                    Ok(vec![format!(
                        "{pad}return ({value_text}) as {return_type};"
                    )])
                } else {
                    Ok(vec![format!("{pad}return {value_text};")])
                }
            }

            MacroStmt::Goto(_) | MacroStmt::Label(_) => Err(MacroLimit::GotoUnsupported),

            MacroStmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_text: String = self.lower_condition(ctx, cond);

                let mut out: Vec<String> = vec![format!("{pad}if {cond_text} {{")];

                for stmt in then_branch.iter() {
                    out.extend(stmt.lower(ctx, indent + 1)?);
                }

                if else_branch.is_empty() {
                    out.push(format!("{pad}}}"));
                } else {
                    out.push(format!("{pad}}} else {{"));

                    for stmt in else_branch.iter() {
                        out.extend(stmt.lower(ctx, indent + 1)?);
                    }

                    out.push(format!("{pad}}}"));
                }

                Ok(out)
            }

            MacroStmt::Switch { cond, arms } => {
                self.lower_switch(ctx, indent, cond, arms)
            }

            MacroStmt::While { cond, body } => {
                let cond_text: String = self.lower_condition(ctx, cond);

                ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
                    continue_action: None,
                });

                let mut out: Vec<String> = vec![format!("{pad}while {cond_text} {{")];

                for stmt in body.iter() {
                    out.extend(stmt.lower(ctx, indent + 1)?);
                }

                ctx.pop_loop_context();

                out.push(format!("{pad}}}"));

                Ok(out)
            }

            MacroStmt::DoWhile { body, cond } => {
                if matches!(cond, MacroExpr::Literal(text) if text == "0") {
                    if self.contains_break_to_current_loop(body)
                        || self.contains_continue_to_current_loop(body)
                    {
                        ctx.push_loop_context(crate::macros::MacroLoopContext::DoWhileZero);

                        let mut out: Vec<String> = vec![format!("{pad}loop {{")];

                        for stmt in body.iter() {
                            out.extend(stmt.lower(ctx, indent + 1)?);
                        }

                        ctx.pop_loop_context();

                        out.push(format!("{}break;", "    ".repeat(indent + 1)));
                        out.push(format!("{pad}}}"));

                        return Ok(out);
                    }

                    let mut out: Vec<String> = Vec::new();

                    for stmt in body.iter() {
                        out.extend(stmt.lower(ctx, indent)?);
                    }

                    return Ok(out);
                }

                let cond_text: String = self.lower_condition(ctx, cond);

                ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
                    continue_action: None,
                });

                let mut out: Vec<String> = vec![format!("{pad}loop {{")];

                for stmt in body.iter() {
                    out.extend(stmt.lower(ctx, indent + 1)?);
                }

                ctx.pop_loop_context();

                out.push(format!(
                    "{}if !({cond_text}) {{",
                    "    ".repeat(indent + 1)
                ));
                out.push(format!("{}break;", "    ".repeat(indent + 2)));
                out.push(format!("{}}}", "    ".repeat(indent + 1)));
                out.push(format!("{pad}}}"));

                Ok(out)
            }

            MacroStmt::For {
                init,
                cond,
                inc,
                body,
            } => self.lower_for(
                ctx,
                indent,
                init.as_ref(),
                cond.as_ref(),
                inc.as_ref(),
                body,
            ),

            MacroStmt::Compound(inner) => {
                let mut out: Vec<String> = Vec::new();

                for stmt in inner.iter() {
                    out.extend(stmt.lower(ctx, indent)?);
                }

                Ok(out)
            }
        }
    }
}

impl MacroStmt {
    fn lower_for(
        &self,
        ctx: &mut crate::macros::MacroContext,
        indent: usize,
        init: Option<&ForInit>,
        cond: Option<&MacroExpr>,
        inc: Option<&MacroExpr>,
        body: &[MacroStmt],
    ) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);

        if init.is_none() && cond.is_none() && inc.is_none() {
            ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
                continue_action: None,
            });

            let mut out: Vec<String> = vec![format!("{pad}for ; ; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

            ctx.pop_loop_context();

            out.push(format!("{pad}}}"));

            return Ok(out);
        }

        if let Some(ForInit::Decl { ty, name, init }) = init {
            let clean: String = crate::util::normalize_to_thrust_identifier(name);

            let init_text: String = if let Some(init) = init {
                let value: String =
                    crate::macro_expr::lower(ctx, init, crate::location::Location::RValue, ty);

                format!("var {clean}: {ty} = {value}")
            } else {
                format!("var {clean}: {ty}")
            };

            let cond_text: String = cond
                .map(|cond| self.lower_condition(ctx, cond))
                .unwrap_or_else(|| "true".to_string());

            let inc_text: String = inc
                .map(|inc| self.lower_inc_dec(ctx, inc))
                .unwrap_or_default();

            ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
                continue_action: None,
            });

            let mut out: Vec<String> =
                vec![format!("{pad}for {init_text}; {cond_text}; {inc_text}; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

            ctx.pop_loop_context();

            out.push(format!("{pad}}}"));

            return Ok(out);
        }

        if let Some(ForInit::Decls(decls)) = init {
            let mut out: Vec<String> = Vec::new();

            for decl in decls.iter() {
                let clean: String = crate::util::normalize_to_thrust_identifier(decl.get_name());

                if let Some(init) = decl.get_init() {
                    let init_text: String = crate::macro_expr::lower(
                        ctx,
                        init,
                        crate::location::Location::RValue,
                        decl.get_ty(),
                    );

                    out.push(format!(
                        "{pad}var {clean}: {} = {init_text};",
                        decl.get_ty()
                    ));
                } else {
                    out.push(format!("{pad}var {clean}: {};", decl.get_ty()));
                }
            }

            let cond_text: String = cond
                .map(|cond| self.lower_condition(ctx, cond))
                .unwrap_or_else(|| "true".to_string());

            let inc_text: Option<String> = inc.map(|inc| self.lower_inc_dec(ctx, inc));

            ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
                continue_action: inc_text.clone(),
            });

            out.push(format!("{pad}while {cond_text} {{"));

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

            ctx.pop_loop_context();

            if let Some(inc_text) = inc_text {
                if !inc_text.is_empty() {
                    out.push(format!("{}    {inc_text};", pad));
                }
            }

            out.push(format!("{pad}}}"));

            return Ok(out);
        }

        let mut out: Vec<String> = Vec::new();

        if let Some(ForInit::Expr(init)) = init {
            let init_text: String = self.lower_inc_dec(ctx, init);

            out.push(format!("{pad}{init_text};"));
        }

        let cond_text: String = cond
            .map(|cond| self.lower_condition(ctx, cond))
            .unwrap_or_else(|| "true".to_string());

        let inc_text: Option<String> = inc.map(|inc| self.lower_inc_dec(ctx, inc));

        ctx.push_loop_context(crate::macros::MacroLoopContext::Loop {
            continue_action: inc_text.clone(),
        });

        out.push(format!("{pad}while {cond_text} {{"));

        for stmt in body.iter() {
            out.extend(stmt.lower(ctx, indent + 1)?);
        }

        ctx.pop_loop_context();

        if let Some(inc_text) = inc_text {
            if !inc_text.is_empty() {
                out.push(format!("{}    {inc_text};", pad));
            }
        }

        out.push(format!("{pad}}}"));

        Ok(out)
    }
}

impl MacroStmt {
    fn lower_continue(
        &self,
        ctx: &mut crate::macros::MacroContext,
        indent: usize,
    ) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);

        match ctx.get_last_loop_context().cloned() {
            Some(crate::macros::MacroLoopContext::Switch { continue_flag }) => Ok(vec![
                format!("{pad}{continue_flag} = true;"),
                format!("{pad}break;"),
            ]),

            Some(crate::macros::MacroLoopContext::DoWhileZero) => {
                Ok(vec![format!("{pad}break;")])
            }

            Some(crate::macros::MacroLoopContext::Loop { continue_action }) => {
                let mut out: Vec<String> = Vec::new();

                if let Some(action) = continue_action {
                    out.push(format!("{pad}{action};"));
                }

                out.push(format!("{pad}continue;"));

                Ok(out)
            }

            None => Ok(vec![format!("{pad}continue;")]),
        }
    }
}

impl MacroStmt {
    fn lower_switch(
        &self,
        ctx: &mut crate::macros::MacroContext,
        indent: usize,
        cond: &MacroExpr,
        arms: &[crate::macro_ast::MacroSwitchArm],
    ) -> Result<Vec<String>, MacroLimit> {
        let pad: String = "    ".repeat(indent);
        let inner_pad: String = "    ".repeat(indent + 1);
        let body_pad: String = "    ".repeat(indent + 2);

        let switch_type: String = crate::macro_type::MacroType::expression_type(ctx, cond)
            .map(|ty| ty.text())
            .unwrap_or_else(|| "s32".to_string());

        let cond_text: String =
            crate::macro_expr::lower(ctx, cond, crate::location::Location::RValue, &switch_type);

        let value_name: String = ctx.next_temporary_name();
        let matched_name: String = ctx.next_temporary_name();
        let any_name: String = ctx.next_temporary_name();
        let continue_flag: String = ctx.next_temporary_name();

        let case_matches: Vec<String> = arms
            .iter()
            .flat_map(|arm| arm.get_labels().iter().flatten())
            .map(|label| {
                let label_text: String = crate::macro_expr::lower(
                    ctx,
                    label,
                    crate::location::Location::RValue,
                    &switch_type,
                );

                format!("{value_name} == {label_text}")
            })
            .collect();

        let any_match: String = if case_matches.is_empty() {
            "false".to_string()
        } else {
            case_matches.join(" || ")
        };

        let mut out: Vec<String> = vec![
            format!("{pad}var {value_name}: {switch_type} = {cond_text};"),
            format!("{pad}var {matched_name}: bool = false;"),
            format!("{pad}var {continue_flag}: bool = false;"),
        ];

        if !case_matches.is_empty() {
            out.push(format!("{pad}var {any_name}: bool = {any_match};"));
        }

        out.push(format!("{pad}loop {{"));

        ctx.push_loop_context(crate::macros::MacroLoopContext::Switch {
            continue_flag: continue_flag.clone(),
        });

        for arm in arms.iter() {
            let labels: Vec<&MacroExpr> = arm.get_labels().iter().flatten().collect();
            let has_default: bool = arm.get_labels().iter().any(Option::is_none);

            let mut conditions: Vec<String> = Vec::new();

            for label in labels {
                let label_text: String = crate::macro_expr::lower(
                    ctx,
                    label,
                    crate::location::Location::RValue,
                    &switch_type,
                );

                conditions.push(format!("{value_name} == {label_text}"));
            }

            if has_default {
                if case_matches.is_empty() {
                    conditions.push("true".to_string());
                } else {
                    conditions.push(format!("!{any_name}"));
                }
            }

            let condition: String = if conditions.is_empty() {
                "true".to_string()
            } else {
                format!("{} || {matched_name}", conditions.join(" || "))
            };

            out.push(format!("{inner_pad}if {condition} {{"));
            out.push(format!("{body_pad}{matched_name} = true;"));

            for statement in arm.get_body().iter() {
                out.extend(statement.lower(ctx, indent + 2)?);
            }

            out.push(format!("{inner_pad}}}"));
        }

        ctx.pop_loop_context();

        out.push(format!("{inner_pad}break;"));
        out.push(format!("{pad}}}"));

        if ctx.get_last_loop_context().is_some() {
            out.push(format!("{pad}if {continue_flag} {{"));

            let continuation: Vec<String> = self.lower_continue(ctx, indent + 1)?;

            out.extend(continuation);
            out.push(format!("{pad}}}"));
        }

        Ok(out)
    }
}

impl MacroStmt {
    pub fn requires_caller_context(&self, loop_depth: usize, switch_depth: usize) -> bool {
        match self {
            MacroStmt::Return(_) => true,
            MacroStmt::Break => loop_depth == 0 && switch_depth == 0,
            MacroStmt::Continue => loop_depth == 0,
            MacroStmt::VarDecl { storage, .. } => {
                *storage == crate::macro_ast::MacroStorageClass::Static
            }
            MacroStmt::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .iter()
                .chain(else_branch.iter())
                .any(|statement| statement.requires_caller_context(loop_depth, switch_depth)),
            MacroStmt::Switch { arms, .. } => arms.iter().any(|arm| {
                arm.get_body()
                    .iter()
                    .any(|statement| statement.requires_caller_context(loop_depth, switch_depth + 1))
            }),
            MacroStmt::While { body, .. } | MacroStmt::DoWhile { body, .. } => body
                .iter()
                .any(|statement| statement.requires_caller_context(loop_depth + 1, switch_depth)),
            MacroStmt::For { body, .. } => body
                .iter()
                .any(|statement| statement.requires_caller_context(loop_depth + 1, switch_depth)),
            MacroStmt::Compound(body) => body
                .iter()
                .any(|statement| statement.requires_caller_context(loop_depth, switch_depth)),
            MacroStmt::Empty
            | MacroStmt::Expr(_)
            | MacroStmt::Goto(_)
            | MacroStmt::Label(_) => false,
        }
    }
}

impl MacroStmt {
    fn contains_break_to_current_loop(&self, body: &[MacroStmt]) -> bool {
        body.iter().any(|statement| statement.breaks_current_loop())
    }

    fn contains_continue_to_current_loop(&self, body: &[MacroStmt]) -> bool {
        body.iter().any(|statement| statement.continues_current_loop())
    }

    fn breaks_current_loop(&self) -> bool {
        match self {
            MacroStmt::Break => true,
            MacroStmt::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .iter()
                .chain(else_branch.iter())
                .any(|statement| statement.breaks_current_loop()),
            MacroStmt::Compound(body) => body
                .iter()
                .any(|statement| statement.breaks_current_loop()),
            MacroStmt::Switch { arms, .. } => arms.iter().any(|arm| {
                arm.get_body()
                    .iter()
                    .any(|statement| statement.breaks_inside_switch())
            }),
            MacroStmt::While { .. }
            | MacroStmt::DoWhile { .. }
            | MacroStmt::For { .. }
            | MacroStmt::Empty
            | MacroStmt::VarDecl { .. }
            | MacroStmt::Expr(_)
            | MacroStmt::Continue
            | MacroStmt::Return(_)
            | MacroStmt::Goto(_)
            | MacroStmt::Label(_) => false,
        }
    }

    fn breaks_inside_switch(&self) -> bool {
        match self {
            MacroStmt::Break => false,
            MacroStmt::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .iter()
                .chain(else_branch.iter())
                .any(|statement| statement.breaks_inside_switch()),
            MacroStmt::Compound(body) => body
                .iter()
                .any(|statement| statement.breaks_inside_switch()),
            MacroStmt::Switch { arms, .. } => arms.iter().any(|arm| {
                arm.get_body()
                    .iter()
                    .any(|statement| statement.breaks_inside_switch())
            }),
            MacroStmt::While { body, .. } | MacroStmt::DoWhile { body, .. } => body
                .iter()
                .any(|statement| statement.breaks_current_loop()),
            MacroStmt::For { body, .. } => body
                .iter()
                .any(|statement| statement.breaks_current_loop()),
            MacroStmt::Empty
            | MacroStmt::VarDecl { .. }
            | MacroStmt::Expr(_)
            | MacroStmt::Continue
            | MacroStmt::Return(_)
            | MacroStmt::Goto(_)
            | MacroStmt::Label(_) => false,
        }
    }

    fn continues_current_loop(&self) -> bool {
        match self {
            MacroStmt::Continue => true,
            MacroStmt::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .iter()
                .chain(else_branch.iter())
                .any(|statement| statement.continues_current_loop()),
            MacroStmt::Compound(body) => body
                .iter()
                .any(|statement| statement.continues_current_loop()),
            MacroStmt::Switch { arms, .. } => arms.iter().any(|arm| {
                arm.get_body()
                    .iter()
                    .any(|statement| statement.continues_current_loop())
            }),
            MacroStmt::While { .. }
            | MacroStmt::DoWhile { .. }
            | MacroStmt::For { .. }
            | MacroStmt::Empty
            | MacroStmt::VarDecl { .. }
            | MacroStmt::Expr(_)
            | MacroStmt::Break
            | MacroStmt::Return(_)
            | MacroStmt::Goto(_)
            | MacroStmt::Label(_) => false,
        }
    }
}

impl MacroStmt {
    fn lower_condition(&self, ctx: &mut crate::macros::MacroContext, cond: &MacroExpr) -> String {
        if let MacroExpr::Binary { op, .. } = cond {
            if op == "=="
                || op == "!="
                || op == "<"
                || op == "<="
                || op == ">"
                || op == ">="
                || op == "&&"
                || op == "||"
            {
                return crate::macro_expr::lower(
                    ctx,
                    cond,
                    crate::location::Location::RValue,
                    "bool",
                );
            }
        }

        let is_boolean: bool = cond
            .base_identifier()
            .and_then(|name| ctx.get_type_environment().get(name))
            .is_some_and(
                |ty| matches!(ty, crate::macro_type::InferredType::Known(text) if text == "bool"),
            );

        if is_boolean {
            return crate::macro_expr::lower(ctx, cond, crate::location::Location::RValue, "bool");
        }

        format!(
            "({}) != 0",
            crate::macro_expr::lower(ctx, cond, crate::location::Location::RValue, "s32")
        )
    }
}

impl MacroStmt {
    fn lower_inc_dec(&self, ctx: &mut crate::macros::MacroContext, expr: &MacroExpr) -> String {
        match expr {
            MacroExpr::Unary { op, arg }
                if matches!(op, MacroUnOp::PreIncrement | MacroUnOp::PreDecrement) =>
            {
                let arg_text: String =
                    crate::macro_expr::lower(ctx, arg, crate::location::Location::LValue, "T1");

                if matches!(op, MacroUnOp::PreIncrement) {
                    format!("{arg_text} += 1")
                } else {
                    format!("{arg_text} -= 1")
                }
            }

            MacroExpr::Postfix { op, arg } => {
                let arg_text: String =
                    crate::macro_expr::lower(ctx, arg, crate::location::Location::LValue, "T1");

                match op {
                    crate::macro_ast::MacroPostOp::Increment => format!("{arg_text} += 1"),
                    crate::macro_ast::MacroPostOp::Decrement => format!("{arg_text} -= 1"),
                }
            }

            _ => crate::macro_expr::lower(ctx, expr, crate::location::Location::RValue, "T1"),
        }
    }
}
