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
            MacroStmt::VarDecl { ty, name, init } => {
                let clean: String = crate::util::normalize_to_thrust_identifier(name);

                if let Some(init) = init {
                    let init_text: String =
                        crate::macro_expr::lower(ctx, init, crate::location::Location::RValue, ty);

                    Ok(vec![format!("{pad}var {clean}: {ty} = {init_text};")])
                } else {
                    Ok(vec![format!("{pad}var {clean}: {ty};")])
                }
            }

            MacroStmt::Expr(expr) => {
                let text: String = self.lower_inc_dec(ctx, expr);

                Ok(vec![format!("{pad}{text};")])
            }

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

            MacroStmt::While { cond, body } => {
                let cond_text: String = self.lower_condition(ctx, cond);

                let mut out: Vec<String> = vec![format!("{pad}while {cond_text} {{")];

                for stmt in body.iter() {
                    out.extend(stmt.lower(ctx, indent + 1)?);
                }

                out.push(format!("{pad}}}"));

                Ok(out)
            }

            MacroStmt::DoWhile { body, cond } => {
                if matches!(cond, MacroExpr::Literal(text) if text == "0") {
                    let mut out: Vec<String> = Vec::new();

                    for stmt in body.iter() {
                        out.extend(stmt.lower(ctx, indent)?);
                    }

                    return Ok(out);
                }

                let cond_text: String = self.lower_condition(ctx, cond);

                let mut out: Vec<String> = Vec::new();

                for stmt in body.iter() {
                    out.extend(stmt.lower(ctx, indent)?);
                }

                out.push(format!("{pad}while {cond_text} {{"));

                for stmt in body.iter() {
                    out.extend(stmt.lower(ctx, indent + 1)?);
                }

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
            let mut out: Vec<String> = vec![format!("{pad}for ; ; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

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

            let mut out: Vec<String> =
                vec![format!("{pad}for {init_text}; {cond_text}; {inc_text}; {{")];

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

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

            out.push(format!("{pad}while {cond_text} {{"));

            for stmt in body.iter() {
                out.extend(stmt.lower(ctx, indent + 1)?);
            }

            if let Some(inc) = inc {
                let inc_text: String = self.lower_inc_dec(ctx, inc);

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

        out.push(format!("{pad}while {cond_text} {{"));

        for stmt in body.iter() {
            out.extend(stmt.lower(ctx, indent + 1)?);
        }

        if let Some(inc) = inc {
            let inc_text: String = self.lower_inc_dec(ctx, inc);

            if !inc_text.is_empty() {
                out.push(format!("{}    {inc_text};", pad));
            }
        }

        out.push(format!("{pad}}}"));

        Ok(out)
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
