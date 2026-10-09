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

use crate::macro_ast::{MacroExpr, MacroPostOp, MacroUnOp};

impl MacroExpr {
    #[inline]
    pub fn is_atomic(&self) -> bool {
        matches!(
            self,
            MacroExpr::Ident(_)
                | MacroExpr::Literal(_)
                | MacroExpr::Null
                | MacroExpr::Paren(_)
                | MacroExpr::Call { .. }
                | MacroExpr::Index { .. }
                | MacroExpr::Member { .. }
                | MacroExpr::Postfix { .. }
        )
    }
}

impl MacroExpr {
    #[inline]
    pub fn base_identifier(&self) -> Option<&str> {
        match self {
            MacroExpr::Ident(name) => Some(name),
            MacroExpr::Paren(inner) => Self::base_identifier(inner),
            _ => None,
        }
    }
}

impl MacroExpr {
    /// Returns `(is_pointer, pointee_is_pointer)` for an operand identifier.
    #[inline]
    fn pointer_info(&self, ctx: &crate::macros::MacroContext<'_>) -> (bool, bool) {
        let ty: Option<&crate::macro_type::InferredType> = self
            .base_identifier()
            .and_then(|name| ctx.get_type_environment().get(name));

        match ty {
            Some(crate::macro_type::InferredType::Pointer(inner)) => (
                true,
                matches!(inner.as_ref(), crate::macro_type::InferredType::Pointer(_)),
            ),
            _ => (false, false),
        }
    }
}

pub fn lower(
    ctx: &mut crate::macros::MacroContext,
    expr: &MacroExpr,
    location: crate::location::Location,
    result_type: &str,
) -> String {
    match expr {
        MacroExpr::Ident(name) => name.to_string(),
        MacroExpr::Literal(text) => text.to_string(),
        MacroExpr::Null => "nullptr".to_string(),

        MacroExpr::Paren(inner) => {
            let inner_text: String = self::lower(ctx, inner, location, result_type);

            format!("({inner_text})")
        }

        MacroExpr::Unary { op, arg } => {
            if matches!(op, MacroUnOp::Ref) {
                let arg_text: String =
                    self::lower(ctx, arg, crate::location::Location::AddressOf, result_type);

                let grouped: String = if arg.is_atomic() {
                    arg_text
                } else {
                    format!("({arg_text})")
                };

                return format!("ref ({grouped})");
            }

            let arg_text: String =
                self::lower(ctx, arg, crate::location::Location::RValue, result_type);

            let grouped: String = if arg.is_atomic() {
                arg_text.clone()
            } else {
                format!("({arg_text})")
            };

            if matches!(op, MacroUnOp::PreIncrement | MacroUnOp::PreDecrement) {
                let (is_pointer, pointee_is_pointer) = arg.pointer_info(ctx);

                if is_pointer {
                    return crate::pointer::Pointer::lower_advance(
                        &arg_text,
                        "1",
                        matches!(op, MacroUnOp::PreIncrement),
                        pointee_is_pointer,
                    );
                }
            }

            match op {
                MacroUnOp::Ref => format!("ref ({grouped})"),
                MacroUnOp::Deref => format!("deref {grouped}"),
                MacroUnOp::Not => format!("!{grouped}"),
                MacroUnOp::Invert => format!("~{grouped}"),
                MacroUnOp::Negate => format!("-{grouped}"),
                MacroUnOp::Positive => format!("+{grouped}"),
                MacroUnOp::PreIncrement => format!("++{grouped}"),
                MacroUnOp::PreDecrement => format!("--{grouped}"),
            }
        }

        MacroExpr::Postfix { op, arg } => {
            let arg_text: String =
                self::lower(ctx, arg, crate::location::Location::RValue, result_type);

            let grouped: String = if arg.is_atomic() {
                arg_text.clone()
            } else {
                format!("({arg_text})")
            };

            let (is_pointer, pointee_is_pointer) = arg.pointer_info(ctx);

            if is_pointer {
                return crate::pointer::Pointer::lower_advance(
                    &arg_text,
                    "1",
                    matches!(op, MacroPostOp::Increment),
                    pointee_is_pointer,
                );
            }

            match op {
                MacroPostOp::Increment => format!("{grouped}++"),
                MacroPostOp::Decrement => format!("{grouped}--"),
            }
        }

        MacroExpr::Binary { op, left, right } => {
            let left_text: String =
                self::lower(ctx, left, crate::location::Location::RValue, result_type);
            let right_text: String =
                self::lower(ctx, right, crate::location::Location::RValue, result_type);

            let (left_is_pointer, left_pointee_is_pointer) = left.pointer_info(ctx);
            let (right_is_pointer, right_pointee_is_pointer) = right.pointer_info(ctx);

            let pointee_is_pointer: bool = if left_is_pointer {
                left_pointee_is_pointer
            } else {
                right_pointee_is_pointer
            };

            if let Some(transformed) = crate::pointer::Pointer::lower_binary(
                left_is_pointer,
                right_is_pointer,
                pointee_is_pointer,
                op,
                &left_text,
                &right_text,
            ) {
                return transformed;
            }

            format!("{left_text} {op} {right_text}")
        }

        MacroExpr::Comma(items) => {
            let item_texts: Vec<String> = items
                .iter()
                .map(|item| self::lower(ctx, item, crate::location::Location::RValue, result_type))
                .collect();

            format!("({})", item_texts.join(", "))
        }

        MacroExpr::Assign { op, target, value } => {
            let target_text: String =
                self::lower(ctx, target, crate::location::Location::LValue, result_type);
            let value_text: String =
                self::lower(ctx, value, crate::location::Location::RValue, result_type);

            let (is_pointer, pointee_is_pointer) = target.pointer_info(ctx);

            if is_pointer && (op == "+=" || op == "-=") {
                return crate::pointer::Pointer::lower_advance(
                    &target_text,
                    &value_text,
                    op == "+=",
                    pointee_is_pointer,
                );
            }

            format!("{target_text} {op} {value_text}")
        }

        MacroExpr::Ternary {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_text: String = if let MacroExpr::Binary { op, .. } = cond.as_ref() {
                if op == "=="
                    || op == "!="
                    || op == "<"
                    || op == "<="
                    || op == ">"
                    || op == ">="
                    || op == "&&"
                    || op == "||"
                {
                    self::lower(ctx, cond, crate::location::Location::RValue, "bool")
                } else {
                    format!(
                        "({}) != 0",
                        self::lower(ctx, cond, crate::location::Location::RValue, "bool")
                    )
                }
            } else {
                format!(
                    "({}) != 0",
                    self::lower(ctx, cond, crate::location::Location::RValue, "bool")
                )
            };

            let then_type: Option<crate::macro_type::InferredType> =
                crate::macro_type::MacroType::expression_type(ctx, then_branch);
            let else_type: Option<crate::macro_type::InferredType> =
                crate::macro_type::MacroType::expression_type(ctx, else_branch);

            let ternary_type: String = then_type
                .or(else_type)
                .map(|ty| ty.text())
                .unwrap_or_else(|| result_type.to_string());

            let then_text: String = self::lower(ctx, then_branch, location, &ternary_type);
            let else_text: String = self::lower(ctx, else_branch, location, &ternary_type);

            let temporary: String = ctx.next_temporary_name();

            ctx.push_pending_statement(format!("var {temporary}: {ternary_type};"));
            ctx.push_pending_statement(format!("if {cond_text} {{"));
            ctx.push_pending_statement(format!(
                "    {temporary} = ({then_text}) as {ternary_type};"
            ));
            ctx.push_pending_statement("} else {".into());
            ctx.push_pending_statement(format!(
                "    {temporary} = ({else_text}) as {ternary_type};"
            ));
            ctx.push_pending_statement("}".into());

            temporary
        }

        MacroExpr::Call { callee, args } => {
            let callee_text: String =
                self::lower(ctx, callee, crate::location::Location::RValue, result_type);

            let grouped_callee: String = if matches!(callee.as_ref(), MacroExpr::Ident(_)) {
                callee_text
            } else {
                format!("({callee_text})")
            };

            let argument_texts: Vec<String> = args
                .iter()
                .map(|arg| self::lower(ctx, arg, crate::location::Location::RValue, result_type))
                .collect();

            format!("{grouped_callee}({})", argument_texts.join(", "))
        }

        MacroExpr::Index { base, index } => {
            fn lower_base(
                ctx: &mut crate::macros::MacroContext,
                expr: &MacroExpr,
                result_type: &str,
            ) -> String {
                match expr {
                    MacroExpr::Ident(name) => name.to_string(),

                    MacroExpr::Paren(inner) => lower_base(ctx, inner, result_type),

                    MacroExpr::Member { base, field } => {
                        let base_text: String = lower_base(ctx, base, result_type);

                        format!("{base_text}.{field}")
                    }

                    _ => format!(
                        "({})",
                        self::lower(ctx, expr, crate::location::Location::RValue, result_type)
                    ),
                }
            }

            let index_text: String =
                self::lower(ctx, index, crate::location::Location::RValue, result_type);

            if location.is_address_of() {
                let base_text: String = self::lower(ctx, base, location, result_type);

                return format!("{base_text}[{index_text}]");
            }

            let base_text: String = lower_base(ctx, base, result_type);

            format!("{base_text}->[{index_text}]")
        }

        MacroExpr::Member { base, field } => {
            let base_text: String =
                self::lower(ctx, base, crate::location::Location::RValue, result_type);

            let grouped_base: String = if matches!(base.as_ref(), MacroExpr::Ident(_)) {
                base_text
            } else {
                format!("({base_text})")
            };

            if location.is_address_of() {
                format!("{grouped_base}.{field}")
            } else {
                format!("{grouped_base}->{field}")
            }
        }

        MacroExpr::Cast { target, arg } => {
            let arg_text: String =
                self::lower(ctx, arg, crate::location::Location::RValue, result_type);

            format!("({arg_text}) as {target}")
        }

        MacroExpr::SizeOf(target) => {
            format!("abiSizeOf({target})")
        }
    }
}

pub fn lower_function_body(
    ctx: &mut crate::macros::MacroContext,
    expr: &MacroExpr,
    return_type: &str,
) -> String {
    let body_text: String = self::lower(ctx, expr, crate::location::Location::RValue, return_type);

    format!("({body_text}) as {return_type}")
}
