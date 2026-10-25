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
                | MacroExpr::SizeOfExpr(_)
                | MacroExpr::CompoundLiteral { .. }
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

impl MacroExpr {
    fn resolved_value_type(
        &self,
        ctx: &crate::macros::MacroContext<'_>,
    ) -> Option<crate::macro_type::MacroValueType> {
        match self {
            MacroExpr::Ident(name) => {
                if let Some(binding) = ctx.get_inline_binding(name) {
                    if !matches!(binding, MacroExpr::Ident(bound) if bound == name) {
                        return binding.resolved_value_type(ctx);
                    }
                }

                ctx.get_value_type(name).cloned()
            }

            MacroExpr::Index { base, .. } => base
                .resolved_value_type(ctx)
                .and_then(|kind| kind.index_element()),

            MacroExpr::Member { base, field } => {
                let base_type: crate::macro_type::MacroValueType =
                    base.resolved_value_type(ctx)?;

                let record: &crate::macro_type::MacroValueType = base_type.strip_const();

                let record_name: Option<&str> = match record {
                    crate::macro_type::MacroValueType::Struct { name } => Some(name),
                    crate::macro_type::MacroValueType::Pointer { pointee } => {
                        match pointee.strip_const() {
                            crate::macro_type::MacroValueType::Struct { name } => Some(name),
                            _ => None,
                        }
                    }
                    _ => None,
                };

                record_name
                    .and_then(|name| ctx.get_record_field(name, field))
                    .cloned()
            }

            MacroExpr::Paren(inner) => inner.resolved_value_type(ctx),

            MacroExpr::Cast { target, .. } => {
                Some(crate::macro_type::MacroValueType::from_text(target))
            }

            MacroExpr::CompoundLiteral { ty, .. } => {
                Some(crate::macro_type::MacroValueType::from_text(ty))
            }

            MacroExpr::Literal(_) | MacroExpr::Null => {
                Some(crate::macro_type::MacroValueType::Scalar)
            }

            MacroExpr::Unary { arg, .. }
            | MacroExpr::Postfix { arg, .. }
            | MacroExpr::Assign { value: arg, .. } => arg.resolved_value_type(ctx),

            MacroExpr::Call { .. }
            | MacroExpr::Binary { .. }
            | MacroExpr::Comma(_)
            | MacroExpr::Ternary { .. }
            | MacroExpr::SizeOf(_)
            | MacroExpr::SizeOfExpr(_)
            | MacroExpr::InitList(_) => None,
        }
    }
}

impl MacroExpr {
    fn index_is_complex(&self, ctx: &crate::macros::MacroContext<'_>) -> bool {
        match self {
            MacroExpr::Index { .. } => true,
            MacroExpr::Ident(name) => ctx
                .get_inline_binding(name)
                .is_some_and(|binding| binding.index_is_complex(ctx)),
            MacroExpr::Paren(inner) => inner.index_is_complex(ctx),
            _ => false,
        }
    }
}

impl MacroExpr {
    fn lower_index_base(
        &self,
        ctx: &mut crate::macros::MacroContext,
        result_type: &str,
    ) -> String {
        match self {
            MacroExpr::Ident(name) => {
                if let Some(binding) = ctx.get_inline_binding(name).cloned() {
                    return binding.lower_index_base(ctx, result_type);
                }

                name.to_string()
            }

            MacroExpr::Paren(inner) => inner.lower_index_base(ctx, result_type),

            MacroExpr::Member { base, field } => {
                let base_text: String = base.lower_index_base(ctx, result_type);

                format!("{base_text}.{field}")
            }

            _ => format!(
                "({})",
                self::lower(ctx, self, crate::location::Location::RValue, result_type)
            ),
        }
    }
}

impl MacroExpr {
    fn lower_index_address(
        &self,
        ctx: &mut crate::macros::MacroContext,
        result_type: &str,
    ) -> String {
        match self {
            MacroExpr::Index { base, index } => {
                let base_text: String = base.lower_index_address_base(ctx, result_type);
                let index_text: String =
                    self::lower(ctx, index, crate::location::Location::RValue, result_type);

                format!("{base_text}[{index_text}]")
            }

            _ => self.lower_index_base(ctx, result_type),
        }
    }

    fn lower_index_address_base(
        &self,
        ctx: &mut crate::macros::MacroContext,
        result_type: &str,
    ) -> String {
        match self {
            MacroExpr::Ident(name) => {
                if let Some(binding) = ctx.get_inline_binding(name).cloned() {
                    return binding.lower_index_address_base(ctx, result_type);
                }

                name.to_string()
            }

            MacroExpr::Index { .. } => self.lower_index_address(ctx, result_type),

            MacroExpr::Member { .. } => {
                self::lower(ctx, self, crate::location::Location::RValue, result_type)
            }

            MacroExpr::Paren(inner) => inner.lower_index_address_base(ctx, result_type),

            _ => self::lower(ctx, self, crate::location::Location::AddressOf, result_type),
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
        MacroExpr::Ident(name) => {
            if let Some(binding) = ctx.get_inline_binding(name).cloned() {
                if matches!(&binding, MacroExpr::Ident(bound) if bound == name) {
                    return name.to_string();
                }

                return self::lower(ctx, &binding, location, result_type);
            }

            name.to_string()
        }
        MacroExpr::Literal(text) => text.to_string(),
        MacroExpr::Null => "nullptr".to_string(),

        MacroExpr::Paren(inner) => {
            let inner_text: String = self::lower(ctx, inner, location, result_type);

            format!("({inner_text})")
        }

        MacroExpr::Unary { op, arg } => {
            if matches!(op, MacroUnOp::Ref) {
                if matches!(
                    arg.as_ref(),
                    MacroExpr::Index { .. }
                        | MacroExpr::Member { .. }
                        | MacroExpr::Unary {
                            op: MacroUnOp::Deref,
                            ..
                        }
                ) {
                    return self::lower(
                        ctx,
                        arg,
                        crate::location::Location::AddressOf,
                        result_type,
                    );
                }

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
                MacroUnOp::Deref if location.is_direct() => format!("{grouped}->[0]"),
                MacroUnOp::Deref => format!("(deref {grouped})"),
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
            if op != "="
                && matches!(target.as_ref(), MacroExpr::Index { .. })
                && target.index_is_complex(ctx)
            {
                let target_address: String =
                    self::lower(ctx, target, crate::location::Location::LValue, result_type);
                let target_value: String =
                    self::lower(ctx, target, crate::location::Location::RValue, result_type);
                let value_text: String =
                    self::lower(ctx, value, crate::location::Location::RValue, result_type);
                let binary_operator: &str = &op[..op.len().saturating_sub(1)];

                return format!(
                    "{target_address} = {target_value} {binary_operator} {value_text}"
                );
            }

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
            let index_text: String =
                self::lower(ctx, index, crate::location::Location::RValue, result_type);

            let base_type: Option<crate::macro_type::MacroValueType> =
                base.resolved_value_type(ctx);

            let result_shape: Option<crate::macro_type::MacroValueType> =
                base_type.as_ref().and_then(|kind| kind.index_element());

            let complex: bool = base.index_is_complex(ctx);

            if location.is_address_of() || (location.is_direct() && complex) {
                let address: String = base.lower_index_address(ctx, result_type);

                return format!("{address}[{index_text}]");
            }

            let base_text: String = base.lower_index_base(ctx, result_type);

            if complex {
                let address: String = base.lower_index_address(ctx, result_type);

                let address: String = format!("{address}[{index_text}]");

                let returns_pointer: bool = result_shape
                    .as_ref()
                    .is_some_and(|kind| kind.is_pointer_or_array());

                if returns_pointer {
                    return address;
                }

                return format!("(deref {address})");
            }

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

        MacroExpr::SizeOfExpr(inner) => {
            let target: String = crate::macro_type::MacroType::expression_type(ctx, inner)
                .map(|ty| ty.text())
                .unwrap_or_else(|| result_type.to_string());

            format!("abiSizeOf({target})")
        }

        MacroExpr::InitList(_) => self::lower_initializer(ctx, expr, result_type),

        MacroExpr::CompoundLiteral { ty, init } => {
            let _ = init;

            self::lower_initializer(ctx, expr, ty)
        }
    }
}

pub fn lower_initializer(
    ctx: &mut crate::macros::MacroContext,
    expr: &MacroExpr,
    target_type: &str,
) -> String {
    let MacroExpr::InitList(items) = expr else {
        if let MacroExpr::CompoundLiteral { ty, init } = expr {
            return self::lower_initializer(ctx, init, ty);
        }

        return self::lower(ctx, expr, crate::location::Location::RValue, target_type);
    };

    let array_element: Option<String> = target_type
        .strip_prefix("array[")
        .and_then(|value| value.strip_suffix(']'))
        .and_then(|value| value.rsplit_once("; ").map(|(inner, _)| inner.to_string()));

    if let Some(element_type) = array_element {
        let values: Vec<String> = items
            .iter()
            .map(|item| self::lower_initializer(ctx, item.get_value(), &element_type))
            .collect();

        return format!("fixed[{}]", values.join(", "));
    }

    let fields: Vec<String> = items
        .iter()
        .filter_map(|item| match item.get_designator() {
            Some(crate::macro_ast::MacroDesignator::Field(field)) => {
                let value: String = self::lower(
                    ctx,
                    item.get_value(),
                    crate::location::Location::RValue,
                    target_type,
                );

                Some(format!("{field}: {value}"))
            }
            _ => None,
        })
        .collect();

    if !fields.is_empty() {
        return format!("new {target_type} {{ {} }}", fields.join(", "));
    }

    let values: Vec<String> = items
        .iter()
        .map(|item| {
            self::lower(
                ctx,
                item.get_value(),
                crate::location::Location::RValue,
                target_type,
            )
        })
        .collect();

    format!("fixed[{}]", values.join(", "))
}

pub fn lower_function_body(
    ctx: &mut crate::macros::MacroContext,
    expr: &MacroExpr,
    return_type: &str,
) -> String {
    let body_text: String = self::lower(ctx, expr, crate::location::Location::RValue, return_type);

    format!("({body_text}) as {return_type}")
}
