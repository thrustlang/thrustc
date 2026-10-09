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

use std::collections::HashMap;

use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::macro_ast::{ForInit, MacroExpr, MacroStmt, MacroUnOp};

#[derive(Debug)]
pub struct MacroType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferredType {
    Known(String),
    Pointer(Box<InferredType>),
}

impl InferredType {
    #[inline]
    pub fn text(&self) -> String {
        match self {
            InferredType::Known(text) => text.clone(),
            InferredType::Pointer(inner) => format!("ptr[{}]", inner.text()),
        }
    }
}

type TypeSlots = HashMap<String, Option<InferredType>>;

impl MacroType {
    pub fn infer_parameter_types(
        parameters: &[String],
        body: &[MacroStmt],
        seeds: &[Option<InferredType>],
    ) -> Result<Vec<String>, String> {
        let mut slots: TypeSlots = HashMap::new();

        for (index, parameter) in parameters.iter().enumerate() {
            let seed: Option<InferredType> = seeds.get(index).cloned().flatten();

            slots.insert(parameter.clone(), seed);
        }

        for statement in body.iter() {
            Self::infer_statement(&mut slots, statement);
        }

        let mut resolved: Vec<String> = Vec::with_capacity(parameters.len());

        for (index, parameter) in parameters.iter().enumerate() {
            let mut ty: Option<InferredType> = slots.get(parameter).cloned().flatten();

            if ty.is_none() {
                ty = seeds.get(index).cloned().flatten();
            }

            let text: String = match ty {
                Some(InferredType::Known(text)) => text,
                Some(InferredType::Pointer(inner)) => match *inner {
                    InferredType::Known(text) => text,
                    InferredType::Pointer(_) => {
                        return Err(parameter.clone());
                    }
                },
                None => {
                    return Err(parameter.clone());
                }
            };

            resolved.push(text);
        }

        Ok(resolved)
    }
}

impl MacroType {
    fn infer_expression(
        slots: &mut TypeSlots,
        expr: &MacroExpr,
        expected: Option<&InferredType>,
    ) -> Option<InferredType> {
        match expr {
            MacroExpr::Ident(name) => {
                if slots.contains_key(name) {
                    if let Some(expected) = expected {
                        Self::bind_slot(slots, name, expected.clone());
                    }

                    slots
                        .get(name)
                        .cloned()
                        .flatten()
                        .or_else(|| expected.cloned())
                } else {
                    expected.cloned()
                }
            }

            MacroExpr::Literal(text) => {
                Self::literal_spelling_type(text).or_else(|| expected.cloned())
            }

            MacroExpr::Null => expected.cloned(),

            MacroExpr::Paren(inner) => Self::infer_expression(slots, inner, expected),

            MacroExpr::Unary { op, arg } => match op {
                MacroUnOp::Not | MacroUnOp::PreIncrement | MacroUnOp::PreDecrement => {
                    Self::infer_expression(slots, arg, None);
                    expected.cloned()
                }

                MacroUnOp::Ref => {
                    let inner_expected: Option<InferredType> = match expected {
                        Some(InferredType::Pointer(inner)) => Some((**inner).clone()),
                        _ => None,
                    };

                    let inner: Option<InferredType> =
                        Self::infer_expression(slots, arg, inner_expected.as_ref());

                    match expected.cloned() {
                        Some(expected) => Some(expected),
                        None => inner.map(|inner| InferredType::Pointer(Box::new(inner))),
                    }
                }

                MacroUnOp::Deref => {
                    let pointee: Option<InferredType> = Self::infer_expression(slots, arg, None);

                    match expected.cloned() {
                        Some(expected) => Some(expected),
                        None => match pointee {
                            Some(InferredType::Pointer(inner)) => Some(*inner),
                            _ => None,
                        },
                    }
                }

                MacroUnOp::Negate | MacroUnOp::Positive | MacroUnOp::Invert => {
                    Self::infer_expression(slots, arg, expected)
                }
            },

            MacroExpr::Postfix { op: _, arg } => {
                Self::infer_expression(slots, arg, expected);
                expected.cloned()
            }

            MacroExpr::Binary { op, left, right } => {
                let is_comparison: bool = matches!(
                    op.as_str(),
                    "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"
                );

                if is_comparison {
                    let left_type: Option<InferredType> = Self::infer_expression(slots, left, None);
                    let right_type: Option<InferredType> =
                        Self::infer_expression(slots, right, None);

                    let unified: Option<InferredType> = left_type.or(right_type);

                    if unified.is_some() {
                        Self::infer_expression(slots, left, unified.as_ref());
                        Self::infer_expression(slots, right, unified.as_ref());
                    }

                    return Some(InferredType::Known("bool".to_string()));
                }

                let left_type: Option<InferredType> = Self::infer_expression(slots, left, expected);
                let right_type: Option<InferredType> =
                    Self::infer_expression(slots, right, expected);

                let unified: Option<InferredType> = expected.cloned().or(left_type).or(right_type);

                if unified.is_some() {
                    Self::infer_expression(slots, left, unified.as_ref());
                    Self::infer_expression(slots, right, unified.as_ref());
                }

                unified
            }

            MacroExpr::Comma(items) => {
                let last_index: usize = items.len().saturating_sub(1);

                let mut result: Option<InferredType> = None;

                for (index, item) in items.iter().enumerate() {
                    let item_expected: Option<&InferredType> =
                        if index == last_index { expected } else { None };

                    result = Self::infer_expression(slots, item, item_expected);
                }

                expected.cloned().or(result)
            }

            MacroExpr::Ternary {
                cond,
                then_branch,
                else_branch,
            } => {
                Self::infer_expression(slots, cond, None);

                let then_type: Option<InferredType> =
                    Self::infer_expression(slots, then_branch, expected);
                let else_type: Option<InferredType> =
                    Self::infer_expression(slots, else_branch, expected);

                let unified: Option<InferredType> = expected.cloned().or(then_type).or(else_type);

                if unified.is_some() {
                    Self::infer_expression(slots, then_branch, unified.as_ref());
                    Self::infer_expression(slots, else_branch, unified.as_ref());
                }

                unified
            }

            MacroExpr::Call { callee, args } => {
                for arg in args.iter() {
                    Self::infer_expression(slots, arg, None);
                }

                Self::infer_expression(slots, callee, None);

                match expected.cloned() {
                    Some(expected) => Some(expected),
                    None => match callee.as_ref() {
                        MacroExpr::Ident(name) if matches!(name.as_str(), "sizeOf" | "alignOf") => {
                            Some(InferredType::Known("usize".to_string()))
                        }
                        _ => None,
                    },
                }
            }

            MacroExpr::Index { base, index } => {
                Self::infer_expression(slots, index, None);

                match expected.cloned() {
                    Some(expected) => {
                        let pointee: InferredType =
                            InferredType::Pointer(Box::new(expected.clone()));

                        Self::infer_expression(slots, base, Some(&pointee));

                        Some(expected)
                    }
                    None => {
                        let base_type: Option<InferredType> =
                            Self::infer_expression(slots, base, None);

                        match base_type {
                            Some(InferredType::Pointer(inner)) => Some(*inner),
                            _ => None,
                        }
                    }
                }
            }

            MacroExpr::Member { base, field: _ } => {
                Self::infer_expression(slots, base, None);
                expected.cloned()
            }

            MacroExpr::Cast { target, arg } => {
                let target_type: InferredType = InferredType::Known(target.clone());

                Self::infer_expression(slots, arg, Some(&target_type));

                Some(target_type)
            }

            MacroExpr::Assign {
                op: _,
                target,
                value,
            } => {
                if let Some(name) = Self::base_identifier(target) {
                    if slots.contains_key(name) {
                        let target_expected: Option<InferredType> =
                            slots.get(name).cloned().flatten();

                        let value_type: Option<InferredType> =
                            Self::infer_expression(slots, value, target_expected.as_ref());

                        if let Some(value_type) = value_type {
                            Self::bind_slot(slots, name, value_type);
                        }

                        return slots.get(name).cloned().flatten();
                    }
                }

                let value_type: Option<InferredType> = Self::infer_expression(slots, value, None);

                if let Some(value_type) = value_type {
                    Self::infer_expression(slots, target, Some(&value_type));
                } else {
                    Self::infer_expression(slots, target, None);
                }

                None
            }

            MacroExpr::SizeOf(_) => Some(InferredType::Known("usize".to_string())),
        }
    }
}

impl MacroType {
    fn infer_statement(slots: &mut TypeSlots, statement: &MacroStmt) {
        match statement {
            MacroStmt::VarDecl { ty, name, init } => {
                slots.insert(name.clone(), Some(InferredType::Known(ty.clone())));

                if let Some(init) = init {
                    let expected: InferredType = InferredType::Known(ty.clone());

                    Self::infer_expression(slots, init, Some(&expected));
                }
            }

            MacroStmt::Expr(expr) => {
                Self::infer_expression(slots, expr, None);
            }

            MacroStmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let expected: InferredType = InferredType::Known("bool".to_string());

                Self::infer_expression(slots, cond, Some(&expected));

                for statement in then_branch.iter() {
                    Self::infer_statement(slots, statement);
                }

                for statement in else_branch.iter() {
                    Self::infer_statement(slots, statement);
                }
            }

            MacroStmt::While { cond, body } => {
                let expected: InferredType = InferredType::Known("bool".to_string());

                Self::infer_expression(slots, cond, Some(&expected));

                for statement in body.iter() {
                    Self::infer_statement(slots, statement);
                }
            }

            MacroStmt::DoWhile { body, cond } => {
                for statement in body.iter() {
                    Self::infer_statement(slots, statement);
                }

                let expected: InferredType = InferredType::Known("bool".to_string());

                Self::infer_expression(slots, cond, Some(&expected));
            }

            MacroStmt::For {
                init,
                cond,
                inc,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ForInit::Decl { ty, name, init } => {
                            slots.insert(name.clone(), Some(InferredType::Known(ty.clone())));

                            if let Some(init) = init {
                                let expected: InferredType = InferredType::Known(ty.clone());

                                Self::infer_expression(slots, init, Some(&expected));
                            }
                        }

                        ForInit::Decls(decls) => {
                            for decl in decls.iter() {
                                let ty: String = decl.get_ty().to_string();

                                slots.insert(
                                    decl.get_name().to_string(),
                                    Some(InferredType::Known(ty.clone())),
                                );

                                if let Some(init) = decl.get_init() {
                                    let expected: InferredType = InferredType::Known(ty);

                                    Self::infer_expression(slots, init, Some(&expected));
                                }
                            }
                        }

                        ForInit::Expr(expr) => {
                            Self::infer_expression(slots, expr, None);
                        }
                    }
                }

                if let Some(cond) = cond {
                    let expected: InferredType = InferredType::Known("bool".to_string());

                    Self::infer_expression(slots, cond, Some(&expected));
                }

                if let Some(inc) = inc {
                    Self::infer_expression(slots, inc, None);
                }

                for statement in body.iter() {
                    Self::infer_statement(slots, statement);
                }
            }

            MacroStmt::Compound(inner) => {
                for statement in inner.iter() {
                    Self::infer_statement(slots, statement);
                }
            }
        }
    }
}

impl MacroType {
    fn literal_spelling_type(text: &str) -> Option<InferredType> {
        if text.starts_with('"') {
            return None;
        }

        if text.starts_with('\'') {
            return Some(InferredType::Known("char".to_string()));
        }

        if text.contains('.') || text.contains('e') || text.contains('E') {
            return Some(InferredType::Known("f64".to_string()));
        }

        Some(InferredType::Known("s32".to_string()))
    }
}

impl MacroType {
    #[inline]
    pub fn literal_argument_type(expr: &MacroExpr) -> Option<InferredType> {
        match expr {
            MacroExpr::Literal(text) => Self::literal_spelling_type(text),
            MacroExpr::Paren(inner) => Self::literal_argument_type(inner),
            _ => None,
        }
    }
}

impl MacroType {
    /// Computes the inferred type of an expression using the current type
    /// environment, mirroring the parser's binary/unary type generation.
    #[inline]
    pub fn expression_type(
        ctx: &crate::macros::MacroContext<'_>,
        expr: &MacroExpr,
    ) -> Option<InferredType> {
        let mut slots: TypeSlots = ctx
            .get_type_environment()
            .iter()
            .map(|(name, ty)| (name.clone(), Some(ty.clone())))
            .collect();

        Self::infer_expression(&mut slots, expr, None)
    }
}

impl MacroType {
    /// Builds a parameter type seed from the clang type of a macro argument.
    ///
    /// Pointer and array arguments seed a pointer slot so the inference can
    /// reconstruct `ptr[T]` parameters; other types seed a plain value slot.
    #[inline]
    pub fn from_clang_type(
        ctx: &mut crate::macros::MacroContext<'_>,
        ty: &clang::Type<'_>,
        span: Span,
    ) -> Option<InferredType> {
        let canonical: clang::Type<'_> = ty.get_canonical_type();

        let prefix: String = String::new();

        if canonical.get_kind() == clang::TypeKind::Pointer {
            let pointee: clang::Type<'_> = canonical.get_pointee_type()?;

            let pointee_text: String =
                crate::type_format::format_clang_type_thrust(&pointee, ctx, &prefix, span).ok()?;

            let pointee_text: &str = pointee_text.strip_prefix("const ").unwrap_or(&pointee_text);

            if pointee_text.is_empty() {
                return None;
            }

            return Some(InferredType::Pointer(Box::new(InferredType::Known(
                pointee_text.to_string(),
            ))));
        }

        if matches!(
            canonical.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        ) {
            let element: clang::Type<'_> = canonical.get_element_type()?;

            let element_text: String =
                crate::type_format::format_clang_type_thrust(&element, ctx, &prefix, span).ok()?;

            let element_text: &str = element_text.strip_prefix("const ").unwrap_or(&element_text);

            if element_text.is_empty() {
                return None;
            }

            return Some(InferredType::Pointer(Box::new(InferredType::Known(
                element_text.to_string(),
            ))));
        }

        let text: String = crate::type_format::format_clang_type_thrust(ty, ctx, &prefix, span).ok()?;

        let text: &str = text.strip_prefix("const ").unwrap_or(&text);

        if text.is_empty() {
            return None;
        }

        Some(InferredType::Known(text.to_string()))
    }
}

impl MacroType {
    #[inline]
    fn base_identifier(expr: &MacroExpr) -> Option<&str> {
        match expr {
            MacroExpr::Ident(name) => Some(name),
            MacroExpr::Paren(inner) => Self::base_identifier(inner),
            _ => None,
        }
    }
}

impl MacroType {
    #[inline]
    fn bind_slot(slots: &mut TypeSlots, name: &str, ty: InferredType) {
        if let Some(slot) = slots.get_mut(name) {
            let keep_existing_pointer: bool = matches!(slot.as_ref(), Some(InferredType::Pointer(_)))
                && matches!(&ty, InferredType::Known(text) if text.starts_with("ptr["));

            if !keep_existing_pointer {
                *slot = Some(ty);
            }
        }
    }
}

impl MacroType {
    #[inline]
    pub fn from_thrust_text(text: &str, span: Span) -> Type {
        match text {
            "s8" => Type::S8 { span },
            "s16" => Type::S16 { span },
            "s32" => Type::S32 { span },
            "s64" => Type::S64 { span },
            "ssize" => Type::SSize { span },
            "u8" => Type::U8 { span },
            "u16" => Type::U16 { span },
            "u32" => Type::U32 { span },
            "u64" => Type::U64 { span },
            "u128" => Type::U128 { span },
            "usize" => Type::USize { span },
            "f32" => Type::F32 { span },
            "f64" => Type::F64 { span },
            "f128" => Type::F128 { span },
            "bool" => Type::Bool { span },
            "char" => Type::Char { span },
            "void" => Type::Void { span },
            _ => Type::Unresolved {
                hint: text.to_string(),
                span,
            },
        }
    }

    #[inline]
    pub fn to_thrust_type(ty: &Type) -> String {
        crate::type_format::format_type_thrust(ty)
    }
}
