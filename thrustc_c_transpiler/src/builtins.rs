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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CanonicalBuiltin {
    MemSet,
    MemCpy,
    MemMove,
    BZero,
    Expect,
}

impl CanonicalBuiltin {
    pub(crate) fn rewrite_builtin_call(
        self,
        argument_entities: &[clang::Entity<'_>],
        arguments: &[String],
    ) -> Option<String> {
        if argument_entities.len() != arguments.len() {
            return None;
        }

        match self {
            Self::MemSet => {
                if arguments.len() != 3 {
                    return None;
                }

                let destination: String =
                    Self::ensure_pointer_text(&argument_entities[0], &arguments[0]);

                let byte_value: String =
                    Self::ensure_byte_value_text(&argument_entities[1], &arguments[1]);

                let byte_size: String =
                    Self::ensure_size_value_text(&argument_entities[2], &arguments[2]);

                Some(format!("memset({destination}, {byte_value}, {byte_size})"))
            }

            Self::MemCpy => {
                if arguments.len() != 3 {
                    return None;
                }

                let source: String =
                    Self::ensure_pointer_text(&argument_entities[1], &arguments[1]);

                let destination: String =
                    Self::ensure_pointer_text(&argument_entities[0], &arguments[0]);

                let byte_size: String =
                    Self::ensure_signed_size_value_text(&argument_entities[2], &arguments[2]);

                Some(format!("memcpy({source}, {destination}, {byte_size})"))
            }

            Self::MemMove => {
                if arguments.len() != 3 {
                    return None;
                }

                let source: String =
                    Self::ensure_pointer_text(&argument_entities[1], &arguments[1]);

                let destination: String =
                    Self::ensure_pointer_text(&argument_entities[0], &arguments[0]);

                let byte_size: String =
                    Self::ensure_size_value_text(&argument_entities[2], &arguments[2]);

                Some(format!("memmove({source}, {destination}, {byte_size})"))
            }

            Self::BZero => {
                if arguments.len() != 2 {
                    return None;
                }

                let destination: String =
                    Self::ensure_pointer_text(&argument_entities[0], &arguments[0]);

                let byte_size: String =
                    Self::ensure_size_value_text(&argument_entities[1], &arguments[1]);

                Some(format!("memset({destination}, (0) as u8, {byte_size})"))
            }

            Self::Expect => arguments.first().cloned(),
        }
    }
}

impl CanonicalBuiltin {
    #[inline]
    pub(crate) fn from_called_function_name(called_function_name: &str) -> Option<Self> {
        let unqualified_name: &str = called_function_name
            .rsplit("::")
            .next()
            .unwrap_or(called_function_name);

        match unqualified_name {
            "memset" | "__builtin_memset" => Some(Self::MemSet),
            "memcpy" | "__builtin_memcpy" => Some(Self::MemCpy),
            "memmove" | "__builtin_memmove" => Some(Self::MemMove),
            "bzero" | "explicit_bzero" => Some(Self::BZero),
            "__builtin_expect" => Some(Self::Expect),
            _ => None,
        }
    }
}

impl CanonicalBuiltin {
    fn ensure_pointer_text(argument_entity: &clang::Entity<'_>, expression_text: &str) -> String {
        if let Some(argument_clang_type) = argument_entity.get_type() {
            let canonical_clang_type: clang::Type<'_> = argument_clang_type.get_canonical_type();

            if matches!(canonical_clang_type.get_kind(), clang::TypeKind::Pointer) {
                return expression_text.to_string();
            }
        }

        if expression_text.contains("as ptr") {
            return expression_text.to_string();
        }

        format!("({expression_text}) as ptr")
    }
}

impl CanonicalBuiltin {
    fn ensure_byte_value_text(
        argument_entity: &clang::Entity<'_>,
        expression_text: &str,
    ) -> String {
        if let Some(integer_literal) = Self::bare_integer_literal_spelling(argument_entity) {
            return format!("({integer_literal}) as u8");
        }

        if let Some(cast_inner_text) = Self::remove_cast_suffix(expression_text) {
            return format!("({cast_inner_text}) as u8");
        }

        format!("({expression_text}) as u8")
    }
}

impl CanonicalBuiltin {
    fn ensure_size_value_text(
        argument_entity: &clang::Entity<'_>,
        expression_text: &str,
    ) -> String {
        if let Some(integer_literal) = Self::bare_integer_literal_spelling(argument_entity) {
            return format!("({integer_literal}) as u64");
        }

        if let Some(cast_inner_text) = Self::remove_cast_suffix(expression_text) {
            return format!("({cast_inner_text}) as u64");
        }

        format!("({expression_text}) as u64")
    }
}

impl CanonicalBuiltin {
    fn ensure_signed_size_value_text(
        argument_entity: &clang::Entity<'_>,
        expression_text: &str,
    ) -> String {
        if let Some(integer_literal) = Self::bare_integer_literal_spelling(argument_entity) {
            return format!("({integer_literal}) as s32");
        }

        if expression_text.ends_with(" as s32")
            || expression_text.ends_with(" as s16")
            || expression_text.ends_with(" as s8")
            || expression_text.ends_with(" as s64")
            || expression_text.ends_with(" as ssize")
        {
            return expression_text.to_string();
        }

        if Self::remove_cast_suffix(expression_text).is_some() {
            let cast_inner_text: String =
                Self::remove_cast_suffix(expression_text).unwrap_or_default();

            return format!("({cast_inner_text}) as s32");
        }

        format!("({expression_text}) as s32")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeapOperation {
    Malloc,
    Calloc,
    Realloc,
    Free,
}

impl HeapOperation {
    #[inline]
    pub(crate) fn from_called_function_name(called_function_name: &str) -> Option<Self> {
        let unqualified_name: &str = called_function_name
            .rsplit("::")
            .next()
            .unwrap_or(called_function_name);

        match unqualified_name {
            "malloc" => Some(Self::Malloc),
            "calloc" => Some(Self::Calloc),
            "realloc" => Some(Self::Realloc),
            "free" => Some(Self::Free),
            _ => None,
        }
    }
}

impl HeapOperation {
    pub(crate) fn resolve_heap_call<'clang>(
        call_entity: &clang::Entity<'clang>,
    ) -> Option<(Self, clang::Entity<'clang>)> {
        let mut cursor_entity: clang::Entity<'_> = *call_entity;

        while matches!(
            cursor_entity.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            let nested_children: Vec<clang::Entity<'_>> = cursor_entity.get_children();

            if nested_children.len() != 1 {
                return None;
            }

            cursor_entity = nested_children[0];
        }

        if cursor_entity.get_kind() != clang::EntityKind::CallExpr {
            return None;
        }

        let child_entities: Vec<clang::Entity<'_>> = cursor_entity.get_children();

        let callee_entity: clang::Entity<'_> = *child_entities.first()?;

        let callee_name: String = Self::unwrap_parenthesized_name(&callee_entity)?;

        Some((
            Self::from_called_function_name(&callee_name)?,
            cursor_entity,
        ))
    }
}

impl HeapOperation {
    pub(crate) fn try_lower_heap_call(
        self,
        call_entity: &clang::Entity<'_>,
        pointee_type: Option<&clang::Type<'_>>,
        macro_ctx: &mut crate::macros::MacroContext,
        prefix: &str,
        span: thrustc_code_location::Span,
    ) -> Option<String> {
        match self {
            Self::Malloc => {
                Self::lower_malloc_call(call_entity, pointee_type, macro_ctx, prefix, span)
            }
            Self::Calloc | Self::Realloc | Self::Free => None,
        }
    }
}

impl HeapOperation {
    pub(crate) fn rejection_reason(self) -> &'static str {
        match self {
            Self::Malloc => {
                "Heap allocation size is not a sizeof expression; rewrite the allocation as malloc(sizeof(T)) or malloc(N * sizeof(T)) with a constant N."
            }
            Self::Calloc => {
                "calloc has no Thrust builtin; rewrite the C input with halloc plus an explicit memset."
            }
            Self::Realloc => {
                "realloc has no Thrust builtin; rewrite the C input to allocate the new size explicitly."
            }
            Self::Free => "free has no Thrust builtin; remove the explicit free call.",
        }
    }
}

impl HeapOperation {
    fn lower_malloc_call(
        call_entity: &clang::Entity<'_>,
        pointee_type: Option<&clang::Type<'_>>,
        macro_ctx: &mut crate::macros::MacroContext,
        prefix: &str,
        span: thrustc_code_location::Span,
    ) -> Option<String> {
        let child_entities: Vec<clang::Entity<'_>> = call_entity.get_children();

        if child_entities.len() != 2 {
            return None;
        }

        let size_argument: clang::Entity<'_> = Self::unwrap_parenthesized(child_entities[1]);

        if let Some(sizeof_operand) = Self::sizeof_operand_clang_type(&size_argument) {
            let thrust_type_name: String =
                Self::format_thrust_type(&sizeof_operand, macro_ctx, prefix, span)?;

            return Some(format!("halloc({thrust_type_name})"));
        }

        if let Some(thrust_type_name) = Self::sizeof_spelled_type_name(&size_argument) {
            return Some(format!("halloc({thrust_type_name})"));
        }

        if size_argument.get_kind() == clang::EntityKind::BinaryOperator {
            let operand_entities: Vec<clang::Entity<'_>> = size_argument.get_children();

            if operand_entities.len() == 2 {
                if Self::sizeof_operand_clang_type(&Self::unwrap_parenthesized(operand_entities[0]))
                    .is_some()
                    || Self::sizeof_spelled_type_name(&Self::unwrap_parenthesized(
                        operand_entities[0],
                    ))
                    .is_some()
                {
                    if let Some(element_count) =
                        Self::constant_u64(&Self::unwrap_parenthesized(operand_entities[1]))
                    {
                        return Self::sized_heap_allocate(
                            &Self::unwrap_parenthesized(operand_entities[0]),
                            element_count,
                            macro_ctx,
                            prefix,
                            span,
                        );
                    }
                }

                if Self::sizeof_operand_clang_type(&Self::unwrap_parenthesized(operand_entities[1]))
                    .is_some()
                    || Self::sizeof_spelled_type_name(&Self::unwrap_parenthesized(
                        operand_entities[1],
                    ))
                    .is_some()
                {
                    if let Some(element_count) =
                        Self::constant_u64(&Self::unwrap_parenthesized(operand_entities[0]))
                    {
                        return Self::sized_heap_allocate(
                            &Self::unwrap_parenthesized(operand_entities[1]),
                            element_count,
                            macro_ctx,
                            prefix,
                            span,
                        );
                    }
                }
            }

            return None;
        }

        let pointee_type: &clang::Type<'_> = pointee_type?;

        let thrust_type_name: String =
            Self::format_thrust_type(pointee_type, macro_ctx, prefix, span)?;

        let requested_bytes: u64 = Self::constant_u64(&size_argument)?;

        let pointee_bytes: u64 = u64::try_from(pointee_type.get_sizeof().ok()?).ok()?;

        if requested_bytes != pointee_bytes {
            return None;
        }

        Some(format!("halloc({thrust_type_name})"))
    }
}

impl HeapOperation {
    fn sized_heap_allocate(
        sizeof_entity: &clang::Entity<'_>,
        element_count: u64,
        macro_ctx: &mut crate::macros::MacroContext,
        prefix: &str,
        span: thrustc_code_location::Span,
    ) -> Option<String> {
        let thrust_type_name: String =
            Self::sizeof_operand_name(sizeof_entity, macro_ctx, prefix, span)?;

        if element_count == 1 {
            return Some(format!("halloc({thrust_type_name})"));
        }

        let array_length: u32 = u32::try_from(element_count).ok()?;

        Some(format!(
            "halloc(array[{thrust_type_name}; {array_length}]) as ptr[{thrust_type_name}]"
        ))
    }
}

impl HeapOperation {
    fn unwrap_parenthesized(candidate_entity: clang::Entity<'_>) -> clang::Entity<'_> {
        let mut cursor_entity: clang::Entity<'_> = candidate_entity;

        while matches!(
            cursor_entity.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            let nested_children: Vec<clang::Entity<'_>> = cursor_entity.get_children();

            if nested_children.len() != 1 {
                break;
            }

            cursor_entity = nested_children[0];
        }

        cursor_entity
    }
}

impl HeapOperation {
    fn unwrap_parenthesized_name(candidate_entity: &clang::Entity<'_>) -> Option<String> {
        let mut cursor_entity: clang::Entity<'_> = *candidate_entity;

        while matches!(
            cursor_entity.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            let nested_children: Vec<clang::Entity<'_>> = cursor_entity.get_children();

            if nested_children.len() != 1 {
                return None;
            }

            cursor_entity = nested_children[0];
        }

        cursor_entity.get_name()
    }
}

impl HeapOperation {
    fn sizeof_operand_name(
        sizeof_entity: &clang::Entity<'_>,
        macro_ctx: &mut crate::macros::MacroContext,
        prefix: &str,
        span: thrustc_code_location::Span,
    ) -> Option<String> {
        if let Some(sizeof_operand) = Self::sizeof_operand_clang_type(sizeof_entity) {
            if let Some(thrust_type_name) =
                Self::format_thrust_type(&sizeof_operand, macro_ctx, prefix, span)
            {
                return Some(thrust_type_name);
            }
        }

        Self::sizeof_spelled_type_name(sizeof_entity)
    }
}

impl HeapOperation {
    fn sizeof_spelled_type_name(sizeof_entity: &clang::Entity<'_>) -> Option<String> {
        let range: clang::source::SourceRange<'_> = sizeof_entity.get_range()?;

        let token_spellings: Vec<String> = range
            .tokenize()
            .into_iter()
            .map(|source_token| source_token.get_spelling())
            .collect();

        if token_spellings.len() < 4 {
            return None;
        }

        if token_spellings
            .first()
            .is_some_and(|source_token| source_token != "sizeof")
        {
            return None;
        }

        if token_spellings
            .get(1)
            .is_some_and(|source_token| source_token != "(")
        {
            return None;
        }

        if token_spellings
            .last()
            .is_some_and(|source_token| source_token != ")")
        {
            return None;
        }

        let inner_spelling: String = token_spellings[2..token_spellings.len() - 1].join(" ");

        crate::macro_lex::macro_type_name(&inner_spelling)
    }
}

impl HeapOperation {
    fn sizeof_operand_clang_type<'clang>(
        sizeof_entity: &clang::Entity<'clang>,
    ) -> Option<clang::Type<'clang>> {
        if !matches!(
            sizeof_entity.get_kind(),
            clang::EntityKind::UnaryOperator | clang::EntityKind::UnaryExpr
        ) {
            return None;
        }

        let range: clang::source::SourceRange<'_> = sizeof_entity.get_range()?;

        let first_spelling: String = range
            .tokenize()
            .first()
            .map(|source_token| source_token.get_spelling())
            .unwrap_or_default();

        if first_spelling != "sizeof" {
            return None;
        }

        let child_entities: Vec<clang::Entity<'_>> = sizeof_entity.get_children();

        child_entities
            .iter()
            .find_map(|child_entity| child_entity.get_type())
            .or_else(|| {
                child_entities
                    .last()
                    .and_then(|child_entity| child_entity.get_type())
            })
    }
}

impl HeapOperation {
    fn constant_u64(constant_entity: &clang::Entity<'_>) -> Option<u64> {
        match Self::unwrap_parenthesized(*constant_entity).evaluate()? {
            clang::EvaluationResult::UnsignedInteger(unsigned_value) => Some(unsigned_value),
            clang::EvaluationResult::SignedInteger(signed_value) => {
                u64::try_from(signed_value).ok()
            }
            _ => None,
        }
    }
}

impl HeapOperation {
    fn format_thrust_type(
        clang_type: &clang::Type<'_>,
        macro_ctx: &mut crate::macros::MacroContext,
        prefix: &str,
        span: thrustc_code_location::Span,
    ) -> Option<String> {
        let formatted_name: String =
            crate::type_format::format_clang_type_thrust(clang_type, macro_ctx, prefix, span);

        if formatted_name.is_empty() {
            return None;
        }

        Some(
            formatted_name
                .strip_prefix("const ")
                .unwrap_or(&formatted_name)
                .to_string(),
        )
    }
}

impl CanonicalBuiltin {
    fn bare_integer_literal_spelling(argument_entity: &clang::Entity<'_>) -> Option<String> {
        let mut cursor_entity: clang::Entity<'_> = *argument_entity;

        while matches!(
            cursor_entity.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            let nested_children: Vec<clang::Entity<'_>> = cursor_entity.get_children();

            if nested_children.len() != 1 {
                return None;
            }

            cursor_entity = nested_children[0];
        }

        if cursor_entity.get_kind() != clang::EntityKind::IntegerLiteral {
            return None;
        }

        let range: clang::source::SourceRange<'_> = cursor_entity.get_range()?;

        let literal_spelling: String = range
            .tokenize()
            .first()
            .map(|source_token| source_token.get_spelling())
            .unwrap_or_default();

        if literal_spelling.is_empty() {
            return None;
        }

        Some(
            literal_spelling
                .trim_end_matches(['u', 'U', 'l', 'L'])
                .to_string(),
        )
    }
}

impl CanonicalBuiltin {
    fn remove_cast_suffix(expression_text: &str) -> Option<String> {
        let cast_suffixes: [&str; 13] = [
            " as s32",
            " as s16",
            " as s8",
            " as s64",
            " as ssize",
            " as u32",
            " as u16",
            " as u8",
            " as u64",
            " as usize",
            " as f32",
            " as f64",
            " as char",
        ];

        for cast_suffix in cast_suffixes.iter() {
            if let Some(cast_inner_text) = expression_text.strip_suffix(cast_suffix) {
                if cast_inner_text.ends_with(')') {
                    return Some(cast_inner_text.to_string());
                }
            }
        }

        None
    }
}
