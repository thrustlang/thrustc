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

use thrustc_ast::ast_metadata::CastingMetadata;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

use crate::context::TypeCheckerControlContext;

pub fn check_type_cast(
    cast_type: &Type,
    from_type: &Type,
    metadata: &CastingMetadata,
    span: &Span,
    control_context: &mut TypeCheckerControlContext,
) -> Result<(), CompilationIssue> {
    let is_allocated: bool = metadata.is_allocated();

    control_context.increase_type_cast_depth();

    if control_context.get_type_cast_depth() >= thrustc_constants::COMPILER_TOO_MANY_TYPE_DEPTH {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0037,
            "Too many type depth, the type exceeds type checking bounds!".into(),
            "You should remove nested types.".into(),
            None,
            *span,
        ));
    }

    match (from_type, cast_type) {
        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. },
        ) => Ok(()),

        (Type::FX8680 { .. }, Type::FX8680 { .. }) => Ok(()),
        (Type::FPPC128 { .. }, Type::FPPC128 { .. }) => Ok(()),

        (
            Type::Ptr { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
        ) => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
            Type::Bool { .. },
        ) => Ok(()),

        (
            Type::Bool { .. },
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
        ) => Ok(()),

        (Type::Ptr { .. }, Type::Ptr { .. }) => Ok(()),
        (Type::Ptr { .. }, Type::Array { .. }) if is_allocated => Ok(()),
        (Type::Ptr { subtype: None, .. }, Type::Fn { .. }) if is_allocated => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. }
            | Type::Char { .. }
            | Type::F32 { .. }
            | Type::F64 { .. }
            | Type::F128 { .. }
            | Type::FX8680 { .. }
            | Type::FPPC128 { .. }
            | Type::Bool { .. }
            | Type::Struct { .. }
            | Type::Array { .. }
            | Type::FixedArray { .. }
            | Type::Fn { .. },
            Type::Ptr { .. },
        ) if is_allocated => Ok(()),

        (
            Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::U128 { .. }
            | Type::USize { .. }
            | Type::SSize { .. },
            Type::Ptr { .. },
        ) => Ok(()),

        (Type::Const(from_type, ..), cast_type) => {
            self::check_type_cast(from_type, cast_type, metadata, span, control_context)
        }

        (from_type, Type::Const(cast_type, ..)) => {
            self::check_type_cast(cast_type, from_type, metadata, span, control_context)
        }

        (
            Type::Array {
                base_type: from_type,
                ..
            },
            Type::Array {
                base_type: target_type,
                ..
            },
        ) if from_type == target_type => Ok(()),

        (
            Type::FixedArray {
                base_type: provided_type,
                ..
            },
            Type::Array {
                base_type: target_type,
                ..
            },
        ) if provided_type == target_type && is_allocated => Ok(()),

        _ => Err(CompilationIssue::Error(
            CompilationIssueCode::E0032,
            format!(
                "Cannot cast type '{}' to '{}'. Types are incompatible for cast.",
                from_type, cast_type
            ),
            "You should try other approach.".into(),
            None,
            *span,
        )),
    }
}
