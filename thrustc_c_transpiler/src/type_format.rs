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

use thrustc_compile_time::BuiltinValue;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_typesystem::Type;

pub(crate) fn format_clang_type_thrust(ty: &clang::Type<'_>) -> Result<String, String> {
    let is_const: bool = ty.is_const_qualified();
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    let mut out: String = match canonical.get_kind() {
        clang::TypeKind::Void => "void".into(),
        clang::TypeKind::Bool => "bool".into(),

        clang::TypeKind::CharS | clang::TypeKind::CharU => "char".into(),
        clang::TypeKind::SChar => "s8".into(),
        clang::TypeKind::UChar => "u8".into(),

        clang::TypeKind::Short => "s16".into(),
        clang::TypeKind::UShort => "u16".into(),

        clang::TypeKind::Int => "s32".into(),
        clang::TypeKind::UInt => "u32".into(),

        clang::TypeKind::Long => "ssize".into(),
        clang::TypeKind::ULong => "usize".into(),

        clang::TypeKind::LongLong => "s64".into(),
        clang::TypeKind::ULongLong => "u64".into(),

        clang::TypeKind::UInt128 => "u128".into(),

        clang::TypeKind::Float => "f32".into(),
        clang::TypeKind::Double => "f64".into(),

        clang::TypeKind::Pointer => {
            let Some(pointee) = canonical.get_pointee_type() else {
                return Ok("ptr".into());
            };

            let pointee_kind = pointee.get_canonical_type().get_kind();

            if matches!(
                pointee_kind,
                clang::TypeKind::FunctionPrototype | clang::TypeKind::FunctionNoPrototype
            ) {
                return self::format_clang_type_thrust(&pointee);
            }

            if pointee_kind == clang::TypeKind::Void {
                return Ok("ptr".into());
            }

            let inner_ty: clang::Type<'_> = pointee.get_canonical_type();
            let pointee_const: bool = pointee.is_const_qualified();
            let mut inner: String = self::format_clang_type_thrust(&inner_ty)?;

            if let Some(stripped) = inner.strip_prefix("const ") {
                inner = stripped.to_string();
            }

            if pointee_const {
                format!("const ptr[{inner}]")
            } else {
                format!("ptr[{inner}]")
            }
        }

        clang::TypeKind::ConstantArray => {
            let element_type: clang::Type<'_> = canonical
                .get_element_type()
                .ok_or_else(|| "constant array without element type".to_string())?;

            let size: u32 = canonical
                .get_size()
                .and_then(|s| u32::try_from(s).ok())
                .ok_or_else(|| "constant array with unknown or too-large size".to_string())?;

            let inner: String = self::format_clang_type_thrust(&element_type)?;

            format!("array[{inner}; {size}]")
        }

        clang::TypeKind::FunctionPrototype | clang::TypeKind::FunctionNoPrototype => {
            let return_type: clang::Type<'_> = canonical
                .get_result_type()
                .ok_or_else(|| "function type without return type".to_string())?;

            let return_type_text: String = self::format_clang_type_thrust(&return_type)?;
            let mut parameter_types_text: Vec<String> = Vec::new();

            if let Some(argument_types) = canonical.get_argument_types() {
                for argument_type in argument_types.iter() {
                    parameter_types_text.push(self::format_parameter_type_thrust(argument_type)?);
                }
            }

            let mut out: String = String::new();

            out.push_str("Fn[");
            out.push_str(&parameter_types_text.join(", "));
            out.push(']');

            if canonical.is_variadic() {
                out.push_str(" @arbitraryArgs");
            }

            out.push_str(" -> ");
            out.push_str(&return_type_text);
            out
        }

        clang::TypeKind::Record => {
            let decl = canonical
                .get_declaration()
                .ok_or_else(|| "record without declaration".to_string())?;

            let name: String = decl
                .get_name()
                .ok_or_else(|| "anonymous record type".to_string())?;

            {
                let __sanitized: String = name.to_string();

                match __sanitized.as_str() {
                    "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                    | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for" | "if"
                    | "import" | "importC" | "load" | "loop" | "ptr" | "ref" | "return"
                    | "struct" | "true" | "type" | "union" | "var" | "void" | "while" => {
                        format!("{__sanitized}_")
                    }

                    _ => __sanitized,
                }
            }
        }

        clang::TypeKind::Enum => {
            // Use the enum name in source when available.

            let Some(decl) = canonical.get_declaration() else {
                return Ok("s32".into());
            };

            decl.get_name()
                .map(|name| {
                    let __sanitized: String = name.to_string();

                    match __sanitized.as_str() {
                        "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
                        | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for"
                        | "if" | "import" | "importC" | "load" | "loop" | "ptr" | "ref"
                        | "return" | "struct" | "true" | "type" | "union" | "var" | "void"
                        | "while" => format!("{__sanitized}_"),

                        _ => __sanitized,
                    }
                })
                .unwrap_or_else(|| "s32".into())
        }

        other => return Err(format!("unsupported C type kind: {other:?}")),
    };

    if is_const {
        out = format!("const {out}");
    }

    Ok(out)
}

pub fn format_type_thrust(ty: &Type) -> String {
    match ty {
        Type::S8 { .. } => "s8".into(),
        Type::S16 { .. } => "s16".into(),
        Type::S32 { .. } => "s32".into(),
        Type::S64 { .. } => "s64".into(),
        Type::SSize { .. } => "ssize".into(),
        Type::U8 { .. } => "u8".into(),
        Type::U16 { .. } => "u16".into(),
        Type::U32 { .. } => "u32".into(),
        Type::U64 { .. } => "u64".into(),
        Type::U128 { .. } => "u128".into(),
        Type::USize { .. } => "usize".into(),
        Type::F32 { .. } => "f32".into(),
        Type::F64 { .. } => "f64".into(),
        Type::F128 { .. } => "f128".into(),
        Type::FX8680 { .. } => "fx86_80".into(),
        Type::FPPC128 { .. } => "fppc_128".into(),
        Type::Bool { .. } => "bool".into(),
        Type::Char { .. } => "char".into(),
        Type::Void { .. } => "void".into(),

        Type::Unresolved { hint, .. } => format!("unresolved[{hint}]"),

        Type::Const(inner, ..) => format!("const {}", self::format_type_thrust(inner)),

        Type::Ptr {
            subtype,
            address_space,
            ..
        } => {
            let Some(inner) = subtype.as_deref() else {
                return "ptr".into();
            };

            let mut out: String = String::with_capacity(32);

            out.push_str("ptr[");
            out.push_str(&self::format_type_thrust(inner));

            if let Some(space) = address_space {
                out.push_str(", ");
                out.push_str(&space.to_string());
            }

            out.push(']');
            out
        }

        // A struct type is referenced by name in source, not by inline layout.
        Type::Struct { name, .. } => name.clone(),

        Type::FixedArray {
            base_type,
            size,
            metadata,
            ..
        } => {
            let mut out: String = String::with_capacity(48);

            out.push_str("array[");
            out.push_str(&self::format_type_thrust(base_type));
            out.push_str("; ");
            out.push_str(&size.to_string());

            if let Some(space) = metadata.get_address_space() {
                out.push_str(", ");
                out.push_str(&space.to_string());
            }

            out.push(']');
            out
        }

        Type::Array {
            base_type,
            metadata,
            ..
        } => {
            let mut out: String = String::with_capacity(48);

            out.push_str("array[");
            out.push_str(&self::format_type_thrust(base_type));

            if let Some(space) = metadata.get_address_space() {
                out.push_str(", ");
                out.push_str(&space.to_string());
            }

            out.push(']');
            out
        }

        Type::NativeVector {
            element_type,
            element_count,
            ..
        } => format!(
            "NativeVector[{}; {}]",
            self::format_type_thrust(element_type),
            element_count
        ),

        Type::Fn {
            parameter_types,
            return_type,
            modificator,
            ..
        } => {
            let mut out: String = String::with_capacity(64);

            out.push_str("Fn[");
            for (idx, p) in parameter_types.iter().enumerate() {
                if idx != 0 {
                    out.push_str(", ");
                }

                out.push_str(&self::format_type_thrust(p));
            }
            out.push(']');

            if modificator.llvm().has_ignore() {
                out.push_str(" @arbitraryArgs");
            }

            out.push_str(" -> ");
            out.push_str(&self::format_type_thrust(return_type));
            out
        }
    }
}

pub(crate) fn format_builtin_value_thrust(
    value: &BuiltinValue,
    expected_type: &Type,
    macro_ctx: &mut crate::macros::MacroContext,
) -> String {
    match value {
        BuiltinValue::Integer(v) => v.to_string(),
        BuiltinValue::Float(v) => {
            if v.is_finite() {
                return v.to_string();
            }

            macro_ctx
                .get_mut_transpiler_context()
                .add_warning(CompilationIssue::Warning(
                    CompilationIssueCode::W0105,
                    "Non-finite float constant emitted as 0.0".into(),
                    thrustc_code_location::Span::nothing(),
                ));

            "0.0".into()
        }
        BuiltinValue::Bool(v) => {
            if *v {
                "true".into()
            } else {
                "false".into()
            }
        }
        BuiltinValue::Char(b) => {
            if *b >= 0x80 {
                macro_ctx
                    .get_mut_transpiler_context()
                    .add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0105,
                        format!("Non-ASCII char macro emitted as cast: {b}"),
                        thrustc_code_location::Span::nothing(),
                    ));

                return format!("({b} as char)");
            }

            match *b {
                b'\\' => "'\\\\'".into(),
                b'\n' => "'\\n'".into(),
                b'\r' => "'\\r'".into(),
                b'\t' => "'\\t'".into(),
                b'\0' => "'\\0'".into(),
                b'\'' => "'\\\''".into(),
                b'"' => "'\\\"'".into(),
                other if matches!(other, 0x20..=0x7E) => format!("'{}'", other as char),
                other => format!("({other} as char)"),
            }
        }
        BuiltinValue::CString(bytes) | BuiltinValue::CNString(bytes) => {
            let mut is_cstring: bool = matches!(value, BuiltinValue::CString(..));

            if !is_cstring {
                let contains_nul: bool = bytes.contains(&0);

                if contains_nul {
                    macro_ctx.get_mut_transpiler_context().add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0105,
                        "CNString macro contains a null byte; emitted as CString to remain parseable.".into(),
                        thrustc_code_location::Span::nothing(),
                    ));
                    is_cstring = true;
                }
            }

            let had_non_ascii: bool = bytes.iter().any(|b| !b.is_ascii());
            let text_lossy: std::borrow::Cow<'_, str> = String::from_utf8_lossy(bytes);
            let had_replacement: bool = text_lossy
                .chars()
                .any(|ch| ch == char::REPLACEMENT_CHARACTER);

            if had_non_ascii || had_replacement {
                let mut msg: String = String::new();

                if had_replacement {
                    msg.push_str(
                        "Macro string contains invalid UTF-8; emitted with lossy replacement.",
                    );
                } else {
                    msg.push_str(
                        "Macro string contains non-ASCII UTF-8; emitted as Unicode literal.",
                    );
                }

                let kind: &str = if is_cstring { "CString" } else { "CNString" };

                msg.push(' ');
                msg.push_str("Literal kind: ");
                msg.push_str(kind);

                macro_ctx
                    .get_mut_transpiler_context()
                    .add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0105,
                        msg,
                        thrustc_code_location::Span::nothing(),
                    ));
            }

            let escaped: String = crate::clang_util::escape_string_for_thrust_literal(&text_lossy);

            if is_cstring {
                format!("\"{escaped}\"")
            } else {
                format!("n#\"{escaped}\"")
            }
        }
        BuiltinValue::NullPtr => "nullptr".into(),
        BuiltinValue::Void => {
            macro_ctx
                .get_mut_transpiler_context()
                .add_warning(CompilationIssue::Warning(
                    CompilationIssueCode::W0105,
                    format!(
                        "Void constant emitted as nullptr for type '{}'",
                        self::format_type_thrust(expected_type)
                    ),
                    thrustc_code_location::Span::nothing(),
                ));
            "nullptr".into()
        }
    }
}

pub(crate) fn format_parameter_type_thrust(ty: &clang::Type<'_>) -> Result<String, String> {
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    if matches!(
        canonical.get_kind(),
        clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
    ) {
        let mut current: clang::Type<'_> = canonical;
        let mut depth: usize = 0;

        while matches!(
            current.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        ) {
            depth = depth.saturating_add(1);

            current = current
                .get_element_type()
                .ok_or_else(|| "array parameter without element type".to_string())?
                .get_canonical_type();
        }

        if depth > 1
            && !matches!(
                current.get_kind(),
                clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
                    | clang::TypeKind::Record
            )
        {
            let inner: String = self::format_clang_type_thrust(&current)?;

            return Ok(format!("ptr[{inner}]"));
        }

        let element_type: clang::Type<'_> = canonical
            .get_element_type()
            .ok_or_else(|| "array parameter without element type".to_string())?;

        let inner: String = self::format_clang_type_thrust(&element_type)?;

        return Ok(format!("ptr[{inner}]"));
    }

    if canonical.get_kind() == clang::TypeKind::Pointer {
        let Some(pointee_type) = canonical.get_pointee_type() else {
            return self::format_clang_type_thrust(ty);
        };

        let mut current: clang::Type<'_> = pointee_type.get_canonical_type();
        let mut depth: usize = 0;

        while matches!(
            current.get_kind(),
            clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
        ) {
            depth = depth.saturating_add(1);

            current = current
                .get_element_type()
                .ok_or_else(|| "pointer-to-array parameter without element type".to_string())?
                .get_canonical_type();
        }

        if depth > 0
            && !matches!(
                current.get_kind(),
                clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
                    | clang::TypeKind::Record
            )
        {
            let inner: String = self::format_clang_type_thrust(&current)?;

            return Ok(format!("ptr[{inner}]"));
        }
    }

    self::format_clang_type_thrust(ty)
}

pub(crate) fn format_clang_calling_convention_thrust(
    convention: Option<clang::CallingConvention>,
) -> Result<&'static str, String> {
    match convention.unwrap_or(clang::CallingConvention::Cdecl) {
        clang::CallingConvention::Cdecl => Ok("C"),
        clang::CallingConvention::SysV64 => Ok("X86_64_SysV"),
        clang::CallingConvention::Win64 => Ok("Win64"),
        clang::CallingConvention::Stdcall => Ok("X86StdCall"),
        clang::CallingConvention::Fastcall => Ok("X86FastCall"),
        clang::CallingConvention::Thiscall => Ok("X86ThisCall"),
        clang::CallingConvention::Vectorcall => Ok("X86VectorCall"),
        clang::CallingConvention::Swift => Ok("Swift"),
        clang::CallingConvention::PreserveMost => Ok("weakReg"),
        clang::CallingConvention::PreserveAll => Ok("strongReg"),
        clang::CallingConvention::Aapcs => Ok("ARMAAPCS"),
        clang::CallingConvention::AapcsVfp => Ok("ARM_AAPCS_VFP"),
        clang::CallingConvention::IntelOcl => Ok("Intel_OCL_BI"),
        clang::CallingConvention::RegCall => Ok("X86RegCall"),
        other => Err(format!("unsupported calling convention: {other:?}")),
    }
}
