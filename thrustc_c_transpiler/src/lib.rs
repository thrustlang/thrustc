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

use std::path::{Path, PathBuf};

use thrustc_compile_time::BuiltinValue;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::ImportCOptions;
use thrustc_options::TranslateCOptions;
use thrustc_typesystem::Type;

mod expr;
mod stmt;
mod top_level;

type TranslateCOutput = (PathBuf, String, Vec<CompilationIssue>);

#[derive(Debug, Clone)]
pub struct EmitCBindingsOptions {
    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn new() -> Self {
        Self {
            out_dir: None,
            output: None,
        }
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn out_dir(&self) -> Option<&Path> {
        self.out_dir.as_deref()
    }

    #[inline]
    pub fn output(&self) -> Option<&Path> {
        self.output.as_deref()
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn set_out_dir(&mut self, out_dir: PathBuf) {
        self.out_dir = Some(out_dir);
    }

    #[inline]
    pub fn set_output(&mut self, output: PathBuf) {
        self.output = Some(output);
    }
}

impl EmitCBindingsOptions {
    #[inline]
    pub fn out_dir_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.out_dir
    }

    #[inline]
    pub fn output_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.output
    }
}

pub fn translate_c_to_thrust(
    inputs: &[PathBuf],
    translate_opts: &TranslateCOptions,
) -> Result<Vec<TranslateCOutput>, Vec<CompilationIssue>> {
    let mut outputs: Vec<TranslateCOutput> = Vec::new();

    let mut errors: Vec<CompilationIssue> = Vec::new();

    if inputs.is_empty() {
        errors.push(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "No input C files were provided.".into(),
            "Pass at least one '.c' file.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        return Err(errors);
    }

    if inputs.len() > 1 && translate_opts.output().is_some() {
        errors.push(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "'--translate-c-output' only supports a single input file.".into(),
            "Use '--translate-c-out-dir' or omit '--translate-c-output' when translating multiple inputs.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        return Err(errors);
    }

    for input in inputs {
        match self::translate_single_c_to_thrust(input, translate_opts) {
            Ok(result) => outputs.push(result),
            Err(mut issues) => errors.append(&mut issues),
        }
    }

    if errors.is_empty() {
        Ok(outputs)
    } else {
        Err(errors)
    }
}

pub fn emit_c_bindings_thrust(
    header: PathBuf,
    import_opts: &ImportCOptions,
    emit_opts: &EmitCBindingsOptions,
) -> Result<(PathBuf, String, Vec<CompilationIssue>), String> {
    let span = thrustc_code_location::Span::nothing();

    let clang_args: Vec<String> = self::build_clang_args_from_import_options(import_opts);

    let mut warnings: Vec<CompilationIssue> = Vec::new();

    let mut opts: thrustc_c_import_synthesis::options::CImportOptions =
        thrustc_c_import_synthesis::options::CImportOptions::new();
    *opts.clang_args_mut() = clang_args;

    let mut ctx: thrustc_c_import_synthesis::context::CImportContext =
        thrustc_c_import_synthesis::context::CImportContext::new(header.clone(), span, opts);

    ctx.import_header()?;

    for diagnostic in ctx.diagnostics() {
        let code: CompilationIssueCode = match diagnostic.kind() {
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::ClangDiagnostic => {
                CompilationIssueCode::W0100
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::InventedFieldName => {
                CompilationIssueCode::W0101
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedUnion => {
                CompilationIssueCode::W0102
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedBitfieldStruct => {
                CompilationIssueCode::W0103
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedDeclaration => {
                CompilationIssueCode::W0104
            }
        };

        warnings.push(CompilationIssue::Warning(
            code,
            diagnostic.message().to_string(),
            span,
        ));
    }

    let mut out: String = String::with_capacity(32 * 1024);

    top_level::append_imported_top_level_declarations(&ctx, span, &mut out, &mut warnings);

    let output_path: PathBuf = if let Some(output) = emit_opts.output() {
        output.to_path_buf()
    } else {
        let stem: String = header
            .file_stem()
            .map_or_else(String::new, |s| s.to_string_lossy().to_string());
        let file_name: String = format!("{stem}.bindings.thrust");

        if let Some(out_dir) = emit_opts.out_dir() {
            out_dir.join(file_name)
        } else {
            PathBuf::from(file_name)
        }
    };

    Ok((output_path, out, warnings))
}

fn translate_single_c_to_thrust(
    input: &Path,
    translate_opts: &TranslateCOptions,
) -> Result<TranslateCOutput, Vec<CompilationIssue>> {
    let span = thrustc_code_location::Span::nothing();

    let mut issues: Vec<CompilationIssue> = Vec::new();

    let input_path: PathBuf = input.to_path_buf();

    let canonical_input: PathBuf = input_path
        .canonicalize()
        .unwrap_or_else(|_| input_path.clone());

    let mut clang_args: Vec<String> = Vec::new();

    // Be explicit; libclang sometimes sees unsaved or non-standard inputs.
    clang_args.push("-xc".into());

    if let Some(res) = self::detect_clang_resource_include_dir() {
        clang_args.push(format!("-isystem{}", res.display()));
    }

    for inc in translate_opts.include_paths() {
        clang_args.push(format!("-I{}", inc.display()));
    }

    for inc in translate_opts.system_include_paths() {
        clang_args.push(format!("-isystem{}", inc.display()));
    }

    for def in translate_opts.defines() {
        clang_args.push(format!("-D{def}"));
    }

    for und in translate_opts.undefs() {
        clang_args.push(format!("-U{und}"));
    }

    if let Some(target) = translate_opts.target() {
        clang_args.push(format!("--target={target}"));
    }

    if let Some(sysroot) = translate_opts.sysroot() {
        clang_args.push(format!("--sysroot={}", sysroot.display()));
    }

    if let Some(std_) = translate_opts.std() {
        clang_args.push(format!("-std={std_}"));
    }

    clang_args.extend(translate_opts.args().iter().cloned());

    let clang: clang::Clang = match clang::Clang::new() {
        Ok(c) => c,
        Err(_) => {
            return Err(vec![CompilationIssue::Error(
                CompilationIssueCode::E0111,
                "Failed to initialize libclang.".into(),
                "Ensure libclang is available in this build.".into(),
                None,
                span,
            )]);
        }
    };

    let index: clang::Index = clang::Index::new(&clang, false, false);

    let args_refs: Vec<&str> = clang_args.iter().map(String::as_str).collect();

    let mut parser: clang::Parser = index.parser(&canonical_input);

    parser.detailed_preprocessing_record(true);
    parser.arguments(&args_refs);

    let tu: clang::TranslationUnit<'_> = match parser.parse() {
        Ok(tu) => tu,
        Err(e) => {
            return Err(vec![CompilationIssue::Error(
                CompilationIssueCode::E0111,
                format!("Failed to parse C file '{}'.", canonical_input.display()),
                format!("Clang error: {e}"),
                None,
                span,
            )]);
        }
    };

    let mut has_errors: bool = false;
    let mut fatal_diagnostics: Vec<String> = Vec::new();

    for diagnostic in tu.get_diagnostics() {
        let severity: clang::diagnostic::Severity = diagnostic.get_severity();
        let text: String = diagnostic.get_text();

        if matches!(
            severity,
            clang::diagnostic::Severity::Error | clang::diagnostic::Severity::Fatal
        ) {
            has_errors = true;
            fatal_diagnostics.push(format!("{:?}: {text}", severity));
        }

        issues.push(CompilationIssue::Warning(
            CompilationIssueCode::W0100,
            format!("{:?}: {text}", severity),
            span,
        ));
    }

    if has_errors {
        let note: Option<String> = if fatal_diagnostics.is_empty() {
            None
        } else {
            Some(fatal_diagnostics.join("\n"))
        };

        return Err(vec![CompilationIssue::Error(
            CompilationIssueCode::E0111,
            "Clang reported errors while parsing the C input.".into(),
            "Fix the C input or provide the correct --translate-c-* options.".into(),
            note,
            span,
        )]);
    }

    let Some(main_file) = tu.get_file(&canonical_input) else {
        let errors: Vec<CompilationIssue> = vec![CompilationIssue::Error(
            CompilationIssueCode::E0111,
            "Unable to locate the main file inside the translation unit.".into(),
            "Ensure the input path points to a real file.".into(),
            None,
            span,
        )];

        return Err(errors);
    };

    let cwd: PathBuf = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    let includes: Vec<clang::Entity<'_>> = main_file.get_includes();

    let mut import_c_include_dirs: Vec<PathBuf> = Vec::new();
    let mut import_c_specs: Vec<String> = Vec::new();

    for inc in includes.iter() {
        let Some(spec) = self::extract_include_spec(inc) else {
            continue;
        };

        import_c_specs.push(spec);

        let Some(file) = inc.get_file() else {
            continue;
        };

        let path: PathBuf = file.get_path();
        let Some(parent) = path.parent() else {
            continue;
        };

        let mut dir: PathBuf = parent.to_path_buf();
        if let Ok(rel) = dir.strip_prefix(&cwd) {
            dir = rel.to_path_buf();
        }

        // System header directories tend to be absolute and non-portable; rely on Clang defaults.
        let is_system: bool = file.get_location(1, 1).is_in_system_header();
        if is_system {
            continue;
        }

        if !import_c_include_dirs.contains(&dir) {
            import_c_include_dirs.push(dir);
        }
    }

    import_c_include_dirs.sort();
    import_c_specs.sort();
    import_c_specs.dedup();

    let mut out: String = String::with_capacity(64 * 1024);

    for dir in import_c_include_dirs.iter() {
        out.push_str("directive \"--import-c-include=");
        out.push_str(&dir.to_string_lossy());
        out.push_str("\";\n");
    }

    if !import_c_include_dirs.is_empty() {
        out.push('\n');
    }

    for spec in import_c_specs.iter() {
        out.push_str("importC \"");
        out.push_str(spec);
        out.push_str("\";\n");
    }

    if !import_c_specs.is_empty() {
        out.push('\n');
    }

    let root: clang::Entity<'_> = tu.get_entity();

    top_level::append_translated_top_level_declarations(&root, &canonical_input, &mut out, span)
        .map_err(|err| vec![err])?;

    let output_path: PathBuf = if let Some(output) = translate_opts.output() {
        output.to_path_buf()
    } else {
        let stem: String = canonical_input
            .file_stem()
            .map_or_else(String::new, |s| s.to_string_lossy().to_string());

        let file_name: String = format!("{stem}.thrust");

        if let Some(out_dir) = translate_opts.out_dir() {
            out_dir.join(&file_name)
        } else if let Some(parent) = canonical_input.parent() {
            parent.join(&file_name)
        } else {
            PathBuf::from(file_name)
        }
    };

    Ok((output_path, out, issues))
}

pub fn format_builtin_value_thrust(
    value: &BuiltinValue,
    expected_type: &Type,
    warnings: &mut Vec<CompilationIssue>,
) -> String {
    match value {
        BuiltinValue::Integer(v) => v.to_string(),
        BuiltinValue::Float(v) => {
            if v.is_finite() {
                return v.to_string();
            }

            warnings.push(CompilationIssue::Warning(
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
                warnings.push(CompilationIssue::Warning(
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
                    warnings.push(CompilationIssue::Warning(
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

                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0105,
                    msg,
                    thrustc_code_location::Span::nothing(),
                ));
            }

            let escaped: String = self::escape_string_for_thrust_literal(&text_lossy);

            if is_cstring {
                format!("\"{escaped}\"")
            } else {
                format!("n#\"{escaped}\"")
            }
        }
        BuiltinValue::NullPtr => "nullptr".into(),
        BuiltinValue::Void => {
            warnings.push(CompilationIssue::Warning(
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

pub(crate) fn format_parameter_type_thrust(ty: &clang::Type<'_>) -> Result<String, String> {
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    if canonical.get_kind() == clang::TypeKind::ConstantArray {
        let element_type: clang::Type<'_> = canonical
            .get_element_type()
            .ok_or_else(|| "array parameter without element type".to_string())?;
        let inner: String = self::format_clang_type_thrust(&element_type)?;

        return Ok(format!("ptr[{inner}]"));
    }

    self::format_clang_type_thrust(ty)
}

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
            let element_type = canonical
                .get_element_type()
                .ok_or_else(|| "constant array without element type".to_string())?;

            let size: u32 = canonical
                .get_size()
                .and_then(|s| u32::try_from(s).ok())
                .ok_or_else(|| "constant array with unknown or too-large size".to_string())?;

            let inner: String = self::format_clang_type_thrust(&element_type)?;
            format!("array[{inner}; {size}]")
        }

        clang::TypeKind::Record => {
            let decl = canonical
                .get_declaration()
                .ok_or_else(|| "record without declaration".to_string())?;

            decl.get_name()
                .ok_or_else(|| "anonymous record type".to_string())?
        }

        clang::TypeKind::Enum => {
            // Use the enum name in source when available.
            let Some(decl) = canonical.get_declaration() else {
                return Ok("s32".into());
            };

            decl.get_name().unwrap_or_else(|| "s32".into())
        }

        other => return Err(format!("unsupported C type kind: {other:?}")),
    };

    if is_const {
        out = format!("const {out}");
    }

    Ok(out)
}

pub(crate) fn needs_space_between_tokens(prev: &str, current: &str) -> bool {
    if current == ")" || current == "]" || current == ";" || current == "," {
        return false;
    }

    if prev == "(" || prev == "[" || prev == "!" || prev == "~" {
        return false;
    }

    let prev_ident: bool = prev
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

    let current_ident: bool = current
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');

    if prev_ident && current_ident {
        return true;
    }

    let prev_is_operator: bool = matches!(
        prev,
        "=" | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "&&"
            | "||"
            | "+"
            | "-"
            | "*"
            | "/"
            | "%"
            | "&"
            | "|"
            | "^"
            | "<<"
            | ">>"
            | "<<="
            | ">>="
    );
    let current_is_operator: bool = matches!(
        current,
        "=" | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "&&"
            | "||"
            | "+"
            | "-"
            | "*"
            | "/"
            | "%"
            | "&"
            | "|"
            | "^"
            | "<<"
            | ">>"
            | "<<="
            | ">>="
    );

    if prev_is_operator || current_is_operator {
        return true;
    }

    false
}

pub(crate) fn extract_binary_operator_from_tokens(tokens: &[String]) -> Option<&'static str> {
    let mut depth: i32 = 0;

    for t in tokens.iter() {
        match t.as_str() {
            "(" | "[" | "{" => {
                depth += 1;
                continue;
            }
            ")" | "]" | "}" => {
                depth -= 1;
                continue;
            }
            _ => {}
        }

        if depth != 0 {
            continue;
        }

        let op: Option<&'static str> = match t.as_str() {
            "=" => Some("="),
            "+=" => Some("+="),
            "-=" => Some("-="),
            "*=" => Some("*="),
            "/=" => Some("/="),
            "%=" => Some("%="),
            "==" => Some("=="),
            "!=" => Some("!="),
            "<" => Some("<"),
            "<=" => Some("<="),
            ">" => Some(">"),
            ">=" => Some(">="),
            "&&" => Some("&&"),
            "||" => Some("||"),
            "+" => Some("+"),
            "-" => Some("-"),
            "*" => Some("*"),
            "/" => Some("/"),
            "%" => Some("%"),
            "&" => Some("&"),
            "|" => Some("|"),
            "^" => Some("^"),
            "<<" => Some("<<"),
            ">>" => Some(">>"),
            "<<=" => Some("<<="),
            ">>=" => Some(">>="),
            _ => None,
        };

        if op.is_some() {
            return op;
        }
    }

    None
}

fn extract_include_spec(entity: &clang::Entity<'_>) -> Option<String> {
    let range = entity.get_range()?;
    let tokens: Vec<clang::token::Token<'_>> = range.tokenize();
    let spellings: Vec<String> = tokens.into_iter().map(|t| t.get_spelling()).collect();

    for s in spellings.iter() {
        if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
            return Some(s[1..s.len() - 1].to_string());
        }

        if s.starts_with('<') && s.ends_with('>') && s.len() >= 2 {
            return Some(s[1..s.len() - 1].to_string());
        }
    }

    let mut in_angle: bool = false;
    let mut out: String = String::new();

    for s in spellings.iter() {
        if s == "<" {
            in_angle = true;
            continue;
        }

        if s == ">" {
            if !out.is_empty() {
                return Some(out);
            }

            break;
        }

        if in_angle {
            out.push_str(s);
        }
    }

    None
}

pub(crate) fn is_assignment_expression(entity: &clang::Entity<'_>) -> bool {
    if entity.get_kind() == clang::EntityKind::UnaryOperator {
        let Some(range) = entity.get_range() else {
            return false;
        };

        return range
            .tokenize()
            .iter()
            .map(|t| t.get_spelling())
            .any(|s| s == "++" || s == "--");
    }

    if !matches!(
        entity.get_kind(),
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator
    ) {
        return false;
    }

    let Some(range) = entity.get_range() else {
        return false;
    };

    let tokens: Vec<String> = range
        .tokenize()
        .into_iter()
        .map(|t| t.get_spelling())
        .collect();

    matches!(
        self::extract_binary_operator_from_tokens(&tokens),
        Some("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=")
    )
}

fn build_clang_args_from_import_options(opts: &ImportCOptions) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();

    for inc in opts.include_paths() {
        args.push(format!("-I{}", inc.display()));
    }

    for inc in opts.system_include_paths() {
        args.push(format!("-isystem{}", inc.display()));
    }

    for def in opts.defines() {
        args.push(format!("-D{def}"));
    }

    for und in opts.undefs() {
        args.push(format!("-U{und}"));
    }

    if let Some(target) = opts.target() {
        args.push(format!("--target={target}"));
    }

    if let Some(sysroot) = opts.sysroot() {
        args.push(format!("--sysroot={}", sysroot.display()));
    }

    if let Some(std_) = opts.std() {
        args.push(format!("-std={std_}"));
    }

    args.extend(opts.args().iter().cloned());

    args
}

pub(crate) fn extract_binary_operator(
    entity: &clang::Entity<'_>,
    left: &clang::Entity<'_>,
    right: &clang::Entity<'_>,
) -> Option<&'static str> {
    let range: clang::source::SourceRange<'_> = entity.get_range()?;
    let left_range: clang::source::SourceRange<'_> = left.get_range()?;
    let right_range: clang::source::SourceRange<'_> = right.get_range()?;
    let source_tokens: Vec<String> = range
        .tokenize()
        .into_iter()
        .map(|t| t.get_spelling())
        .collect();
    let left_tokens: Vec<String> = left_range
        .tokenize()
        .into_iter()
        .map(|t| t.get_spelling())
        .collect();
    let right_tokens: Vec<String> = right_range
        .tokenize()
        .into_iter()
        .map(|t| t.get_spelling())
        .collect();

    let start: usize =
        self::find_subsequence_from(&source_tokens, &left_tokens, 0)? + left_tokens.len();
    let end: usize = self::find_subsequence_from(&source_tokens, &right_tokens, start)?;

    self::extract_binary_operator_from_tokens(&source_tokens[start..end])
}

fn detect_clang_resource_include_dir() -> Option<PathBuf> {
    for command in ["clang", "clang-17", "clang-18"] {
        let output = std::process::Command::new(command)
            .arg("-print-resource-dir")
            .output();

        let Ok(output) = output else {
            continue;
        };

        if !output.status.success() {
            continue;
        }

        let resource_dir: std::borrow::Cow<'_, str> = String::from_utf8_lossy(&output.stdout);
        let resource_dir: &str = resource_dir.trim();

        if resource_dir.is_empty() {
            continue;
        }

        let include_dir: PathBuf = PathBuf::from(resource_dir).join("include");

        if include_dir.is_dir() {
            return Some(include_dir);
        }
    }

    None
}

pub(crate) fn entity_originates_in_main_file(entity: &clang::Entity<'_>, main_file: &Path) -> bool {
    if entity.is_in_main_file() {
        return true;
    }

    let Some(location) = entity.get_location() else {
        return false;
    };

    location
        .get_expansion_location()
        .file
        .map(|file| {
            file.get_path()
                .canonicalize()
                .unwrap_or_else(|_| file.get_path())
                == main_file
        })
        .unwrap_or(false)
}

pub(crate) fn is_supported_expr_kind(kind: clang::EntityKind) -> bool {
    matches!(
        kind,
        clang::EntityKind::IntegerLiteral
            | clang::EntityKind::FloatingLiteral
            | clang::EntityKind::StringLiteral
            | clang::EntityKind::CharacterLiteral
            | clang::EntityKind::DeclRefExpr
            | clang::EntityKind::MemberRefExpr
            | clang::EntityKind::CallExpr
            | clang::EntityKind::ParenExpr
            | clang::EntityKind::UnaryOperator
            | clang::EntityKind::ArraySubscriptExpr
            | clang::EntityKind::BinaryOperator
            | clang::EntityKind::CompoundAssignOperator
            | clang::EntityKind::CStyleCastExpr
            | clang::EntityKind::ConditionalOperator
            | clang::EntityKind::UnexposedExpr
    )
}

fn escape_string_for_thrust_literal(s: &str) -> String {
    let mut out: String = String::with_capacity(s.len().saturating_add(8));

    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            '\'' => out.push_str("\\'"),
            other => out.push(other),
        }
    }

    out
}

pub(crate) fn tokens_to_thrust_source(tokens: &[clang::token::Token<'_>]) -> String {
    let mut out: String = String::new();
    let mut previous: Option<String> = None;

    for tk in tokens.iter() {
        let raw: String = tk.get_spelling();
        let s: String = match tk.get_kind() {
            clang::token::TokenKind::Identifier => self::sanitize_identifier_for_thrust(&raw),
            clang::token::TokenKind::Literal => self::normalize_literal_token_spelling(&raw),
            clang::token::TokenKind::Punctuation if raw == "." => "->".into(),
            _ => raw,
        };

        if let Some(prev) = previous.as_deref() {
            if self::needs_space_between_tokens(prev, &s) {
                out.push(' ');
            }
        }

        out.push_str(&s);
        previous = Some(s);
    }

    out
}

pub(crate) fn normalize_literal_token_spelling(spelling: &str) -> String {
    if spelling.starts_with('"') || spelling.starts_with('\'') {
        return spelling.to_string();
    }

    if spelling.starts_with("0x") || spelling.starts_with("0X") {
        return spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string();
    }

    if spelling.contains('.') || spelling.contains('e') || spelling.contains('E') {
        return spelling.trim_end_matches(['f', 'F', 'l', 'L']).to_string();
    }

    spelling.trim_end_matches(['u', 'U', 'l', 'L']).to_string()
}

pub(crate) fn sanitize_identifier_for_thrust(name: &str) -> String {
    match name {
        "array" | "asm" | "bool" | "break" | "char" | "const" | "continue"
        | "deref" | "directive" | "else" | "enum" | "false" | "fn" | "for"
        | "if" | "import" | "importC" | "load" | "loop" | "ptr" | "ref"
        | "return" | "struct" | "true" | "type" | "union" | "var" | "void"
        | "while" => format!("{name}_"),
        _ => name.to_string(),
    }
}

fn find_subsequence_from(haystack: &[String], needle: &[String], start: usize) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() || start >= haystack.len() {
        return None;
    }

    (start..=haystack.len() - needle.len())
        .find(|&idx| haystack[idx..idx + needle.len()] == *needle)
}
