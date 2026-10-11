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

use thrustc_ast_modificators::Modificators;
use thrustc_attributes::ThrustAttribute;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use thrustc_typesystem::type_metadata::StructTypeMetadata;

use crate::{
    context::PreprocessorContext,
    module::Module,
    signatures::{Signature, Symbol, Variant},
};

#[derive(Debug)]
struct ImportC {
    import_str: String,
    import_path: PathBuf,
    header_path: PathBuf,
    outside_header: bool,
    resolved_from_import_search: bool,
    parse_outside_header: bool,
    alias: Option<Vec<String>>,
    import_opts: thrustc_options::ImportCOptions,
    span: Span,
}

impl ImportC {
    #[allow(clippy::too_many_arguments)]
    fn new(
        import_str: String,
        import_path: PathBuf,
        header_path: PathBuf,
        outside_header: bool,
        resolved_from_import_search: bool,
        parse_outside_header: bool,
        alias: Option<Vec<String>>,
        import_opts: thrustc_options::ImportCOptions,
        span: Span,
    ) -> Self {
        Self {
            import_str,
            import_path,
            header_path,
            outside_header,
            resolved_from_import_search,
            parse_outside_header,
            alias,
            import_opts,
            span,
        }
    }
}

impl ImportC {
    #[inline]
    fn import_str(&self) -> &String {
        &self.import_str
    }

    #[inline]
    fn span(&self) -> Span {
        self.span
    }

    #[inline]
    fn import_path(&self) -> &PathBuf {
        &self.import_path
    }

    #[inline]
    fn header_path(&self) -> &PathBuf {
        &self.header_path
    }

    #[inline]
    fn outside_header(&self) -> bool {
        self.outside_header
    }

    #[inline]
    fn resolved_from_import_search(&self) -> bool {
        self.resolved_from_import_search
    }

    #[inline]
    fn parse_outside_header(&self) -> bool {
        self.parse_outside_header
    }

    #[inline]
    fn alias(&self) -> &Option<Vec<String>> {
        &self.alias
    }

    #[inline]
    fn import_opts(&self) -> &thrustc_options::ImportCOptions {
        &self.import_opts
    }
}

pub fn parse_import_c<'preprocessor>(
    parser: &mut PreprocessorContext<'preprocessor>,
) -> Result<Option<Module>, ()> {
    let request: ImportC = self::parse_import_request(parser)?;

    let base_name: String = Path::new(if request.outside_header() {
        request.import_path()
    } else {
        request.header_path()
    })
    .file_stem()
    .map_or_else(String::new, |stem| stem.to_string_lossy().to_string());

    let mut module: Module = Module::new(base_name, request.header_path().clone());

    if let Some(alias) = request.alias().clone() {
        module.set_alias(alias);
    }

    let options: thrustc_import_c::options::CImportOptions = self::build_import_options(
        request.import_opts(),
        request.outside_header(),
        request.resolved_from_import_search(),
    );

    let context: thrustc_import_c::context::CImportContext =
        self::synthesize_imported_header(parser, &request, options)?;

    self::emit_import_diagnostics(parser, &context, request.span());

    self::emit_imported_symbols(&mut module, &context, request.span());

    parser.get_registry().borrow_mut().register(&module);

    Ok(Some(module))
}

fn emit_import_diagnostics(
    parser: &mut PreprocessorContext,
    context: &thrustc_import_c::context::CImportContext,
    span: Span,
) {
    for diagnostic in context.diagnostics() {
        let code: CompilationIssueCode = match diagnostic.kind() {
            thrustc_import_c::diagnostics::CImportDiagnosticKind::ClangDiagnostic => {
                CompilationIssueCode::W0100
            }
            thrustc_import_c::diagnostics::CImportDiagnosticKind::InventedFieldName => {
                CompilationIssueCode::W0101
            }
            thrustc_import_c::diagnostics::CImportDiagnosticKind::SkippedUnion => {
                CompilationIssueCode::W0102
            }
            thrustc_import_c::diagnostics::CImportDiagnosticKind::SkippedBitfieldStruct => {
                CompilationIssueCode::W0103
            }
            thrustc_import_c::diagnostics::CImportDiagnosticKind::UnsupportedCallingConvention => {
                CompilationIssueCode::E0100
            }
            thrustc_import_c::diagnostics::CImportDiagnosticKind::SkippedDeclaration => {
                CompilationIssueCode::W0104
            }
        };

        if diagnostic.kind()
            == thrustc_import_c::diagnostics::CImportDiagnosticKind::UnsupportedCallingConvention
        {
            parser.add_error(CompilationIssue::Error(
                code,
                diagnostic.message().to_string(),
                "Use a function with a calling convention that Thrust can import, or remove the importC dependency on that declaration.".into(),
                None,
                span,
            ));
        } else {
            parser.add_warning(CompilationIssue::Warning(
                code,
                diagnostic.message().to_string(),
                span,
            ));
        }
    }
}

fn emit_imported_symbols(
    module: &mut Module,
    context: &thrustc_import_c::context::CImportContext,
    span: Span,
) {
    for function in context.functions() {
        let name: String = function.name().to_string();

        let mut parameters: Vec<(String, Type, Span)> =
            Vec::with_capacity(function.parameter_types().len());

        for (param_name, param_type) in function
            .parameter_names()
            .iter()
            .cloned()
            .zip(function.parameter_types().iter().cloned())
        {
            parameters.push((param_name, param_type, span));
        }

        let mut attributes: thrustc_attributes::ThrustAttributes = Vec::with_capacity(4);

        attributes.push(ThrustAttribute::Convention(
            function.convention().to_string(),
            span,
        ));

        if function.variadic() {
            attributes.push(ThrustAttribute::Ignore(span));
            attributes.push(ThrustAttribute::NoArgCount(span));
        }

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::Function {
                kind: function.return_type().clone(),
                invalid_kind: Type::Void { span },
                demangling_name: function.external_name().to_string(),
                type_params: None,
                parameters,
                attributes,
                span,
            },
            variant: Variant::Function,
            public: true,
        };

        module.add_symbol(symbol);
    }

    for record in context.structs() {
        let name: String = record.name().to_string();

        let mut fields: Vec<(String, Type, Span)> = Vec::with_capacity(record.fields().len());
        let mut field_types: Vec<Type> = Vec::with_capacity(record.fields().len());

        for (field_name, field_type) in record.fields().iter() {
            fields.push((field_name.clone(), field_type.clone(), span));
            field_types.push(field_type.clone());
        }

        let metadata: StructTypeMetadata = *record.metadata();

        let kind: Type = Type::Struct {
            name: name.clone(),
            fields: field_types,
            metadata,
            span,
        };

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::Struct {
                kind,
                invalid_kind: Type::Void { span },
                type_params: None,
                fields,
                span,
            },
            variant: Variant::Struct,
            public: true,
        };

        module.add_symbol(symbol);
    }

    for imported_enum in context.enums() {
        let name: String = imported_enum.name().to_string();
        let underlying_type: Type = imported_enum.underlying_type().clone();

        let mut fields: Vec<(
            String,
            Type,
            Option<thrustc_compile_time::BuiltinValue>,
            Span,
        )> = Vec::with_capacity(imported_enum.fields().len());

        for (field_name, value) in imported_enum.fields().iter() {
            fields.push((
                field_name.clone(),
                underlying_type.clone(),
                Some(thrustc_compile_time::BuiltinValue::Integer(*value)),
                span,
            ));
        }

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::Enum {
                invalid_kind: Type::Void { span },
                fields,
                attributes: Vec::new(),
                span,
            },
            variant: Variant::Enum,
            public: true,
        };

        module.add_symbol(symbol);
    }

    for imported_type in context.typedefs() {
        let name: String = imported_type.name().to_string();
        let ty: Type = imported_type.ty().clone();

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::CustomType {
                kind: ty,
                invalid_kind: Type::Void { span },
                type_params: None,
                attributes: Vec::new(),
                span,
            },
            variant: Variant::CustomType,
            public: true,
        };

        module.add_symbol(symbol);
    }

    for imported_static in context.statics() {
        let name: String = imported_static.name().to_string();
        let kind: Type = imported_static.kind().clone();

        let attributes: thrustc_attributes::ThrustAttributes = vec![
            ThrustAttribute::Public(span),
            ThrustAttribute::Extern(imported_static.external_name().to_string(), span),
        ];

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::Static {
                kind,
                invalid_kind: Type::Void { span },
                is_mutable: imported_static.is_mutable(),
                attributes,
                modificators: Modificators::new(),
                span,
            },
            variant: Variant::Static,
            public: true,
        };

        module.add_symbol(symbol);
    }

    for constant in context.constants() {
        let name: String = constant.name().to_string();
        let kind: Type = constant.kind().clone();
        let value: thrustc_compile_time::BuiltinValue = constant.value().clone();

        let symbol: Symbol = Symbol {
            name,
            signature: Signature::Constant {
                kind,
                invalid_kind: Type::Void { span },
                value: Some(value),
                attributes: Vec::new(),
                modificators: Modificators::new(),
                span,
            },
            variant: Variant::Constant,
            public: true,
        };

        module.add_symbol(symbol);
    }
}

fn parse_import_request(parser: &mut PreprocessorContext) -> Result<ImportC, ()> {
    parser.consume(TokenType::ImportC)?;

    let current_path: PathBuf = parser.get_compilation_unit().get_path().to_path_buf();

    let current_dir: PathBuf = current_path
        .parent()
        .map_or_else(|| PathBuf::from("."), |p| p.to_path_buf());

    let header_path_tk: &Token =
        parser.consume_these(&[TokenType::CString, TokenType::CNString])?;

    let import_str: String = header_path_tk
        .get_lexeme()
        .trim()
        .trim_matches('"')
        .to_string();

    let span: Span = header_path_tk.get_span();

    let import_path: PathBuf = PathBuf::from(&import_str);
    let mut header_path: PathBuf = import_path.clone();
    let mut outside_header: bool = false;
    let mut resolved_from_import_search: bool = false;
    let mut parse_outside_header: bool = false;

    if parser.check(TokenType::Only) {
        let only_span: Span = parser.peek().get_span();

        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The 'only' list is not supported by importC.".into(),
            "Remove the 'only { ... }' clause.".into(),
            None,
            only_span,
        ));

        return Err(());
    }

    let mut alias: Option<Vec<String>> = None;

    if parser.check(TokenType::As) {
        parser.consume(TokenType::As)?;

        let alias_tk: &Token = parser.consume(TokenType::Identifier)?;
        let mut alias_parts: Vec<String> = vec![alias_tk.get_lexeme().to_string()];

        while parser.check(TokenType::ColonColon) {
            parser.only_advance()?;

            let part_tk: &Token = parser.consume(TokenType::Identifier)?;

            alias_parts.push(part_tk.get_lexeme().to_string());
        }

        alias = Some(alias_parts);
    }

    parser.consume(TokenType::SemiColon)?;

    let file_options: &thrustc_directive::FileOptions<'_, '_> = parser.get_file_options();
    let import_opts: thrustc_options::ImportCOptions =
        thrustc_directive::combine_import_c_options(file_options);

    if header_path.is_relative() {
        let local_candidate: PathBuf = current_dir.join(&import_str);

        if local_candidate.exists() {
            header_path = local_candidate;
        } else {
            let search_dirs = import_opts
                .include_paths()
                .iter()
                .chain(import_opts.system_include_paths().iter());

            let found: Option<PathBuf> = search_dirs
                .map(|include_dir| include_dir.join(&import_str))
                .find(|candidate| candidate.exists());

            if let Some(candidate) = found {
                header_path = candidate;
                resolved_from_import_search = true;
                parse_outside_header = true;
            } else {
                outside_header = true;
            }
        }
    }

    if !outside_header && let Ok(canonicalized) = header_path.canonicalize() {
        header_path = canonicalized;
    }

    let mut current_file_path: PathBuf = current_path.clone();

    if let Ok(canonicalized_current) = current_path.canonicalize() {
        current_file_path = canonicalized_current;
    }

    if !outside_header && header_path == current_file_path {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The header cannot be imported itself.".into(),
            "You should remove it.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !outside_header && !header_path.exists() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not exist.".into(),
            "Use an existing local header path or pass '--import-c-include <dir>' / '--import-c-system-include <dir>' so Clang can resolve the outside header.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !outside_header && !header_path.is_file() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not point to a file.".into(),
            "You should make sure it is a valid path to file.".into(),
            None,
            span,
        ));

        return Err(());
    }

    Ok(ImportC::new(
        import_str,
        import_path,
        header_path,
        outside_header,
        resolved_from_import_search,
        parse_outside_header,
        alias,
        import_opts,
        span,
    ))
}

fn build_import_options(
    import_opts: &thrustc_options::ImportCOptions,
    outside_header: bool,
    resolved_from_import_search: bool,
) -> thrustc_import_c::options::CImportOptions {
    let mut options: thrustc_import_c::options::CImportOptions =
        thrustc_import_c::options::CImportOptions::new();

    let effective_scope: thrustc_options::ImportCScope = if import_opts.import_scope_overridden() {
        import_opts.import_scope()
    } else if outside_header || resolved_from_import_search {
        thrustc_options::ImportCScope::TransitiveAll
    } else {
        thrustc_options::ImportCScope::MainOnly
    };

    let import_scope: thrustc_import_c::options::CImportScope = match effective_scope {
        thrustc_options::ImportCScope::MainOnly => {
            thrustc_import_c::options::CImportScope::MainOnly
        }
        thrustc_options::ImportCScope::TransitiveNoSystem => {
            thrustc_import_c::options::CImportScope::TransitiveNoSystem
        }
        thrustc_options::ImportCScope::TransitiveAll => {
            thrustc_import_c::options::CImportScope::TransitiveAll
        }
    };

    options.set_import_scope(import_scope);

    // Build clang arguments from `--import-c-*` flags.
    {
        let clang_args: &mut Vec<String> = options.clang_args_mut();

        if let Some(resource_dir) =
            crate::highmodule_parsing::util::detect_clang_resource_include_dir()
        {
            clang_args.push(format!("-isystem{}", resource_dir.display()));
        }

        for inc in import_opts.include_paths() {
            clang_args.push(format!("-I{}", inc.display()));
        }

        for inc in import_opts.system_include_paths() {
            clang_args.push(format!("-isystem{}", inc.display()));
        }

        let auto_system: Vec<PathBuf> = if import_opts.system_include_paths().is_empty() {
            crate::highmodule_parsing::util::detect_host_system_include_dirs()
        } else {
            Vec::new()
        };

        let auto_iter = auto_system
            .iter()
            .map(|dir| format!("-isystem{}", dir.display()))
            .filter(|flag| !clang_args.contains(flag));

        let auto_flags: Vec<String> = auto_iter.collect();

        clang_args.extend(auto_flags);

        for def in import_opts.defines() {
            clang_args.push(format!("-D{def}"));
        }

        for und in import_opts.undefs() {
            clang_args.push(format!("-U{und}"));
        }

        if let Some(target) = import_opts.target() {
            clang_args.push(format!("--target={target}"));
        }

        if let Some(sysroot) = import_opts.sysroot() {
            clang_args.push(format!("--sysroot={}", sysroot.display()));
        }

        if let Some(std_) = import_opts.std() {
            clang_args.push(format!("-std={std_}"));
        }

        clang_args.extend(import_opts.args().iter().cloned());
    }

    options
}

fn synthesize_imported_header(
    parser: &mut PreprocessorContext,
    import_c: &ImportC,
    options: thrustc_import_c::options::CImportOptions,
) -> Result<thrustc_import_c::context::CImportContext, ()> {
    let importer_header_path: PathBuf = if import_c.parse_outside_header() {
        import_c.import_path().clone()
    } else {
        import_c.header_path().clone()
    };

    let mut context: thrustc_import_c::context::CImportContext =
        thrustc_import_c::context::CImportContext::new(
            importer_header_path,
            import_c.span(),
            options,
        );

    if let Err(message) = context.import_header() {
        let outside_header: bool = import_c.outside_header();

        let import_str: &String = import_c.import_str();

        let header_display: String = if outside_header {
            import_str.clone()
        } else {
            import_c.header_path().display().to_string()
        };

        let help: String = if outside_header {
            "Pass '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers so importC can resolve the header.".into()
        } else {
            "Pass '--import-c-system-include <dir>' or '--import-c-include <dir>' so Clang can resolve the header and its dependencies.".into()
        };

        let has_diagnostics: bool = !context.diagnostics().is_empty();

        let looks_like_outside_header: bool =
            import_str.ends_with(".h") && !import_str.contains(std::path::MAIN_SEPARATOR);

        let diagnostic_count: usize = context.diagnostics().len();

        let joined: String = context
            .diagnostics()
            .iter()
            .map(|diagnostic| diagnostic.message().to_string())
            .collect::<Vec<String>>()
            .join("\n");

        let message_text: String = if joined.is_empty() {
            format!("Failed to import C header '{header_display}'.")
        } else {
            format!("Failed to import C header '{header_display}':\n{joined}")
        };

        let hint: Option<String> = match (has_diagnostics, outside_header, looks_like_outside_header) {
            (true, _, _) if outside_header => Some(
                "Try '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers.".into(),
            ),
            (true, _, _) => None,
            (false, false, _) => Some(
                "System headers may be filtered by the configured import scope. Try '--import-c-scope=transitive-all' if you need them."
                    .into(),
            ),
            (false, true, true) => Some(
                "This looks like an outside header. Try '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers."
                    .into(),
            ),
            (false, true, false) => None,
        };

        let extra: String = format!("{diagnostic_count} clang diagnostic(s) reported.");

        let note: Option<String> = match hint {
            Some(hint) => Some(format!("{extra} {hint}")),
            None if diagnostic_count > 0 => Some(extra),
            None => Some(message),
        };

        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0100,
            message_text,
            help,
            note,
            import_c.span(),
        ));

        return Err(());
    }

    Ok(context)
}
