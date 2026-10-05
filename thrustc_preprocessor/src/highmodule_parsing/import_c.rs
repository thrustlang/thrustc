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
struct ImportRequest {
    import_str: String,
    span: Span,
    import_spec_path: PathBuf,
    header_path: PathBuf,
    treat_as_header_spec: bool,
    resolved_from_import_search: bool,
    parse_via_header_spec: bool,
    alias: Option<Vec<String>>,
    import_opts: thrustc_options::ImportCOptions,
}

impl ImportRequest {
    #[inline]
    #[allow(clippy::too_many_arguments)]
    fn new(
        import_str: String,
        span: Span,
        import_spec_path: PathBuf,
        header_path: PathBuf,
        treat_as_header_spec: bool,
        resolved_from_import_search: bool,
        parse_via_header_spec: bool,
        alias: Option<Vec<String>>,
        import_opts: thrustc_options::ImportCOptions,
    ) -> Self {
        Self {
            import_str,
            span,
            import_spec_path,
            header_path,
            treat_as_header_spec,
            resolved_from_import_search,
            parse_via_header_spec,
            alias,
            import_opts,
        }
    }
}

impl ImportRequest {
    #[inline]
    fn import_str(&self) -> &String {
        &self.import_str
    }

    #[inline]
    fn span(&self) -> Span {
        self.span
    }

    #[inline]
    fn import_spec_path(&self) -> &PathBuf {
        &self.import_spec_path
    }

    #[inline]
    fn header_path(&self) -> &PathBuf {
        &self.header_path
    }

    #[inline]
    fn treat_as_header_spec(&self) -> bool {
        self.treat_as_header_spec
    }

    #[inline]
    fn resolved_from_import_search(&self) -> bool {
        self.resolved_from_import_search
    }

    #[inline]
    fn parse_via_header_spec(&self) -> bool {
        self.parse_via_header_spec
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

fn emit_imported_symbols(
    module: &mut Module,
    context: &thrustc_c_import_synthesis::context::CImportContext,
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

fn parse_import_request(parser: &mut PreprocessorContext) -> Result<ImportRequest, ()> {
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

    let import_spec_path: PathBuf = PathBuf::from(&import_str);
    let mut header_path: PathBuf = import_spec_path.clone();
    let mut treat_as_header_spec: bool = false;
    let mut resolved_from_import_search: bool = false;
    let mut parse_via_header_spec: bool = false;

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
                parse_via_header_spec = true;
            } else {
                treat_as_header_spec = true;
            }
        }
    }

    if !treat_as_header_spec && let Ok(canonicalized) = header_path.canonicalize() {
        header_path = canonicalized;
    }

    let mut current_file_path: PathBuf = current_path.clone();

    if let Ok(canonicalized_current) = current_path.canonicalize() {
        current_file_path = canonicalized_current;
    }

    if !treat_as_header_spec && header_path == current_file_path {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The header cannot be imported itself.".into(),
            "You should remove it.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !treat_as_header_spec && !header_path.exists() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not exist.".into(),
            "Use an existing local header path or pass '--import-c-include <dir>' / '--import-c-system-include <dir>' so Clang can resolve the header spec.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !treat_as_header_spec && !header_path.is_file() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not point to a file.".into(),
            "You should make sure it is a valid path to file.".into(),
            None,
            span,
        ));

        return Err(());
    }

    Ok(ImportRequest::new(
        import_str,
        span,
        import_spec_path,
        header_path,
        treat_as_header_spec,
        resolved_from_import_search,
        parse_via_header_spec,
        alias,
        import_opts,
    ))
}

fn build_import_options(
    import_opts: &thrustc_options::ImportCOptions,
    treat_as_header_spec: bool,
    resolved_from_import_search: bool,
) -> thrustc_c_import_synthesis::options::CImportOptions {
    let mut options: thrustc_c_import_synthesis::options::CImportOptions =
        thrustc_c_import_synthesis::options::CImportOptions::new();

    let effective_scope: thrustc_options::ImportCScope = if import_opts.import_scope_overridden() {
        import_opts.import_scope()
    } else if treat_as_header_spec || resolved_from_import_search {
        thrustc_options::ImportCScope::TransitiveAll
    } else {
        thrustc_options::ImportCScope::MainOnly
    };

    let import_scope: thrustc_c_import_synthesis::options::CImportScope = match effective_scope {
        thrustc_options::ImportCScope::MainOnly => {
            thrustc_c_import_synthesis::options::CImportScope::MainOnly
        }
        thrustc_options::ImportCScope::TransitiveNoSystem => {
            thrustc_c_import_synthesis::options::CImportScope::TransitiveNoSystem
        }
        thrustc_options::ImportCScope::TransitiveAll => {
            thrustc_c_import_synthesis::options::CImportScope::TransitiveAll
        }
    };

    options.set_import_scope(import_scope);

    // Build clang arguments from `--import-c-*` flags.
    {
        let clang_args: &mut Vec<String> = options.clang_args_mut();

        if let Some(resource_dir) = self::detect_clang_resource_include_dir() {
            clang_args.push(format!("-isystem{}", resource_dir.display()));
        }

        for inc in import_opts.include_paths() {
            clang_args.push(format!("-I{}", inc.display()));
        }

        for inc in import_opts.system_include_paths() {
            clang_args.push(format!("-isystem{}", inc.display()));
        }

        let auto_system: Vec<PathBuf> = if import_opts.system_include_paths().is_empty() {
            self::detect_host_system_include_dirs()
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
    request: &ImportRequest,
    options: thrustc_c_import_synthesis::options::CImportOptions,
) -> Result<thrustc_c_import_synthesis::context::CImportContext, ()> {
    let importer_header_path: PathBuf = if request.parse_via_header_spec() {
        request.import_spec_path().clone()
    } else {
        request.header_path().clone()
    };

    let mut context: thrustc_c_import_synthesis::context::CImportContext =
        thrustc_c_import_synthesis::context::CImportContext::new(
            importer_header_path,
            request.span(),
            options,
        );

    if let Err(message) = context.import_header() {
        let treat_as_spec: bool = request.treat_as_header_spec();

        let import_str: &String = request.import_str();

        let header_display: String = if treat_as_spec {
            import_str.clone()
        } else {
            request.header_path().display().to_string()
        };

        let help: String = if treat_as_spec {
            "Pass '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers so importC can resolve the header.".into()
        } else {
            "Pass '--import-c-system-include <dir>' or '--import-c-include <dir>' so Clang can resolve the header and its dependencies.".into()
        };

        let has_diagnostics: bool = !context.diagnostics().is_empty();

        let looks_like_spec: bool =
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

        let hint: Option<String> = match (has_diagnostics, treat_as_spec, looks_like_spec) {
            (true, _, _) if treat_as_spec => Some(
                "Try '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers.".into(),
            ),
            (true, _, _) => None,
            (false, false, _) => Some(
                "System headers may be filtered by the configured import scope. Try '--import-c-scope=transitive-all' if you need them."
                    .into(),
            ),
            (false, true, true) => Some(
                "This looks like a header spec. Try '--import-c-system-include <dir>' for system headers or '--import-c-include <dir>' for local/vendor headers."
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
            request.span(),
        ));

        return Err(());
    }

    Ok(context)
}

fn verbose_include_dirs(command: &str) -> Vec<PathBuf> {
    let mut dirs: Vec<PathBuf> = Vec::new();

    let output: std::process::Output = match std::process::Command::new(command)
        .args(["-E", "-xc", "-v", "-"])
        .stdin(std::process::Stdio::null())
        .output()
    {
        Ok(output) => output,
        Err(_) => return dirs,
    };

    let stderr: std::borrow::Cow<'_, str> = String::from_utf8_lossy(&output.stderr);

    let mut tokens: Vec<String> = Vec::new();

    for token in stderr.split_whitespace() {
        tokens.push(token.to_string());
    }

    let mut idx: usize = 0;

    while idx < tokens.len() {
        if tokens[idx] == "-internal-isystem" || tokens[idx] == "-internal-externc-isystem" {
            if let Some(dir) = tokens.get(idx.saturating_add(1)) {
                dirs.push(PathBuf::from(dir));
            }

            idx = idx.saturating_add(2);
        } else {
            idx = idx.saturating_add(1);
        }
    }

    let mut in_list: bool = false;

    for line in stderr.lines() {
        let trimmed: &str = line.trim();

        if trimmed == "#include <...> search starts here:" {
            in_list = true;
        } else if trimmed == "End of search list." {
            in_list = false;
        } else if in_list && !trimmed.is_empty() {
            dirs.push(PathBuf::from(trimmed));
        }
    }

    dirs
}

fn detect_host_system_include_dirs() -> Vec<PathBuf> {
    let mut collected: Vec<PathBuf> = Vec::new();

    let mut push_unique = |candidate: PathBuf| {
        let canonical: PathBuf = candidate
            .canonicalize()
            .unwrap_or_else(|_| candidate.clone());

        let already: bool = collected.iter().any(|known: &PathBuf| {
            known.canonicalize().unwrap_or_else(|_| known.clone()) == canonical
        });

        if !already && canonical.is_dir() {
            collected.push(canonical);
        }
    };

    let from_verbose: Vec<PathBuf> = self::clang_commands()
        .into_iter()
        .map(|command| self::verbose_include_dirs(&command))
        .find(|dirs| !dirs.is_empty())
        .unwrap_or_default();

    from_verbose.into_iter().for_each(&mut push_unique);

    let env_iter = ["C_INCLUDE_PATH", "CPATH", "INCLUDE"]
        .iter()
        .filter_map(std::env::var_os)
        .flat_map(|paths| {
            std::env::split_paths(&paths)
                .filter(|entry| !entry.as_os_str().is_empty())
                .collect::<Vec<PathBuf>>()
        });

    env_iter.for_each(&mut push_unique);

    let fallback_dirs: Vec<PathBuf> = if cfg!(windows) {
        Vec::new()
    } else {
        ["/usr/local/include", "/usr/include"]
            .iter()
            .map(PathBuf::from)
            .collect()
    };

    fallback_dirs.into_iter().for_each(&mut push_unique);

    collected
}

fn emit_import_diagnostics(
    parser: &mut PreprocessorContext,
    context: &thrustc_c_import_synthesis::context::CImportContext,
    span: Span,
) {
    for diagnostic in context.diagnostics() {
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
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::UnsupportedCallingConvention => {
                CompilationIssueCode::E0100
            }
            thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedDeclaration => {
                CompilationIssueCode::W0104
            }
        };

        if diagnostic.kind()
            == thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::UnsupportedCallingConvention
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

pub fn parse_import_c<'preprocessor>(
    parser: &mut PreprocessorContext<'preprocessor>,
) -> Result<Option<Module>, ()> {
    let request: ImportRequest = self::parse_import_request(parser)?;

    let base_name: String = Path::new(if request.treat_as_header_spec() {
        request.import_spec_path()
    } else {
        request.header_path()
    })
    .file_stem()
    .map_or_else(String::new, |stem| stem.to_string_lossy().to_string());

    let mut module: Module = Module::new(base_name, request.header_path().clone());

    if let Some(alias) = request.alias().clone() {
        module.set_alias(alias);
    }

    let options: thrustc_c_import_synthesis::options::CImportOptions = self::build_import_options(
        request.import_opts(),
        request.treat_as_header_spec(),
        request.resolved_from_import_search(),
    );

    let context: thrustc_c_import_synthesis::context::CImportContext =
        self::synthesize_imported_header(parser, &request, options)?;

    self::emit_import_diagnostics(parser, &context, request.span());

    self::emit_imported_symbols(&mut module, &context, request.span());

    parser.get_registry().borrow_mut().register(&module);

    Ok(Some(module))
}

fn detect_clang_resource_include_dir() -> Option<PathBuf> {
    let commands: Vec<String> = self::clang_commands();

    let outputs = commands.iter().filter_map(|command| {
        std::process::Command::new(command)
            .arg("-print-resource-dir")
            .output()
            .ok()
    });

    let successful = outputs.filter(|output| output.status.success());

    let mut include_dirs = successful.filter_map(|output| {
        let resource_dir: std::borrow::Cow<'_, str> = String::from_utf8_lossy(&output.stdout);

        let trimmed: &str = resource_dir.trim();

        if trimmed.is_empty() {
            None
        } else {
            Some(PathBuf::from(trimmed).join("include"))
        }
    });

    include_dirs.find(|include_dir| include_dir.is_dir())
}

fn clang_commands() -> Vec<String> {
    let env_opt: Option<String> = std::env::var("CLANG")
        .ok()
        .filter(|value| !value.trim().is_empty());

    let mut commands: Vec<String> = env_opt.into_iter().collect();

    let fallback_iter = [
        "clang", "clang-16", "clang-17", "clang-18", "clang-19", "clang-20", "clang-21",
        "clang-22", "clang-23", "cc",
    ]
    .iter()
    .map(|name| name.to_string());

    commands.extend(fallback_iter);

    commands
}
