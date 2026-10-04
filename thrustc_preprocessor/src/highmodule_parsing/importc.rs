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

use thrustc_attributes::ThrustAttribute;
use thrustc_ast_modificators::Modificators;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use thrustc_typesystem::type_metadata::StructTypeMetadata;
use thrustc_typesystem::type_modificators::{GCCStructureTypeModificator, LLVMStructureTypeModificator, StructureTypeModificator};

use crate::{context::PreprocessorContext, module::Module, signatures::{Signature, Symbol, Variant}};

pub fn parse_import_c<'preprocessor>(
    parser: &mut PreprocessorContext<'preprocessor>,
) -> Result<Option<Module>, ()> {
    parser.consume(TokenType::ImportC)?;

    let current_path: PathBuf = parser.get_compilation_unit().get_path().to_path_buf();

    let current_dir: PathBuf = current_path
        .parent()
        .map_or_else(|| PathBuf::from("."), |p| p.to_path_buf());

    let header_path_tk: &Token =
        parser.consume_these(&[TokenType::CString, TokenType::CNString])?;

    let import_str: &str = header_path_tk.get_lexeme();
    let span: Span = header_path_tk.get_span();

    let mut header_path: PathBuf = PathBuf::from(import_str);

    if header_path.is_relative() {
        header_path = current_dir.join(import_str);
    }

    if let Ok(canonicalized) = header_path.canonicalize() {
        header_path = canonicalized;
    }

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

    let mut current_file_path: PathBuf = current_path.clone();

    if let Ok(canonicalized_current) = current_path.canonicalize() {
        current_file_path = canonicalized_current;
    }

    if header_path == current_file_path {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The header cannot be imported itself.".into(),
            "You should remove it.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !header_path.exists() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not exist.".into(),
            "You should make sure it is a valid path.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !header_path.is_file() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not point to a file.".into(),
            "You should make sure it is a valid path to file.".into(),
            None,
            span,
        ));

        return Err(());
    }

    let base_name: String = Path::new(&header_path)
        .file_stem()
        .map_or_else(String::new, |stem| stem.to_string_lossy().to_string());

    let mut module: Module = Module::new(base_name, header_path.clone());

    if let Some(alias) = alias {
        module.set_alias(alias);
    }

    // C header parsing and symbol synthesis.
    {
        let file_options: &thrustc_directive::FileOptions<'_, '_> = parser.get_file_options();
        let import_opts: thrustc_options::ImportCOptions =
            thrustc_directive::combine_import_c_options(file_options);

        let mut options: thrustc_c_import_synthesis::options::CImportOptions =
            thrustc_c_import_synthesis::options::CImportOptions::new();

            // Build clang arguments from `--import-c-*` flags.
            {
                let clang_args: &mut Vec<String> = options.clang_args_mut();

                for inc in import_opts.include_paths() {
                    clang_args.push(format!("-I{}", inc.display()));
                }

                for inc in import_opts.system_include_paths() {
                    clang_args.push(format!("-isystem{}", inc.display()));
                }

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

        let mut context: thrustc_c_import_synthesis::context::CImportContext =
            thrustc_c_import_synthesis::context::CImportContext::new(header_path, span, options);

        if let Err(message) = context.import_header() {
            parser.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0100,
                "Failed to import C header.".into(),
                message,
                None,
                span,
            ));

            return Err(());
        }

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
                thrustc_c_import_synthesis::diagnostics::CImportDiagnosticKind::SkippedDeclaration => {
                    CompilationIssueCode::W0104
                }
            };

            parser.add_warning(CompilationIssue::Warning(
                code,
                diagnostic.message().to_string(),
                span,
            ));
        }

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

            let mut attributes: thrustc_attributes::ThrustAttributes =
                Vec::with_capacity(4);

            attributes.push(ThrustAttribute::Convention("C".into(), span));

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

            let llvm_mod = LLVMStructureTypeModificator::new(false);
            let gcc_mod = GCCStructureTypeModificator::new();
            let modificator: StructureTypeModificator = StructureTypeModificator::new(llvm_mod, gcc_mod);
            let metadata: StructTypeMetadata = StructTypeMetadata::new(modificator);

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

            let mut fields: Vec<(String, Type, Option<thrustc_compile_time::BuiltinValue>, Span)> =
                Vec::with_capacity(imported_enum.fields().len());

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

    parser.get_registry().borrow_mut().register(&module);

    Ok(Some(module))
}
