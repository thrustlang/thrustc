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

use std::path::PathBuf;

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_lexer::Lexer;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;

use crate::{module::Module, parser::ModuleParser, std_library};

pub fn parse_import<'module_parser>(parser: &mut ModuleParser<'module_parser>) -> Result<(), ()> {
    parser.consume(TokenType::Import)?;

    if parser.check(TokenType::Identifier) {
        return self::parse_std_import(parser);
    }

    let current_path: PathBuf = parser.get_module().get_path().to_path_buf();

    let current_dir: PathBuf = current_path
        .parent()
        .map_or_else(|| PathBuf::from("."), |p| p.to_path_buf());

    let module_path_tk: &Token =
        parser.consume_these(&[TokenType::CString, TokenType::CNString])?;

    let import_str: &str = module_path_tk.get_lexeme();
    let span: Span = module_path_tk.get_span();

    let mut module_path: PathBuf = PathBuf::from(import_str);

    if let Ok(canonicalized) = module_path.canonicalize() {
        module_path = canonicalized;
    }

    if module_path.is_relative() {
        module_path = current_dir.join(import_str);
    }

    let mut only: Option<Vec<String>> = None;

    if parser.check(TokenType::Only) {
        parser.consume(TokenType::Only)?;

        only = Some(self::parse_only_list(parser)?);
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

    if module_path == current_file_path {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The module cannot be imported itself.".into(),
            "You should remove it.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if parser.has_visited(&module_path) {
        parser.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0018,
            "A circular import was founded here. Omitting it by default. The recomendation is to remove it."
                .into(),
            span,
        ));

        return Ok(());
    }

    if !module_path.exists() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not exist.".into(),
            "You should make sure it is a valid path.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !module_path.is_file() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The path does not point to a file.".into(),
            "You should make sure it is a valid path to file.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if module_path.file_stem().is_none() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "An name was expected in the path.".into(),
            "You should check that it points to a file with a valid the name.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if module_path.extension().is_none() {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "An extension was expected in the path.".into(),
            "You should check that it points to a file with a valid the extension.".into(),
            None,
            span,
        ));

        return Err(());
    }

    if !module_path.extension().is_some_and(|ext| {
        thrustc_constants::COMPILER_OWN_FILE_EXTENSIONS.contains(&ext.to_str().unwrap_or("unknown"))
    }) {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "It has an invalid extension.".into(),
            "You should make sure they are valid thrust files.".into(),
            None,
            span,
        ));

        return Err(());
    }

    let name: String = match module_path.file_name() {
        Some(name) => name.to_string_lossy().to_string(),
        None => return Err(()),
    };

    let base_name: String = match module_path.file_stem() {
        Some(base_name) => base_name.to_string_lossy().to_string(),
        None => return Err(()),
    };

    let options: &CompilerOptions = parser.get_options();

    let content: String = thrustc_reader::get_file_source_code(&module_path);

    parser.mark_visited(module_path.clone());

    let file: CompilationUnit =
        CompilationUnit::new(name, module_path.clone(), content, base_name.clone());

    let tokens: Vec<Token> = Lexer::lex_for_preprocessor(&file, options)?;

    let subparser: ModuleParser = ModuleParser::new(
        base_name,
        tokens,
        options,
        &file,
        parser.get_global_visited_modules(),
        parser.get_registry(),
    );

    let mut submodule: Module = subparser.parse()?;

    parser.unmark_visited(&module_path);

    if let Some(alias) = alias {
        submodule.set_alias(alias);
    }

    if let Some(only) = only {
        submodule.set_only(only);
    }

    parser.get_registry().borrow_mut().register(&submodule);

    parser.get_mut_module().merge_submodule(submodule);

    Ok(())
}

fn parse_std_import<'module_parser>(parser: &mut ModuleParser<'module_parser>) -> Result<(), ()> {
    let mut access: Vec<String> = Vec::with_capacity(4);

    let first_tk: &Token = parser.consume(TokenType::Identifier)?;
    let first_span: Span = first_tk.get_span();

    access.push(first_tk.get_lexeme().to_string());

    let mut last_span: Span = first_span;

    while parser.check(TokenType::ColonColon) {
        parser.only_advance()?;

        let part_tk: &Token = parser.consume(TokenType::Identifier)?;
        last_span = part_tk.get_span();
        access.push(part_tk.get_lexeme().to_string());
    }

    let mut only: Option<Vec<String>> = None;

    if parser.check(TokenType::Only) {
        parser.consume(TokenType::Only)?;

        only = Some(self::parse_only_list(parser)?);
    }

    let mut alias: Option<Vec<String>> = None;

    if parser.check(TokenType::As) {
        parser.consume(TokenType::As)?;

        let alias_tk: &Token = parser.consume(TokenType::Identifier)?;
        alias = Some(vec![alias_tk.get_lexeme().to_string()]);
    }

    parser.consume(TokenType::SemiColon)?;

    if access.first().map(String::as_str) != Some("std") {
        parser.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0035,
            "The identifier import is only supported for the standard library.".into(),
            "You should import standard library modules as 'std.mem' or use a path string otherwise."
                .into(),
            None,
            first_span,
        ));

        return Err(());
    }

    match std_library::find_std_module(&access, parser.get_options()) {
        Ok(mut module) => {
            if let Some(alias) = alias {
                module.set_alias(alias);
            }

            if let Some(only) = only {
                module.set_only(only);
            }

            parser.get_registry().borrow_mut().register(&module);

            parser.get_mut_module().merge_submodule(module);

            Ok(())
        }
        Err(()) => {
            parser.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0035,
                format!(
                    "The standard library module '{}' could not be resolved.",
                    access.join("::")
                ),
                "You should make sure the module exists in the current standard library version."
                    .into(),
                None,
                last_span,
            ));

            Err(())
        }
    }
}

fn parse_only_list<'module_parser>(
    parser: &mut ModuleParser<'module_parser>,
) -> Result<Vec<String>, ()> {
    parser.consume(TokenType::LBrace)?;

    let mut names: Vec<String> = Vec::with_capacity(u8::MAX as usize);

    while !parser.check(TokenType::RBrace) {
        let name_tk: &Token = parser.consume(TokenType::Identifier)?;
        names.push(name_tk.get_lexeme().to_string());

        if parser.check(TokenType::Comma) {
            parser.only_advance()?;
        }
    }

    parser.consume(TokenType::RBrace)?;

    Ok(names)
}
