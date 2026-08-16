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

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use thrustc_lexer::Lexer;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::Token;

use crate::module::Module;
use crate::parser::ModuleParser;
use crate::registry::{ModuleRegistry, SharedModuleRegistry};

#[derive(Debug)]
pub struct StdLibrary {
    registry: SharedModuleRegistry,
    imported: RefCell<Vec<Module>>,
}

thread_local! {
    static STD_LIBRARY: RefCell<Option<StdLibrary>> = const { RefCell::new(None) };
    static STD_BUILDING: RefCell<bool> = const { RefCell::new(false) };
}

pub fn find_std_module(access: &[String], options: &CompilerOptions) -> Result<Module, ()> {
    let building: bool = STD_BUILDING.with(|cell| *cell.borrow());

    if building {
        return Err(());
    }

    self::ensure_built(options)?;

    STD_LIBRARY.with(|cell| {
        let slot: std::cell::Ref<'_, Option<StdLibrary>> = cell.borrow();
        let library: &StdLibrary = slot.as_ref().ok_or(())?;

        let root: Rc<Module> = library.registry.borrow().find("std").ok_or(())?;

        let relative: &[String] = if access.first().map(String::as_str) == Some("std") {
            &access[1..]
        } else {
            access
        };

        let module: Module = if relative.is_empty() {
            (*root).clone()
        } else {
            root.find_submodule(relative.to_vec()).ok_or(())?.clone()
        };

        let module_path: std::path::PathBuf = module.get_path().to_path_buf();

        let mut imported: std::cell::RefMut<'_, Vec<Module>> = library.imported.borrow_mut();

        let modules_to_track: Vec<Module> = if module_path.is_file() {
            vec![module.clone()]
        } else {
            let mut leaves: Vec<Module> = Vec::new();
            self::collect_compileable(&module, &mut leaves);
            leaves
        };

        for tracked in modules_to_track {
            let already_imported: bool = imported
                .iter()
                .any(|item| item.get_path() == tracked.get_path());

            if !already_imported {
                imported.push(tracked);
            }
        }

        Ok(module)
    })
}

fn collect_compileable(module: &Module, out: &mut Vec<Module>) {
    if module.get_path().is_file() {
        out.push(module.clone());
    }

    for submodule in module.get_submodules() {
        self::collect_compileable(submodule, out);
    }
}

pub fn get_imported_std_modules() -> Vec<Module> {
    STD_LIBRARY.with(|cell| {
        cell.borrow()
            .as_ref()
            .map(|library| library.imported.borrow().clone())
            .unwrap_or_default()
    })
}

pub fn has_imported_std() -> bool {
    STD_LIBRARY.with(|cell| {
        cell.borrow()
            .as_ref()
            .is_some_and(|library| !library.imported.borrow().is_empty())
    })
}

fn ensure_built(options: &CompilerOptions) -> Result<(), ()> {
    STD_LIBRARY.with(|cell| {
        let mut slot: std::cell::RefMut<'_, Option<StdLibrary>> = cell.borrow_mut();

        if slot.is_some() {
            return Ok(());
        }

        let library: StdLibrary = self::build_library(options)?;

        *slot = Some(library);

        Ok(())
    })
}

fn build_library(options: &CompilerOptions) -> Result<StdLibrary, ()> {
    let root: std::path::PathBuf = thrustc_std::resolve_std_root(options.get_std_root_path());
    let version: String = thrustc_std::resolve_target_version(options.get_std_version());

    let version_dir: std::path::PathBuf =
        thrustc_std::ensure_std_present(&root, &version).map_err(|_| ())?;

    thrustc_std::validate_version(&root, &version).map_err(|_| ())?;

    let registry: SharedModuleRegistry = Rc::new(RefCell::new(ModuleRegistry::new()));

    STD_BUILDING.with(|cell| *cell.borrow_mut() = true);

    let mut std_module: Module = Module::new("std".to_string(), root.clone());

    let result: Result<(), ()> =
        self::parse_std_directory(&version_dir, options, &mut std_module, &registry);

    STD_BUILDING.with(|cell| *cell.borrow_mut() = false);

    result?;

    registry.borrow_mut().register(&std_module);

    Ok(StdLibrary {
        registry,
        imported: RefCell::new(Vec::new()),
    })
}

fn parse_std_directory(
    directory: &Path,
    options: &CompilerOptions,
    parent: &mut Module,
    registry: &SharedModuleRegistry,
) -> Result<(), ()> {
    let entries: std::fs::ReadDir = std::fs::read_dir(directory).map_err(|_| ())?;

    for entry in entries {
        let entry: std::fs::DirEntry = entry.map_err(|_| ())?;
        let path: std::path::PathBuf = entry.path();

        if path.is_dir() {
            let folder_name: String = path
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().to_string());

            let mut folder_module: Module = Module::new(folder_name, path.clone());

            self::parse_std_directory(&path, options, &mut folder_module, registry)?;

            parent.add_submodule(folder_module);
        } else if self::is_thrust_file(&path) {
            let module: Module = self::parse_std_module_file(&path, options, registry)?;

            parent.add_submodule(module);
        }
    }

    Ok(())
}

fn is_thrust_file(path: &Path) -> bool {
    path.extension()
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| {
            thrustc_constants::COMPILER_OWN_FILE_EXTENSIONS.contains(&extension)
        })
}

fn parse_std_module_file(
    path: &Path,
    options: &CompilerOptions,
    registry: &SharedModuleRegistry,
) -> Result<Module, ()> {
    let name: String = path
        .file_name()
        .map_or_else(String::new, |name| name.to_string_lossy().to_string());

    let base_name: String = path.file_stem().map_or_else(String::new, |base_name| {
        base_name.to_string_lossy().to_string()
    });

    let content: String = thrustc_reader::get_file_source_code(path);
    let file: CompilationUnit =
        CompilationUnit::new(name, path.to_path_buf(), content, base_name.clone());

    let tokens: Vec<Token> = Lexer::lex_for_preprocessor(&file, options).map_err(|_| ())?;

    let subparser: ModuleParser = ModuleParser::new(
        base_name,
        tokens,
        options,
        &file,
        Default::default(),
        registry.clone(),
    );

    subparser.parse()
}
