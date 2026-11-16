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
use std::path::{Path, PathBuf};
use std::rc::Rc;

use ahash::AHashMap as HashMap;
use ahash::AHashSet as HashSet;

use thrustc_backends::llvm::LLVMBackend;
use thrustc_backends::llvm::target::LLVMTarget;
use thrustc_builtins::BuiltinRegistry;
use thrustc_builtins::default_registry;
use thrustc_diagnostician::Diagnostician;
use thrustc_lexer::Lexer;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_token::Token;
use thrustc_typesystem::type_layout::TargetInfo;

use crate::module::Module;
use crate::parser::ModuleParser;
use crate::registry::{ModuleRegistry, SharedModuleRegistry};

// SIMD std modules use NativeVector, native[...] and @simd, so they need unstable mode.
const UNSTABLE_STD_MODULES: &[&[&str]] = &[&["simd"], &["intrinsics", "x86_64", "simd"]];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StdModuleError {
    NotFound,
    Unstable,
}

#[derive(Debug)]
pub struct StdLibrary {
    registry: SharedModuleRegistry,
    imported: RefCell<Vec<Module>>,
}

#[derive(Debug)]
struct BuildingStd {
    root: RefCell<Module>,
    registry: SharedModuleRegistry,
    builtins: BuiltinRegistry,
    version_dir: PathBuf,
    parsed: RefCell<HashMap<PathBuf, Module>>,
    parsing: RefCell<HashSet<PathBuf>>,
}

thread_local! {
    static STD_LIBRARY: RefCell<Option<StdLibrary>> = const { RefCell::new(None) };
    static STD_BUILDING_STATE: RefCell<Option<Rc<BuildingStd>>> = const { RefCell::new(None) };
}

pub fn find_std_module(
    access: &[String],
    options: &CompilerOptions,
) -> Result<Module, StdModuleError> {
    let relative: &[String] = if access.first().map(String::as_str) == Some("std") {
        &access[1..]
    } else {
        access
    };

    if options.get_compiler_features().is_stable_mode() && self::is_unstable_std_module(relative) {
        return Err(StdModuleError::Unstable);
    }

    let building_state: Option<Rc<BuildingStd>> =
        STD_BUILDING_STATE.with(|cell| cell.borrow().as_ref().cloned());

    if let Some(state) = building_state {
        return self::find_std_module_building(&state, relative, options);
    }

    self::ensure_built(options).map_err(|_| StdModuleError::NotFound)?;

    STD_LIBRARY.with(|cell| {
        let slot: std::cell::Ref<'_, Option<StdLibrary>> = cell.borrow();
        let library: &StdLibrary = slot.as_ref().ok_or(StdModuleError::NotFound)?;

        let root: Rc<Module> = library
            .registry
            .borrow()
            .find("std")
            .ok_or(StdModuleError::NotFound)?;

        let module: Module = if relative.is_empty() {
            (*root).clone()
        } else {
            root.find_submodule(relative.to_vec())
                .ok_or(StdModuleError::NotFound)?
                .clone()
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

fn find_std_module_building(
    state: &Rc<BuildingStd>,
    relative: &[String],
    options: &CompilerOptions,
) -> Result<Module, StdModuleError> {
    if relative.is_empty() {
        return Ok(state.root.borrow().clone());
    }

    if options.get_compiler_features().is_stable_mode() && self::is_unstable_std_module(relative) {
        return Err(StdModuleError::Unstable);
    }

    {
        let root: std::cell::Ref<'_, Module> = state.root.borrow();

        if let Some(module) = root.find_submodule(relative.to_vec()) {
            return Ok(module.clone());
        }
    }

    let file_path: PathBuf =
        self::resolve_std_path(state, relative).map_err(|_| StdModuleError::NotFound)?;
    let module: Module =
        self::ensure_parsed(state, &file_path, options).map_err(|_| StdModuleError::NotFound)?;
    self::attach_into_root(state, &file_path, module.clone())
        .map_err(|_| StdModuleError::NotFound)?;

    Ok(module)
}

fn resolve_std_path(state: &Rc<BuildingStd>, relative: &[String]) -> Result<PathBuf, ()> {
    if relative.is_empty() {
        return Err(());
    }

    let mut path: PathBuf = state.version_dir.clone();

    for (index, part) in relative.iter().enumerate() {
        let is_last: bool = index + 1 == relative.len();

        if !is_last {
            path.push(part);
            continue;
        }

        for extension in thrustc_constants::COMPILER_OWN_FILE_EXTENSIONS {
            let candidate: PathBuf = path.join(format!("{part}.{extension}"));

            if candidate.is_file() {
                return Ok(candidate);
            }
        }

        return Err(());
    }

    Err(())
}

fn ensure_parsed(
    state: &Rc<BuildingStd>,
    file_path: &Path,
    options: &CompilerOptions,
) -> Result<Module, ()> {
    if let Some(module) = state.parsed.borrow().get(file_path) {
        return Ok(module.clone());
    }

    if !state.parsing.borrow_mut().insert(file_path.to_path_buf()) {
        return Err(());
    }

    let result: Result<Module, ()> =
        self::parse_std_module_file(file_path, options, &state.registry, &state.builtins);

    state.parsing.borrow_mut().remove(file_path);

    let module: Module = result?;

    state
        .parsed
        .borrow_mut()
        .insert(file_path.to_path_buf(), module.clone());

    state.registry.borrow_mut().register(&module);

    Ok(module)
}

fn attach_into_root(state: &Rc<BuildingStd>, file_path: &Path, module: Module) -> Result<(), ()> {
    let relative: &Path = file_path
        .strip_prefix(state.version_dir.as_path())
        .map_err(|_| ())?;

    let components: Vec<String> = relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect();

    let folder_parts: usize = components.len().saturating_sub(1);

    let mut root: std::cell::RefMut<'_, Module> = state.root.borrow_mut();
    let mut current: &mut Module = &mut root;

    let mut current_path: PathBuf = state.version_dir.clone();

    for (index, part) in components.iter().enumerate() {
        if index >= folder_parts {
            break;
        }

        current_path.push(part);

        if current.get_submodule_mut(part).is_none() {
            current.add_submodule(Module::new(part.clone(), current_path.clone()));
        }

        current = current.get_submodule_mut(part).ok_or(())?;
    }

    current.merge_submodule(module);

    Ok(())
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
    let root: PathBuf = thrustc_std::resolve_std_root(options.get_std_root_path());
    let version: String = thrustc_std::resolve_target_version(options.get_std_version());

    let version_dir: PathBuf = thrustc_std::ensure_std_present(&root, &version).map_err(|_| ())?;

    thrustc_std::validate_version(&root, &version).map_err(|_| ())?;

    let registry: SharedModuleRegistry = Rc::new(RefCell::new(ModuleRegistry::new()));

    let llvm_backend: &LLVMBackend = options.get_llvm_backend();
    let target: &LLVMTarget = llvm_backend.get_target();
    let target_triple: &LLVMTargetTriple = target.get_normalized_target_triple();

    let target_info: TargetInfo = TargetInfo::new(target_triple.clone());

    let builtins: BuiltinRegistry = default_registry(target_info);

    let state: Rc<BuildingStd> = Rc::new(BuildingStd {
        root: RefCell::new(Module::new("std".to_string(), root.clone())),
        registry: registry.clone(),
        builtins,
        version_dir: version_dir.clone(),
        parsed: RefCell::new(HashMap::with_capacity(u8::MAX as usize)),
        parsing: RefCell::new(HashSet::with_capacity(u8::MAX as usize)),
    });

    STD_BUILDING_STATE.with(|cell| *cell.borrow_mut() = Some(state.clone()));

    let result: Result<(), ()> = {
        let version_dir: PathBuf = state.version_dir.clone();

        self::walk_and_attach(&state, &version_dir, options)
    };

    STD_BUILDING_STATE.with(|cell| *cell.borrow_mut() = None);

    result?;

    let root_borrow: std::cell::Ref<'_, Module> = state.root.borrow();
    registry.borrow_mut().register(&root_borrow);

    Ok(StdLibrary {
        registry,
        imported: RefCell::new(Vec::new()),
    })
}

fn parse_std_module_file(
    path: &Path,
    options: &CompilerOptions,
    registry: &SharedModuleRegistry,
    builtins: &BuiltinRegistry,
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
    let directives: thrustc_directive::FileDirectives =
        thrustc_directive::apply_file_directives(&tokens, options.get_compiler_features())
            .map_err(|error| {
                let mut diagnostician: Diagnostician = Diagnostician::new(&file, options);
                diagnostician.dispatch_diagnostic(&error, thrustc_logging::LoggingType::Error);
            })?;
    let file_options: thrustc_directive::FileOptions<'_, '_> =
        thrustc_directive::FileOptions::new(options, &directives);

    let subparser: ModuleParser = ModuleParser::new(
        base_name,
        tokens,
        options,
        &file_options,
        &file,
        Default::default(),
        registry.clone(),
        builtins,
    );

    subparser.parse()
}

fn walk_and_attach(
    state: &Rc<BuildingStd>,
    directory: &Path,
    options: &CompilerOptions,
) -> Result<(), ()> {
    let entries: std::fs::ReadDir = std::fs::read_dir(directory).map_err(|_| ())?;

    for entry in entries {
        let entry: std::fs::DirEntry = entry.map_err(|_| ())?;
        let path: PathBuf = entry.path();

        if options.get_compiler_features().is_stable_mode()
            && self::is_unstable_std_path(&state.version_dir, &path)
        {
            continue;
        }

        if path.is_dir() {
            self::walk_and_attach(state, &path, options)?;
        } else if self::is_thrust_file(&path) {
            let module: Module = self::ensure_parsed(state, &path, options)?;
            self::attach_into_root(state, &path, module)?;
        }
    }

    Ok(())
}

fn is_unstable_std_path(version_dir: &Path, path: &Path) -> bool {
    let Ok(relative_path): Result<&Path, std::path::StripPrefixError> =
        path.strip_prefix(version_dir)
    else {
        return false;
    };

    let mut components: Vec<String> = relative_path
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect();

    if path.is_file() {
        if let Some(last) = components.last_mut() {
            let stem: String = Path::new(last.as_str())
                .file_stem()
                .map_or_else(|| last.clone(), |stem| stem.to_string_lossy().into_owned());

            *last = stem;
        }
    }

    self::is_unstable_std_module(&components)
}

fn is_unstable_std_module(relative: &[String]) -> bool {
    UNSTABLE_STD_MODULES.iter().any(|prefix| {
        relative.len() >= prefix.len()
            && relative
                .iter()
                .zip(prefix.iter())
                .all(|(part, expected)| part == expected)
    })
}

fn is_thrust_file(path: &Path) -> bool {
    path.extension()
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| {
            thrustc_constants::COMPILER_OWN_FILE_EXTENSIONS.contains(&extension)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::signatures::Variant;
    use thrustc_options::CompilerOptions;

    fn with_temp_std(files: &[(&str, &str)], run: impl FnOnce(&CompilerOptions, &std::path::Path)) {
        let root: PathBuf = std::env::temp_dir().join(format!(
            "thrustc_std_reexport_test_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|duration| duration.subsec_nanos())
                .unwrap_or_default()
        ));

        let version_dir: PathBuf = root.join("vtest");

        std::fs::create_dir_all(&version_dir).unwrap();
        std::fs::write(root.join("VERSION.txt"), "test\n").unwrap();

        for (name, content) in files {
            std::fs::write(version_dir.join(name), content).unwrap();
        }

        let mut options: CompilerOptions = CompilerOptions::new();

        options.set_std_root_path(root.clone());
        options.set_std_version("test".to_string());

        run(&options, &root);

        std::fs::remove_dir_all(&root).ok();
    }

    // spec/types/primitives.md:87 marks NativeVector unstable, and SIMD std modules also use @simd. https://github.com/thrustlang/thrustc/blob/master/spec/types/primitives.md#L87
    // The test pins the simd prefix and the low-level intrinsic path as unstable before std parsing.
    #[test]
    fn unstable_std_modules_match_simd_paths() {
        assert!(super::is_unstable_std_module(&[
            "simd".to_string(),
            "x86_64".to_string(),
        ]));
        assert!(super::is_unstable_std_module(&[
            "intrinsics".to_string(),
            "x86_64".to_string(),
            "simd".to_string(),
        ]));
        assert!(!super::is_unstable_std_module(&[
            "intrinsics".to_string(),
            "float".to_string(),
        ]));
    }

    // A std module must see symbols from another std module it imports.
    #[test]
    fn std_module_can_import_another_std_module() {
        with_temp_std(
            &[
                ("a.thrust", "import std::b;\n"),
                ("b.thrust", "type Foo = u8;\n"),
            ],
            |options, _| {
                let module: Module =
                    super::find_std_module(&["std".to_string(), "a".to_string()], options)
                        .expect("std::a should resolve");

                assert_eq!(module.get_name(), "a");

                let b: &Module = module
                    .get_submodules()
                    .iter()
                    .find(|submodule| submodule.get_name() == "b")
                    .expect("std::a should reexport std::b");

                assert!(
                    b.search_symbol("Foo".to_string(), Variant::CustomType)
                        .is_some(),
                    "std::b symbols must be resolvable through std::a"
                );
            },
        );
    }
}
