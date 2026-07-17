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

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::thread;

use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use walkdir::WalkDir;

use crate::LinuxCRuntimeVariant;

pub fn find_libgcc_linux(library_paths: &[String], is_64_bit: bool) -> Option<PathBuf> {
    if library_paths.is_empty() {
        return None;
    }

    let available: usize = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let num_threads: usize = (available / 2).max(1).min(library_paths.len());

    if num_threads <= 1 {
        for base in library_paths {
            if let Some(path) = search_libgcc(Path::new(base), is_64_bit) {
                return Some(path);
            }
        }

        return None;
    }

    let mut results: Vec<Option<PathBuf>> = vec![None; library_paths.len()];
    let chunks: Vec<Vec<usize>> = chunk_indices(library_paths.len(), num_threads);

    thread::scope(|scope| {
        #[allow(clippy::type_complexity)]
        let mut handles: Vec<thread::ScopedJoinHandle<'_, Vec<(usize, Option<PathBuf>)>>> =
            Vec::with_capacity(chunks.len());

        for chunk in chunks {
            let paths: &[String] = library_paths;

            handles.push(scope.spawn(move || {
                chunk
                    .into_iter()
                    .map(|idx| (idx, search_libgcc(Path::new(&paths[idx]), is_64_bit)))
                    .collect::<Vec<_>>()
            }));
        }

        for handle in handles {
            for (idx, found) in handle.join().unwrap() {
                results[idx] = found;
            }
        }
    });

    results.into_iter().flatten().next()
}

fn search_libgcc(dir: &Path, is_64_bit: bool) -> Option<PathBuf> {
    let Ok(entries) = fs::read_dir(dir) else {
        return None;
    };

    for entry in entries.flatten() {
        let Ok(file_type) = entry.file_type() else {
            continue;
        };

        let path: PathBuf = entry.path();
        let name: &str = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        if is_64_bit && name == "32" {
            continue;
        }

        if file_type.is_dir() {
            if !is_64_bit && name == "32" {
                return search_libgcc(&path, is_64_bit);
            }

            if let Some(found) = search_libgcc(&path, is_64_bit) {
                return Some(found);
            }
        } else if file_type.is_file() {
            #[allow(clippy::collapsible_if)]
            if name == "libgcc.a" || name == "libgcc.so" || name.starts_with("libgcc.so.") {
                return path.parent().map(|p| p.to_path_buf());
            }
        }
    }
    None
}

struct CRuntimeSearchControl {
    crt1: Mutex<Option<PathBuf>>,
    crti: Mutex<Option<PathBuf>>,
    crtn: Mutex<Option<PathBuf>>,
    done: AtomicBool,
}

impl CRuntimeSearchControl {
    fn new() -> Self {
        Self {
            crt1: Mutex::new(None),
            crti: Mutex::new(None),
            crtn: Mutex::new(None),
            done: AtomicBool::new(false),
        }
    }

    fn set_once(&self, slot: &Mutex<Option<PathBuf>>, path: &Path) {
        let mut guard = slot.lock().unwrap();
        if guard.is_none() {
            *guard = Some(path.to_path_buf());
        }
    }

    fn check_complete(&self) -> bool {
        if self.done.load(Ordering::Acquire) {
            return true;
        }

        let complete: bool = self.crt1.lock().unwrap().is_some()
            && self.crti.lock().unwrap().is_some()
            && self.crtn.lock().unwrap().is_some();

        if complete {
            self.done.store(true, Ordering::Release);
        }

        complete
    }

    fn into_tuple(self) -> Option<(PathBuf, PathBuf, PathBuf)> {
        match (
            self.crt1.into_inner().unwrap_or_default(),
            self.crti.into_inner().unwrap_or_default(),
            self.crtn.into_inner().unwrap_or_default(),
        ) {
            (Some(a), Some(b), Some(c)) => Some((a, b, c)),
            _ => None,
        }
    }
}

pub fn find_c_runtime_objects_linux(
    library_paths: &[String],
    runtime_variant: LinuxCRuntimeVariant,
) -> Option<(PathBuf, PathBuf, PathBuf)> {
    if library_paths.is_empty() {
        return None;
    }

    let available: usize = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let num_threads: usize = (available / 2).max(1).min(library_paths.len());

    let state: CRuntimeSearchControl = CRuntimeSearchControl::new();

    if num_threads <= 1 {
        for base in library_paths {
            search_dir_walk(Path::new(base), &state, runtime_variant);

            if state.check_complete() {
                break;
            }
        }

        return state.into_tuple();
    }

    let chunks: Vec<&[String]> = chunk_paths(library_paths, num_threads);

    thread::scope(|scope| {
        for chunk in chunks {
            let state_ref = &state;
            scope.spawn(move || {
                for base in chunk {
                    if state_ref.done.load(Ordering::Acquire) {
                        break;
                    }

                    search_dir_walk(Path::new(base), state_ref, runtime_variant);

                    if state_ref.check_complete() {
                        break;
                    }
                }
            });
        }
    });

    state.into_tuple()
}

fn chunk_paths(paths: &[String], num_chunks: usize) -> Vec<&[String]> {
    let len: usize = paths.len();
    let base_size: usize = len / num_chunks;
    let remainder: usize = len % num_chunks;
    let mut chunks: Vec<&[String]> = Vec::with_capacity(num_chunks);
    let mut start: usize = 0;

    for i in 0..num_chunks {
        let extra: usize = if i < remainder { 1 } else { 0 };
        let end: usize = start + base_size + extra;

        if start < end {
            chunks.push(&paths[start..end]);
        }

        start = end;
    }

    chunks
}

fn search_dir_walk(
    root: &Path,
    state: &CRuntimeSearchControl,
    runtime_variant: LinuxCRuntimeVariant,
) {
    let contrary: Vec<&str> = runtime_variant.get_contrary_system_representations();

    let walker = WalkDir::new(root).into_iter().filter_entry(move |entry| {
        if entry.depth() == 0 {
            return true;
        }

        let path_str: std::borrow::Cow<'_, str> = entry.path().to_string_lossy();

        !contrary.iter().any(|c| path_str.contains(c))
    });

    for entry in walker {
        if state.done.load(Ordering::Acquire) {
            return;
        }

        let Ok(entry) = entry else { continue };

        if !entry.file_type().is_file() {
            continue;
        }

        match entry.file_name().to_str() {
            Some("crt1.o") => state.set_once(&state.crt1, entry.path()),
            Some("crti.o") => state.set_once(&state.crti, entry.path()),
            Some("crtn.o") => state.set_once(&state.crtn, entry.path()),

            _ => {}
        }

        if state.check_complete() {
            return;
        }
    }
}

pub fn find_dynamic_linker(library_paths: &[String]) -> Option<PathBuf> {
    if library_paths.is_empty() {
        return None;
    }

    let available: usize = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let num_threads: usize = (available / 2).max(1).min(library_paths.len());

    if num_threads <= 1 {
        for base in library_paths {
            if let Some(found) = search_dynamic_linker_walk(Path::new(base)) {
                return Some(found);
            }
        }

        return None;
    }

    let mut results: Vec<Option<PathBuf>> = vec![None; library_paths.len()];

    let chunks = chunk_indices(library_paths.len(), num_threads);

    thread::scope(|scope| {
        let mut handles = Vec::with_capacity(chunks.len());
        for chunk in chunks {
            let paths = library_paths;
            handles.push(scope.spawn(move || {
                chunk
                    .into_iter()
                    .map(|idx| (idx, search_dynamic_linker_walk(Path::new(&paths[idx]))))
                    .collect::<Vec<_>>()
            }));
        }
        for handle in handles {
            for (idx, found) in handle.join().unwrap() {
                results[idx] = found;
            }
        }
    });

    results.into_iter().flatten().next()
}

fn chunk_indices(len: usize, num_chunks: usize) -> Vec<Vec<usize>> {
    let base_size: usize = len / num_chunks;
    let remainder: usize = len % num_chunks;
    let mut chunks: Vec<Vec<usize>> = Vec::with_capacity(num_chunks);
    let mut start: usize = 0;

    for i in 0..num_chunks {
        let extra: usize = if i < remainder { 1 } else { 0 };
        let end: usize = start + base_size + extra;

        if start < end {
            chunks.push((start..end).collect());
        }

        start = end;
    }
    chunks
}

fn search_dynamic_linker_walk(root: &Path) -> Option<PathBuf> {
    for entry in WalkDir::new(root).follow_links(true) {
        let Ok(entry) = entry else { continue };

        let is_candidate: bool = entry.file_type().is_file() || entry.path_is_symlink();

        if !is_candidate {
            continue;
        }

        let name: &str = entry.file_name().to_str().unwrap_or("");

        if self::is_dynamic_linker(name) {
            return Some(entry.into_path());
        }
    }

    None
}

fn is_dynamic_linker(name: &str) -> bool {
    matches!(
        name,
        "ld-linux-x86-64.so.2"
            | "ld-linux.so.2"
            | "ld-linux-aarch64.so.1"
            | "ld-linux-armhf.so.3"
            | "ld-linux-arm.so.3"
    )
}
