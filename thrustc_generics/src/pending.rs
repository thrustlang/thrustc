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
use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::hashing;
use crate::solve::TypeEnv;

#[derive(Debug, Clone)]
pub struct PendingInstantiation {
    pub module: PathBuf,
    pub function: String,
    pub env: TypeEnv,
}

#[derive(Debug, Default)]
struct PendingInstantiations {
    required: Vec<PendingInstantiation>,
    dirty: HashSet<PathBuf>,
}

thread_local! {
    static PENDING_INSTANTIATIONS: RefCell<PendingInstantiations> = RefCell::new(PendingInstantiations::default());
}

pub fn record_pending(module: PathBuf, function: String, env: TypeEnv) {
    PENDING_INSTANTIATIONS.with(|cell| {
        let mut pending: std::cell::RefMut<'_, PendingInstantiations> = cell.borrow_mut();

        let key: String = hashing::type_env_fingerprint(&env);

        if !pending.required.iter().any(|entry| {
            entry.module == module
                && entry.function == function
                && hashing::type_env_fingerprint(&entry.env) == key
        }) {
            pending.required.push(PendingInstantiation {
                module: module.clone(),
                function,
                env,
            });

            pending.dirty.insert(module);
        }
    });
}

pub fn take_pending(module: &Path) -> Vec<PendingInstantiation> {
    PENDING_INSTANTIATIONS.with(|cell| {
        let mut pending: std::cell::RefMut<'_, PendingInstantiations> = cell.borrow_mut();

        pending.dirty.remove(module);

        pending
            .required
            .iter()
            .filter(|entry| entry.module == module)
            .cloned()
            .collect()
    })
}

pub fn pending_module_paths() -> Vec<PathBuf> {
    PENDING_INSTANTIATIONS.with(|cell| cell.borrow().dirty.iter().cloned().collect())
}

pub fn reset_pending_instantiations() {
    PENDING_INSTANTIATIONS.with(|cell| {
        let mut pending: std::cell::RefMut<'_, PendingInstantiations> = cell.borrow_mut();

        pending.required.clear();
        pending.dirty.clear();
    });
}

#[inline]
pub fn has_pending_instantiations() -> bool {
    PENDING_INSTANTIATIONS.with(|cell| !cell.borrow().dirty.is_empty())
}

#[inline]
pub fn has_pending_for(module: &Path) -> bool {
    PENDING_INSTANTIATIONS.with(|cell| cell.borrow().dirty.contains(module))
}
