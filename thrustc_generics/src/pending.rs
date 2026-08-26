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

thread_local! {
    static PENDING_INSTANTIATIONS: RefCell<Vec<PendingInstantiation>> = const { RefCell::new(Vec::new()) };
}

pub fn record_pending(module: PathBuf, function: String, env: TypeEnv) {
    PENDING_INSTANTIATIONS.with(|cell| {
        let mut pending: std::cell::RefMut<'_, Vec<PendingInstantiation>> = cell.borrow_mut();

        let key: String = hashing::type_env_fingerprint(&env);

        if !pending.iter().any(|entry| {
            entry.module == module
                && entry.function == function
                && hashing::type_env_fingerprint(&entry.env) == key
        }) {
            pending.push(PendingInstantiation {
                module,
                function,
                env,
            });
        }
    });
}

pub fn drain_pending(module: &Path) -> Vec<PendingInstantiation> {
    PENDING_INSTANTIATIONS.with(|cell| {
        let mut pending: std::cell::RefMut<'_, Vec<PendingInstantiation>> = cell.borrow_mut();

        let mut drained: Vec<PendingInstantiation> = Vec::with_capacity(u8::MAX as usize);
        let mut seen: HashSet<(PathBuf, String, String)> = HashSet::with_capacity(u8::MAX as usize);

        let mut index: usize = 0;

        while index < pending.len() {
            if pending[index].module == module {
                let entry: PendingInstantiation = pending.remove(index);

                let key: String = hashing::type_env_fingerprint(&entry.env);

                if seen.insert((entry.module.clone(), entry.function.clone(), key)) {
                    drained.push(entry);
                }
            } else {
                index = index.saturating_add(1);
            }
        }

        drained
    })
}

#[inline]
pub fn has_pending_instantiations() -> bool {
    PENDING_INSTANTIATIONS.with(|cell| !cell.borrow().is_empty())
}

#[inline]
pub fn has_pending_for(module: &Path) -> bool {
    PENDING_INSTANTIATIONS.with(|cell| cell.borrow().iter().any(|pending| pending.module == module))
}
