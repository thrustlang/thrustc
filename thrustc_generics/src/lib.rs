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

mod fingerprint;
mod pending;
mod scope;
mod solve;
mod substitution;

pub use self::fingerprint::{instantiation_key, type_env_fingerprint, type_fingerprint};
pub use self::pending::{
    drain_pending, has_pending_for, has_pending_instantiations, record_pending,
    PendingInstantiation,
};
pub use self::scope::GenericScope;
pub use self::solve::{solve, SolveResult, TypeEnv};
pub use self::substitution::{substitute, substitute_ast};