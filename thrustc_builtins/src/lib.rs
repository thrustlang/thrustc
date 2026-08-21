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

pub mod builtin_type;
pub mod builtins;
pub mod context;
pub mod registry;
pub mod traits;
pub mod value;

pub use crate::builtin_type::BuiltinTypeInfo;
pub use crate::context::BuiltinContext;
pub use crate::registry::BuiltinRegistry;
pub use crate::traits::BuiltinFunctionSignature;
pub use crate::traits::BuiltinParameter;
pub use crate::traits::CompileTimeBuiltinFunction;
pub use crate::value::BuiltinArgument;
pub use crate::value::BuiltinValue;

use thrustc_typesystem::type_layout::TargetInfo;

pub fn default_registry(target_info: TargetInfo) -> BuiltinRegistry {
    let mut registry: BuiltinRegistry = BuiltinRegistry::new(target_info);

    builtins::register_default_builtins(&mut registry);

    registry
}
