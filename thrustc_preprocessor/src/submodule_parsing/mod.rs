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

pub mod attributes;
pub mod constant;
pub mod customtype;
pub mod expressions;
pub mod function;
pub mod import;
pub mod modificators;
pub mod reinterpret;
pub mod r#static;
pub mod structure;
pub mod typegeneration;

use thrustc_attributes::{ThrustAttribute, ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;

/// Ensures a module symbol is exposed for cross-file reference.
///
/// It adds the missing visibility attributes without duplicating existing ones:
/// `@public` is always ensured, and `@extern` is ensured when `needs_extern` is
/// true (functions and statics, whose cross-file declarations are bodyless).
///
/// Returns `true` when `@public` had to be added because the original signature
/// did not provide it.
pub fn ensure_exposed(
    attributes: &mut ThrustAttributes,
    name: &str,
    span: Span,
    needs_extern: bool,
) -> bool {
    let added_public: bool = !attributes.has_public_attribute();

    if added_public {
        attributes.push(ThrustAttribute::Public(span));
    }

    if needs_extern && !attributes.has_extern_attribute() {
        attributes.push(ThrustAttribute::Extern(name.to_string(), span));
    }

    added_public
}
