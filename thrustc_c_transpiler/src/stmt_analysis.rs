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

pub fn expression_produces_condition(entity: &clang::Entity<'_>) -> bool {
    match entity.get_kind() {
        clang::EntityKind::ConditionalOperator => true,
        clang::EntityKind::UnaryOperator => crate::macro_lex::entity_spellings(entity)
            .iter()
            .any(|token| token == "!"),
        clang::EntityKind::BinaryOperator | clang::EntityKind::CompoundAssignOperator => {
            let children: Vec<clang::Entity<'_>> = entity.get_children();

            if children.len() < 2 {
                return false;
            }

            crate::macro_lex::extract_binary_operator(entity, &children[0], &children[1])
                .map(|op| matches!(op, "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="))
                .unwrap_or(false)
        }
        _ => entity
            .get_type()
            .is_some_and(|ty| ty.get_canonical_type().get_kind() == clang::TypeKind::Bool),
    }
}
