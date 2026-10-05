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

pub(crate) fn resolve_expression_type<'tu>(
    entity: &clang::Entity<'tu>,
) -> Option<clang::Type<'tu>> {
    let mut resolved: Option<clang::Type<'tu>> = entity.get_type();
    let mut probe: clang::Entity<'tu> = *entity;

    loop {
        if !matches!(
            probe.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            break;
        }

        let children: Vec<clang::Entity<'tu>> = probe.get_children();

        if children.len() != 1 {
            break;
        }

        probe = children[0];

        if let Some(probe_type) = probe.get_type() {
            let canonical_type: clang::Type<'tu> = probe_type.get_canonical_type();

            if matches!(
                canonical_type.get_kind(),
                clang::TypeKind::ConstantArray
                    | clang::TypeKind::IncompleteArray
                    | clang::TypeKind::Record
            ) {
                return Some(probe_type);
            }

            resolved = Some(probe_type);
        }
    }

    resolved
}

pub(crate) fn analyze_nested_scalar_array_type(ty: &clang::Type<'_>) -> Option<(bool, Vec<usize>)> {
    let canonical: clang::Type<'_> = ty.get_canonical_type();

    let (pointer_root, mut current): (bool, clang::Type<'_>) =
        if canonical.get_kind() == clang::TypeKind::Pointer {
            (true, canonical.get_pointee_type()?.get_canonical_type())
        } else {
            (false, canonical)
        };

    let mut extents: Vec<usize> = Vec::new();

    while matches!(
        current.get_kind(),
        clang::TypeKind::ConstantArray | clang::TypeKind::IncompleteArray
    ) {
        let size: usize = current.get_size()?;

        extents.push(size);
        current = current.get_element_type()?.get_canonical_type();
    }

    if extents.is_empty()
        || matches!(
            current.get_kind(),
            clang::TypeKind::ConstantArray
                | clang::TypeKind::IncompleteArray
                | clang::TypeKind::Record
        )
    {
        return None;
    }

    Some((pointer_root, extents))
}

pub(crate) fn peel_expression_wrappers<'tu>(entity: &clang::Entity<'tu>) -> clang::Entity<'tu> {
    let mut probe: clang::Entity<'tu> = *entity;

    loop {
        if !matches!(
            probe.get_kind(),
            clang::EntityKind::ParenExpr | clang::EntityKind::UnexposedExpr
        ) {
            break;
        }

        let children: Vec<clang::Entity<'tu>> = probe.get_children();

        if children.len() != 1 || !crate::clang_util::is_supported_expr_kind(children[0].get_kind())
        {
            break;
        }

        probe = children[0];
    }

    probe
}
