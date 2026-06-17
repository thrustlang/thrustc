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

use crate::ThrustAttribute;

impl std::fmt::Display for ThrustAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThrustAttribute::AlwaysInline(..) => write!(f, "@alwaysInline"),
            ThrustAttribute::NoInline(..) => write!(f, "@noInline"),
            ThrustAttribute::InlineHint(..) => write!(f, "@inline"),
            ThrustAttribute::Linkage(linkage, ..) => write!(f, "@linkage(\"{}\")", linkage),
            ThrustAttribute::Extern(name, ..) => write!(f, "@extern(\"{}\")", name),
            ThrustAttribute::Convention(convention, ..) => {
                write!(f, "@convention(\"{}\")", convention)
            }
            ThrustAttribute::Stack(..) => write!(f, "@stack"),
            ThrustAttribute::Heap(..) => write!(f, "@heap"),
            ThrustAttribute::Public(..) => write!(f, "@public"),
            ThrustAttribute::StrongStack(..) => write!(f, "@strongStack"),
            ThrustAttribute::WeakStack(..) => write!(f, "@weakStack"),
            ThrustAttribute::SafeStack(..) => write!(f, "@safeStack"),
            ThrustAttribute::PreciseFloats(..) => write!(f, "@preciseFloatingPoint"),
            ThrustAttribute::MinSize(..) => write!(f, "@minSize"),
            ThrustAttribute::Hot(..) => write!(f, "@hot"),
            ThrustAttribute::Ignore(..) => write!(f, "@arbitraryArgs"),
            ThrustAttribute::NoUnwind(..) => write!(f, "@noUnwind"),
            ThrustAttribute::AsmThrow(..) => write!(f, "@asmThrow"),
            ThrustAttribute::AsmSyntax(..) => write!(f, "@asmSyntax"),
            ThrustAttribute::AsmSideEffects(..) => write!(f, "@asmEffects"),
            ThrustAttribute::AsmAlignStack(..) => write!(f, "@asmAlignStack"),
            ThrustAttribute::Packed(..) => write!(f, "@packed"),
            ThrustAttribute::OptFuzzing(..) => write!(f, "@optFuzzing"),
            ThrustAttribute::Align(align, ..) => write!(f, "@align({})", align),
            ThrustAttribute::Pure(..) => write!(f, "@pure"),
            ThrustAttribute::Thunk(..) => write!(f, "@thunk"),
            ThrustAttribute::Constructor(..) => write!(f, "@constructor"),
            ThrustAttribute::Destructor(..) => write!(f, "@destructor"),
            ThrustAttribute::Cuda(..) => write!(f, "@cuda"),
            ThrustAttribute::Promote(promote, ..) => {
                let type_displayed: Vec<String> = promote
                    .iter()
                    .map(|(from, target)| format!("{} -> {}", from, target))
                    .collect();

                write!(f, "@promote({})", type_displayed.join(", "))
            }
        }
    }
}
