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

#[derive(Debug)]
pub struct Pointer;

impl Pointer {
    /// Lowers a C pointer arithmetic binary expression into Thrust.
    ///
    /// Handles `p + n`, `n + p`, `p - n` and the valid pointer-to-pointer
    /// operations (`p - q`, `p == q`, `p != q`). Returns `None` when the
    /// operands are not a pointer arithmetic situation.
    #[inline]
    pub fn lower_binary(
        left_is_pointer: bool,
        right_is_pointer: bool,
        pointee_is_pointer: bool,
        op: &str,
        left: &str,
        right: &str,
    ) -> Option<String> {
        if left_is_pointer && !right_is_pointer {
            if op == "+" {
                return Some(Self::address(left, right, pointee_is_pointer));
            }

            if op == "-" {
                let index: String = format!("0 - {right}");

                return Some(Self::address(left, &index, pointee_is_pointer));
            }

            return None;
        }

        if !left_is_pointer && right_is_pointer {
            if op == "+" {
                return Some(Self::address(right, left, pointee_is_pointer));
            }

            return None;
        }

        if left_is_pointer && right_is_pointer && (op == "-" || op == "==" || op == "!=") {
            return Some(format!("{left} {op} {right}"));
        }

        None
    }
}

impl Pointer {
    /// Lowers a pointer advance into Thrust: `p++`, `p--`, `p += n`, `p -= n`.
    #[inline]
    pub fn lower_advance(
        pointer: &str,
        step: &str,
        forward: bool,
        pointee_is_pointer: bool,
    ) -> String {
        let index: String = if forward {
            step.to_string()
        } else {
            format!("0 - {step}")
        };

        let address: String = Self::address(pointer, &index, pointee_is_pointer);

        format!("{pointer} = {address}")
    }
}

impl Pointer {
    /// Lowers the dereference of a pointer arithmetic expression.
    ///
    /// `*(p + n)` yields the element value (`p->[n]`) unless the pointee is
    /// itself a pointer, in which case `load p[n]` reads the stored pointer.
    #[inline]
    pub fn lower_deref(pointer: &str, index: &str, pointee_is_pointer: bool) -> String {
        let grouped: String = if pointer.starts_with("load ") {
            format!("({pointer})")
        } else {
            pointer.to_string()
        };

        if pointee_is_pointer {
            return format!("load {grouped}[{index}]");
        }

        format!("{grouped}->[{index}]")
    }
}

impl Pointer {
    /// Composes the Thrust address expression for `pointer[index]`.
    ///
    /// A pointer whose pointee is itself a pointer needs `ref` so the result
    /// stays a direct address instead of being auto-loaded.
    #[inline]
    fn address(pointer: &str, index: &str, pointee_is_pointer: bool) -> String {
        if pointee_is_pointer {
            return format!("ref ({pointer}[{index}])");
        }

        format!("{pointer}[{index}]")
    }
}
