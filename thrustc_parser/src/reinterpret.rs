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

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

pub fn floating_point(lexeme: &str, span: Span) -> Result<(Type, f64), CompilationIssue> {
    if lexeme.bytes().filter(|&b| b == b'.').count() > 1 {
        Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Floating-point number only expects a one decimal marker.".into(),
            "You should delete the extra dot.".into(),
            None,
            span,
        ))
    } else {
        match lexeme.parse::<f64>() {
            Ok(f64_value) => {
                if let Ok(f32_value) = lexeme.parse::<f32>() {
                    if (f32_value as f64) == f64_value {
                        return Ok((Type::F32 { span }, f32_value as f64));
                    }
                }

                Ok((Type::F64 { span }, f64_value))
            }

            Err(_) => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Literal is too large to be represented in a standard floating-point type."
                    .into(),
                "You can shorten it.".into(),
                None,
                span,
            )),
        }
    }
}

pub fn integer(lexeme: &str, span: Span) -> Result<(Type, u64), CompilationIssue> {
    const I8_MIN: isize = -128;
    const I8_MAX: isize = 127;
    const I16_MIN: isize = -32768;
    const I16_MAX: isize = 32767;
    const I32_MIN: isize = -2147483648;
    const I32_MAX: isize = 2147483647;

    const U8_MAX: usize = 255;
    const U16_MAX: usize = 65535;
    const U32_MAX: usize = 4294967295;

    fn match_signed(number: isize, span: Span) -> Result<(Type, u64), CompilationIssue> {
        match number {
            n if (I8_MIN..=I8_MAX).contains(&n) => Ok((Type::S8 { span }, n as u64)),
            n if (I16_MIN..=I16_MAX).contains(&n) => Ok((Type::S16 { span }, n as u64)),
            n if (I32_MIN..=I32_MAX).contains(&n) => Ok((Type::S32 { span }, n as u64)),
            n if (isize::MIN..=isize::MAX).contains(&n) => Ok((Type::S64 { span }, n as u64)),

            _ => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Literal is too large to be represented in a integer type.".into(),
                "You can shorten it.".into(),
                None,
                span,
            )),
        }
    }

    fn match_unsigned(number: usize, span: Span) -> Result<(Type, u64), CompilationIssue> {
        match number {
            n if (0..=U8_MAX).contains(&n) => Ok((Type::U8 { span }, n as u64)),
            n if (0..=U16_MAX).contains(&n) => Ok((Type::U16 { span }, n as u64)),
            n if (0..=U32_MAX).contains(&n) => Ok((Type::U32 { span }, n as u64)),
            n if (0..=usize::MAX).contains(&n) => Ok((Type::U64 { span }, n as u64)),

            _ => Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Literal is too large to be represented in a integer type.".into(),
                "You can shorten it.".into(),
                None,
                span,
            )),
        }
    }

    let hexadecimal: bool = lexeme.strip_prefix("0x").is_some();
    let octal: bool = lexeme.strip_prefix("0o").is_some();
    let binary: bool = lexeme.strip_prefix("0b").is_some();

    let (radix, prefix) = if hexadecimal {
        (16, "0x")
    } else if octal {
        (8, "0o")
    } else if binary {
        (2, "0b")
    } else {
        (10, "")
    };

    let cleaned: String = if radix != 10 {
        lexeme
            .strip_prefix(prefix)
            .unwrap_or(lexeme)
            .replace('_', "")
    } else {
        lexeme.replace('_', "")
    };

    if radix != 10 {
        if let Ok(n) = usize::from_str_radix(&cleaned, radix) {
            return match_unsigned(n, span);
        }

        if let Ok(n) = isize::from_str_radix(&cleaned, radix) {
            return match_signed(n, span);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!(
                "Invalid {} integer literal",
                if hexadecimal {
                    "hexadecimal"
                } else if octal {
                    "octal"
                } else {
                    "binary"
                }
            ),
            format!(
                "It is not in {} format.",
                if hexadecimal {
                    "hexadecimal"
                } else if octal {
                    "octal"
                } else {
                    "binary"
                }
            ),
            None,
            span,
        ))
    } else {
        if let Ok(n) = lexeme.parse::<usize>() {
            return match_unsigned(n, span);
        }
        if let Ok(n) = lexeme.parse::<isize>() {
            return match_signed(n, span);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            "Literal is too large to be represented in a integer type.".into(),
            "You can shorten it.".into(),
            None,
            span,
        ))
    }
}
