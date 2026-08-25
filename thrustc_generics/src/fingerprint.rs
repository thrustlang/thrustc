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

use thrustc_typesystem::Type;

use crate::solve::TypeEnv;

pub fn type_fingerprint(ty: &Type, output: &mut String) {
    match ty {
        Type::S8 { .. } => output.push_str("s8"),
        Type::S16 { .. } => output.push_str("s16"),
        Type::S32 { .. } => output.push_str("s32"),
        Type::S64 { .. } => output.push_str("s64"),
        Type::SSize { .. } => output.push_str("ssize"),
        Type::U8 { .. } => output.push_str("u8"),
        Type::U16 { .. } => output.push_str("u16"),
        Type::U32 { .. } => output.push_str("u32"),
        Type::U64 { .. } => output.push_str("u64"),
        Type::U128 { .. } => output.push_str("u128"),
        Type::USize { .. } => output.push_str("usize"),
        Type::F32 { .. } => output.push_str("f32"),
        Type::F64 { .. } => output.push_str("f64"),
        Type::F128 { .. } => output.push_str("f128"),
        Type::FX8680 { .. } => output.push_str("f80"),
        Type::FPPC128 { .. } => output.push_str("fppc_128"),
        Type::Bool { .. } => output.push_str("bool"),
        Type::Char { .. } => output.push_str("char"),
        Type::Void { .. } => output.push_str("void"),
        Type::Unresolved { hint, .. } => {
            output.push('$');
            output.push_str(hint);
        }
        Type::Const(inner, _) => {
            output.push_str("const(");
            self::type_fingerprint(inner, output);
            output.push(')');
        }
        Type::Ptr {
            subtype,
            address_space,
            ..
        } => {
            output.push_str("ptr[");
            if let Some(inner) = subtype {
                self::type_fingerprint(inner, output);
            } else {
                output.push('_');
            }
            if let Some(space) = address_space {
                output.push(',');
                output.push_str(&space.to_string());
            }
            output.push(']');
        }
        Type::Struct {
            name,
            fields,
            metadata,
            ..
        } => {
            output.push_str("struct(");
            output.push_str(name);
            output.push(':');
            for (index, field) in fields.iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                self::type_fingerprint(field, output);
            }
            output.push(':');
            if metadata.get_struct_type_modificator().llvm().is_packed() {
                output.push('p');
            } else {
                output.push('u');
            }
            output.push(')');
        }
        Type::FixedArray {
            base_type,
            size,
            metadata,
            ..
        } => {
            output.push_str("fixedarray[");
            self::type_fingerprint(base_type, output);
            output.push(';');
            output.push_str(&size.to_string());
            if let Some(space) = metadata.get_address_space() {
                output.push(',');
                output.push_str(&space.to_string());
            }
            output.push(']');
        }
        Type::Array {
            base_type,
            infered_type,
            metadata,
            ..
        } => {
            output.push_str("array[");
            self::type_fingerprint(base_type, output);
            if let Some((_, count)) = infered_type {
                output.push(';');
                output.push_str(&count.to_string());
            }
            if let Some(space) = metadata.get_address_space() {
                output.push(',');
                output.push_str(&space.to_string());
            }
            output.push(']');
        }
        Type::Fn {
            return_type,
            parameter_types,
            ..
        } => {
            output.push_str("fn[");
            for (index, parameter) in parameter_types.iter().enumerate() {
                if index != 0 {
                    output.push(',');
                }
                self::type_fingerprint(parameter, output);
            }
            output.push_str("]->");
            self::type_fingerprint(return_type, output);
        }
    }
}

pub fn type_env_fingerprint(env: &TypeEnv) -> String {
    let mut entries: Vec<(String, String)> = env
        .iter()
        .map(|(hint, ty)| {
            let mut fingerprint: String = String::with_capacity(16);
            self::type_fingerprint(ty, &mut fingerprint);

            (hint.clone(), fingerprint)
        })
        .collect();

    entries.sort_by(|left, right| left.0.cmp(&right.0));

    let mut output: String = String::with_capacity(16);

    for (index, (hint, fingerprint)) in entries.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        output.push_str(hint);
        output.push('=');
        output.push_str(fingerprint);
    }

    output
}

pub fn instantiation_key(module: Option<&str>, name: &str, env: &TypeEnv) -> String {
    let module: &str = module.unwrap_or("");
    let content: String = format!("{module}::{name}({})", self::type_env_fingerprint(env));

    format!("__generic_{}_{:x}", name, self::fnv1a(content.as_bytes()))
}

fn fnv1a(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;

    for byte in bytes.iter() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }

    hash
}