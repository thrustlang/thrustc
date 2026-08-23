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

mod alignof;
mod compile_time;
mod compiler;
mod layout;
mod location;
mod predicates;
mod sizeof;
mod strings;
mod target;
mod typeinfo;

use thrustc_code_location::Span;
use thrustc_typesystem::Type;
use thrustc_typesystem::type_metadata::ArrayTypeMetadata;

use crate::builtin_type::BuiltinTypeInfo;
use crate::registry::BuiltinRegistry;

pub fn register_default_builtins(registry: &mut BuiltinRegistry) {
    registry.register_function(alignof::AlignOf);
    registry.register_function(sizeof::SizeOf);
    registry.register_function(location::File);
    registry.register_function(location::FileLine);
    registry.register_function(location::CurrentFuncName);
    registry.register_function(compile_time::StaticAssert);
    registry.register_function(compile_time::CompileError);
    registry.register_function(compile_time::CompileWarning);

    registry.register_function(predicates::IsSigned);
    registry.register_function(predicates::IsUnsigned);
    registry.register_function(predicates::IsInteger);
    registry.register_function(predicates::IsFloat);
    registry.register_function(predicates::IsBool);
    registry.register_function(predicates::IsChar);
    registry.register_function(predicates::IsPointer);
    registry.register_function(predicates::IsArray);
    registry.register_function(predicates::IsFixedArray);
    registry.register_function(predicates::IsStruct);
    registry.register_function(predicates::IsVoid);
    registry.register_function(predicates::IsConst);
    registry.register_function(predicates::IsNumeric);
    registry.register_function(predicates::IsFunction);

    registry.register_function(layout::TypeWidth);
    registry.register_function(layout::FieldCount);

    registry.register_function(typeinfo::FixedArraySize);
    registry.register_function(typeinfo::IsSameType);
    registry.register_function(typeinfo::IsPtrLike);
    registry.register_function(typeinfo::IsFixedArrayOfSize);

    registry.register_function(compiler::CompilerVersion);
    registry.register_function(compiler::DebugBuild);

    registry.register_function(strings::StringLength);

    registry.register_function(target::TargetOS);
    registry.register_function(target::TargetArch);
    registry.register_function(target::TargetVendor);
    registry.register_function(target::TargetAbi);
    registry.register_function(target::TargetTriple);
    registry.register_function(target::IsLinux);
    registry.register_function(target::IsWindows);
    registry.register_function(target::IsDarwin);
    registry.register_function(target::IsApple);
    registry.register_function(target::IsAix);
    registry.register_function(target::Is64Bit);
    registry.register_function(target::Is32Bit);
    registry.register_function(target::IsBigEndian);
    registry.register_function(target::IsLittleEndian);
    registry.register_function(target::IsX86);
    registry.register_function(target::IsX8664);
    registry.register_function(target::IsArm);
    registry.register_function(target::IsAarch64);
    registry.register_function(target::IsRiscv64);
    registry.register_function(target::IsPpc);
    registry.register_function(target::IsPpc64);
    registry.register_function(target::IsMips64);
    registry.register_function(target::IsSystemz);
    registry.register_function(target::IsLoongarch64);
    registry.register_function(target::IsWasm);
    registry.register_function(target::IsElf);
    registry.register_function(target::IsMachO);
    registry.register_function(target::IsCoff);
    registry.register_function(target::HasPosixThreads);
    registry.register_function(target::HasSysvAbi);

    registry.register_type(BuiltinTypeInfo::new(
        "CString",
        Type::Array {
            base_type: Type::Char {
                span: Span::nothing(),
            }
            .into(),
            infered_type: None,
            metadata: ArrayTypeMetadata::new(None, None),
            span: Span::nothing(),
        },
    ));
}