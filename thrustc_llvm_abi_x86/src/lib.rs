#![allow(non_camel_case_types)]

use thrustc_typesystem::{Type, traits::TypePointerExtensions, type_layout::TargetInfo};

#[derive(Debug)]
pub struct X86SystemVABIContext<'system_v> {
    target_info: &'system_v mut TargetInfo,
}

impl<'system_v> X86SystemVABIContext<'system_v> {
    pub fn new(target_info: &'system_v mut TargetInfo) -> Self {
        Self { target_info }
    }
}

impl<'system_v> X86SystemVABIContext<'system_v> {
    pub fn get_mut_target_info(&mut self) -> &mut TargetInfo {
        self.target_info
    }
}

// https://gitlab.com/x86-psABIs/x86-64-ABI - System V
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum X86SystemVABITypeClass {
    INTEGER,
    SSE,
    SSEUP,
    X87,
    X87UP,
    COMPLEX_X87,
    NO_CLASS,
    MEMORY,
}

impl std::fmt::Display for X86SystemVABITypeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class_str: &str = match self {
            X86SystemVABITypeClass::INTEGER => "INTEGER",
            X86SystemVABITypeClass::SSE => "SSE",
            X86SystemVABITypeClass::SSEUP => "SSEUP",
            X86SystemVABITypeClass::X87 => "X87",
            X86SystemVABITypeClass::X87UP => "X87UP",
            X86SystemVABITypeClass::COMPLEX_X87 => "COMPLEX_X87",
            X86SystemVABITypeClass::NO_CLASS => "NO_CLASS",
            X86SystemVABITypeClass::MEMORY => "MEMORY",
        };

        write!(f, "{}", class_str)
    }
}

pub const X86_SYSTEMV_ABI_TWO_INTEGERS: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_ONE_INTEGER: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::INTEGER,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_F32_F64: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::SSE,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_F128: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::SSE,
    X86SystemVABITypeClass::SSEUP,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

pub const X86_SYSTEM_V_ABI_STACK: [X86SystemVABITypeClass; 8] = [
    X86SystemVABITypeClass::MEMORY,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
    X86SystemVABITypeClass::NO_CLASS,
];

// https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig#L412

impl X86SystemVABITypeClass {}

impl X86SystemVABITypeClass {
    pub fn combine(
        accum: X86SystemVABITypeClass,
        current: X86SystemVABITypeClass,
    ) -> X86SystemVABITypeClass {
        if accum == current {
            return accum;
        }

        if accum == X86SystemVABITypeClass::NO_CLASS {
            return current;
        }

        if accum == X86SystemVABITypeClass::MEMORY || current == X86SystemVABITypeClass::MEMORY {
            return X86SystemVABITypeClass::MEMORY;
        }

        if accum == X86SystemVABITypeClass::INTEGER || current == X86SystemVABITypeClass::INTEGER {
            return X86SystemVABITypeClass::INTEGER;
        }

        X86SystemVABITypeClass::SSE
    }

    pub fn get_system_v_type_class<'system_v>(
        abi_context: &mut X86SystemVABIContext<'system_v>,
        ty: &Type,
    ) -> [X86SystemVABITypeClass; 8] {
        let type_layout: either::Either<
            thrustc_typesystem::type_layout::TypeLayout,
            thrustc_typesystem::type_layout::StructTypeLayout,
        > = abi_context.get_mut_target_info().get_type_layout(ty);

        let layout: thrustc_typesystem::type_layout::Layout = match type_layout {
            either::Either::Left(ty) => ty.into_layout(),
            either::Either::Right(ty) => ty.into_layout(),
        };

        match ty {
            Type::Const(subtype, ..) => Self::get_system_v_type_class(abi_context, subtype),

            Type::U8 { .. }
            | Type::U16 { .. }
            | Type::U32 { .. }
            | Type::U64 { .. }
            | Type::S8 { .. }
            | Type::S16 { .. }
            | Type::S32 { .. }
            | Type::S64 { .. }
            | Type::Char(..)
            | Type::Bool(..) => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::SSize { .. } | Type::USize { .. }
                if layout.alignof == 8 || layout.alignof == 4 =>
            {
                X86_SYSTEM_V_ABI_ONE_INTEGER
            }

            Type::SSize { .. } | Type::USize { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::U128 { .. } => X86_SYSTEMV_ABI_TWO_INTEGERS,

            Type::F32 { .. } | Type::F64 { .. } => X86_SYSTEM_V_ABI_F32_F64,

            Type::F128 { .. } => X86_SYSTEM_V_ABI_F128,

            t if t.is_ptr_like_type() => X86_SYSTEM_V_ABI_ONE_INTEGER,

            Type::FixedArray(..) => {
                let abi_size: u32 = layout.abi_size;

                if abi_size <= 8 {
                    X86_SYSTEM_V_ABI_ONE_INTEGER
                } else if abi_size <= 16 {
                    X86_SYSTEMV_ABI_TWO_INTEGERS
                } else {
                    X86_SYSTEM_V_ABI_STACK
                }
            }

            Type::Struct { fields, .. } => {
                let abi_size: u32 = layout.abi_size;

                if abi_size > 64 {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                let mut current_classes: [X86SystemVABITypeClass; 8] =
                    [X86SystemVABITypeClass::NO_CLASS; 8];

                for (i, field_type) in fields.iter().enumerate() {
                    let field_offset_bytes: u32 = layout.field_offsets[i] / 8;

                    let field_classes: [X86SystemVABITypeClass; 8] =
                        Self::get_system_v_type_class(abi_context, field_type);

                    for (sub_idx, _) in field_classes.iter().enumerate() {
                        let field_class: X86SystemVABITypeClass = field_classes[sub_idx];

                        if matches!(field_class, X86SystemVABITypeClass::NO_CLASS) {
                            continue;
                        }

                        let target_eightbyte_idx: u32 = (field_offset_bytes / 8) + sub_idx as u32;

                        if target_eightbyte_idx < 8 {
                            current_classes[target_eightbyte_idx as usize] = Self::combine(
                                current_classes[target_eightbyte_idx as usize],
                                field_class,
                            );
                        }
                    }
                }

                for (idx, _) in current_classes.iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::MEMORY) {
                        return X86_SYSTEM_V_ABI_STACK;
                    }
                }

                // https://github.com/ziglang/zig/blob/738d2be9d6b6ef3ff3559130c05159ef53336224/src/codegen/x86_64/abi.zig
                /*

                   "If the size of the aggregate exceeds two eightbytes and the first eight-
                    byte isn’t SSE or any other eightbyte isn’t SSEUP, the whole argument
                    is passed in memory."

                */
                if abi_size > 16
                    && (current_classes
                        .first()
                        .is_some_and(|c| !matches!(c, X86SystemVABITypeClass::SSE))
                        || current_classes
                            .get(1)
                            .iter()
                            .any(|c| !matches!(c, X86SystemVABITypeClass::SSEUP)))
                {
                    return X86_SYSTEM_V_ABI_STACK;
                }

                for (idx, _) in current_classes.clone().iter().enumerate() {
                    if matches!(current_classes[idx], X86SystemVABITypeClass::SSEUP) && idx > 0 {
                        match current_classes[idx - 1] {
                            X86SystemVABITypeClass::SSE | X86SystemVABITypeClass::SSEUP => {
                                continue;
                            }
                            _ => {
                                current_classes[idx] = X86SystemVABITypeClass::SSE;
                            }
                        }
                    }
                }

                current_classes
            }

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum x86SytemVABIType {
    Same(Type),
    ToMemory(Type),
    Decompose(Type),
    DecomposeAndExpand(Vec<Type>),
    Ignore,
}

impl x86SytemVABIType {
    pub fn class_to_abi_strategy(
        classes: &[X86SystemVABITypeClass; 8],
        ty: Type,
    ) -> x86SytemVABIType {
        if classes.contains(&X86SystemVABITypeClass::MEMORY) {
            return x86SytemVABIType::ToMemory(ty);
        }

        let used: usize = classes
            .iter()
            .take_while(|&&c| c != X86SystemVABITypeClass::NO_CLASS)
            .count();

        if used == 0 {
            return x86SytemVABIType::Ignore;
        }

        match used {
            1 => match classes[0] {
                X86SystemVABITypeClass::INTEGER | X86SystemVABITypeClass::SSE => {
                    x86SytemVABIType::Same(ty)
                }
                _ => x86SytemVABIType::Same(ty),
            },

            2 => match (classes[0], classes[1]) {
                (X86SystemVABITypeClass::INTEGER, X86SystemVABITypeClass::INTEGER) => {
                    x86SytemVABIType::Same(ty)
                }

                (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSE) => {
                    x86SytemVABIType::Same(ty)
                }

                (X86SystemVABITypeClass::SSE, X86SystemVABITypeClass::SSEUP) => {
                    x86SytemVABIType::Same(ty)
                }

                _ => {
                    if let Type::Struct { fields, .. } = &ty {
                        x86SytemVABIType::DecomposeAndExpand(fields.clone())
                    } else {
                        x86SytemVABIType::DecomposeAndExpand(vec![ty])
                    }
                }
            },

            _ => x86SytemVABIType::ToMemory(ty),
        }
    }

    fn is_valid_sse_sequence(classes: &[X86SystemVABITypeClass; 8]) -> bool {
        let mut seen_sse: bool = false;

        for &c in classes {
            match c {
                X86SystemVABITypeClass::SSE | X86SystemVABITypeClass::SSEUP => seen_sse = true,
                X86SystemVABITypeClass::NO_CLASS => continue,
                _ if seen_sse => return false,
                _ => {}
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use thrustc_span::Span;
    use thrustc_typesystem::type_modificators::StructureTypeModificator;

    use super::*;

    #[test]
    fn test_x86_system_v_abi_classification() {
        let mut target_info: TargetInfo =
            TargetInfo::new(thrustc_llvm_target_triple::LLVMTargetTriple::new(
                "x86_64-unknown-linux-gnu".to_string(),
            ));

        let mut abi_context: X86SystemVABIContext = X86SystemVABIContext::new(&mut target_info);

        let struct_type: Type = Type::Struct {
            name: "".into(),
            fields: vec![
                Type::U64 {
                    span: Span::nothing(),
                },
                Type::F64 {
                    span: Span::nothing(),
                },
            ],
            modifier: StructureTypeModificator::default(),
            span: Span::nothing(),
        };

        let class: [X86SystemVABITypeClass; 8] =
            X86SystemVABITypeClass::get_system_v_type_class(&mut abi_context, &struct_type);

        let ty: x86SytemVABIType = x86SytemVABIType::class_to_abi_strategy(&class, struct_type);

        println!("{:?}", ty);
    }
}
