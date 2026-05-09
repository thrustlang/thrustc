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

use serde::Serialize;
use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct CastingMetadata {
    is_constant: bool,
    is_allocated: bool,
}

impl CastingMetadata {
    #[inline]
    pub fn new(is_constant: bool, is_allocated: bool) -> Self {
        Self {
            is_constant,
            is_allocated,
        }
    }
}

impl CastingMetadata {
    #[inline]
    pub fn is_constant(&self) -> bool {
        self.is_constant
    }

    #[inline]
    pub fn is_allocated(&self) -> bool {
        self.is_allocated
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct FunctionParameterMetadata {
    is_mutable: bool,
}

impl FunctionParameterMetadata {
    #[inline]
    pub fn new(is_mutable: bool) -> Self {
        Self { is_mutable }
    }
}

impl FunctionParameterMetadata {
    #[inline]
    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct PropertyMetadata {
    is_allocated: bool,
}

impl PropertyMetadata {
    #[inline]
    pub fn new(is_allocated: bool) -> Self {
        Self { is_allocated }
    }
}

impl PropertyMetadata {
    #[inline]
    pub fn is_allocated(&self) -> bool {
        self.is_allocated
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct ReferenceMetadata {
    is_allocated: bool,
    is_mutable: bool,
    reference_type: ReferenceType,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Default, Serialize)]
pub enum ReferenceType {
    Constant,
    Static,

    #[default]
    None,
}

impl ReferenceMetadata {
    #[inline]
    pub fn new(is_allocated: bool, is_mutable: bool, reference_type: ReferenceType) -> Self {
        Self {
            is_allocated,
            is_mutable,
            reference_type,
        }
    }
}

impl ReferenceMetadata {
    #[inline]
    pub fn is_allocated(&self) -> bool {
        self.is_allocated
    }

    #[inline]
    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

impl ReferenceMetadata {
    #[inline]
    pub fn is_constant_ref(&self) -> bool {
        matches!(self.reference_type, ReferenceType::Constant)
    }

    #[inline]
    pub fn is_static_ref(&self) -> bool {
        matches!(self.reference_type, ReferenceType::Static)
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct ConstantMetadata {
    is_global: bool,

    llvm_metadata: LLVMConstantMetadata,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct LLVMConstantMetadata {
    pub thread_local: bool,
    pub volatile: bool,
    pub atomic_ord: Option<ThrustAtomicOrdering>,
}

impl ConstantMetadata {
    #[inline]
    pub fn new(
        is_global: bool,
        thread_local: bool,
        volatile: bool,
        atomic_ord: Option<ThrustAtomicOrdering>,
    ) -> Self {
        Self {
            is_global,

            llvm_metadata: LLVMConstantMetadata {
                thread_local,
                volatile,
                atomic_ord,
            },
        }
    }
}

impl ConstantMetadata {
    #[inline]
    pub fn is_global(&self) -> bool {
        self.is_global
    }

    #[inline]
    pub fn get_llvm_metadata(&self) -> LLVMConstantMetadata {
        self.llvm_metadata
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Default, Serialize)]
pub struct DereferenceMetadata {
    llvm_metadata: LLVMDereferenceMetadata,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Default, Serialize)]
pub struct LLVMDereferenceMetadata {
    pub volatile: bool,
    pub atomic_ord: Option<ThrustAtomicOrdering>,
}

impl DereferenceMetadata {
    #[inline]
    pub fn new(is_volatile: bool, atomic_ord: Option<ThrustAtomicOrdering>) -> Self {
        Self {
            llvm_metadata: LLVMDereferenceMetadata {
                volatile: is_volatile,
                atomic_ord,
            },
        }
    }
}

impl DereferenceMetadata {
    #[inline]
    pub fn get_llvm_metadata(&self) -> LLVMDereferenceMetadata {
        self.llvm_metadata
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct LocalMetadata {
    is_undefined: bool,
    is_mutable: bool,

    llvm_metadata: LLVMLocalMetadata,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct LLVMLocalMetadata {
    pub volatile: bool,
    pub atomic_ord: Option<ThrustAtomicOrdering>,
}

impl LocalMetadata {
    #[inline]
    pub fn new(
        is_undefined: bool,
        is_mutable: bool,
        volatile: bool,
        atomic_ord: Option<ThrustAtomicOrdering>,
    ) -> Self {
        Self {
            is_undefined,
            is_mutable,

            llvm_metadata: LLVMLocalMetadata {
                volatile,
                atomic_ord,
            },
        }
    }
}

impl LocalMetadata {
    #[inline]
    pub fn is_undefined(&self) -> bool {
        self.is_undefined
    }

    #[inline]
    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }

    #[inline]
    pub fn get_llvm_metadata(&self) -> LLVMLocalMetadata {
        self.llvm_metadata
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct StaticMetadata {
    is_global: bool,
    is_mutable: bool,
    is_unitialized: bool,

    llvm_metadata: LLVMStaticMetadata,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub struct LLVMStaticMetadata {
    pub unnamed_addr: bool,
    pub constant: bool,
    pub thread_local: bool,
    pub thread_mode: Option<ThrustThreadMode>,
    pub volatile: bool,
    pub atomic_ord: Option<ThrustAtomicOrdering>,
}

impl StaticMetadata {
    #[allow(clippy::too_many_arguments)]
    #[inline]
    pub fn new(
        is_global: bool,
        is_mutable: bool,
        is_unitialized: bool,
        thread_local: bool,
        volatile: bool,
        external: bool,
        atomic_ord: Option<ThrustAtomicOrdering>,
        thread_mode: Option<ThrustThreadMode>,
    ) -> Self {
        Self {
            is_global,
            is_mutable,
            is_unitialized,

            llvm_metadata: LLVMStaticMetadata {
                unnamed_addr: !is_mutable && !external,
                constant: !is_mutable,
                thread_local,
                thread_mode,
                volatile,
                atomic_ord,
            },
        }
    }
}

impl StaticMetadata {
    #[inline]
    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }

    #[inline]
    pub fn is_unitialized(&self) -> bool {
        self.is_unitialized
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        self.is_global
    }

    #[inline]
    pub fn get_llvm_metadata(&self) -> LLVMStaticMetadata {
        self.llvm_metadata
    }
}
