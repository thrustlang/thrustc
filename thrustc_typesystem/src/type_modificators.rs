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

#![allow(dead_code)]

use std::fmt::Display;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct FunctionReferenceTypeModificator {
    llvm: LLVMFunctionReferenceTypeModificator,
    gcc: GCCFunctionReferenceTypeModificator,
}

impl FunctionReferenceTypeModificator {
    #[inline]
    pub fn new(
        llvm: LLVMFunctionReferenceTypeModificator,
        gcc: GCCFunctionReferenceTypeModificator,
    ) -> Self {
        Self { llvm, gcc }
    }
}

impl FunctionReferenceTypeModificator {
    #[inline]
    pub fn llvm(&self) -> &LLVMFunctionReferenceTypeModificator {
        &self.llvm
    }

    #[inline]
    pub fn gcc(&self) -> &GCCFunctionReferenceTypeModificator {
        &self.gcc
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct LLVMFunctionReferenceTypeModificator {
    ignore_args: bool,
}

impl LLVMFunctionReferenceTypeModificator {
    #[inline]
    pub fn new(ignore_args: bool) -> Self {
        Self { ignore_args }
    }

    #[inline]
    pub fn has_ignore(&self) -> bool {
        self.ignore_args
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct GCCFunctionReferenceTypeModificator {}

impl GCCFunctionReferenceTypeModificator {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct StructureTypeModificator {
    llvm: LLVMStructureTypeModificator,
    gcc: GCCStructureTypeModificator,
}

impl StructureTypeModificator {
    #[inline]
    pub fn new(llvm: LLVMStructureTypeModificator, gcc: GCCStructureTypeModificator) -> Self {
        Self { llvm, gcc }
    }

    #[inline]
    pub fn llvm(&self) -> &LLVMStructureTypeModificator {
        &self.llvm
    }

    #[inline]
    pub fn gcc(&self) -> &GCCStructureTypeModificator {
        &self.gcc
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct GCCStructureTypeModificator {}

impl GCCStructureTypeModificator {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize)]
pub struct LLVMStructureTypeModificator {
    packed: bool,
}

impl LLVMStructureTypeModificator {
    #[inline]
    pub fn new(packed: bool) -> Self {
        Self { packed }
    }

    #[inline]
    pub fn is_packed(&self) -> bool {
        self.packed
    }
}

impl Display for StructureTypeModificator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.llvm.packed {
            write!(f, "@packed;")?;
        }

        Ok(())
    }
}

impl Display for FunctionReferenceTypeModificator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.llvm.ignore_args {
            write!(f, "@ignore;")?;
        }

        Ok(())
    }
}
