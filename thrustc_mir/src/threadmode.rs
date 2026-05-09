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

#![allow(clippy::enum_variant_names)]

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize)]
pub enum ThrustThreadMode {
    GeneralDynamicTLSModel,
    LocalDynamicTLSModel,
    InitialExecTLSModel,
    LocalExecTLSModel,
}

impl ThrustThreadMode {
    #[inline]
    pub fn as_llvm_threadmode(&self) -> inkwell::ThreadLocalMode {
        match self {
            ThrustThreadMode::GeneralDynamicTLSModel => {
                inkwell::ThreadLocalMode::GeneralDynamicTLSModel
            }
            ThrustThreadMode::LocalDynamicTLSModel => {
                inkwell::ThreadLocalMode::LocalDynamicTLSModel
            }
            ThrustThreadMode::InitialExecTLSModel => inkwell::ThreadLocalMode::InitialExecTLSModel,
            ThrustThreadMode::LocalExecTLSModel => inkwell::ThreadLocalMode::LocalExecTLSModel,
        }
    }
}
