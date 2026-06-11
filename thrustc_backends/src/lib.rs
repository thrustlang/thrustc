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

use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, RelocMode},
};

pub mod llvm;

#[derive(Default, Debug, Clone, Copy)]
pub enum ThrustOptimization {
    #[default]
    None,
    Low,
    Mid,
    High,
    Size,
    Zize,
}

impl ThrustOptimization {
    #[inline]
    pub fn to_llvm_opt(self) -> OptimizationLevel {
        match self {
            ThrustOptimization::None => OptimizationLevel::None,
            ThrustOptimization::Low => OptimizationLevel::Default,
            ThrustOptimization::Mid | ThrustOptimization::Size | ThrustOptimization::Zize => {
                OptimizationLevel::Less
            }
            ThrustOptimization::High => OptimizationLevel::Aggressive,
        }
    }

    #[inline]
    pub fn is_high_opt(self) -> bool {
        matches!(
            self,
            ThrustOptimization::Low
                | ThrustOptimization::Mid
                | ThrustOptimization::High
                | ThrustOptimization::Size
                | ThrustOptimization::Zize
        )
    }

    #[inline]
    pub fn is_none_opt(&self) -> bool {
        matches!(self, ThrustOptimization::None)
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub enum ThrustRelocMode {
    Default,
    Static,

    #[default]
    PIC,
    DynamicNoPic,
}

impl ThrustRelocMode {
    #[inline]
    pub fn to_llvm(self) -> RelocMode {
        match self {
            ThrustRelocMode::Default => RelocMode::Default,
            ThrustRelocMode::Static => RelocMode::Static,
            ThrustRelocMode::PIC => RelocMode::PIC,
            ThrustRelocMode::DynamicNoPic => RelocMode::DynamicNoPic,
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub enum ThrustCodeModel {
    #[default]
    Default,
    JITDefault,
    Small,
    Kernel,
    Medium,
    Large,
}

impl ThrustCodeModel {
    #[inline]
    pub fn to_llvm(self) -> CodeModel {
        match self {
            ThrustCodeModel::Default => CodeModel::Default,
            ThrustCodeModel::JITDefault => CodeModel::JITDefault,
            ThrustCodeModel::Small => CodeModel::Small,
            ThrustCodeModel::Kernel => CodeModel::Kernel,
            ThrustCodeModel::Medium => CodeModel::Medium,
            ThrustCodeModel::Large => CodeModel::Large,
        }
    }
}
