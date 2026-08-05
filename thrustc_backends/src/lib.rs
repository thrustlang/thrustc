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

use std::sync::Mutex;

use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, RelocMode},
};

use lazy_static::lazy_static;

pub mod llvm;

lazy_static! {
    pub static ref COMPILER_FEATURES: Mutex<CompilerFeaturesMode> =
        CompilerFeaturesMode::Stable.into();
}

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

#[derive(Debug, Clone, Copy, Default)]
pub enum CompilerFeaturesMode {
    #[default]
    Stable,
    Unstable,
}

impl CompilerFeaturesMode {
    #[inline]
    pub fn is_stable_mode(&self) -> bool {
        matches!(self, CompilerFeaturesMode::Stable)
    }

    #[inline]
    pub fn is_unstable_mode(&self) -> bool {
        matches!(self, CompilerFeaturesMode::Unstable)
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

pub fn get_compiler_features() -> CompilerFeaturesMode {
    *COMPILER_FEATURES.lock().unwrap_or_else(|_| {
        thrustc_logging::print_critical_error(
            thrustc_logging::LoggingType::Panic,
            "Unable to get the compiler features!",
        )
    })
}

pub fn set_compiler_features(new_mode: CompilerFeaturesMode) {
    *COMPILER_FEATURES.lock().unwrap_or_else(|_| {
        thrustc_logging::print_critical_error(
            thrustc_logging::LoggingType::Panic,
            "Unable to set the compiler features mode!",
        )
    }) = new_mode;

    if matches!(new_mode, CompilerFeaturesMode::Unstable) {
        thrustc_logging::print_warning(
            thrustc_logging::LoggingType::Warning,
            "Compiler features mode set to 'Unstable'. This may lead to unexpected behavior or unexpected panics using unstable features.",
        );
    }
}
