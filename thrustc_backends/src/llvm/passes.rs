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

use thrustc_logging::{self, LoggingType};

#[derive(Debug, Clone, Copy)]
pub enum LLVMModificatorPasses {
    LoopVectorization,
    LoopUnroll,
    LoopInterleaving,
    LoopSimplifyVectorization,
    MergeFunctions,
    CallGraphProfile,
    ForgetAllScevInLoopUnroll,
    LicmMssaNoAccForPromotionCap(u32),
    LicmMssaOptCap(u32),
}

impl LLVMModificatorPasses {
    pub fn into_llvm_modificator_passes(raw: &str) -> Vec<LLVMModificatorPasses> {
        let mut passes: Vec<LLVMModificatorPasses> = Vec::with_capacity(10);

        for pass in raw.split(';') {
            let pass = pass.trim();
            if pass.is_empty() {
                continue;
            }

            if let Some((key, value)) = pass.split_once('=') {
                let key: String = key.trim().to_lowercase();
                let value: &str = value.trim();

                match key.as_str() {
                    "licmmssaaccpromcap" => match value.parse::<u32>() {
                        Ok(cap) => {
                            passes.push(LLVMModificatorPasses::LicmMssaNoAccForPromotionCap(cap))
                        }
                        Err(_) => thrustc_logging::print_warning(
                            LoggingType::Warning,
                            &format!(
                                "Invalid cap value for LicmMssaNoAccForPromotionCap: '{}'",
                                value
                            ),
                        ),
                    },

                    "licmmssaoptcap" => match value.parse::<u32>() {
                        Ok(cap) => passes.push(LLVMModificatorPasses::LicmMssaOptCap(cap)),
                        Err(_) => thrustc_logging::print_warning(
                            LoggingType::Warning,
                            &format!("Invalid cap value for LicmMssaOptCap: '{}'", value),
                        ),
                    },

                    _ => thrustc_logging::print_warning(
                        LoggingType::Warning,
                        &format!(
                            "Unknown parameterized LLVM optimization pass: '{}={}'",
                            key, value
                        ),
                    ),
                }
            } else {
                match pass.to_lowercase().as_str() {
                    "loopvectorization" => passes.push(LLVMModificatorPasses::LoopVectorization),
                    "loopunroll" => passes.push(LLVMModificatorPasses::LoopUnroll),
                    "loopinterleaving" => passes.push(LLVMModificatorPasses::LoopInterleaving),
                    "loopsimplifyvectorization" => {
                        passes.push(LLVMModificatorPasses::LoopSimplifyVectorization)
                    }
                    "mergefunctions" => passes.push(LLVMModificatorPasses::MergeFunctions),
                    "callgraphprofile" => passes.push(LLVMModificatorPasses::CallGraphProfile),
                    "forgetallscevinloopunroll" => {
                        passes.push(LLVMModificatorPasses::ForgetAllScevInLoopUnroll)
                    }

                    _ => thrustc_logging::print_warning(
                        LoggingType::Warning,
                        &format!("Unknown LLVM modificator pass: '{}'", pass),
                    ),
                }
            }
        }

        passes
    }
}
