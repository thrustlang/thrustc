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

use serde_json::Value;
use thrustc_backends::CompilerFeaturesMode;
use thrustc_options::CompilerOptions;

#[derive(Clone, Debug)]
pub struct LspConfiguration {
    mode: CompilerFeaturesMode,
    cpu: String,
    cpu_features: String,
}

impl Default for LspConfiguration {
    fn default() -> Self {
        Self {
            mode: CompilerFeaturesMode::Stable,
            cpu: String::new(),
            cpu_features: String::new(),
        }
    }
}

impl LspConfiguration {
    pub fn from_initialize_params(payload: &Value) -> Self {
        let Some(options) = payload
            .get("params")
            .and_then(|params| params.get("initializationOptions"))
        else {
            return Self::default();
        };

        let mode: CompilerFeaturesMode = match options.get("mode").and_then(Value::as_str) {
            Some("unstable") => CompilerFeaturesMode::Unstable,
            _ => CompilerFeaturesMode::Stable,
        };
        let cpu: String = options
            .get("cpu")
            .and_then(Value::as_str)
            .map(str::trim)
            .unwrap_or_default()
            .to_string();
        let cpu_features: String = options
            .get("cpuFeatures")
            .and_then(Value::as_str)
            .map(str::trim)
            .unwrap_or_default()
            .to_string();

        Self {
            mode,
            cpu,
            cpu_features,
        }
    }

    pub fn build_compiler_options(&self) -> CompilerOptions {
        let mut options: CompilerOptions = CompilerOptions::new();

        options.set_compiler_feature_mode(self.mode);

        if !self.cpu.is_empty() {
            options
                .get_mut_llvm_backend()
                .get_mut_target_cpu()
                .set_cpu_name(self.cpu.clone());
        }

        options
            .get_mut_llvm_backend()
            .get_mut_target_cpu()
            .set_processador_features(self.cpu_features.clone());

        options
    }

    #[inline]
    pub fn get_mode(&self) -> CompilerFeaturesMode {
        self.mode
    }

    #[inline]
    pub fn get_cpu(&self) -> &str {
        &self.cpu
    }

    #[inline]
    pub fn get_cpu_features(&self) -> &str {
        &self.cpu_features
    }
}

#[cfg(test)]
mod tests {
    use super::LspConfiguration;
    use serde_json::Value;

    #[test]
    fn reads_mode_cpu_and_cpu_features() {
        let payload: Value = serde_json::json!({
            "params": {
                "initializationOptions": {
                    "mode": "unstable",
                    "cpu": "haswell",
                    "cpuFeatures": "+sse2,+avx"
                }
            }
        });
        let configuration: LspConfiguration = LspConfiguration::from_initialize_params(&payload);

        assert!(configuration.get_mode().is_unstable_mode());
        assert_eq!(configuration.get_cpu(), "haswell");
        assert_eq!(configuration.get_cpu_features(), "+sse2,+avx");
    }

    #[test]
    fn builds_compiler_options_with_cpu_and_features() {
        let payload: Value = serde_json::json!({
            "params": {
                "initializationOptions": {
                    "cpu": "haswell",
                    "cpuFeatures": "+sse2,+avx"
                }
            }
        });
        let configuration: LspConfiguration = LspConfiguration::from_initialize_params(&payload);
        let options: thrustc_options::CompilerOptions = configuration.build_compiler_options();

        assert_eq!(
            options.get_llvm_backend().get_target_cpu().get_cpu_name(),
            "haswell"
        );
        assert_eq!(
            options
                .get_llvm_backend()
                .get_target_cpu()
                .get_cpu_features(),
            "+sse2,+avx"
        );
    }
}
