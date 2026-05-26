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

use inkwell::targets::TargetTriple;
use thrustc_llvm_target_triple::LLVMTargetTriple;

#[derive(Debug)]
pub struct LLVMTarget {
    pub arch: String,
    pub target_triple: TargetTriple,
    pub normalized_target_triple: LLVMTargetTriple,
    pub target_triple_darwin_variant: Option<TargetTriple>,
    pub macos_version: Option<String>,
    pub ios_version: Option<String>,
    pub cuda_version: Option<String>,
}

impl LLVMTarget {
    #[inline]
    pub fn get_arch(&self) -> &str {
        &self.arch
    }

    #[inline]
    pub fn get_target_triple(&self) -> &TargetTriple {
        &self.target_triple
    }

    #[inline]
    pub fn get_normalized_target_triple(&self) -> &LLVMTargetTriple {
        &self.normalized_target_triple
    }

    #[inline]
    pub fn get_target_triple_darwin_variant(&self) -> Option<&TargetTriple> {
        self.target_triple_darwin_variant.as_ref()
    }
}

impl LLVMTarget {
    pub fn get_macos_version(&self) -> Option<(u64, u64, u64)> {
        let macos_version: &str = self.macos_version.as_ref()?;
        let mut split: std::str::Split<'_, char> = macos_version.split('.');

        let major: u64 = split.next()?.parse::<u64>().ok()?;
        let minor: u64 = split.next()?.parse::<u64>().ok()?;
        let patch: u64 = split.next()?.parse::<u64>().ok()?;

        Some((major, minor, patch))
    }

    pub fn get_ios_version(&self) -> Option<(u64, u64, u64)> {
        let ios_version: &str = self.ios_version.as_ref()?;
        let mut split: std::str::Split<'_, char> = ios_version.split('.');

        let major: u64 = split.next()?.parse::<u64>().ok()?;
        let minor: u64 = split.next()?.parse::<u64>().ok()?;
        let patch: u64 = split.next()?.parse::<u64>().ok()?;

        Some((major, minor, patch))
    }

    pub fn get_cuda_version(&self) -> Option<(u64, u64)> {
        let cuda_version: &str = self.cuda_version.as_ref()?;
        let mut split: std::str::Split<'_, char> = cuda_version.split('.');

        let major: u64 = split.next()?.parse::<u64>().ok()?;
        let minor: u64 = split.next()?.parse::<u64>().ok()?;

        Some((major, minor))
    }

    pub fn dissamble_target_triple(&self) -> (String, String, String, String) {
        let triple: std::borrow::Cow<'_, str> = self.target_triple.as_str().to_string_lossy();
        let mut split: std::str::Split<'_, char> = triple.split('-');

        let arch: String = split.next().unwrap_or_default().to_string();
        let vendor: String = split.next().unwrap_or_default().to_string();
        let os: String = split.next().unwrap_or_default().to_string();
        let abi: String = split.next().unwrap_or_default().to_string();

        (arch, vendor, os, abi)
    }
}

impl LLVMTarget {
    #[inline]
    pub fn set_arch(&mut self, arch: String) {
        self.arch = arch;
    }

    #[inline]
    pub fn set_target_triple(&mut self, raw_target_triple: String) {
        self.target_triple = TargetTriple::create(&raw_target_triple);
        self.normalized_target_triple =
            LLVMTargetTriple::new(self.target_triple.as_str().to_string_lossy().to_string())
    }

    #[inline]
    pub fn set_target_triple_darwin_variant(&mut self, raw_target_triple: String) {
        self.target_triple_darwin_variant = Some(TargetTriple::create(&raw_target_triple));
    }

    #[inline]
    pub fn set_nvidia_cuda_version(&mut self, version: String) {
        self.cuda_version = Some(version);
    }

    #[inline]
    pub fn set_macos_version(&mut self, version: String) {
        self.macos_version = Some(version);
    }

    #[inline]
    pub fn set_ios_version(&mut self, version: String) {
        self.ios_version = Some(version);
    }
}
