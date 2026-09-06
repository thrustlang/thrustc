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

use inkwell::targets::{TargetMachine, TargetTriple};

mod impls;
pub mod traits;

#[derive(Debug, Clone)]
pub struct LLVMTargetTriple {
    arch: String,
    vendor: String,
    os: String,
    abi: String,
}

impl LLVMTargetTriple {
    #[inline]
    pub fn new(target_triple: String) -> Self {
        let triple_dissasembled: Vec<&str> = target_triple.split('-').collect();

        let arch: String = triple_dissasembled
            .first()
            .unwrap_or(&"unknown")
            .to_lowercase();

        let vendor: String = triple_dissasembled
            .get(1)
            .unwrap_or(&"unknown")
            .to_lowercase();
        let os: String = triple_dissasembled
            .get(2)
            .unwrap_or(&"unknown")
            .to_lowercase();
        let abi: String = triple_dissasembled
            .get(3)
            .unwrap_or(&"unknown")
            .to_lowercase();

        Self {
            arch,
            vendor,
            os,
            abi,
        }
    }

    pub fn generate_default_from_llvm() -> Self {
        let llvm_triple: TargetTriple = TargetMachine::get_default_triple();
        let llvm_triple_transformed: String = llvm_triple.as_str().to_string_lossy().to_string();

        let triple_dissasembled: Vec<&str> = llvm_triple_transformed.split('-').collect();

        let arch: String = triple_dissasembled
            .first()
            .unwrap_or(&"unknown")
            .to_lowercase();

        let vendor: String = triple_dissasembled
            .get(1)
            .unwrap_or(&"unknown")
            .to_lowercase();
        let os: String = triple_dissasembled
            .get(2)
            .unwrap_or(&"unknown")
            .to_lowercase();
        let abi: String = triple_dissasembled
            .get(3)
            .unwrap_or(&"unknown")
            .to_lowercase();

        Self {
            arch,
            vendor,
            os,
            abi,
        }
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_valid_llvm_target_triple_format(raw: &str) -> bool {
        let dash_count: usize = raw.chars().filter(|&c| c == '-').count();

        matches!(dash_count, 3 | 4)
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn get_abi(&self) -> &str {
        &self.abi
    }

    #[inline]
    pub fn get_arch(&self) -> &str {
        &self.arch
    }

    #[inline]
    pub fn get_os(&self) -> &str {
        &self.os
    }

    #[inline]
    pub fn get_vendor(&self) -> &str {
        &self.vendor
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn has_posix_thread_model(&self) -> bool {
        matches!(
            self.get_os(),
            "linux" | "android" | "freebsd" | "netbsd" | "openbsd"
        ) || matches!(self.get_abi(), "gnu")
    }

    #[inline]
    pub fn has_sysv_abi(&self) -> bool {
        // ref: https://llvm.org/doxygen/Triple_8cpp_source.html
        // ref: https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI

        self.is_object_format_elf()
            && matches!(
                self.arch.as_str(),
                "x86_64"
                    | "amd64"
                    | "x86"
                    | "i386"
                    | "i486"
                    | "i586"
                    | "i686"
                    | "aarch64"
                    | "aarch64_be"
                    | "riscv32"
                    | "riscv64"
                    | "riscv32be"
                    | "riscv64be"
                    | "ppc64"
                    | "ppc64le"
                    | "powerpc64"
                    | "powerpc64le"
                    | "mips"
                    | "mipsel"
                    | "mips64"
                    | "mips64el"
                    | "sparc"
                    | "sparcel"
                    | "sparcv9"
                    | "loongarch64"
                    | "loongarch32"
                    | "s390x"
                    | "systemz"
                    | "m68k"
            )
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_x86_64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "x86_64" | "amd64")
    }

    #[inline]
    pub fn is_x86_arch(&self) -> bool {
        matches!(
            self.arch.as_str(),
            "x86" | "i386" | "i486" | "i586" | "i686"
        )
    }

    #[inline]
    pub fn is_aarch64_arch(&self) -> bool {
        matches!(
            self.arch.as_str(),
            "aarch64" | "arm64" | "aarch64_32" | "aarch64_be"
        )
    }

    #[inline]
    pub fn is_riscv64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "riscv64" | "riscv64be")
    }

    #[inline]
    pub fn is_ppc64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "ppc64" | "ppc64le" | "powerpc64le")
    }

    #[inline]
    pub fn is_mips64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "mips64" | "mips64el")
    }

    #[inline]
    pub fn is_systemz_arch(&self) -> bool {
        matches!(self.arch.as_str(), "systemz" | "s390x")
    }

    #[inline]
    pub fn is_loongarch64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "loongarch64")
    }

    #[inline]
    pub fn is_wasm64_arch(&self) -> bool {
        matches!(self.arch.as_str(), "wasm64")
    }

    #[inline]
    pub fn is_avr_arch(&self) -> bool {
        self.arch.contains("avr")
    }

    #[inline]
    pub fn is_arc_arch(&self) -> bool {
        self.arch.contains("arc")
    }

    #[inline]
    pub fn is_csky_arch(&self) -> bool {
        self.arch.contains("csky")
    }

    #[inline]
    pub fn is_arm_family(&self) -> bool {
        self.arch.contains("arm") || self.arch.contains("aarch64") || self.arch.contains("thumb")
    }

    #[inline]
    pub fn is_hexagon_arch(&self) -> bool {
        self.arch.contains("hexagon")
    }

    #[inline]
    pub fn is_msp430_arch(&self) -> bool {
        self.arch.contains("msp430")
    }

    #[inline]
    pub fn is_ppc_arch(&self) -> bool {
        self.arch.contains("powerpc") || self.arch.contains("ppc")
    }

    #[inline]
    pub fn is_sparc_arch(&self) -> bool {
        self.arch.contains("sparc")
    }

    #[inline]
    pub fn is_xcore_arch(&self) -> bool {
        self.arch.contains("xcore")
    }

    #[inline]
    pub fn is_os_aix(&self) -> bool {
        self.os.contains("aix")
    }

    #[inline]
    pub fn is_64_bit(&self) -> bool {
        self.is_x86_64_arch()
            || self.is_aarch64_arch()
            || self.is_wasm64_arch()
            || self.is_riscv64_arch()
            || self.is_ppc64_arch()
            || self.is_mips64_arch()
            || self.is_systemz_arch()
            || self.is_loongarch64_arch()
            || self.arch.contains("64")
    }

    #[inline]
    pub fn is_nvptx_arch(&self) -> bool {
        self.arch.contains("nvptx") || self.arch.contains("nvidia")
    }

    #[inline]
    pub fn get_normalized(&self) -> String {
        format!("{}-{}-{}-{}", self.arch, self.vendor, self.os, self.abi)
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_object_format_mach_o(&self) -> bool {
        // https://llvm.org/doxygen/Triple_8cpp_source.html
        // https://github.com/llvm/llvm-project/blob/648193e1619f7af68230f6eddc526af542446cd8/llvm/include/llvm/TargetParser/Triple.h#L804

        let arch_ok: bool = matches!(
            self.arch.as_str(),
            "aarch64"
                | "aarch64_32"
                | "aarch64_be"
                | "arm"
                | "thumb"
                | "armeb"
                | "thumbeb"
                | "x86"
                | "x86_64"
                | "ppc"
                | "ppc64"
        );

        if !arch_ok {
            return false;
        }

        let darwin_os: bool = self.is_darwin_os();

        let macho_abi: bool = self.abi.eq_ignore_ascii_case("macho")
            || self.abi.ends_with("macho")
            || self.abi.contains("macho");

        darwin_os || macho_abi
    }

    #[inline]
    pub fn is_xcoff_object_format(&self) -> bool {
        // https://llvm.org/doxygen/Triple_8cpp_source.html
        // https://github.com/llvm/llvm-project/blob/648193e1619f7af68230f6eddc526af542446cd8/llvm/include/llvm/TargetParser/Triple.h#L804

        let arch_ok: bool = matches!(
            self.arch.as_str(),
            "powerpc" | "ppc" | "powerpc64" | "ppc64" | "powerpcle" | "ppcle"
        );

        if !arch_ok {
            return false;
        }

        let is_aix: bool = self.os.eq_ignore_ascii_case("aix")
            || self.os.starts_with("aix")
            || self.os.ends_with("aix");

        let xcoff_abi: bool = self.abi.eq_ignore_ascii_case("xcoff")
            || self.abi.ends_with("xcoff")
            || self.abi.contains("xcoff");

        is_aix || xcoff_abi
    }

    #[inline]
    pub fn is_object_format_elf(&self) -> bool {
        // https://llvm.org/doxygen/Triple_8cpp_source.html

        let arch_in_elf_list: bool = matches!(
            self.arch.as_str(),
            "aarch64"
                | "aarch64_be"
                | "aarch64_32"
                | "amdgcn"
                | "amdil"
                | "amdil64"
                | "arc"
                | "armeb"
                | "arm"
                | "thumb"
                | "thumbeb"
                | "avr"
                | "bpfeb"
                | "bpfel"
                | "csky"
                | "hexagon"
                | "hsail"
                | "hsail64"
                | "kalimba"
                | "lanai"
                | "loongarch32"
                | "loongarch64"
                | "m68k"
                | "mips"
                | "mipsel"
                | "mips64"
                | "mips64el"
                | "msp430"
                | "nvptx"
                | "nvptx64"
                | "ppc"
                | "ppcle"
                | "ppc64"
                | "ppc64le"
                | "r600"
                | "riscv32"
                | "riscv64"
                | "shave"
                | "sparc"
                | "sparcel"
                | "sparcv9"
                | "spir"
                | "spir64"
                | "s390x"
                | "systemz"
                | "tce"
                | "tcele"
                | "ve"
                | "wasm32"
                | "wasm64"
                | "x86"
                | "x86_64"
                | "xcore"
                | "xtensa"
        );

        if !arch_in_elf_list {
            return false;
        }

        if self.is_object_format_mach_o() {
            return false;
        }

        if self.is_xcoff_object_format() {
            return false;
        }

        let coff_os: bool = matches!(self.os.as_str(), "win32" | "windows" | "uefi")
            || self.os.eq_ignore_ascii_case("windows");

        if coff_os {
            return false;
        }

        let is_systemz_arch: bool = self.arch == "systemz" || self.arch == "s390x";
        let is_zos: bool = self.os.eq_ignore_ascii_case("zos") || self.os.starts_with("zos");

        if is_systemz_arch && is_zos {
            return false;
        }

        if self.arch.contains("wasm32") || self.arch.contains("wasm64") {
            return false;
        }

        if self.arch.contains("spirv") {
            return false;
        }

        if self.arch.contains("dxil") {
            return false;
        }

        let explicit_elf_abi: bool = self.abi.eq_ignore_ascii_case("elf")
            || self.abi.ends_with("elf")
            || self.abi.contains("elf");

        if explicit_elf_abi {
            return true;
        }

        true
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_darwin_os(&self) -> bool {
        // https://llvm.org/doxygen/Triple_8cpp_source.html
        // https://github.com/llvm/llvm-project/blob/648193e1619f7af68230f6eddc526af542446cd8/llvm/include/llvm/TargetParser/Triple.h#L804

        matches!(
            self.os.as_str(),
            "darwin"
                | "macosx"
                | "macos"
                | "ios"
                | "tvos"
                | "watchos"
                | "xros"
                | "bridgeos"
                | "driverkit"
        ) || self.os.contains("darwin")
            || self.os.contains("macos")
            || self.os.contains("ios")
            || self.os.contains("tvos")
            || self.os.contains("watchos")
            || self.os.contains("xros")
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_linux_based(&self) -> bool {
        let linux_os: bool = self.os.eq_ignore_ascii_case("linux")
            || self.os.starts_with("linux")
            || self.os.contains("linux");

        let linux_abi: bool = matches!(
            self.abi.as_str(),
            "gnu"
                | "gnueabi"
                | "gnueabihf"
                | "gnuabi64"
                | "musl"
                | "musleabi"
                | "musleabihf"
                | "muslabi64"
                | "android"
                | "androideabi"
                | "ohos"
        );

        let linux_arch: bool = matches!(
            self.arch.as_str(),
            "x86_64"
                | "amd64"
                | "x86"
                | "i386"
                | "i486"
                | "i586"
                | "i686"
                | "aarch64"
                | "arm64"
                | "aarch64_be"
                | "arm"
                | "armeb"
                | "thumb"
                | "thumbeb"
                | "riscv32"
                | "riscv64"
                | "riscv64be"
                | "mips"
                | "mipsel"
                | "mips64"
                | "mips64el"
                | "ppc"
                | "ppc64"
                | "ppc64le"
                | "powerpc"
                | "powerpc64"
                | "powerpc64le"
                | "s390x"
                | "systemz"
                | "loongarch64"
                | "loongarch32"
                | "m68k"
                | "sparc"
                | "sparcel"
                | "sparcv9"
        );

        linux_arch && (linux_os || linux_abi)
    }

    #[inline]
    pub fn is_apple_based(&self) -> bool {
        let apple_arch: bool = matches!(
            self.arch.as_str(),
            "aarch64" | "arm64" | "aarch64_32" | "x86_64" | "amd64"
        );

        let apple_os: bool = matches!(
            self.os.as_str(),
            "darwin"
                | "macosx"
                | "macos"
                | "ios"
                | "tvos"
                | "watchos"
                | "xros"
                | "bridgeos"
                | "driverkit"
        ) || self.os.contains("darwin")
            || self.os.contains("macos")
            || self.os.contains("ios")
            || self.os.contains("tvos")
            || self.os.contains("watchos")
            || self.os.contains("xros");

        let apple_vendor: bool = self.vendor.eq_ignore_ascii_case("apple");

        apple_vendor || (apple_arch && apple_os)
    }

    #[inline]
    pub fn is_windows_based(&self) -> bool {
        let windows_arch: bool = matches!(
            self.arch.as_str(),
            "x86_64"
                | "amd64"
                | "x86"
                | "i386"
                | "i486"
                | "i586"
                | "i686"
                | "aarch64"
                | "arm64"
                | "thumbv7a"
        );

        let windows_os: bool = matches!(self.os.as_str(), "win32" | "windows" | "win64")
            || self.os.contains("windows")
            || self.os.contains("windows")
            || self.os.contains("win32");

        let windows_abi: bool =
            matches!(self.abi.as_str(), "msvc" | "gnu" | "gnullvm" | "uwp") && windows_os;

        let windows_env: bool = self.vendor.eq_ignore_ascii_case("pc") && windows_os;

        windows_arch && (windows_os || windows_abi || windows_env)
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_32_bit(&self) -> bool {
        self.is_x86_arch()
            || matches!(
                self.arch.as_str(),
                "arm"
                    | "thumb"
                    | "thumbv7a"
                    | "armv7"
                    | "armv7s"
                    | "riscv32"
                    | "mips"
                    | "mipsel"
                    | "ppc"
                    | "powerpc"
                    | "wasm32"
                    | "loongarch32"
                    | "m68k"
                    | "avr"
                    | "msp430"
                    | "sparc"
                    | "sparcel"
            )
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_big_endian(&self) -> bool {
        if self.arch.ends_with("be") || self.arch.ends_with("eb") {
            return true;
        }

        if self.arch.ends_with("le") || self.arch.ends_with("el") {
            return false;
        }

        matches!(
            self.arch.as_str(),
            "mips"
                | "mips64"
                | "powerpc"
                | "powerpc64"
                | "ppc"
                | "ppc64"
                | "s390x"
                | "systemz"
                | "sparc"
                | "sparcv9"
                | "m68k"
        )
    }
}

impl LLVMTargetTriple {
    #[inline]
    pub fn is_coff_object_format(&self) -> bool {
        // ref: https://llvm.org/doxygen/Triple_8cpp_source.html
        matches!(self.os.as_str(), "win32" | "windows" | "win64" | "uefi")
            || self.os.eq_ignore_ascii_case("windows")
            || self.os.contains("win32")
    }
}

impl Default for LLVMTargetTriple {
    fn default() -> Self {
        Self::new("unknown-unknown-unknown-unknown".to_string())
    }
}
