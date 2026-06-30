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

use std::path::PathBuf;

#[derive(Debug)]
pub struct LinkerConfiguration {
    linux_configuration: LinkerLinuxConfiguration,

    build_executable: bool,
    build_dynamic_library: bool,
    build_static_library: bool,
    build_relocatable_object: bool,
    extra_library_paths: Vec<PathBuf>,
    link_libraries: Vec<String>,
    debug_command: bool,
    link_dynamic: bool,
    link_static: bool,
    ansi_colors: bool,
    entry: String,
    output: String,
}

impl LinkerConfiguration {
    pub fn new() -> Self {
        Self {
            linux_configuration: LinkerLinuxConfiguration::new(),

            extra_library_paths: Vec::with_capacity(u8::MAX as usize),
            link_libraries: Vec::with_capacity(u8::MAX as usize),
            build_executable: true,
            build_dynamic_library: false,
            build_static_library: false,
            build_relocatable_object: false,
            debug_command: false,
            link_dynamic: true,
            link_static: false,
            ansi_colors: false,
            entry: "main".into(),
            output: String::new(),
        }
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn get_mut_linux_configuration(&mut self) -> &mut LinkerLinuxConfiguration {
        &mut self.linux_configuration
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn add_library_path(&mut self, path: PathBuf) {
        self.extra_library_paths.push(path);
    }

    #[inline]
    pub fn add_link_library(&mut self, prefix: String) {
        self.link_libraries.push(prefix);
    }

    #[inline]
    pub fn set_build_executable(&mut self, value: bool) {
        self.build_executable = value;
    }

    #[inline]
    pub fn set_debug_linker_command(&mut self, value: bool) {
        self.debug_command = value
    }

    #[inline]
    pub fn set_entry(&mut self, entry: String) {
        self.entry = entry
    }

    #[inline]
    pub fn set_output(&mut self, output: String) {
        self.output = output
    }

    #[inline]
    pub fn set_link_static(&mut self, value: bool) {
        self.link_static = value
    }

    #[inline]
    pub fn set_link_dynamic(&mut self, value: bool) {
        self.link_dynamic = value;
    }

    #[inline]
    pub fn set_use_ansi_colors(&mut self) {
        self.ansi_colors = true;
    }

    #[inline]
    pub fn set_build_dynamic_library(&mut self) {
        self.build_dynamic_library = true;
        self.build_static_library = false;
        self.build_executable = false;
        self.build_relocatable_object = false;
    }

    #[inline]
    pub fn set_build_static_library(&mut self) {
        self.build_static_library = true;
        self.build_dynamic_library = false;
        self.build_executable = false;
        self.build_relocatable_object = false;
    }

    #[inline]
    pub fn set_build_relocatable_object(&mut self) {
        self.build_relocatable_object = true;
        self.build_dynamic_library = false;
        self.build_static_library = false;
        self.build_executable = false;
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn get_libraries_paths(&self) -> &[PathBuf] {
        &self.extra_library_paths
    }

    #[inline]
    pub fn get_link_libraries(&self) -> &[String] {
        &self.link_libraries
    }

    #[inline]
    pub fn get_linux_configuration(&self) -> LinkerLinuxConfiguration {
        self.linux_configuration
    }
}

impl LinkerConfiguration {
    #[inline]
    pub fn entry(&self) -> &str {
        &self.entry
    }

    #[inline]
    pub fn build_executable(&self) -> bool {
        self.build_executable
    }

    #[inline]
    pub fn build_dynamic_library(&self) -> bool {
        self.build_dynamic_library
    }

    #[inline]
    pub fn build_static_library(&self) -> bool {
        self.build_static_library
    }

    #[inline]
    pub fn build_relocatable_object(&self) -> bool {
        self.build_relocatable_object
    }

    #[inline]
    pub fn debug_command(&self) -> bool {
        self.debug_command
    }

    #[inline]
    pub fn output(&self) -> &str {
        &self.output
    }

    #[inline]
    pub fn link_dynamic(&self) -> bool {
        self.link_dynamic
    }

    #[inline]
    pub fn link_static(&self) -> bool {
        self.link_static
    }

    #[inline]
    pub fn use_ansi_colors(&self) -> bool {
        self.ansi_colors
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LinkerLinuxConfiguration {
    emulation: LinuxEmulationVariant,
    lto_optimization: LinuxLTOOptimization,
}

impl LinkerLinuxConfiguration {
    pub fn new() -> Self {
        Self {
            emulation: LinuxEmulationVariant::ElfX86_64,
            lto_optimization: LinuxLTOOptimization::OO,
        }
    }
}

impl LinkerLinuxConfiguration {
    #[inline]
    pub fn get_emulation(&self) -> LinuxEmulationVariant {
        self.emulation
    }

    #[inline]
    pub fn get_lto_optimization(&self) -> LinuxLTOOptimization {
        self.lto_optimization
    }
}

impl LinkerLinuxConfiguration {
    #[inline]
    pub fn set_emulation(&mut self, emulation: LinuxEmulationVariant) {
        self.emulation = emulation
    }

    #[inline]
    pub fn set_lto_optimization(&mut self, optimization: LinuxLTOOptimization) {
        self.lto_optimization = optimization
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LinuxLTOOptimization {
    OO,
    O1,
    O2,
    O3,
}

impl From<&str> for LinuxLTOOptimization {
    fn from(s: &str) -> Self {
        match s {
            "1" => LinuxLTOOptimization::O1,
            "2" => LinuxLTOOptimization::O2,
            "3" => LinuxLTOOptimization::O3,

            _ => LinuxLTOOptimization::OO,
        }
    }
}

impl From<LinuxLTOOptimization> for String {
    fn from(variant: LinuxLTOOptimization) -> Self {
        match variant {
            LinuxLTOOptimization::O1 => "1".to_string(),
            LinuxLTOOptimization::O2 => "2".to_string(),
            LinuxLTOOptimization::O3 => "3".to_string(),

            _ => "0".to_string(),
        }
    }
}

impl From<LinuxLTOOptimization> for &'static str {
    fn from(variant: LinuxLTOOptimization) -> Self {
        match variant {
            LinuxLTOOptimization::O1 => "1",
            LinuxLTOOptimization::O2 => "2",
            LinuxLTOOptimization::O3 => "3",

            _ => "0",
        }
    }
}

// https://github.com/llvm/llvm-project/blob/llvmorg-17.0.6/lld/ELF/Driver.cpp#L163

#[derive(Debug, Clone, Copy)]
pub enum LinuxEmulationVariant {
    Aarch64elf,
    Aarch64linux,
    Aarch64elfb,
    Aarch64linuxb,
    Armelf,
    ArmelfLinuxEabi,
    Armelfb,
    ArmelfbLinuxEabi,
    Elf32X86_64,
    Elf32btsmip,
    Elf32btsmipn32,
    Elf32ltsmip,
    Elf32ltsmipn32,
    Elf32lriscv,
    Elf32ppc,
    Elf32ppclinux,
    Elf32lppc,
    Elf32lppclinux,
    Elf32loongarch,
    Elf64loongarch,
    Elf64btsmip,
    Elf64ltsmip,
    Elf64lriscv,
    Elf64ppc,
    Elf64lppc,
    ElfX86_64,
    ElfI386,
    ElfIamcu,
    Elf64Sparc,
    Msp430elf,
    Elf64Amdgpu,
}

impl From<&str> for LinuxEmulationVariant {
    fn from(s: &str) -> Self {
        match s {
            "aarch64elf" => LinuxEmulationVariant::Aarch64elf,
            "aarch64linux" => LinuxEmulationVariant::Aarch64linux,
            "aarch64elfb" => LinuxEmulationVariant::Aarch64elfb,
            "aarch64linuxb" => LinuxEmulationVariant::Aarch64linuxb,
            "armelf" => LinuxEmulationVariant::Armelf,
            "armelf_linux_eabi" => LinuxEmulationVariant::ArmelfLinuxEabi,
            "armelfb" => LinuxEmulationVariant::Armelfb,
            "armelfb_linux_eabi" => LinuxEmulationVariant::ArmelfbLinuxEabi,
            "elf32_x86_64" => LinuxEmulationVariant::Elf32X86_64,
            "elf32btsmip" => LinuxEmulationVariant::Elf32btsmip,
            "elf32btsmipn32" => LinuxEmulationVariant::Elf32btsmipn32,
            "elf32ltsmip" => LinuxEmulationVariant::Elf32ltsmip,
            "elf32ltsmipn32" => LinuxEmulationVariant::Elf32ltsmipn32,
            "elf32lriscv" => LinuxEmulationVariant::Elf32lriscv,
            "elf64ppc" => LinuxEmulationVariant::Elf64ppc,
            "elf64lppc" => LinuxEmulationVariant::Elf64lppc,
            "elf32ppc" => LinuxEmulationVariant::Elf32ppc,
            "elf32ppclinux" => LinuxEmulationVariant::Elf32ppclinux,
            "elf32lppc" => LinuxEmulationVariant::Elf32lppc,
            "elf32lppclinux" => LinuxEmulationVariant::Elf32lppclinux,
            "elf32loongarch" => LinuxEmulationVariant::Elf32loongarch,
            "elf64loongarch" => LinuxEmulationVariant::Elf64loongarch,
            "elf64btsmip" => LinuxEmulationVariant::Elf64btsmip,
            "elf64ltsmip" => LinuxEmulationVariant::Elf64ltsmip,
            "elf64lriscv" => LinuxEmulationVariant::Elf64lriscv,
            "elf_i386" => LinuxEmulationVariant::ElfI386,
            "elf_iamcu" => LinuxEmulationVariant::ElfIamcu,
            "elf64_sparc" => LinuxEmulationVariant::Elf64Sparc,
            "msp430elf" => LinuxEmulationVariant::Msp430elf,
            "elf64_amdgpu" => LinuxEmulationVariant::Elf64Amdgpu,
            _ => LinuxEmulationVariant::ElfX86_64,
        }
    }
}

impl From<LinuxEmulationVariant> for &'static str {
    fn from(variant: LinuxEmulationVariant) -> Self {
        match variant {
            LinuxEmulationVariant::Aarch64elf => "aarch64elf",
            LinuxEmulationVariant::Aarch64linux => "aarch64linux",
            LinuxEmulationVariant::Aarch64elfb => "aarch64elfb",
            LinuxEmulationVariant::Aarch64linuxb => "aarch64linuxb",
            LinuxEmulationVariant::Armelf => "armelf",
            LinuxEmulationVariant::ArmelfLinuxEabi => "armelf_linux_eabi",
            LinuxEmulationVariant::Armelfb => "armelfb",
            LinuxEmulationVariant::ArmelfbLinuxEabi => "armelfb_linux_eabi",
            LinuxEmulationVariant::Elf32X86_64 => "elf32_x86_64",
            LinuxEmulationVariant::Elf32btsmip => "elf32btsmip",
            LinuxEmulationVariant::Elf32btsmipn32 => "elf32btsmipn32",
            LinuxEmulationVariant::Elf32ltsmip => "elf32ltsmip",
            LinuxEmulationVariant::Elf32ltsmipn32 => "elf32ltsmipn32",
            LinuxEmulationVariant::Elf32lriscv => "elf32lriscv",
            LinuxEmulationVariant::Elf64ppc => "elf64ppc",
            LinuxEmulationVariant::Elf64lppc => "elf64lppc",
            LinuxEmulationVariant::Elf32ppc => "elf32ppc",
            LinuxEmulationVariant::Elf32ppclinux => "elf32ppclinux",
            LinuxEmulationVariant::Elf32lppc => "elf32lppc",
            LinuxEmulationVariant::Elf32lppclinux => "elf32lppclinux",
            LinuxEmulationVariant::Elf32loongarch => "elf32loongarch",
            LinuxEmulationVariant::Elf64loongarch => "elf64loongarch",
            LinuxEmulationVariant::Elf64btsmip => "elf64btsmip",
            LinuxEmulationVariant::Elf64ltsmip => "elf64ltsmip",
            LinuxEmulationVariant::Elf64lriscv => "elf64lriscv",
            LinuxEmulationVariant::ElfX86_64 => "elf32_x86_64",
            LinuxEmulationVariant::ElfI386 => "elf_i386",
            LinuxEmulationVariant::ElfIamcu => "elf_iamcu",
            LinuxEmulationVariant::Elf64Sparc => "elf64_sparc",
            LinuxEmulationVariant::Msp430elf => "msp430elf",
            LinuxEmulationVariant::Elf64Amdgpu => "elf64_amdgpu",
        }
    }
}

impl From<LinuxEmulationVariant> for String {
    fn from(variant: LinuxEmulationVariant) -> Self {
        match variant {
            LinuxEmulationVariant::Aarch64elf => "aarch64elf".to_string(),
            LinuxEmulationVariant::Aarch64linux => "aarch64linux".to_string(),
            LinuxEmulationVariant::Aarch64elfb => "aarch64elfb".to_string(),
            LinuxEmulationVariant::Aarch64linuxb => "aarch64linuxb".to_string(),
            LinuxEmulationVariant::Armelf => "armelf".to_string(),
            LinuxEmulationVariant::ArmelfLinuxEabi => "armelf_linux_eabi".to_string(),
            LinuxEmulationVariant::Armelfb => "armelfb".to_string(),
            LinuxEmulationVariant::ArmelfbLinuxEabi => "armelfb_linux_eabi".to_string(),
            LinuxEmulationVariant::Elf32X86_64 => "elf32_x86_64".to_string(),
            LinuxEmulationVariant::Elf32btsmip => "elf32btsmip".to_string(),
            LinuxEmulationVariant::Elf32btsmipn32 => "elf32btsmipn32".to_string(),
            LinuxEmulationVariant::Elf32ltsmip => "elf32ltsmip".to_string(),
            LinuxEmulationVariant::Elf32ltsmipn32 => "elf32ltsmipn32".to_string(),
            LinuxEmulationVariant::Elf32lriscv => "elf32lriscv".to_string(),
            LinuxEmulationVariant::Elf64ppc => "elf64ppc".to_string(),
            LinuxEmulationVariant::Elf64lppc => "elf64lppc".to_string(),
            LinuxEmulationVariant::Elf32ppc => "elf32ppc".to_string(),
            LinuxEmulationVariant::Elf32ppclinux => "elf32ppclinux".to_string(),
            LinuxEmulationVariant::Elf32lppc => "elf32lppc".to_string(),
            LinuxEmulationVariant::Elf32lppclinux => "elf32lppclinux".to_string(),
            LinuxEmulationVariant::Elf32loongarch => "elf32loongarch".to_string(),
            LinuxEmulationVariant::Elf64loongarch => "elf64loongarch".to_string(),
            LinuxEmulationVariant::Elf64btsmip => "elf64btsmip".to_string(),
            LinuxEmulationVariant::Elf64ltsmip => "elf64ltsmip".to_string(),
            LinuxEmulationVariant::Elf64lriscv => "elf64lriscv".to_string(),
            LinuxEmulationVariant::ElfX86_64 => "elf32_x86_64".to_string(),
            LinuxEmulationVariant::ElfI386 => "elf_i386".to_string(),
            LinuxEmulationVariant::ElfIamcu => "elf_iamcu".to_string(),
            LinuxEmulationVariant::Elf64Sparc => "elf64_sparc".to_string(),
            LinuxEmulationVariant::Msp430elf => "msp430elf".to_string(),
            LinuxEmulationVariant::Elf64Amdgpu => "elf64_amdgpu".to_string(),
        }
    }
}
