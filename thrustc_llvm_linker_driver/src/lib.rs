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

use std::fs;
use std::path::Path;
use std::path::PathBuf;

use thrustc_backends::llvm::LLVMBackend;
use thrustc_backends::llvm::linker::LinkerConfiguration;
use thrustc_llvm_target_triple::LLVMTargetTriple;
use thrustc_options::CompilerOptions;

#[derive(Debug)]
pub struct LLVMLinkerWrapper<'lld> {
    compiler_options: &'lld CompilerOptions,
    object_files: &'lld [PathBuf],
    target_triple: &'lld LLVMTargetTriple,
    flags_builder: FlagsBuilder,
}

#[derive(Debug, Clone, Copy)]
pub enum LinuxCRuntimeVariant {
    GLIBC,
    MUSL,
}

impl LinuxCRuntimeVariant {
    pub fn get_dir_system_representation(&self) -> &str {
        match self {
            LinuxCRuntimeVariant::GLIBC => "",
            LinuxCRuntimeVariant::MUSL => "musl",
        }
    }

    pub fn get_contrary_system_representations(&self) -> Vec<&str> {
        match self {
            LinuxCRuntimeVariant::GLIBC => {
                vec![LinuxCRuntimeVariant::MUSL.get_dir_system_representation()]
            }
            LinuxCRuntimeVariant::MUSL => {
                vec![LinuxCRuntimeVariant::GLIBC.get_dir_system_representation()]
            }
        }
    }
}

impl<'lld> LLVMLinkerWrapper<'lld> {
    #[inline]
    pub fn new(
        compiler_options: &'lld CompilerOptions,
        object_files: &'lld [PathBuf],
        target_triple: &'lld LLVMTargetTriple,
    ) -> Self {
        Self {
            compiler_options,
            object_files,
            target_triple,
            flags_builder: FlagsBuilder::new(),
        }
    }
}

impl LLVMLinkerWrapper<'_> {
    pub fn generate_command_line(&mut self) {
        let is_linux: bool = self.target_triple.is_linux_based();
        let is_macos: bool = self.target_triple.is_apple_based();
        let is_windows: bool = self.target_triple.is_windows_based();

        let llvm_backend: &LLVMBackend = self.compiler_options.get_llvm_backend();
        let linker_config: &LinkerConfiguration = llvm_backend.get_linker_config();

        let extra_library_path: &[PathBuf] = linker_config.get_libraries_paths();
        let extra_libarary_path_displayed: Vec<String> = extra_library_path
            .iter()
            .map(|library_path| format!("{}", library_path.display()))
            .collect();

        let mut link_libraries: Vec<String> = linker_config.get_link_libraries().to_vec();

        let mut library_paths: Vec<String> = if is_linux {
            get_default_linux_library_paths()
        } else if is_macos {
            get_default_macos_library_paths()
        } else if is_windows {
            get_default_windows_library_paths()
        } else {
            vec![]
        }
        .iter()
        .map(|path| path.to_string())
        .collect();

        library_paths.extend(extra_libarary_path_displayed);

        let builder: &mut FlagsBuilder = &mut self.flags_builder;

        // https://man.archlinux.org/man/extra/lld/ld.lld.1

        if is_linux {
            // must edit via -link-musl
            let cruntime_variant: LinuxCRuntimeVariant = LinuxCRuntimeVariant::GLIBC;

            builder.add_flag_without_value("--eh-frame-hdr".into());

            if linker_config.link_dynamic() {
                builder.add_flag_without_value("--pie".into());

                let dynamic_linker: PathBuf = self::find_dynamic_linker(&library_paths)
                    .unwrap_or_else(|| {
                        thrustc_logging::print_critical_error(
                            thrustc_logging::LoggingType::Error,
                            "Unable to find dynamic linux linker for the Linker invocation on Linux!",
                        )
                    });

                builder.add_eq_flag(
                    "--dynamic-linker".into(),
                    format!("{}", dynamic_linker.display()),
                );
            }

            if linker_config.link_static() {}

            /*
                ld.lld \
                /usr/lib/x86_64-linux-gnu/crt1.o \
                /usr/lib/x86_64-linux-gnu/crti.o \
                main.o \
                -L/usr/lib/x86_64-linux-gnu \
                -lc \
                /usr/lib/x86_64-linux-gnu/crtn.o \
                -o my_program
            */

            let mut link_c_runtime_end: bool = false;
            let mut crtn_path: PathBuf = PathBuf::new();

            if linker_config.build_executable() {
                let executable_cruntime_libraries: (PathBuf, PathBuf, PathBuf) =
                    self::find_c_runtime_objects_linux(&library_paths, cruntime_variant)
                        .unwrap_or_else(|| {
                            thrustc_logging::print_critical_error(
                                thrustc_logging::LoggingType::Error,
                                "Unable to find C runtime libraries for the Linker on Linux!",
                            )
                        });

                let crt1: PathBuf = executable_cruntime_libraries.0;
                let crti: PathBuf = executable_cruntime_libraries.1;
                let crtn: PathBuf = executable_cruntime_libraries.2;

                builder.add_flag_without_value(format!("{}", crt1.display()));
                builder.add_flag_without_value(format!("{}", crti.display()));

                for library in self.object_files.iter() {
                    builder.add_flag_without_value(format!("{}", library.display()));
                }

                link_c_runtime_end = true;
                crtn_path = crtn;
            }

            if !link_c_runtime_end {
                for library in self.object_files.iter() {
                    builder.add_flag_without_value(format!("{}", library.display()));
                }
            }

            let is_64_bit: bool = self.target_triple.is_64_bit();

            let libgcc_dir: PathBuf = self::find_libgcc_linux(&library_paths, is_64_bit)
                .unwrap_or_else(|| {
                    thrustc_logging::print_critical_error(
                        thrustc_logging::LoggingType::Error,
                        "Unable to find libgcc for the Linker on Linux!",
                    )
                });

            builder.add_side_by_side_flag("-L".into(), format!("{}", libgcc_dir.display()));

            for library_path in std::mem::take(&mut library_paths) {
                builder.add_side_by_side_flag("-L".into(), library_path);
            }

            if linker_config.link_dynamic() {
                builder.add_flag_without_value("-Bdynamic".into());
            }

            if linker_config.link_static() {
                builder.add_flag_without_value("-Bstatic".into());
            }

            builder.add_side_by_side_flag("-l".into(), "c".into());

            if linker_config.link_dynamic() {
                builder.add_flag_without_value("-Bdynamic".into());
            }

            if linker_config.link_static() {
                builder.add_flag_without_value("-Bstatic".into());
            }

            builder.add_side_by_side_flag("-l".into(), "gcc".into());

            if linker_config.link_dynamic() {
                builder.add_flag_without_value("-Bdynamic".into());
            }

            if linker_config.link_static() {
                builder.add_flag_without_value("-Bstatic".into());
            }

            builder.add_side_by_side_flag("-l".into(), "gcc_eh".into());

            for library_path in std::mem::take(&mut link_libraries) {
                if linker_config.link_dynamic() {
                    builder.add_flag_without_value("-Bdynamic".into());
                } else if linker_config.link_static() {
                    builder.add_flag_without_value("-Bstatic".into());
                }

                builder.add_side_by_side_flag("-l".into(), library_path);
            }

            if link_c_runtime_end {
                builder.add_flag_without_value(format!("{}", crtn_path.display()));
            }

            if !linker_config.output().is_empty() {
                builder.add_space_flag("-o".into(), linker_config.output().into());
            }

            if linker_config.debug_command() {
                builder.print_command();
            }
        }

        // https://www.manpagez.com/man/1/ld64/

        if is_macos {
            /*
               ld64.lld \
               -arch arm64 \
               -platform_version macos 13.0 13.0 \
               -syslibroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk \
               -L/usr/local/lib \
               -L/opt/homebrew/lib \
               -lSystem \
               -lmylib \
               -o my_program \
               main.o
            */

            for library_path in std::mem::take(&mut library_paths) {
                builder.add_side_by_side_flag("-L".into(), library_path);
            }

            builder.add_side_by_side_flag("-l".into(), "System".into());

            for library_path in std::mem::take(&mut link_libraries) {
                builder.add_side_by_side_flag("-l".into(), library_path);
            }

            if linker_config.debug_command() {
                builder.print_command();
            }
        }

        // https://learn.microsoft.com/en-us/cpp/build/reference/linker-options?view=msvc-170

        if is_windows {
            /*
               lld-link \
               /out:my_program.exe \
               /entry:mainCRTStartup \
               /subsystem:console \
               /libpath:"C:/mylibs" \
               /libpath:"C:/otherlibs" \
               /defaultlib:libcmt \
               /defaultlib:mylib \
               main.obj
            */

            if linker_config.build_executable() {
                builder.add_colon_flag("/entry".into(), "mainCRTStartup".into());
            }

            builder.add_flag_without_value("/build-id".into());

            for library_path in std::mem::take(&mut library_paths) {
                builder.add_colon_flag("/libpath".into(), library_path);
            }

            builder.add_colon_flag("/defaultlib".into(), "libcmt".into());

            for library_path in std::mem::take(&mut link_libraries) {
                builder.add_colon_flag("/defaultlib".into(), library_path);
            }

            if linker_config.debug_command() {
                builder.print_command();
            }
        }
    }

    pub fn link(&self) {}
}

#[derive(Debug, Clone)]
pub struct FlagsBuilder {
    flags: Vec<String>,
}

impl FlagsBuilder {
    #[inline]
    pub fn new() -> Self {
        Self {
            flags: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl FlagsBuilder {
    #[inline]
    pub fn add_flag_without_value(&mut self, flag: String) {
        self.flags.push(flag);
    }

    #[inline]
    pub fn add_side_by_side_flag(&mut self, prefix: String, flag_content: String) {
        self.flags.push(format!("{}{}", prefix, flag_content));
    }

    #[inline]
    pub fn add_eq_flag(&mut self, prefix: String, flag_content: String) {
        self.flags.push(format!("{}={}", prefix, flag_content));
    }

    #[inline]
    pub fn add_space_flag(&mut self, prefix: String, flag_content: String) {
        self.flags.push(format!("{} {}", prefix, flag_content));
    }

    #[inline]
    pub fn add_colon_flag(&mut self, prefix: String, flag_content: String) {
        self.flags.push(format!("{}:{}", prefix, flag_content));
    }
}

impl FlagsBuilder {
    #[inline]
    pub fn print_command(&self) {
        thrustc_logging::print_debug(
            thrustc_logging::LoggingType::Debug,
            &format!("Linker command: {:?}", self.flags),
        );
    }
}

pub fn find_c_runtime_objects_linux(
    library_paths: &[String],
    runtime_variant: LinuxCRuntimeVariant,
) -> Option<(PathBuf, PathBuf, PathBuf)> {
    let mut crt1: Option<PathBuf> = None;
    let mut crti: Option<PathBuf> = None;
    let mut crtn: Option<PathBuf> = None;

    for base in library_paths {
        self::search_dir(
            Path::new(base),
            &mut crt1,
            &mut crti,
            &mut crtn,
            runtime_variant,
        );

        if crt1.is_some() && crti.is_some() && crtn.is_some() {
            break;
        }
    }

    match (crt1, crti, crtn) {
        (Some(crt1), Some(crti), Some(crtn)) => Some((crt1, crti, crtn)),
        _ => None,
    }
}

fn search_dir(
    dir: &Path,
    crt1: &mut Option<PathBuf>,
    crti: &mut Option<PathBuf>,
    crtn: &mut Option<PathBuf>,
    runtime_variant: LinuxCRuntimeVariant,
) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        let path: PathBuf = entry.path();
        let path_str: String = path.display().to_string();

        if runtime_variant
            .get_contrary_system_representations()
            .iter()
            .any(|contrario| path_str.contains(contrario))
        {
            continue;
        }

        if path.is_dir() {
            search_dir(&path, crt1, crti, crtn, runtime_variant);
        } else if path.is_file() {
            match path.file_name().and_then(|n| n.to_str()) {
                Some("crt1.o") if crt1.is_none() => *crt1 = Some(path),
                Some("crti.o") if crti.is_none() => *crti = Some(path),
                Some("crtn.o") if crtn.is_none() => *crtn = Some(path),

                _ => {}
            }
        }

        if crt1.is_some() && crti.is_some() && crtn.is_some() {
            return;
        }
    }
}

pub fn find_libgcc_linux(library_paths: &[String], is_64_bit: bool) -> Option<PathBuf> {
    for base in library_paths {
        if let Some(path) = search_libgcc(Path::new(base), is_64_bit) {
            return Some(path);
        }
    }
    None
}

fn search_libgcc(dir: &Path, is_64_bit: bool) -> Option<PathBuf> {
    let Ok(entries) = fs::read_dir(dir) else {
        return None;
    };

    for entry in entries.flatten() {
        let path: PathBuf = entry.path();
        let name: &str = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        if is_64_bit && name == "32" {
            continue;
        }

        let Ok(meta) = fs::metadata(&path) else {
            continue;
        };

        if meta.is_dir() {
            if !is_64_bit && name == "32" {
                return search_libgcc(&path, is_64_bit);
            }

            if let Some(found) = search_libgcc(&path, is_64_bit) {
                return Some(found);
            }
        } else if meta.is_file() {
            let name: &str = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name == "libgcc.a" || name == "libgcc.so" || name.starts_with("libgcc.so.") {
                return path.parent().map(|p| p.to_path_buf());
            }
        }
    }
    None
}

pub fn find_dynamic_linker(library_paths: &[String]) -> Option<PathBuf> {
    for base in library_paths {
        if let Some(found) = search_dynamic_linker(Path::new(base)) {
            return Some(found);
        }
    }

    None
}

fn search_dynamic_linker(dir: &Path) -> Option<PathBuf> {
    let Ok(entries) = fs::read_dir(dir) else {
        return None;
    };

    for entry in entries.flatten() {
        let path = entry.path();

        if path.is_dir() {
            if let Some(found) = search_dynamic_linker(&path) {
                return Some(found);
            }
        } else if path.is_file() || path.is_symlink() {
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            if is_dynamic_linker(name) {
                return Some(path);
            }
        }
    }

    None
}

fn is_dynamic_linker(name: &str) -> bool {
    matches!(
        name,
        "ld-linux-x86-64.so.2"
            | "ld-linux.so.2"
            | "ld-linux-aarch64.so.1"
            | "ld-linux-armhf.so.3"
            | "ld-linux-arm.so.3"
    )
}

#[inline]
pub fn get_default_linux_library_paths() -> Vec<&'static str> {
    vec![
        "/usr/lib",
        "/usr/local/lib",
        "/lib",
        "/usr/lib64",
        "/usr/local/lib64",
        "/lib64",
    ]
}

#[inline]
pub fn get_default_macos_library_paths() -> Vec<&'static str> {
    vec![
        "/usr/lib",
        "/usr/local/lib",
        "/lib",
        "/System/Library/Frameworks",
        "/Library/Frameworks",
    ]
}

#[inline]
pub fn get_default_windows_library_paths() -> Vec<&'static str> {
    vec!["C:/Windows/System32"]
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use thrustc_llvm_target_triple::LLVMTargetTriple;
    use thrustc_options::CompilerOptions;

    use crate::LLVMLinkerWrapper;

    #[test]
    fn test_linux_driver() {
        let mut compiler_options: CompilerOptions = CompilerOptions::new();

        compiler_options
            .get_mut_llvm_backend()
            .get_mut_linker_config()
            .set_debug_linker_command(true);

        compiler_options
            .get_mut_llvm_backend()
            .get_mut_linker_config()
            .set_output("test.o".into());

        let target_triple: &LLVMTargetTriple = &LLVMTargetTriple::generate_default_from_llvm();

        let files: &[PathBuf] = &["path/to/test".into()];

        let mut linker_wrapper: LLVMLinkerWrapper<'_> =
            LLVMLinkerWrapper::new(&compiler_options, files, target_triple);

        linker_wrapper.generate_command_line();
    }
}
