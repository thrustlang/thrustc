use std::env;
use std::ffi::OsStr;
use std::io::{self};
use std::path::{Path, PathBuf};
use std::process::Command;

lazy_static::lazy_static! {
    static ref LLVM_CONFIG_PATH: PathBuf = {
        let abort_with_message = |msg: &str| -> ! {
            panic!("{}", msg);
        };

        let sys_user_home: PathBuf = if cfg!(target_os = "linux") {
            env::var("HOME")
                .map(PathBuf::from)
                .unwrap_or_else(|_| abort_with_message("Could not determine user's HOME directory."))
        } else if cfg!(target_os = "windows") {
            env::var("APPDATA")
                .map(PathBuf::from)
                .unwrap_or_else(|_| abort_with_message("Could not determine user's APPDATA directory."))
        } else {
            abort_with_message("Unsupported OS for fetching the LLVM config binary. This build script only supports Linux and Windows.");
        };

        let llvm_build_path: PathBuf = sys_user_home.join(".thrushlang/backends/llvm/build");

        if !llvm_build_path.exists() {
            abort_with_message(&format!(
                "No LLVM build folder found at '{}'; ensure the compiler installer was used first.",
                llvm_build_path.display()
            ));
        }

        let llvm_config_extension: &str = if cfg!(target_os = "windows") { ".exe" } else { "" };
        let llvm_config_path: PathBuf = llvm_build_path.join(format!("bin/llvm-config{}", llvm_config_extension));

        if !llvm_config_path.exists() {
            abort_with_message(&format!(
                "LLVM config binary not found at '{}'. Please check your LLVM installation.",
                llvm_config_path.display()
            ));
        }

        llvm_config_path
    };
}

#[inline]
fn llvm_config(arg: &str) -> String {
    llvm_config_ex(&*LLVM_CONFIG_PATH, arg).expect("Surprising failure from llvm-config.")
}

fn llvm_config_ex<S: AsRef<OsStr>>(binary: S, arg: &str) -> io::Result<String> {
    Command::new(binary)
        .arg(arg)
        .arg("--link-static")
        .arg("core")
        .output()
        .and_then(|output| {
            if output.stdout.is_empty() {
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "llvm-config returned empty output",
                ))
            } else {
                Ok(String::from_utf8(output.stdout)
                    .expect("Output from llvm-config was not valid UTF-8."))
            }
        })
}
fn get_system_libraries() -> Vec<String> {
    self::llvm_config("--system-libs")
        .split_whitespace() 
        .filter(|s| !s.is_empty())
        .filter(|s| !s.starts_with('/'))
        .map(|flag| {
            if self::target_env_is("msvc") {
                flag.strip_suffix(".lib")
                    .expect("MSVC system library does not end with '.lib'.")
                    .to_owned()
            } else {
                flag.strip_prefix("-l")
                    .unwrap_or_else(|| {
                        let maybe_lib_path: &Path = Path::new(&flag);

                        if maybe_lib_path.is_file() {
                            println!(
                                "cargo:rustc-link-search=native={}",
                                maybe_lib_path.parent().unwrap_or_else(|| Path::new("")).display()
                            );

                            let soname: &str = maybe_lib_path
                                .file_name()
                                .expect("Shared library path has no file name.")
                                .to_str()
                                .expect("Shared library path is not valid UTF-8.");

                            let stem = soname
                                .rsplit_once(self::get_target_dylib_extension())
                                .expect("Shared library should have a recognized extension (.so or .dylib).")
                                .0;

                            stem.trim_start_matches("lib")
                        } else {
                            panic!(
                                "Unable to parse result of llvm-config --system-libs: unexpected flag '{}'.",
                                flag
                            );
                        }
                    }).to_owned()
            }
        })
        .chain(self::get_system_libcpp().map(str::to_owned))
        .collect::<Vec<String>>()
}

fn get_link_libraries() -> Vec<String> {
    self::llvm_config("--libnames")
        .split_whitespace()
        .filter(|s| !s.is_empty())
        .map(|name| {
            if self::target_env_is("msvc") {
                name.strip_suffix(".lib")
                    .expect("library name does not appear to be a MSVC library file.")
                    .to_owned()
            } else {
                name.strip_prefix("lib")
                    .and_then(|s| s.strip_suffix(".a"))
                    .expect("library name does not appear to be a static library (libNAME.a).")
                    .to_owned()
            }
        })
        .collect::<Vec<String>>()
}

fn get_llvm_cxxflags() -> String {
    let output: String = llvm_config("--cxxflags");

    if self::target_env_is("msvc") {
        return output;
    }

    self::llvm_config("--cxxflags")
        .split(&[' ', '\n'][..])
        .filter(|word| !word.starts_with("-W"))
        .collect::<Vec<_>>()
        .join(" ")
}

#[inline]
fn get_target_dylib_extension() -> &'static str {
    ".so"
}

#[inline]
fn get_system_libcpp() -> Option<&'static str> {
    if self::target_env_is("msvc") {
        return None;
    }

    Some("stdc++")
}

#[inline]
fn target_env_is(name: &str) -> bool {
    env::var_os("CARGO_CFG_TARGET_ENV").is_some_and(|s| s == name)
}

#[inline]
fn is_llvm_debug() -> bool {
    self::llvm_config("--build-mode").contains("Debug")
}

fn main() {
    unsafe { std::env::set_var("CXXFLAGS", get_llvm_cxxflags()) };

    let mut build: cc::Build = cc::Build::new();

    build.cpp(true).file("wrapper/lld.cpp");

    if build.get_compiler().is_like_msvc() {
        build.flag("/std:c++17");
    } else {
        build.flag("-std=c++17");
    }

    build.compile("lldwrapper");

    println!("cargo:rerun-if-changed=wrapper/lld.cpp");

    let libdir: String = self::llvm_config("--libdir");

    println!("cargo:config_path={}", LLVM_CONFIG_PATH.display());
    println!("cargo:libdir={}", libdir);
    println!("cargo:rustc-link-search=native={}", libdir);

    self::get_link_libraries()
        .iter()
        .filter(|lib| !lib.contains("LLVMLineEditor"))
        .for_each(|libname| {
            println!("cargo:rustc-link-lib=static={}", libname);
        });

    self::get_system_libraries().iter().for_each(|libname| {
        println!("cargo:rustc-link-lib=dylib={}", libname);
    });

    if cfg!(target_env = "msvc") && self::is_llvm_debug() {
        println!("cargo:rustc-link-lib=msvcrtd");
    }

    println!("cargo:rustc-link-lib=static=lldCOFF");
    println!("cargo:rustc-link-lib=static=lldCommon");
    println!("cargo:rustc-link-lib=static=lldELF");
    println!("cargo:rustc-link-lib=static=lldMachO");
    println!("cargo:rustc-link-lib=static=lldWasm");

    if cfg!(not(target_os = "windows")) {
        println!("cargo:rustc-link-lib=dylib=ffi");
    }
}
