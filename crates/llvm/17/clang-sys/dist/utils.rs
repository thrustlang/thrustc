use crate::r#static::logging::{self, LoggingType};

pub fn get_libclang_build_path() -> std::path::PathBuf {
    match std::env::consts::FAMILY {
        "unix" => std::path::PathBuf::from(std::env::var("HOME").unwrap_or_else(|_| {
            logging::log(LoggingType::Panic, "Missing $HOME environment variable.\n");
            std::process::exit(1);
        }))
        .join(".thrustlang/backends/cbindgen/build"),

        "windows" => std::path::PathBuf::from(std::env::var("APPDATA").unwrap_or_else(|_| {
            logging::log(
                LoggingType::Panic,
                "Missing $APPDATA environment variable.\n",
            );
            std::process::exit(1);
        }))
        .join(".thrustlang/backends/cbindgen/build"),

        _ => {
            logging::log(
                LoggingType::Panic,
                "Unsupported OS for libclang dependencies.",
            );

            std::process::exit(1);
        }
    }
}

pub fn get_llvm_config_os_termination() -> std::path::PathBuf {
    match std::env::consts::FAMILY {
        "unix" => std::path::PathBuf::from("llvm-config"),
        "windows" => std::path::PathBuf::from("llvm-config.exe"),

        _ => {
            logging::log(
                LoggingType::Panic,
                "Unsupported OS for libclang dependencies.",
            );

            std::process::exit(1);
        }
    }
}
