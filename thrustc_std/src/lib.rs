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

use std::{
    fs::OpenOptions,
    io::Write,
    path::{Path, PathBuf},
};

use include_dir::{Dir, DirEntry, include_dir};

pub static EMBEDDED_STD_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/../std");
pub const STD_ROOT_DIR_NAME: &str = "std";
pub const STD_VERSION_FILE_NAME: &str = "VERSION.txt";

#[derive(Debug)]
pub enum StdError {
    VersionNotFound(String),
    Io(std::io::Error),
}

impl From<std::io::Error> for StdError {
    fn from(error: std::io::Error) -> Self {
        StdError::Io(error)
    }
}

#[inline]
pub fn resolve_std_root(custom: Option<&Path>) -> PathBuf {
    custom.map_or_else(self::default_std_root, |path| path.to_path_buf())
}

pub fn default_std_root() -> PathBuf {
    match std::env::consts::FAMILY {
        "unix" => PathBuf::from(std::env::var("HOME").unwrap_or_else(|_| {
            thrustc_logging::print_critical_error(
                thrustc_logging::LoggingType::Panic,
                "Missing $HOME environment variable. It is required to locate the standard library.",
            )
        }))
        .join(".thrustlang")
        .join(STD_ROOT_DIR_NAME),

        "windows" => PathBuf::from(std::env::var("APPDATA").unwrap_or_else(|_| {
            thrustc_logging::print_critical_error(
                thrustc_logging::LoggingType::Panic,
                "Missing $APPDATA environment variable. It is required to locate the standard library.",
            )
        }))
        .join(".thrustlang")
        .join(STD_ROOT_DIR_NAME),

        _ => {
            thrustc_logging::print_critical_error(
                thrustc_logging::LoggingType::Panic,
                "OS unsupported for the Thrust standard library.",
            )
        }
    }
}

#[inline]
pub fn resolve_target_version(flag: Option<&str>) -> String {
    flag.map_or_else(
        || thrustc_constants::COMPILER_VERSION.to_string(),
        |version| version.to_string(),
    )
}

pub fn ensure_std_present(root: &Path, version: &str) -> Result<PathBuf, StdError> {
    let version_dir: PathBuf = root.join(format!("v{version}"));
    let version_file: PathBuf = root.join("VERSION.txt");

    if !version_file.exists() {
        if let Some(version_file) = EMBEDDED_STD_DIR.get_file(STD_VERSION_FILE_NAME) {
            let destination: PathBuf = root.join(STD_VERSION_FILE_NAME);

            let needs_append = match std::fs::read(&destination) {
                Ok(existing) => !existing
                    .windows(version_file.contents().len())
                    .any(|window| window == version_file.contents()),
                Err(_) => true,
            };

            if needs_append {
                let mut file: std::fs::File = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&destination)?;

                file.write_all(version_file.contents())?;
            }
        }

        thrustc_logging::print_warning(
            thrustc_logging::LoggingType::Warning,
            &format!(
                "The standard library version history was not found in '{}', so the included standard library was with the version history initialized into: '{}'.\n",
                root.display(),
                version_file.display()
            ),
        );
    }

    if version_dir.is_dir() && version_dir.exists() {
        return Ok(version_dir);
    }

    if EMBEDDED_STD_DIR.get_dir(format!("v{version}")).is_some() {
        self::dump_version_std(root, version)?;

        thrustc_logging::print_warning(
            thrustc_logging::LoggingType::Warning,
            &format!(
                "The standard library version '{version}' was not found in '{}', so the included standard library was installed into: '{}'.\n",
                root.display(),
                version_dir.display()
            ),
        );
    }

    if version_dir.is_dir() && version_dir.exists() {
        Ok(version_dir)
    } else {
        Err(StdError::VersionNotFound(version.to_string()))
    }
}

pub fn validate_version(root: &Path, version: &str) -> Result<(), StdError> {
    let version_file: PathBuf = root.join(STD_VERSION_FILE_NAME);

    if !version_file.is_file() {
        return Err(StdError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "The standard library version file '{}' was not found.",
                version_file.display()
            ),
        )));
    }

    let content: String = std::fs::read_to_string(&version_file)?;

    if content.lines().any(|line| line.trim() == version) {
        Ok(())
    } else {
        Err(StdError::VersionNotFound(version.to_string()))
    }
}

fn dump_version_std(root: &Path, version: &str) -> Result<(), StdError> {
    if !root.exists() {
        std::fs::create_dir_all(root)?;
    }

    if let Some(version_file) = EMBEDDED_STD_DIR.get_file(STD_VERSION_FILE_NAME) {
        let destination: PathBuf = root.join(STD_VERSION_FILE_NAME);

        let needs_append = match std::fs::read(&destination) {
            Ok(existing) => !existing
                .windows(version_file.contents().len())
                .any(|window| window == version_file.contents()),
            Err(_) => true,
        };

        if needs_append {
            let mut file: std::fs::File = OpenOptions::new()
                .create(true)
                .append(true)
                .open(&destination)?;

            file.write_all(version_file.contents())?;
        }
    }

    if let Some(version_dir) = EMBEDDED_STD_DIR.get_dir(format!("v{version}")) {
        self::dump_dir_files(root, version_dir)?;
    }

    Ok(())
}

fn dump_dir_files(root: &Path, directory: &Dir) -> Result<(), StdError> {
    for entry in directory.entries() {
        match entry {
            DirEntry::Dir(subdirectory) => {
                self::dump_dir_files(root, subdirectory)?;
            }
            DirEntry::File(file) => {
                let destination: PathBuf = root.join(file.path());

                if destination.exists() {
                    continue;
                }

                if let Some(parent) = destination.parent() {
                    if !parent.exists() {
                        std::fs::create_dir_all(parent)?;
                    }
                }

                std::fs::write(&destination, file.contents())?;
            }
        }
    }

    Ok(())
}
