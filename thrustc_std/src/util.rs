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

pub const STD_ROOT_DIR_NAME: &str = "std";
pub const STD_VERSION_FILE_NAME: &str = "VERSION.txt";

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
