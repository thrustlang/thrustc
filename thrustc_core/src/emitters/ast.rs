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

use std::fs::File;
use std::io::Write as WriteIO;
use std::path::Path;

use crate::Ast;

pub fn to_file_pretty(
    ast: &[Ast],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base: std::path::PathBuf = build_dir.join("emit").join("ast");
    std::fs::create_dir_all(&base)?;

    let path: std::path::PathBuf = base.join(format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    ));

    let file: File = std::fs::File::create(path)?;
    let mut writer: std::io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer_pretty(&mut writer, ast)?;
    writer.flush()?;

    Ok(())
}

pub fn to_file(
    ast: &[Ast],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base: std::path::PathBuf = build_dir.join("emit").join("ast");
    std::fs::create_dir_all(&base)?;

    let path: std::path::PathBuf = base.join(format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    ));

    let file: File = std::fs::File::create(path)?;
    let mut writer: std::io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer(&mut writer, ast)?;
    writer.flush()?;

    Ok(())
}
