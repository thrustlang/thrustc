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
use std::io;
use std::path::Path;
use std::path::PathBuf;

use thrustc_token::Token;

use serde_json;

pub fn to_file_pretty(
    tokens: &[Token],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base_tokens_path: PathBuf = build_dir.join("emit").join("tokens");

    std::fs::create_dir_all(&base_tokens_path)?;

    let formatted_file_name: String = format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    );

    let file_path: PathBuf = base_tokens_path.join(formatted_file_name);
    let file: File = std::fs::File::create(file_path)?;

    let writer: io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer_pretty(writer, tokens)?;

    Ok(())
}

pub fn to_file(
    tokens: &[Token],
    build_dir: &Path,
    file_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let base_tokens_path: PathBuf = build_dir.join("emit").join("tokens");

    std::fs::create_dir_all(&base_tokens_path)?;

    let formatted_file_name = format!(
        "{}_{}.json",
        thrustc_utils::generate_random_string(),
        file_name
    );

    let file_path: PathBuf = base_tokens_path.join(formatted_file_name);
    let file: File = std::fs::File::create(file_path)?;

    let writer: io::BufWriter<File> = std::io::BufWriter::new(file);

    serde_json::to_writer(writer, tokens)?;

    Ok(())
}
