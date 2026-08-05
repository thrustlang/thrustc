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

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use chrono::Local;
use serde::{Deserialize, Serialize};

pub const BACKLOG_ROOT: &str = "fuzz/backlog";
pub const LOG_ROOT: &str = "fuzz/fuzz_continuous";

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IssueMeta {
    pub id: String,
    pub target: String,
    pub mode: String,
    pub hash: String,
    pub size: u64,
    pub discovered_at: String,
    pub marker: String,
    pub status: String,
    pub input: String,
    pub ast: String,
    pub ir: String,
}

pub struct IssueFiles<'a> {
    pub input: &'a [u8],
    pub ast: Option<&'a str>,
    pub ir: Option<&'a str>,
    pub ir_error: Option<&'a str>,
}

pub fn content_hash(data: &[u8]) -> String {
    let mut hash: u64 = 0xcbf29ce484222325;

    for byte in data {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }

    format!("{hash:016x}")
}

pub fn issue_id_for(data: &[u8]) -> String {
    format!("issue_{}", content_hash(data))
}

pub fn backlog_dir() -> PathBuf {
    current_dir_or_default().join(BACKLOG_ROOT)
}

pub fn log_root() -> PathBuf {
    current_dir_or_default().join(LOG_ROOT)
}

pub fn target_dir(target: &str) -> PathBuf {
    backlog_dir().join(target)
}

pub fn issue_dir(target: &str, id: &str) -> PathBuf {
    target_dir(target).join(id)
}

pub fn meta_path(target: &str, id: &str) -> PathBuf {
    issue_dir(target, id).join("meta.json")
}

pub fn log_path(target: &str) -> PathBuf {
    log_root().join(format!("{target}.log"))
}

fn current_dir_or_default() -> PathBuf {
    std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
}

pub fn ensure_dirs() -> Result<(), String> {
    fs::create_dir_all(backlog_dir())
        .and_then(|_| fs::create_dir_all(log_root()))
        .map_err(|e| format!("failed to create backlog directories: {e}"))
}

fn timestamp() -> String {
    Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

pub fn load_meta(target: &str, id: &str) -> Option<IssueMeta> {
    let path = meta_path(target, id);
    let contents = fs::read_to_string(&path).ok()?;

    let mut meta: IssueMeta = serde_json::from_str(&contents).ok()?;

    let input = absolutize(&meta.input);
    let ast = absolutize(&meta.ast);
    let ir = absolutize(&meta.ir);

    if input != meta.input || ast != meta.ast || ir != meta.ir {
        meta.input = input;
        meta.ast = ast;
        meta.ir = ir;

        if let Ok(json) = serde_json::to_string_pretty(&meta) {
            let _ = fs::write(&path, json);
        }
    }

    Some(meta)
}

fn absolutize(path: &str) -> String {
    if path == "-" {
        return path.to_string();
    }

    let p = PathBuf::from(path);

    if p.is_absolute() {
        return p.to_string_lossy().to_string();
    }

    match std::env::current_dir() {
        Ok(cwd) => cwd.join(p).to_string_lossy().to_string(),
        Err(_) => path.to_string(),
    }
}

pub fn all_issues(target: &str) -> Vec<IssueMeta> {
    let dir = target_dir(target);

    let Ok(entries) = fs::read_dir(&dir) else {
        return Vec::new();
    };

    let mut issues: Vec<IssueMeta> = entries
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_dir())
        .filter_map(|entry| {
            let id = entry.file_name().to_string_lossy().to_string();
            load_meta(target, &id)
        })
        .collect();

    issues.sort_by(|a, b| {
        (a.discovered_at.as_str(), a.id.as_str()).cmp(&(b.discovered_at.as_str(), b.id.as_str()))
    });

    issues
}

pub fn known_hashes(target: &str) -> HashSet<String> {
    all_issues(target)
        .into_iter()
        .map(|meta| meta.hash)
        .collect()
}

pub fn status_of(target: &str, id: &str) -> Option<String> {
    load_meta(target, id).map(|meta| meta.status)
}

pub fn record_issue(
    target: &str,
    mode: &str,
    data: &[u8],
    marker: Option<&str>,
    files: IssueFiles,
) -> Result<IssueMeta, String> {
    ensure_dirs()?;

    let id = issue_id_for(data);
    let dir = issue_dir(target, &id);

    fs::create_dir_all(&dir).map_err(|e| format!("failed to create issue directory: {e}"))?;

    let input_path = dir.join("input.bin");
    fs::write(&input_path, files.input).map_err(|e| format!("failed to write input.bin: {e}"))?;

    let mut ast_path = String::from("-");
    if let Some(ast) = files.ast {
        let path = dir.join("ast.txt");
        fs::write(&path, ast).map_err(|e| format!("failed to write ast.txt: {e}"))?;
        ast_path = path.to_string_lossy().to_string();
    }

    let mut ir_path = String::from("-");
    if let Some(ir) = files.ir {
        let path = dir.join("ir.ll");
        fs::write(&path, ir).map_err(|e| format!("failed to write ir.ll: {e}"))?;
        ir_path = path.to_string_lossy().to_string();
    }

    if let Some(ir_error) = files.ir_error {
        let path = dir.join("ir_error.txt");
        fs::write(&path, ir_error).map_err(|e| format!("failed to write ir_error.txt: {e}"))?;
        if ir_path == "-" {
            ir_path = path.to_string_lossy().to_string();
        }
    }

    let meta = IssueMeta {
        id,
        target: target.to_string(),
        mode: mode.to_string(),
        hash: content_hash(data),
        size: data.len() as u64,
        discovered_at: timestamp(),
        marker: marker.unwrap_or("").to_string(),
        status: "open".to_string(),
        input: input_path.to_string_lossy().to_string(),
        ast: ast_path,
        ir: ir_path,
    };

    let json = serde_json::to_string_pretty(&meta)
        .map_err(|e| format!("failed to serialize metadata: {e}"))?;

    fs::write(meta_path(target, &meta.id), json)
        .map_err(|e| format!("failed to write meta.json: {e}"))?;

    regenerate_log(target)?;

    Ok(meta)
}

pub fn set_status(target: &str, id: &str, status: &str) -> Result<IssueMeta, String> {
    let mut meta = load_meta(target, id)
        .ok_or_else(|| format!("issue '{id}' not found for target '{target}'"))?;

    meta.status = status.to_string();

    let json = serde_json::to_string_pretty(&meta)
        .map_err(|e| format!("failed to serialize metadata: {e}"))?;

    fs::write(meta_path(target, id), json)
        .map_err(|e| format!("failed to write meta.json: {e}"))?;

    regenerate_log(target)?;

    Ok(meta)
}

pub fn remove_issue(target: &str, id: &str) -> Result<(), String> {
    let dir = issue_dir(target, id);

    if dir.exists() {
        fs::remove_dir_all(&dir).map_err(|e| format!("failed to remove issue directory: {e}"))?;
    }

    regenerate_log(target)
}

pub fn regenerate_log(target: &str) -> Result<(), String> {
    ensure_dirs()?;

    let issues = all_issues(target);

    let mut contents = String::new();
    contents.push_str(&format!(
        "# cascade log for target `{target}` ({})\n",
        log_path(target).display()
    ));
    contents.push_str(&format!("# {} issue(s)\n\n", issues.len()));

    for (index, issue) in issues.iter().enumerate() {
        let marker = if issue.marker.is_empty() {
            "-".to_string()
        } else {
            issue.marker.clone()
        };

        contents.push_str(&format!("[{}] {}\n", index + 1, issue.id));
        contents.push_str(&format!("    ├─ discovered_at : {}\n", issue.discovered_at));
        contents.push_str(&format!("    ├─ hash          : {}\n", issue.hash));
        contents.push_str(&format!("    ├─ mode          : {}\n", issue.mode));
        contents.push_str(&format!("    ├─ marker        : {}\n", marker));
        contents.push_str(&format!("    ├─ status        : {}\n", issue.status));
        contents.push_str(&format!("    ├─ input_path    : {}\n", issue.input));
        contents.push_str(&format!("    ├─ ast_path      : {}\n", issue.ast));
        contents.push_str(&format!("    └─ ir_path       : {}\n", issue.ir));
        contents.push('\n');
    }

    fs::write(log_path(target), contents).map_err(|e| format!("failed to write cascade log: {e}"))
}

pub fn print_log(target: &str) -> Result<(), String> {
    let path = log_path(target);

    let contents =
        fs::read_to_string(&path).map_err(|e| format!("could not read {}: {e}", path.display()))?;

    println!("{contents}");

    Ok(())
}

pub fn print_history(targets: &[&str]) -> Result<(), String> {
    let targets = if targets.is_empty() {
        crate::dumps::TARGETS
    } else {
        targets
    };

    for target in targets {
        let path = log_path(target);

        if path.exists() {
            println!("===== {target} ({}) =====", path.display());
            print_log(target)?;
            println!();
        }
    }

    Ok(())
}

pub fn delete_issue_directory(path: &Path) {
    let _ = fs::remove_dir_all(path);
}
