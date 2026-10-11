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

pub fn verbose_include_dirs(command: &str) -> Vec<PathBuf> {
    let mut dirs: Vec<PathBuf> = Vec::new();

    let output: std::process::Output = match std::process::Command::new(command)
        .args(["-E", "-xc", "-v", "-"])
        .stdin(std::process::Stdio::null())
        .output()
    {
        Ok(output) => output,
        Err(_) => return dirs,
    };

    let stderr: std::borrow::Cow<'_, str> = String::from_utf8_lossy(&output.stderr);

    let mut tokens: Vec<String> = Vec::new();

    for token in stderr.split_whitespace() {
        tokens.push(token.to_string());
    }

    let mut idx: usize = 0;

    while idx < tokens.len() {
        if tokens[idx] == "-internal-isystem" || tokens[idx] == "-internal-externc-isystem" {
            if let Some(dir) = tokens.get(idx.saturating_add(1)) {
                dirs.push(PathBuf::from(dir));
            }

            idx = idx.saturating_add(2);
        } else {
            idx = idx.saturating_add(1);
        }
    }

    let mut in_list: bool = false;

    for line in stderr.lines() {
        let trimmed: &str = line.trim();

        if trimmed == "#include <...> search starts here:" {
            in_list = true;
        } else if trimmed == "End of search list." {
            in_list = false;
        } else if in_list && !trimmed.is_empty() {
            dirs.push(PathBuf::from(trimmed));
        }
    }

    dirs
}

pub fn detect_host_system_include_dirs() -> Vec<PathBuf> {
    let mut collected: Vec<PathBuf> = Vec::new();

    let mut push_unique = |candidate: PathBuf| {
        let canonical: PathBuf = candidate
            .canonicalize()
            .unwrap_or_else(|_| candidate.clone());

        let already: bool = collected.iter().any(|known: &PathBuf| {
            known.canonicalize().unwrap_or_else(|_| known.clone()) == canonical
        });

        if !already && canonical.is_dir() {
            collected.push(canonical);
        }
    };

    let from_verbose: Vec<PathBuf> = self::clang_commands()
        .into_iter()
        .map(|command| self::verbose_include_dirs(&command))
        .find(|dirs| !dirs.is_empty())
        .unwrap_or_default();

    from_verbose.into_iter().for_each(&mut push_unique);

    let env_iter = ["C_INCLUDE_PATH", "CPATH", "INCLUDE"]
        .iter()
        .filter_map(std::env::var_os)
        .flat_map(|paths| {
            std::env::split_paths(&paths)
                .filter(|entry| !entry.as_os_str().is_empty())
                .collect::<Vec<PathBuf>>()
        });

    env_iter.for_each(&mut push_unique);

    let fallback_dirs: Vec<PathBuf> = if cfg!(windows) {
        Vec::new()
    } else {
        ["/usr/local/include", "/usr/include"]
            .iter()
            .map(PathBuf::from)
            .collect()
    };

    fallback_dirs.into_iter().for_each(&mut push_unique);

    collected
}

pub fn detect_clang_resource_include_dir() -> Option<PathBuf> {
    let commands: Vec<String> = self::clang_commands();

    let outputs = commands.iter().filter_map(|command| {
        std::process::Command::new(command)
            .arg("-print-resource-dir")
            .output()
            .ok()
    });

    let successful = outputs.filter(|output| output.status.success());

    let mut include_dirs = successful.filter_map(|output| {
        let resource_dir: std::borrow::Cow<'_, str> = String::from_utf8_lossy(&output.stdout);

        let trimmed: &str = resource_dir.trim();

        if trimmed.is_empty() {
            None
        } else {
            Some(PathBuf::from(trimmed).join("include"))
        }
    });

    include_dirs.find(|include_dir| include_dir.is_dir())
}

pub fn clang_commands() -> Vec<String> {
    let env_opt: Option<String> = std::env::var("CLANG")
        .ok()
        .filter(|value| !value.trim().is_empty());

    let mut commands: Vec<String> = env_opt.into_iter().collect();

    let fallback_iter = [
        "clang", "clang-16", "clang-17", "clang-18", "clang-19", "clang-20", "clang-21",
        "clang-22", "clang-23", "cc",
    ]
    .iter()
    .map(|name| name.to_string());

    commands.extend(fallback_iter);

    commands
}
