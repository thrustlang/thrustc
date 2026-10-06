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

use std::path::{Path, PathBuf};

pub trait ClangArgumentsSource {
    fn include_paths(&self) -> &[PathBuf];
    fn system_include_paths(&self) -> &[PathBuf];
    fn defines(&self) -> &[String];
    fn undefs(&self) -> &[String];
    fn target(&self) -> Option<&str>;
    fn sysroot(&self) -> Option<&Path>;
    fn std(&self) -> Option<&str>;
    fn args(&self) -> &[String];
}

impl ClangArgumentsSource for thrustc_options::ImportCOptions {
    fn include_paths(&self) -> &[PathBuf] {
        thrustc_options::ImportCOptions::include_paths(self)
    }

    fn system_include_paths(&self) -> &[PathBuf] {
        thrustc_options::ImportCOptions::system_include_paths(self)
    }

    fn defines(&self) -> &[String] {
        thrustc_options::ImportCOptions::defines(self)
    }

    fn undefs(&self) -> &[String] {
        thrustc_options::ImportCOptions::undefs(self)
    }

    fn target(&self) -> Option<&str> {
        thrustc_options::ImportCOptions::target(self)
    }

    fn sysroot(&self) -> Option<&Path> {
        thrustc_options::ImportCOptions::sysroot(self)
    }

    fn std(&self) -> Option<&str> {
        thrustc_options::ImportCOptions::std(self)
    }

    fn args(&self) -> &[String] {
        thrustc_options::ImportCOptions::args(self)
    }
}

impl ClangArgumentsSource for thrustc_options::TranslateCOptions {
    fn include_paths(&self) -> &[PathBuf] {
        thrustc_options::TranslateCOptions::include_paths(self)
    }

    fn system_include_paths(&self) -> &[PathBuf] {
        thrustc_options::TranslateCOptions::system_include_paths(self)
    }

    fn defines(&self) -> &[String] {
        thrustc_options::TranslateCOptions::defines(self)
    }

    fn undefs(&self) -> &[String] {
        thrustc_options::TranslateCOptions::undefs(self)
    }

    fn target(&self) -> Option<&str> {
        thrustc_options::TranslateCOptions::target(self)
    }

    fn sysroot(&self) -> Option<&Path> {
        thrustc_options::TranslateCOptions::sysroot(self)
    }

    fn std(&self) -> Option<&str> {
        thrustc_options::TranslateCOptions::std(self)
    }

    fn args(&self) -> &[String] {
        thrustc_options::TranslateCOptions::args(self)
    }
}

pub fn build_clang_arguments(source: &impl ClangArgumentsSource) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();

    if let Some(res) = self::detect_clang_resource_include_dir() {
        args.push(format!("-isystem{}", res.display()));
    }

    let include_iter = source
        .include_paths()
        .iter()
        .map(|inc| format!("-I{}", inc.display()));

    args.extend(include_iter);

    let system_iter = source
        .system_include_paths()
        .iter()
        .map(|inc| format!("-isystem{}", inc.display()));

    args.extend(system_iter);

    let auto_system: Vec<PathBuf> = if source.system_include_paths().is_empty() {
        self::detect_host_system_include_dirs()
    } else {
        Vec::new()
    };

    let auto_iter = auto_system
        .iter()
        .map(|dir| format!("-isystem{}", dir.display()))
        .filter(|flag| !args.contains(flag));

    let auto_flags: Vec<String> = auto_iter.collect();

    args.extend(auto_flags);

    let define_iter = source.defines().iter().map(|def| format!("-D{def}"));

    args.extend(define_iter);

    let undef_iter = source.undefs().iter().map(|und| format!("-U{und}"));

    args.extend(undef_iter);

    if let Some(target) = source.target() {
        args.push(format!("--target={target}"));
    }

    if let Some(sysroot) = source.sysroot() {
        args.push(format!("--sysroot={}", sysroot.display()));
    }

    if let Some(std_) = source.std() {
        args.push(format!("-std={std_}"));
    }

    args.extend(source.args().iter().cloned());

    args
}

fn verbose_include_dirs(command: &str) -> Vec<PathBuf> {
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

fn detect_host_system_include_dirs() -> Vec<PathBuf> {
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

fn detect_clang_resource_include_dir() -> Option<PathBuf> {
    let commands: [&str; 3] = ["clang", "clang-17", "clang-18"];

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

pub fn is_supported_expr_kind(kind: clang::EntityKind) -> bool {
    matches!(
        kind,
        clang::EntityKind::IntegerLiteral
            | clang::EntityKind::FloatingLiteral
            | clang::EntityKind::StringLiteral
            | clang::EntityKind::CharacterLiteral
            | clang::EntityKind::UnaryExpr
            | clang::EntityKind::CompoundLiteralExpr
            | clang::EntityKind::InitListExpr
            | clang::EntityKind::GNUNullExpr
            | clang::EntityKind::NullPtrLiteralExpr
            | clang::EntityKind::DeclRefExpr
            | clang::EntityKind::MemberRefExpr
            | clang::EntityKind::CallExpr
            | clang::EntityKind::ParenExpr
            | clang::EntityKind::UnaryOperator
            | clang::EntityKind::ArraySubscriptExpr
            | clang::EntityKind::BinaryOperator
            | clang::EntityKind::CompoundAssignOperator
            | clang::EntityKind::CStyleCastExpr
            | clang::EntityKind::ConditionalOperator
            | clang::EntityKind::UnexposedExpr
    )
}

fn clang_commands() -> Vec<String> {
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

pub fn escape_string_for_thrust_literal(s: &str) -> String {
    let mut out: String = String::with_capacity(s.len().saturating_add(8));

    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            '\'' => out.push_str("\\'"),
            other => out.push(other),
        }
    }

    out
}
