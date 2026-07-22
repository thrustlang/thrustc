//! Interactively replays a saved crash input for the `llvm-codegen` or
//! `pipeline` fuzz target and reports whether it is a REAL fuzzer crash
//! (deadly signal / sanitizer error / unreachable) as opposed to a
//! nonzero exit code caused by something else (e.g. an expected comptime
//! error surfaced by the compiler itself).
//!
//! Usage:
//!   cargo run --bin reproduce
//!     -> lists every crash file found under fuzz/artifacts/<target>/
//!        for both targets, lets you pick one by number, runs it, and
//!        tells you whether it's a genuine crash.
//!
//!   cargo run --bin reproduce -- llvm-codegen fuzz/artifacts/llvm-codegen/crash-xxxx
//!     -> skips the menu and replays that specific file directly.
//!
//! Full stdout/stderr of the run is always saved under
//! fuzz/reproduce_logs/<target>/<crash-file>.log

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

const TARGETS: &[&str] = &["llvm-codegen", "pipeline"];

/// Substrings that only appear when libFuzzer / the sanitizer actually
/// caught a real crash (signal, memory error, unreachable, etc.).
/// A nonzero exit code WITHOUT any of these markers is NOT treated as a
/// crash — it's just the process exiting oddly for some other reason
/// (e.g. an expected compile-time diagnostic).
const CRASH_MARKERS: &[&str] = &[
    "ERROR: libFuzzer: deadly signal",
    "ERROR: libFuzzer: out-of-memory",
    "ERROR: libFuzzer: timeout",
    "ERROR: AddressSanitizer",
    "SUMMARY: AddressSanitizer",
    "SUMMARY: libFuzzer",
    "UNREACHABLE executed",
    "panicked at",
    "SEGV on unknown address",
    "attempt to subtract with overflow",
    "attempt to add with overflow",
    "attempt to multiply with overflow",
    "index out of bounds",
];

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let (target, crash_file) = if args.len() >= 2 {
        // Non-interactive mode: `reproduce <target> <crash-file>`
        (args[0].clone(), PathBuf::from(&args[1]))
    } else {
        match prompt_for_selection() {
            Some(selection) => selection,
            None => {
                println!("No crash files available, nothing to do.");
                return;
            }
        }
    };

    if !TARGETS.contains(&target.as_str()) {
        eprintln!(
            "Unknown target '{target}'. Valid targets are: {}",
            TARGETS.join(", ")
        );
        std::process::exit(1);
    }

    if !crash_file.exists() {
        eprintln!("Crash file not found: {}", crash_file.display());
        std::process::exit(1);
    }

    run_and_report(&target, &crash_file);
}

/// Scans fuzz/artifacts/<target>/ for both targets, prints a numbered
/// menu, and reads the user's choice from stdin. The user can either:
///   - type a number from the menu, or
///   - type/paste an arbitrary file path (doesn't have to be listed,
///     doesn't have to live under fuzz/artifacts/ at all).
/// When a raw path is entered, the target is inferred from the path if
/// possible (looks for "llvm-codegen" or "pipeline" in the path); if it
/// can't be inferred, the user is asked to pick the target explicitly.
fn prompt_for_selection() -> Option<(String, PathBuf)> {
    let mut entries: Vec<(String, PathBuf)> = Vec::new();

    for target in TARGETS {
        let dir = PathBuf::from("fuzz/artifacts").join(target);
        if !dir.exists() {
            continue;
        }

        let mut files: Vec<PathBuf> = fs::read_dir(&dir)
            .expect("failed to read artifacts directory")
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.is_file())
            .collect();
        files.sort();

        for file in files {
            entries.push((target.to_string(), file));
        }
    }

    if entries.is_empty() {
        println!("No crash files found under fuzz/artifacts/, but you can still");
        println!("enter a path to a crash file manually.\n");
    } else {
        println!("Available crash files:\n");
        for (i, (target, path)) in entries.iter().enumerate() {
            println!("  [{}] ({target}) {}", i + 1, path.display());
        }
    }

    print!("\nEnter a number from the list, or paste a file path directly: ");
    io::stdout().flush().ok();

    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("failed to read input");
    let input = input.trim();

    if input.is_empty() {
        return None;
    }

    // Try to interpret the input as a menu number first.
    if let Ok(n) = input.parse::<usize>() {
        if n >= 1 && n <= entries.len() {
            return Some(entries[n - 1].clone());
        }
        eprintln!(
            "'{n}' is not a valid menu number ({} entries available).",
            entries.len()
        );
        std::process::exit(1);
    }

    // Otherwise treat it as a raw path.
    let path = PathBuf::from(input);
    if !path.exists() {
        eprintln!("No such file: {}", path.display());
        std::process::exit(1);
    }

    let target = infer_target_from_path(&path).unwrap_or_else(prompt_for_target);
    Some((target, path))
}

/// Tries to guess the fuzz target ("llvm-codegen" or "pipeline") from a
/// file path by checking whether either name appears as a path component.
fn infer_target_from_path(path: &Path) -> Option<String> {
    let path_str = path.to_string_lossy();
    TARGETS
        .iter()
        .find(|t| path_str.contains(*t))
        .map(|t| t.to_string())
}

/// Asks the user which target the given path belongs to, when it can't
/// be inferred automatically from the path.
fn prompt_for_target() -> String {
    println!("\nCouldn't infer the target from that path.");
    for (i, target) in TARGETS.iter().enumerate() {
        println!("  [{}] {target}", i + 1);
    }
    print!("Which target does this crash belong to? (enter number): ");
    io::stdout().flush().ok();

    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("failed to read input");

    match input.trim().parse::<usize>() {
        Ok(n) if n >= 1 && n <= TARGETS.len() => TARGETS[n - 1].to_string(),
        _ => {
            eprintln!("Invalid selection.");
            std::process::exit(1);
        }
    }
}

fn run_and_report(target: &str, crash_file: &Path) {
    println!(
        "\nRunning `cargo fuzz run {target} {}` ...\n",
        crash_file.display()
    );

    let output = Command::new("cargo")
        .args(["fuzz", "run", target])
        .arg(crash_file)
        .output()
        .expect("failed to spawn `cargo fuzz run` — is cargo-fuzz installed?");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}\n{stderr}");

    let matched_marker = CRASH_MARKERS
        .iter()
        .find(|marker| combined.contains(*marker));

    let is_real_crash = matched_marker.is_some();

    // Save full log regardless of outcome.
    let log_dir = PathBuf::from("fuzz/fuzz_reproduce_logs").join(target);
    fs::create_dir_all(&log_dir).expect("failed to create log directory");

    let file_name = crash_file
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let log_path = log_dir.join(format!("{file_name}.log"));

    let mut log_contents = String::new();
    log_contents.push_str(&format!("target: {target}\n"));
    log_contents.push_str(&format!("crash file: {}\n", crash_file.display()));
    log_contents.push_str(&format!("exit status: {:?}\n", output.status));
    log_contents.push_str(&format!(
        "classified as: {}\n\n",
        if is_real_crash {
            "REAL CRASH"
        } else {
            "no crash"
        }
    ));
    log_contents.push_str("---- stdout ----\n");
    log_contents.push_str(&stdout);
    log_contents.push_str("\n---- stderr ----\n");
    log_contents.push_str(&stderr);

    fs::write(&log_path, &log_contents).expect("failed to write log file");

    println!("==================== RESULT ====================");
    println!("target:       {target}");
    println!("crash file:   {}", crash_file.display());
    println!("exit status:  {:?}", output.status);

    match matched_marker {
        Some(marker) => {
            println!("result:       REAL CRASH");
            println!("matched on:   \"{marker}\"");
        }
        None => {
            println!("result:       no crash detected");
            if !output.status.success() {
                println!("note:         process exited nonzero, but no crash signature was found.");
                println!(
                    "              This usually means it's an expected error (e.g. a comptime"
                );
                println!("              / semantic diagnostic), not an actual fuzzer crash.");
            }
        }
    }

    println!("full log:     {}", log_path.display());
}
