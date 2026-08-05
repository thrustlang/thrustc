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
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use thrustc_fuzz::{backlog, dumps};

const ARTIFACTS_ROOT: &str = "fuzz/artifacts";
const MONITOR_POLL_MS: u64 = 1000;
const STACK_DUMP_BYTES: usize = 64 * 1024 * 1024;

struct Runner {
    target: String,
    mode: String,
    corpus: String,
    dict: String,
    rss: u32,
    stable_flag: bool,
}

impl Runner {
    fn new(target: &str, mode: &str) -> Result<Runner, String> {
        if !dumps::TARGETS.contains(&target) {
            return Err(format!(
                "unknown target '{target}'. Valid targets: {}",
                dumps::TARGETS.join(", ")
            ));
        }

        let mode = if target == "lexer" { "stable" } else { mode };

        if mode != "stable" && mode != "unstable" {
            return Err(format!("unknown mode '{mode}' (expected 'stable' or 'unstable')"));
        }

        let corpus = if target == "lexer" {
            "fuzz/corpus_universal/lexer".to_string()
        } else {
            format!("fuzz/corpus_{mode}/{target}")
        };

        let dict = if mode == "stable" {
            "fuzz/thrust-stable.dict"
        } else {
            "fuzz/thrust-unstable.dict"
        };

        let rss = if mode == "stable"
            && (target == "llvm-codegen-local" || target == "llvm-codegen-local-loops")
        {
            4096
        } else {
            2048
        };

        let stable_flag = mode == "stable" && target != "lexer";

        Ok(Runner {
            target: target.to_string(),
            mode: mode.to_string(),
            corpus: corpus.to_string(),
            dict: dict.to_string(),
            rss,
            stable_flag,
        })
    }
}

fn timestamp() -> String {
    chrono::Local::now().format("%H:%M:%S").to_string()
}

fn print_usage() {
    println!("thrustc continuous fuzz supervisor");
    println!();
    println!("  run <target> [--mode stable|unstable] [--runs N] [--max-time S]");
    println!("      Runs the fuzzer in a loop (mode defaults to 'stable').");
    println!("      Every crash/panic is archived to");
    println!("      fuzz/backlog/<target>/ with AST + LLVM IR dumps, recorded in");
    println!("      fuzz/fuzz_continuous/<target>.log, and fuzzing continues.");
    println!();
    println!("  run-all [--mode stable|unstable]");
    println!("      Same, one fuzzer thread per target.");
    println!();
    println!("  import <target>");
    println!("      Archive already-existing crash artifacts without fuzzing.");
    println!();
    println!("  list [--all]");
    println!("      Show the pending (open) errors per target.");
    println!();
    println!("  history [<target>]");
    println!("      Print the columnar error registry(ies).");
    println!();
    println!("  ignore <target> <issue-id>");
    println!("      Mark an error as ignored so it is never re-archived.");
    println!();
    println!("  reopen <target> <issue-id>");
    println!("      Move an error back to the pending pile.");
    println!();
    println!("  fixed <target> <issue-id>");
    println!("      Mark an error as solved.");
    println!();
    println!("valid targets: {}", dumps::TARGETS.join(", "));
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let Some(command) = args.first() else {
        print_usage();
        return;
    };

    let result = match command.as_str() {
        "run" => cmd_run(&args[1..]),
        "run-all" => cmd_run_all(&args[1..]),
        "import" => cmd_import(&args[1..]),
        "list" => cmd_list(&args[1..]),
        "history" => cmd_history(&args[1..]),
        "ignore" => cmd_set_status(&args[1..], "ignored"),
        "reopen" => cmd_set_status(&args[1..], "open"),
        "fixed" => cmd_set_status(&args[1..], "fixed"),
        other => {
            eprintln!("unknown command '{other}'");
            print_usage();
            std::process::exit(1);
        }
    };

    if let Err(error) = result {
        eprintln!("error: {error}");
        std::process::exit(1);
    }
}

fn parse_run_args(args: &[String]) -> Result<(String, String, Option<u32>, Option<u64>), String> {
    let Some(target) = args.first() else {
        return Err("usage: run <target> [--mode stable|unstable] [--runs N] [--max-time S]".into());
    };

    let mut mode = "stable".to_string();
    let mut runs: Option<u32> = None;
    let mut max_time: Option<u64> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--mode" => {
                i += 1;
                mode = args.get(i).ok_or("--mode requires a value")?.clone();
            }
            "--runs" => {
                i += 1;
                let value = args.get(i).ok_or("--runs requires a value")?;
                runs = Some(value.parse().map_err(|_| format!("invalid --runs value '{value}'"))?);
            }
            "--max-time" => {
                i += 1;
                let value = args.get(i).ok_or("--max-time requires a value")?;
                max_time = Some(
                    value
                        .parse()
                        .map_err(|_| format!("invalid --max-time value '{value}'"))?,
                );
            }
            other => return Err(format!("unknown argument '{other}'")),
        }
        i += 1;
    }

    Ok((target.clone(), mode, runs, max_time))
}

fn cmd_run(args: &[String]) -> Result<(), String> {
    let (target, mode, runs, max_time) = parse_run_args(args)?;

    let runner = Runner::new(&target, &mode)?;

    if runner.target == "lexer" {
        println!(
            "[{}] note: lexer has no stable/unstable mode; using the universal corpus.",
            timestamp()
        );
    }

    backlog::ensure_dirs()?;

    if runs.is_some() || max_time.is_some() {
        println!(
            "[{}] bounded run: fuzzer {} ({}) will stop after completion.",
            timestamp(),
            runner.target,
            runner.mode
        );
        supervise_once(&runner, runs, max_time)
    } else {
        println!(
            "[{}] starting continuous fuzz: {} ({}) — Ctrl+C to stop.",
            timestamp(),
            runner.target,
            runner.mode
        );
        loop {
            supervise_once(&runner, None, None)?;
        }
    }
}

fn cmd_run_all(args: &[String]) -> Result<(), String> {
    let mut mode = "stable".to_string();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--mode" => {
                i += 1;
                mode = args.get(i).ok_or("--mode requires a value")?.clone();
            }
            other => return Err(format!("unknown argument '{other}'")),
        }
        i += 1;
    }

    backlog::ensure_dirs()?;

    let mut handles = Vec::new();

    for target in dumps::TARGETS {
        let runner = Runner::new(target, &mode)?;
        let label = format!("{} ({})", runner.target, runner.mode);

        handles.push(thread::spawn(move || -> Result<(), String> {
            println!("[{}] thread started: fuzzer {}", timestamp(), label);
            loop {
                supervise_once(&runner, None, None)?;
            }
        }));
    }

    for handle in handles {
        handle
            .join()
            .map_err(|_| "a fuzzer thread panicked".to_string())??;
    }

    Ok(())
}

fn cmd_import(args: &[String]) -> Result<(), String> {
    let Some(target) = args.first() else {
        return Err("usage: import <target>".into());
    };

    if !dumps::TARGETS.contains(&target.as_str()) {
        return Err(format!(
            "unknown target '{target}'. Valid targets: {}",
            dumps::TARGETS.join(", ")
        ));
    }

    backlog::ensure_dirs()?;

    let mode = "stable";

    println!(
        "[{}] importing existing artifacts for {target} (mode: {mode})...",
        timestamp()
    );

    let processed: Mutex<HashSet<PathBuf>> = Mutex::new(HashSet::new());

    scan_and_process(target, mode, None, &processed);

    println!(
        "[{}] import finished. Registry: {}",
        timestamp(),
        backlog::log_path(target).display()
    );

    Ok(())
}

fn cmd_list(args: &[String]) -> Result<(), String> {
    let show_all = args.iter().any(|a| a == "--all");

    backlog::ensure_dirs()?;

    let mut any = false;

    for target in dumps::TARGETS {
        let issues = backlog::all_issues(target);

        let visible: Vec<_> = if show_all {
            issues.iter().collect()
        } else {
            issues.iter().filter(|meta| meta.status == "open").collect()
        };

        if visible.is_empty() {
            continue;
        }

        any = true;

        println!(
            "{target}: {} issue(s){}",
            visible.len(),
            if show_all { "" } else { " (open)" }
        );

        for meta in &visible {
            let marker = if meta.marker.is_empty() { "-" } else { &meta.marker };
            println!(
                "  {}  [{}] {} {} ({})  → {}",
                meta.id,
                meta.status,
                meta.discovered_at,
                meta.mode,
                marker,
                backlog::issue_dir(target, &meta.id).display()
            );
        }

        println!();
    }

    if !any {
        println!("No issues recorded yet. Run a fuzzer or `import` some artifacts first.");
    }

    Ok(())
}

fn cmd_history(args: &[String]) -> Result<(), String> {
    let targets: Vec<&str> = if let Some(target) = args.first() {
        vec![target.as_str()]
    } else {
        dumps::TARGETS.to_vec()
    };

    backlog::print_history(&targets)
}

fn cmd_set_status(args: &[String], status: &str) -> Result<(), String> {
    if args.len() < 2 {
        return Err(format!("usage: {status} <target> <issue-id>"));
    }

    let target = &args[0];
    let id = &args[1];

    let meta = backlog::set_status(target, id, status)?;

    println!(
        "[{}] {} → {} ({})",
        timestamp(),
        meta.id,
        status,
        backlog::issue_dir(target, id).display()
    );

    Ok(())
}

fn supervise_once(
    runner: &Runner,
    runs: Option<u32>,
    max_time: Option<u64>,
) -> Result<(), String> {
    let command = build_fuzz_command(runner, runs, max_time);

    println!(
        "[{}] launching: cargo {}",
        timestamp(),
        command.join(" ")
    );

    let child = Command::new("cargo")
        .args(&command)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to spawn `cargo fuzz run` — is cargo-fuzz installed? {e}"))?;

    let stop = Arc::new(AtomicBool::new(false));
    let processed: Arc<Mutex<HashSet<PathBuf>>> = Arc::new(Mutex::new(HashSet::new()));

    let monitor_stop = Arc::clone(&stop);
    let monitor_processed = Arc::clone(&processed);
    let monitor_target = runner.target.clone();
    let monitor_mode = runner.mode.clone();

    let monitor = thread::spawn(move || {
        while !monitor_stop.load(Ordering::Relaxed) {
            scan_and_process(&monitor_target, &monitor_mode, None, &monitor_processed);
            thread::sleep(Duration::from_millis(MONITOR_POLL_MS));
        }
    });

    let output = child
        .wait_with_output()
        .map_err(|e| format!("failed to wait for fuzzer: {e}"))?;

    stop.store(true, Ordering::Relaxed);

    let _ = monitor.join();

    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let marker = dumps::classify(&combined);

    scan_and_process(&runner.target, &runner.mode, marker, &processed);

    println!(
        "[{}] fuzzer {} ({}) exited: {:?}  marker: {}",
        timestamp(),
        runner.target,
        runner.mode,
        output.status,
        marker.unwrap_or("-")
    );

    if marker.is_some() {
        println!("[{}] restarting fuzzer to keep searching...", timestamp());
    }

    Ok(())
}

fn build_fuzz_command(runner: &Runner, runs: Option<u32>, max_time: Option<u64>) -> Vec<String> {
    let mut args = vec![
        toolchain_arg(),
        "fuzz".to_string(),
        "run".to_string(),
        runner.target.clone(),
        runner.corpus.clone(),
    ];

    args.push("--".to_string());
    args.push(format!("-dict={}", runner.dict));
    args.push(format!("-rss_limit_mb={}", runner.rss));

    if let Some(runs) = runs {
        args.push(format!("-runs={runs}"));
    }

    if let Some(max_time) = max_time {
        args.push(format!("-max_total_time={max_time}"));
    }

    if runner.stable_flag {
        args.push("--".to_string());
        args.push("--stable".to_string());
    }

    args
}

/// The fuzz workspace requires a nightly compiler (see `fuzz/rust-toolchain.toml`).
/// Resolves the pinned channel and returns the rustup override argument
/// (e.g. `+nightly`), falling back to `+nightly` if it cannot be parsed.
fn toolchain_arg() -> String {
    let contents = fs::read_to_string("fuzz/rust-toolchain.toml").unwrap_or_default();

    for line in contents.lines() {
        let line = line.trim();

        let Some(rest) = line.strip_prefix("channel") else {
            continue;
        };

        let channel = rest
            .trim()
            .trim_start_matches('=')
            .trim()
            .trim_matches('"')
            .trim();

        if !channel.is_empty() {
            return format!("+{channel}");
        }
    }

    "+nightly".to_string()
}

fn scan_and_process(
    target: &str,
    mode: &str,
    marker_hint: Option<&str>,
    processed: &Mutex<HashSet<PathBuf>>,
) {
    let dir = PathBuf::from(ARTIFACTS_ROOT).join(target);

    let Ok(entries) = fs::read_dir(&dir) else {
        return;
    };

    let mut files: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.is_file())
        .collect();

    files.sort();

    for path in files {
        let mut guard = processed.lock().expect("processed lock poisoned");
        if guard.contains(&path) {
            continue;
        }
        guard.insert(path.clone());
        drop(guard);

        match process_artifact(target, mode, &path, marker_hint) {
            Ok(Some(meta)) => {
                println!(
                    "[{}] NEW ISSUE {}/{} → {}",
                    timestamp(),
                    target,
                    meta.id,
                    backlog::issue_dir(target, &meta.id).display()
                );
            }
            Ok(None) => {}
            Err(error) => {
                eprintln!(
                    "[{}] failed to process {}: {error}",
                    timestamp(),
                    path.display()
                );
            }
        }
    }
}

fn process_artifact(
    target: &str,
    mode: &str,
    path: &Path,
    marker_hint: Option<&str>,
) -> Result<Option<backlog::IssueMeta>, String> {
    let data = fs::read(path).map_err(|e| format!("could not read artifact: {e}"))?;

    if backlog::known_hashes(target).contains(&backlog::content_hash(&data)) {
        let _ = fs::remove_file(path);
        return Ok(None);
    }

    let (ast_text, ir_result) = dump_ast_and_ir(target, data.clone());

    let files = backlog::IssueFiles {
        input: &data,
        ast: ast_text.as_deref(),
        ir: ir_result
            .as_ref()
            .and_then(|result| result.as_ref().ok())
            .and_then(|inner| inner.as_ref())
            .map(String::as_str),
        ir_error: ir_result
            .as_ref()
            .and_then(|result| result.as_ref().err())
            .map(String::as_str),
    };

    let meta = backlog::record_issue(target, mode, &data, marker_hint, files)?;

    let _ = fs::remove_file(path);

    Ok(Some(meta))
}

/// Runs AST reconstruction + LLVM IR codegen on a dedicated thread with a larger
/// stack, so deep compiler recursion while dumping an artifact cannot easily
/// kill the supervisor process. Returns `(ast_text, ir_result)`.
fn dump_ast_and_ir(
    target: &str,
    data: Vec<u8>,
) -> (Option<String>, Option<Result<Option<String>, String>>) {
    let target = target.to_string();

    let handle = thread::Builder::new()
        .stack_size(STACK_DUMP_BYTES)
        .spawn(move || {
            let ast_text = dumps::ast_dump(&target, &data).ok();

            let ir_result = if dumps::CODGEN_TARGETS.contains(&target.as_str()) {
                match dumps::reconstruct_ast(&target, &data) {
                    Ok(ast) => Some(dumps::emit_llvm_ir(&ast)),
                    Err(error) => Some(Err(error)),
                }
            } else {
                None
            };

            (ast_text, ir_result)
        })
        .expect("failed to spawn dump thread");

    handle.join().unwrap_or_else(|_| {
        eprintln!("[{}] dump thread panicked while processing an artifact", timestamp());
        (None, None)
    })
}
