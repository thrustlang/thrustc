<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

# Fuzzing Scripts

This folder contains helper scripts for the compiler fuzzing suite.

- `create_fuzzing_dirs.sh` — Bash (Linux/macOS)
- `create_fuzzing_dirs.fish` — Fish (Linux/macOS)
- `create_fuzzing_dirs.ps1` — PowerShell (Windows)
- `create_fuzzing_dirs.bat` — Command Prompt (Windows)

All variants are idempotent and create every directory required by the fuzzing
suite relative to the `fuzz/` folder, including the corpus folders
(`corpus_stable/`, `corpus_unstable/`, `corpus_universal/`), `fuzz_reproduce_logs/`
and `fuzz_pipeline/`.

> [!IMPORTANT]
> The complete fuzzing guide is in `COMPILER_FUZZING.md` at the repository root.
