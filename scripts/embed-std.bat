@echo off
setlocal

set "SCRIPT_DIR=%~dp0"
set "ROOT_DIR=%SCRIPT_DIR%.."
set "MARKER=%ROOT_DIR%\thrustc_std\src\lib.rs"

REM Batch has no portable newer-than comparison, so the marker is always
REM touched. This forces cargo to recompile thrustc_std, re-embedding the
REM standard library.
echo Standard library changed. Touching "%MARKER%" to force re-embedding.
powershell -NoProfile -Command "$m = Get-Item '%MARKER%'; $m.LastWriteTime = Get-Date"

cargo build --manifest-path "%ROOT_DIR%\Cargo.toml" %*