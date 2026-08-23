$ErrorActionPreference = "Stop"

$SCRIPT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path
$ROOT_DIR = Split-Path -Parent $SCRIPT_DIR

$MARKER = Join-Path $ROOT_DIR "thrustc_std\src\lib.rs"
$STD_DIR = Join-Path $ROOT_DIR "std"

$marker = Get-Item $MARKER

$stdChanged = Get-ChildItem -Path $STD_DIR -Recurse -File |
    Where-Object { $_.LastWriteTime -gt $marker.LastWriteTime } |
    Select-Object -First 1

if ($null -ne $stdChanged) {
    Write-Host "Standard library changed. Touching '$MARKER' to force re-embedding."
    $marker.LastWriteTime = Get-Date
} else {
    Write-Host "Standard library is up to date."
}

& cargo build --manifest-path (Join-Path $ROOT_DIR "Cargo.toml") @args