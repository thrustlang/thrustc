#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

MARKER="$ROOT_DIR/thrustc_std/src/lib.rs"
STD_DIR="$ROOT_DIR/std"

if ! test -e "$MARKER" \
    || find "$STD_DIR" -type f -newer "$MARKER" -print -quit | grep -q .; then
    echo "Standard library changed. Touching '$MARKER' to force re-embedding."
    touch "$MARKER"
else
    echo "Standard library is up to date."
fi