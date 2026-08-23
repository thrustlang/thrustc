#!/usr/bin/env fish

set SCRIPT_DIR (dirname (status --current-filename))
set ROOT_DIR (dirname $SCRIPT_DIR)

set MARKER "$ROOT_DIR/thrustc_std/src/lib.rs"
set STD_DIR "$ROOT_DIR/std"

if not test -e "$MARKER"
    echo "Standard library changed. Touching '$MARKER' to force re-embedding."
    touch "$MARKER"
else if find "$STD_DIR" -type f -newer "$MARKER" -print -quit | string length -q
    echo "Standard library changed. Touching '$MARKER' to force re-embedding."
    touch "$MARKER"
else
    echo "Standard library is up to date."
end

cargo build --manifest-path "$ROOT_DIR/Cargo.toml" $argv