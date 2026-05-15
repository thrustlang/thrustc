#!/bin/bash
set -euo pipefail
 
SCRIPT_DIR="$(dirname "$0")"
 
echo "=== Step 1/3: Deploying documentation ==="
bash "$SCRIPT_DIR/deploy-code-docs.sh"
 
echo
echo "=== Step 2/3: Generating release changelog ==="
bash "$SCRIPT_DIR/release-changelog.sh"
 
echo
echo "=== Step 3/3: Tagging release ==="
bash "$SCRIPT_DIR/tag-manager.sh"
 
echo
echo "Done. Version deployed successfully."
