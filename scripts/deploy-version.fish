#!/usr/bin/env fish

set SCRIPT_DIR (dirname (status filename))

echo "=== Step 1/3: Deploying documentation ==="
fish "$SCRIPT_DIR/deploy-code-docs.fish"
or exit 1

echo ""
echo "=== Step 2/3: Generating release changelog ==="
fish "$SCRIPT_DIR/release-changelog.fish"
or exit 1

echo ""
echo "=== Step 3/3: Tagging release ==="
fish "$SCRIPT_DIR/tag-manager.fish"
or exit 1

echo ""
echo "Done. Version deployed successfully."
