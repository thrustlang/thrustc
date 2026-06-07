#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

mkdir -p changelogs

echo "Available tags:"

git tag --sort=-version:refname | head -20

echo

read -rp "Enter previous tag: " prev_tag
if [ -z "$prev_tag" ]; then
    echo "Error: Previous tag is required."
    exit 1
fi

if ! git rev-parse --verify --quiet "refs/tags/${prev_tag}"; then
    echo "Error: Tag '${prev_tag}' does not exist."
    exit 1
fi

read -rp "Enter new tag name: " tag_name

if [ -z "$tag_name" ]; then
    echo "Error: New tag name is required."
    exit 1
fi

range="${prev_tag}..HEAD"
release_dir="changelogs/${tag_name}"

mkdir -p "$release_dir"

echo "Generating changelog for: ${range}"
git-cliff "${range}" --output "${release_dir}/README.md"

cargo run --quiet 2>/dev/null || true
help_output=$(./target/debug/thrustc --help 2>&1 || true)

if [ -n "$help_output" ]; then
    {
        echo
        echo "## Command Line"
        echo '```console'
        echo "$help_output"
        echo '```'
    } >> "${release_dir}/README.md"
fi

git add "${release_dir}/README.md"
git commit -m "Bumping '${tag_name}'"

echo "Changelog generated at ${release_dir}/README.md"
echo "Done."