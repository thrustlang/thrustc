#!/usr/bin/env fish

cd (dirname (status filename))/..
mkdir -p changelogs

echo "Available tags:"
git tag --sort=-version:refname | head -20
echo

read -P "Enter previous tag: " prev_tag
if test -z "$prev_tag"
    echo "Error: Previous tag is required."
    exit 1
end

if not git rev-parse --verify --quiet "refs/tags/$prev_tag" > /dev/null 2>&1
    echo "Error: Tag '$prev_tag' does not exist."
    exit 1
end

read -P "Enter new tag name: " tag_name
if test -z "$tag_name"
    echo "Error: New tag name is required."
    exit 1
end

set range "$prev_tag..HEAD"
set release_dir "changelogs/$tag_name"
mkdir -p "$release_dir"

echo "Generating changelog for: $range"
git-cliff "$range" --output "$release_dir/README.md"

cargo run --quiet 2>/dev/null; or true
set help_output (./target/debug/thrustc --help 2>&1; or true)

if test -n "$help_output"
    echo "" >> "$release_dir/README.md"
    echo "## Command Line" >> "$release_dir/README.md"
    echo '```console' >> "$release_dir/README.md"
    echo "$help_output" >> "$release_dir/README.md"
    echo '```' >> "$release_dir/README.md"
end

git add "$release_dir/README.md"
git commit -m "Bumping '$tag_name'"

echo "Changelog generated at $release_dir/README.md"
echo "Done."
