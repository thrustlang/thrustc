#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_DIR"

echo "Using project directory: $PROJECT_DIR"

if ! git rev-parse --verify origin/gh-pages >/dev/null 2>&1; then
    echo "Branch 'gh-pages' not found on remote. Creating orphan branch..."
    CURRENT_BRANCH=$(git branch --show-current)
    git checkout --orphan gh-pages
    git rm -rf . >/dev/null
    git commit --allow-empty -m "Initial gh-pages commit"
    git push origin gh-pages
    git checkout "$CURRENT_BRANCH"
fi

echo "Generating documentation..."

cargo clean --doc
cargo docs

echo "Preparing documentation..."
TEMP_DOCS="/tmp/thrust-docs-build"
rm -rf "$TEMP_DOCS"
cp -r target/doc "$TEMP_DOCS"

echo '<meta http-equiv="refresh" content="0; url=thrustc/index.html">' > "$TEMP_DOCS/index.html"

echo "Deploying to GitHub Pages..."
PAGES_WORKTREE="/tmp/thrust-gh-pages"
rm -rf "$PAGES_WORKTREE"

git fetch origin gh-pages >/dev/null 2>&1
git worktree add "$PAGES_WORKTREE" gh-pages

pushd "$PAGES_WORKTREE" > /dev/null
    find . -maxdepth 1 ! -name '.git' ! -name '.' -exec rm -rf {} +
    
    cp -r "$TEMP_DOCS"/* ./
    
    git add -A
    if git diff-index --quiet HEAD --; then
        echo "No changes to documentation."
    else
        git commit -m "Update documentation $(date '+%Y-%m-%d %H:%M')"
        git push origin gh-pages
    fi
popd > /dev/null

git worktree remove "$PAGES_WORKTREE"
rm -rf "$TEMP_DOCS"

echo "Done. Documentation updated successfully."