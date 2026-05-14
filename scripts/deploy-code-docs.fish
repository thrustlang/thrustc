#!/usr/bin/fish

set -e

set SCRIPT_DIR (realpath (dirname (status filename)))
set PROJECT_DIR (realpath "$SCRIPT_DIR/..")

cd $PROJECT_DIR
echo "Using project directory: $PROJECT_DIR"

if not git rev-parse --verify origin/gh-pages >/dev/null 2>&1
    echo "Branch 'gh-pages' not found on remote. Creating..."
    set CURRENT_BRANCH (git branch --show-current)
    git checkout --orphan gh-pages
    git rm -rf .
    git commit --allow-empty -m "Initial gh-pages commit"
    git push origin gh-pages
    git checkout $CURRENT_BRANCH
end

echo "Generating documentation..."
cargo clean --doc
cargo docs

echo "Preparing documentation..."
set TEMP_DOCS "/tmp/thrust-docs-build"
rm -rf $TEMP_DOCS
cp -r target/doc $TEMP_DOCS

echo '<meta http-equiv="refresh" content="0; url=thrustc/index.html">' > "$TEMP_DOCS/index.html"

echo "Deploying to GitHub Pages..."
set PAGES_WORKTREE "/tmp/thrust-gh-pages"
rm -rf $PAGES_WORKTREE

git fetch origin gh-pages
git worktree add $PAGES_WORKTREE gh-pages

pushd $PAGES_WORKTREE
    find . -maxdepth 1 ! -name '.git' ! -name '.' -exec rm -rf {} +
    cp -r $TEMP_DOCS/* ./
    
    git add -A
    if git diff-index --quiet HEAD --
        echo "No changes to documentation."
    else
        git commit -m "Update documentation (date '+%Y-%m-%d %H:%M')"
        git push origin gh-pages
    end
popd

git worktree remove $PAGES_WORKTREE
rm -rf $TEMP_DOCS
echo "Done."