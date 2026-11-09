<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust Compiler

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

Releases run through the scripts under `scripts/`. The work splits into two steps: publish the Rust code documentation, then generate the changelog and tag. `deploy-version` runs the documentation deploy first, then the changelog and tag step.

Every script comes in four flavors so you can run it on any platform:

- `.sh` for bash (Linux, macOS, WSL).
- `.ps1` for PowerShell (Windows).
- `.fish` for the fish shell.
- `.bat` for the classic Windows command prompt.

> [!NOTE]
> Run these scripts from inside a git clone of the repository, with a remote named `origin`. They prompt for input (tag names, push confirmation, and so on), so run them interactively.

## Requirements

The release scripts need a few tools that are not part of the compiler itself:

- `git-cliff` generates the changelog from the git history, configured through `cliff.toml` at the root of the project.
- `sccache` is the rustc wrapper for builds.
- `panic-analyzer` analyzes panics.

Install the three tools with the `cargo-dependencies` script:

```console
bash scripts/cargo-dependencies.sh
```

There are also `.ps1`, `.fish` and `.bat` versions of that script.

The code documentation step also relies on the `cargo docs` alias in `.cargo/config.toml`, which builds rustdoc documentation for the thrustc workspace crates while excluding the vendored LLVM crates.

## The release pipeline

`deploy-version` runs the documentation deploy and then the changelog and tag step. Run it with the flavor of your shell:

```console
bash scripts/deploy-version.sh
```

```powershell
powershell -ExecutionPolicy Bypass -File scripts/deploy-version.ps1
```

```fish
fish scripts/deploy-version.fish
```

```console
scripts\deploy-version.bat
```

If a step fails, the pipeline stops there. Each script also runs on its own if you only need part of the release.

## Step 1. Deploy the code documentation

Script: `deploy-code-docs` (`.sh`, `.ps1`, `.fish`, `.bat`).

This step publishes the Rust code documentation to GitHub Pages, under the `gh-pages` branch.

What it does:

1. Checks if the `gh-pages` branch exists on the `origin` remote. If it does not, it creates it as an orphan branch with an initial empty commit and pushes it.
2. Builds the documentation with `cargo clean --doc` followed by `cargo docs`.
3. Copies the generated `target/doc` folder to a temporary directory and adds an `index.html` that redirects to the main crate page (`thrustc/index.html`).
4. Uses a git worktree of the `gh-pages` branch to replace its contents with the fresh documentation.
5. Commits the changes with a message that includes the current date, and pushes them to `origin`. If there are no documentation changes, nothing is pushed.

> [!NOTE]
> This step does not require any tag, you can run it alone to update the online documentation without doing a release.

## Step 2. Generate the changelog and tag the release

Script: `release-changelog` (`.sh`, `.ps1`, `.fish`, `.bat`).

This step creates the changelog for the new version, bumps the workspace version, commits both, and pushes the tag. It needs `git-cliff` installed and runs from the root of the project.

> [!IMPORTANT]
> Both tags are read from the terminal. The previous tag must exist, otherwise the script exits with an error. The new tag must follow the naming convention used in the project, for example `thrustc-x86_64-linux-ubuntu-v0.1.5`, or the release workflow will not trigger.

What it does:

1. Runs `git-cliff` over `<previous-tag>..HEAD` and writes `changelogs/<new-tag>/README.md`.
2. Appends a `## Command Line` section with the output of `thrustc --help`.
3. Extracts `x.y.z` from the new tag and updates `[workspace.package].version` in `Cargo.toml`.
4. Commits the changelog and `Cargo.toml` with `Bumping '<new-tag>'`, creates the tag, and pushes `HEAD` and the tag to `origin`.

## After the release

Once `deploy-version` finishes, the documentation is live and the release is tagged.

The tag push starts the release workflow for that platform. The workflow needs `changelogs/<tag>/README.md` to exist, so a missing changelog fails the run before anything is published.

Each workflow builds `thrustc`, `thrustc_lsp`, and their `-stripped` variants, packages the VS Code extension, and creates the GitHub release with the changelog as its body. It then sends a `compiler-release` event to `thrustlang/website` with `WEBSITE_DISPATCH_TOKEN`. The website updates that platform's compiler download version and deploys GitHub Pages.

You can:

- Check the changelog at `changelogs/<tag>/README.md`.
- Check the release workflow run for the tag in GitHub Actions.
- Check the website deployment run triggered by the release.

## Release binary variants

The [GitHub Releases](https://github.com/thrustlang/thrustc/releases) page provides four compiler and language server binaries for each platform:

- `thrustc`: the standard compiler, compiled with symbols and debug information for investigating production bugs. It has a larger file size.
- `thrustc-stripped`: the compiler without symbols or debug information, which results in a smaller file.
- `thrustc_lsp`: the language server used by editors such as Visual Studio Code.
- `thrustc_lsp-stripped`: the language server without symbols or debug information.

Releases are published per platform triple: `x86_64-linux-ubuntu`, `x86_64-macos`, `aarch64-macos`, and `x86_64-windows-msvc`. Each release attaches those four binaries and the packaged VS Code extension. The changelog step bumps the workspace version in `Cargo.toml`.
