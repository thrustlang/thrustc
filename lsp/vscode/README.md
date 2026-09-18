<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust LSP - Visual Studio Code

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

This is the official Visual Studio Code extension for the **Thrust** Programming Language with syntax highlighting, file icons, themes, and Language Server Protocol integration through `thrustc_lsp`.

## Requirements

You need Visual Studio Code and the `thrustc_lsp` executable from the Thrust Compiler release.

The language server is not bundled inside the extension. It must be installed manually and available from `PATH`, or configured through `thrust.lsp.path`.

## Install `thrustc_lsp`

Download the compiler release for your platform from:

```text
https://github.com/thrustlang/thrustc/releases
```

The release includes the compiler and the language server:

```text
thrustc
thrustc-stripped
thrustc_lsp
thrustc_lsp-stripped
```

On Windows, the binaries use `.exe`:

```text
thrustc.exe
thrustc-stripped.exe
thrustc_lsp.exe
thrustc_lsp-stripped.exe
```

Install `thrustc_lsp` in a directory available from `PATH`.

Common locations on Linux and macOS:

```console
/home/<username>/.thrustlang/<v<CompilerVersion>>/thrustc_lsp
```

Common location on Windows:

```text
C:\Users\<username>\AppData\.thrustlang\<v<CompilerVersion>>\thrustc_lsp.exe
```

Verify the installation:

```console
thrustc_lsp --version
```

## Build `thrustc_lsp` From Source

From the repository root:

```console
cargo build -p thrustc_lsp
```

The debug binary is generated at:

```console
target/debug/thrustc_lsp
```

For a release build:

```console
cargo build --release -p thrustc_lsp
```

The release binary is generated at:

```console
target/release/thrustc_lsp
```

Verify the binary:

```console
target/release/thrustc_lsp --version
```

To use it from the Visual Studio Code extension, either add the binary directory to `PATH` or configure the absolute path:

```json
{
  "thrust.lsp.path": "/absolute/path/to/thrustc_lsp"
}
```

## Install The Extension

The packaged extension is generated under:

```text
lsp/vscode/packages/
```

Install the generated VSIX manually:

```console
code --install-extension lsp/vscode/packages/thrustlang-vscode-0.2.1.vsix
```

After installation, open any `.thrust` file. Visual Studio Code starts `thrustc_lsp` automatically.

## Configure The Language Server

If `thrustc_lsp` is available from `PATH`, no extra configuration is needed.

If the executable is installed somewhere else, configure the absolute path in Visual Studio Code settings.

Linux or macOS example:

```json
{
  "thrust.lsp.path": "/home/<username>/.thrustlang/<v<CompilerVersion>>/thrustc_lsp"
}
```

Windows example:

```json
{
  "thrust.lsp.path": "C:\Users\<username>\AppData\.thrustlang\<v<CompilerVersion>>\thrustc_lsp.exe"
}
```

You can also pass additional arguments to the language server:

```json
{
  "thrust.lsp.args": []
}
```

## Build The Extension From Source

From this directory:

```console
npm install
npm run compile
npx vsce package --out packages/thrustlang-vscode-0.2.1.vsix
```

Then install the generated package:

```console
code --install-extension packages/thrustlang-vscode-0.2.1.vsix
```

## Notes

The current `thrustc_lsp` binary provides the protocol entry point, editor connection, and compiler-backed module import completion. Additional compiler-backed diagnostics, hover, definition, and document symbols are expected to be connected through the compiler analysis pipeline.

The legacy highlighting-only extension remains available at:

```text
highlighting/vscode/thrust-vscode/
```
