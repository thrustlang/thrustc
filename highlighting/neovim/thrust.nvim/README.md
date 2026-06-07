# thrust.nvim

Syntax highlighting for the [Thrust Programming Language](https://github.com/thrustlang/) language in Neovim/Vim.

## What gets highlighted

| Element | Examples |
|---|---|
| Keywords | `fn`, `var`, `if`, `elif`, `else`, `for`, `while`, `return`, `struct`, ... |
| Types | `s8`–`s64`, `u8`–`u128`, `f32`–`f128`, `ptr`, `array`, `bool`, `void`, ... |
| Attributes | `@public`, `@extern`, `@convention`, `@cuda`, `@inline`, ... |
| Atomics | `volatile`, `atomicSync`, `threadInit`, `threadDyn`, ... |
| Builtins | `sizeOf`, `alignOf`, `memcpy`, `halloc`, ... |
| Memory ops | `deref`, `ref`, `mut`, `alloc`, `addr` |
| Literals | strings, chars, integers, hex (`0x...`), floats |
| Comments | `// line` and `/* block */` |

## Installation

### lazy.nvim

```lua
{
  "yourusername/thrust.nvim",
  ft = "thrust",
}
```

### Manual (no plugin manager)

Copy the three folders into your Neovim config:

```bash
cp -r ftdetect  ~/.config/nvim/
cp -r syntax    ~/.config/nvim/
cp -r ftplugin  ~/.config/nvim/
```

That's it — open any `.thrust` file and highlighting is active.

## File structure

```
thrust.nvim/
  ftdetect/thrust.vim   ← associates *.thrust with this syntax
  syntax/thrust.vim     ← all highlight rules
  ftplugin/thrust.vim   ← indent settings, comment format
```
