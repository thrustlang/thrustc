<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"> </img>

## LLVM Linker Wrapper

A small library that bridges the C++ LLVM Linker (LLD) API to Rust, for use with the Thrust compiler as an integrated linker.

### Flavors

Flavors represent the executable assembler or binary variants that you can link using the library.

- **COFF** (Windows)
- **ELF** (Linux)
- **Mach0** (MacOS)
- **WASM** (WebAssembly)

#### References

- **[LLVM Linker (LLD)](https://lld.llvm.org/)**
