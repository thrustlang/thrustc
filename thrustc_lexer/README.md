<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

# Thrust Lexer

`thrustc_lexer` is the lexical analysis (tokenizer) crate of the Thrust compiler. It
reads the source code of a compilation unit and turns it into a stream of tokens,
each with its lexeme, an ASCII-safe rendering, its `TokenType` classification and a
source span.

## Usage

```rust
let tokens = Lexer::lex(&file, &options)?; // full token stream (ends with Eof)
```

- `Lexer::lex` lexes the whole unit and appends a final `TokenType::Eof`.
- `Lexer::lex_for_preprocessor` behaves the same without dispatching diagnostics
  (used by the preprocessor when scanning imported modules).

## Modules

- `lex.rs` — main token dispatch (delimiters, operators, literals, comments).
- `identifier.rs` — identifier scanning and the keyword / attribute / builtin /
  type / atomic lookup tables.
- `number.rs` — integer (decimal, hex, binary, octal) and float literal scanning.
- `character.rs` — character literals and escape sequences.
- `string.rs` — string literals (`CString`, `CNString`) and ASCII conversion.

## Token classification

The complete list of tokens the lexer can produce, the exact lexical rules, the
known edge cases and the lexer error conditions are documented in:

> **[`LEXER_MICROSYNTAX.md`](LEXER_MICROSYNTAX.md)** — micro-specification of the
> lexer's token classification.
