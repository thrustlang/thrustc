<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# Thrust Compiler 

<img src= "https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt= "standard-separator" style= "width: 1hv;"> </img>

# Lexer Microsyntax

This document is a micro-specification of the token classification performed by the
Thrust lexer (`thrustc_lexer`). It describes every token the lexer can produce, the
lexical rules that govern them, the edge cases that follow from the implementation,
and the error conditions (`E0001`) it can report.

The authoritative source is the `TokenType` enum in `thrustc_token_type` and the
scanning code in `thrustc_lexer/src/{lex,identifier,number,character,string}.rs`.

## Token anatomy

Each token produced by the lexer is a `Token`:

```rust
pub struct Token {
    pub lexeme: String, // exact source text of the token
    pub ascii:  String, // ASCII-safe rendering (escapes non-ASCII characters)
    pub kind:   TokenType,
    pub span:   Span,   // (line, (column_start, column_end))
}
```

The lexer always appends a final `Eof` token after the last real token.

## Delimiters & punctuation

| TokenType      | Lexeme |
|----------------|--------|
| `LParen`       | `(`    |
| `RParen`       | `)`    |
| `LBrace`       | `{`    |
| `RBrace`       | `}`    |
| `LBracket`     | `[`    |
| `RBracket`     | `]`    |
| `Comma`        | `,`    |
| `SemiColon`    | `;`    |
| `Colon`        | `:`    |
| `ColonColon`   | `::`   |

## Operators

### Arithmetic, bitwise and logical symbols

| TokenType   | Lexeme |
|-------------|--------|
| `Plus`      | `+`    |
| `Minus`     | `-`    |
| `Star`      | `*`    |
| `Slash`     | `/`    |
| `Arith`     | `%`    |
| `Xor`       | `^`    |
| `Not`       | `~`    |
| `Bang`      | `!`    |
| `Bor`       | `|`    |
| `BAnd`      | `&`    |

### Assignment

| TokenType  | Lexeme |
|------------|--------|
| `Eq`       | `=`    |
| `PlusEq`   | `+=`   |
| `MinusEq`  | `-=`   |

### Comparison

| TokenType   | Lexeme |
|-------------|--------|
| `EqEq`      | `==`   |
| `BangEq`    | `!=`   |
| `Less`      | `<`    |
| `LessEq`    | `<=`   |
| `Greater`   | `>`    |
| `GreaterEq` | `>=`   |

### Bit shifts

| TokenType | Lexeme |
|-----------|--------|
| `LShift`  | `<<`   |
| `RShift`  | `>>`   |

### Increment / decrement

| TokenType      | Lexeme |
|----------------|--------|
| `PlusPlus`     | `++`   |
| `MinusMinus`   | `--`   |

### Logical gates (symbol and word forms produce the same token)

| TokenType | Lexemes      |
|-----------|--------------|
| `And`     | `&&` , `and` |
| `Or`      | `||` , `or`  |

### Range, pass and dot

| TokenType | Lexeme |
|-----------|--------|
| `Dot`     | `.`    |
| `Range`   | `..`   |
| `Pass`    | `...`  |

### Arrow

| TokenType | Lexeme |
|-----------|--------|
| `Arrow`   | `->`   |

## Literals

| TokenType    | Lexeme                                                |
|--------------|-------------------------------------------------------|
| `Identifier` | any identifier not matching a reserved table (see below) |
| `Integer`    | `42`, `1_000`, `0x1F`, `0b101`, `0o17`                |
| `Float`      | `3.14`, `1.0` (any numeric lexeme containing `.`)      |
| `Char`       | `'a'`, `'\n'`                                          |
| `CString`    | `"hello"` (null-terminated)                           |
| `CNString`   | `n#"hello"` (non-null-terminated)                     |
| `True`       | `true`                                                 |
| `False`      | `false`                                                |
| `NullPtr`    | `nullptr`                                              |

## Keywords

### Stable keywords

```
var fn if elif else for while loop const struct return break continue
breakall continueall defer pass as deref type enum fixed ref mut static
unreachable intrinsic new alloc address addr load write import only directive
```

The literal keywords `true`, `false` and `nullptr` and the logical gates `and` /
`or` are also recognized in stable mode (see [Literals](#literals) and [Logical
gates](#logical-gates-symbol-and-word-forms-produce-the-same-token)).

### Unstable-only keywords

Recognized only when the compiler runs in `CompilerFeaturesMode::Unstable`
(see `thrustc_backends::set_compiler_features`). In stable mode these lex as plain
`Identifier` tokens:

```
asmfn asm global_asm embedded importC
```

## Types

```
s8 s16 s32 s64 ssize u8 u16 u32 u64 u128 usize
f32 f64 f80 f128 fppc_128
bool char ptr array void Fn
```

## Attributes

Attributes are words prefixed with `@`. Only the known spellings below are emitted
as attribute tokens; any other `@`-prefixed word falls back to an `Identifier`.

### Stable attributes

```
@align @optFuzzing @noUnwind @noReturn @packed @heap @public @linkage @extern
@arbitraryArgs @hot @minSize @alwaysInline @noInline @inline @safeStack
@weakStack @strongStack @preciseFloatingPoint @convention @pure @thunk
@cuda @constructor @destructor @if @elif @else
```

### Unstable-only attributes

```
@promote @asmAlignStack @asmSyntax @asmThrowErrors @asmSideEffects
```

## Atomics & thread modificators

```
volatile lazyThread
atomicNone atomicFree atomicRelax atomicGrab atomicDrop atomicSync atomicStrict
threadInit threadDyn threadExec threadLDyn
```

## Builtins

The following names have dedicated tokens in the lexer:

```
halloc memset memmove memcpy abiSizeOf bitSizeOf abiAlignOf
arbitraryArg arbitraryArgs
```

Other compiler builtins, including `sizeOf` and `alignOf`, lex as identifiers and
are resolved later through the builtin registry.

## LLI

```
alloc address addr load write
```

## Special

```
unreachable
```

## Comments

Comments are consumed by the lexer and **do not produce any token**.

| Form            | Meaning                                   |
|-----------------|-------------------------------------------|
| `// ...`        | Line comment, ends at newline or EOF.     |
| `/* ... */`     | Block comment; must be closed with `*/`.  |

## Lexical rules

### Identifiers

An identifier starts with a non-digit and is made of alphanumeric, `_`, `@` and
Unicode "symbol-other" characters:

```
identifier          ::= identifier-start identifier-continue*
identifier-start    ::= alpha | '_' | '@' | unicode-alnum | unicode-symbol-other
identifier-continue ::= alpha | digit | '_' | '@' | unicode-alnum | unicode-symbol-other
```

A word that starts with a digit is lexed as a number, never as an identifier. After
scanning, the full word is checked against the keyword / atomic / attribute /
builtin / type tables before being classified as `Identifier`.

### Numbers

```
decimal-integer ::= digit (digit | '_')*
hex-integer     ::= '0x' hex-digit (hex-digit | '_')*
binary-integer  ::= '0b' binary-digit (binary-digit | '_')*
octal-integer   ::= '0o' octal-digit (octal-digit | '_')*
float           ::= number-with-a-dot
```

Any numeric lexeme containing `.` is classified as a `Float`; otherwise it is an
`Integer`. Underscores are stripped before validation. Hex/binary/octal literals
are range-checked against the signed and unsigned integer widths.

### Characters

```
char-literal ::= '\'' char '\'' | '\'' escape '\''
```

A character literal must contain exactly one character (or one escape sequence)
between quotes. `''` is lexed as a character with lexeme `'`.

### Strings

```
cstring  ::= '"' string-content '"'      // null-terminated
cnstring ::= 'n' '#' '"' string-content '"' // non-null-terminated
```

The `n#"` prefix marks a **non-null-terminated** string; a plain `"` string is
**null-terminated** and therefore must not contain a null character (`\0`).

### Escape sequences

Valid escapes, shared by characters and strings:

```
\n \t \r \\ \0 \' \"
```

## Edge cases

- **`1..2` is a `Float`.** The number scanner greedily consumes `.` characters,
  so `1..2` becomes a single float lexeme. To express a range you must write it
  with spaces: `1 .. 2`.
- **`#` is only valid as part of the `n#"` prefix.** A bare `#` is not recognized
  and produces an `E0001` error. In `n#` without a quote, `n` lexes as an
  `Identifier` and the trailing `#` still errors.
- **Unknown `@`-words are identifiers.** `@foo` is an `Identifier`; only the
  spellings in [Attributes](#attributes) become attribute tokens.
- **Identifiers cannot start with a digit.** `_123` is an identifier; `123` is a
  number.
- **`'ab'` is invalid.** A character literal holds exactly one character; `'ab'`
  reports "Unclosed character".

## Error conditions (`E0001`)

| Condition                                        | Message summary                                |
|--------------------------------------------------|------------------------------------------------|
| Unrecognized character                           | "It isn't recognized as a character."          |
| Unclosed character literal                       | "Unclosed character"                           |
| Invalid escape sequence                          | "Invalid escape sequence"                      |
| Unclosed string literal                          | "Unclosed literal string."                     |
| Null byte in a null-terminated string            | "Invalid non null terminated string literal..."|
| Integer too large for any integer type           | "Literal is too large to be represented..."    |
| Hex/binary/octal out of bounds (signed)          | "Integer out of bounds signed {radix} format." |
| Hex/binary/octal out of bounds (unsigned)        | "Integer out of bounds unsigned {radix} format."|
| Malformed hex/binary/octal digits                | "Integer invalid {radix} format."              |
| Repeated radix prefix                            | "Integer {radix} prefix '0x' cannot be repeated."|
| Unclosed block comment                           | "Expected comment closer."                     |

## Stable vs unstable mode

Keyword and attribute tables are built according to the active compiler features
mode (`thrustc_backends::get_compiler_features`). In **stable** mode, unstable-only
keywords (`asmfn`, `asm`, `global_asm`, `embedded`, `importC`) and unstable-only
attributes (`@promote`, `@asm*`) are not recognized, so those words lex as plain
`Identifier` tokens. In **unstable** mode they are recognized as their dedicated
token types.
