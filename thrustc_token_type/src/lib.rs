/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

pub mod impls;
pub mod traits;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;
use serde::Serialize;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub enum TokenType {
    // --- Operators ---
    LParen,     // ' ( '
    RParen,     // ' ) '
    LBrace,     // ' { '
    RBrace,     // ' } '
    Comma,      // ' , '
    Dot,        // ' . '
    Minus,      // ' - '
    Plus,       // ' + '
    Slash,      // ' / '
    Star,       // ' * '
    Xor,        // ' ^ '
    Not,        // ' ~ '
    Bor,        // ' | '
    BAnd,       // ' & '
    Colon,      // ' : '
    SemiColon,  // ' ; '
    RBracket,   // ' ] '
    LBracket,   // ' [ '
    Arith,      // ' % ',
    Bang,       // ' ! '
    Range,      // ' .. '
    Pass,       // ' ... '
    ColonColon, // ' :: '
    BangEq,     // ' != '
    Eq,         // ' = '
    EqEq,       // ' == '
    Greater,    // ' > '
    GreaterEq,  // ' >= '
    Less,       // ' < '
    LessEq,     // ' <= '
    PlusPlus,   // ' ++ '
    MinusMinus, // ' -- '
    MinusEq,    // -=
    PlusEq,     // +=
    LShift,     // ' << '
    RShift,     // ' >> '
    Arrow,      // ->

    // --- Literals ---
    Identifier,
    Integer,
    Float,

    // --- Attributes ---
    Heap,
    Extern,
    Ignore,
    Public,
    MinSize,
    NoInline,
    AlwaysInline,
    InlineHint,
    Hot,
    SafeStack,
    WeakStack,
    StrongStack,
    PreciseFloats,
    Convention,
    NoUnwind,
    Packed,
    AsmAlignStack,
    AsmSyntax,
    AsmThrow,
    AsmSideEffects,
    OptFuzzing,
    Align,
    Linkage,
    Pure,
    Cuda,
    Thunk,
    Constructor,
    Destructor,
    Promote,

    // --- Special ---
    Unreachable,

    // --- Modificators ---
    Volatile,
    LazyThread,

    AtomNone,
    AtomFree,
    AtomRelax,
    AtomGrab,
    AtomDrop,
    AtomSync,
    AtomStrict,

    ThreadDynamic,
    ThreadExec,
    ThreadInit,
    ThreadLDynamic,

    // --- LLI ---
    Alloc,
    Address,
    Load,
    Write,

    // --- Keywords ---
    AsmFn,
    Asm,
    Intrinsic,
    GlobalAsm,
    Deref,
    As,
    Static,
    New,
    Fixed,
    Import,
    ImportC,
    Embedded,
    Mut,
    Type,
    Enum,
    And,
    Struct,
    Else,
    Fn,
    For,
    Continue,
    Break,
    ContinueAll,
    BreakAll,
    Defer,
    If,
    Elif,
    Or,
    Return,
    Var,
    Const,
    While,
    Loop,
    Reference,

    // --- Literals ---
    True,
    False,
    NullPtr,

    // -- Builtins --
    AlignOf,
    Halloc,
    AbiSizeOf,
    BitSizeOf,
    AbiAlignOf,
    SizeOf,
    MemCpy,
    MemMove,
    MemSet,

    // --- Types ---
    S8,
    S16,
    S32,
    S64,
    Ssize,

    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,

    F32,
    F64,
    FX8680,
    F128,
    FPPC128,

    Bool,
    Char,
    CString,
    CNString,
    Ptr,
    Void,
    Addr,
    Array,

    FnRef,

    Eof,
}
