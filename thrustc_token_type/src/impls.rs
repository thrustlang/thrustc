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

use crate::{
    TokenType,
    traits::{TokenTypeAttributesExtensions, TokenTypeBuiltinExtensions, TokenTypeExtensions},
};

impl TokenTypeExtensions for TokenType {
    #[inline]
    fn is_logical_operator(&self) -> bool {
        matches!(
            self,
            TokenType::BangEq
                | TokenType::EqEq
                | TokenType::LessEq
                | TokenType::Less
                | TokenType::Greater
                | TokenType::GreaterEq
        )
    }

    #[inline]
    fn is_logical_gate(&self) -> bool {
        matches!(self, TokenType::And | TokenType::Or)
    }

    #[inline]
    fn is_minus_minus_operator(&self) -> bool {
        matches!(self, TokenType::MinusMinus)
    }

    #[inline]
    fn is_plus_plus_operator(&self) -> bool {
        matches!(self, TokenType::PlusPlus)
    }

    #[inline]
    fn is_address(&self) -> bool {
        matches!(self, TokenType::Addr)
    }

    #[inline]
    fn is_void(&self) -> bool {
        matches!(self, TokenType::Void)
    }

    #[inline]
    fn is_bool(&self) -> bool {
        matches!(self, TokenType::Bool)
    }

    #[inline]
    fn is_array(&self) -> bool {
        matches!(self, TokenType::Array)
    }

    #[inline]
    fn is_ptr(&self) -> bool {
        matches!(self, TokenType::Ptr)
    }

    #[inline]
    fn is_float(&self) -> bool {
        matches!(
            self,
            TokenType::F32
                | TokenType::F64
                | TokenType::F128
                | TokenType::FX8680
                | TokenType::FPPC128
        )
    }

    #[inline]
    fn is_const(&self) -> bool {
        matches!(self, TokenType::Const)
    }

    #[inline]
    fn is_fn_ref(&self) -> bool {
        matches!(self, TokenType::FnRef)
    }

    #[inline]
    fn is_integer(&self) -> bool {
        matches!(
            self,
            TokenType::S8
                | TokenType::S16
                | TokenType::S32
                | TokenType::S64
                | TokenType::Ssize
                | TokenType::U8
                | TokenType::U16
                | TokenType::U32
                | TokenType::U64
                | TokenType::U128
                | TokenType::Usize
                | TokenType::Char
        )
    }

    #[inline]
    fn is_type(&self) -> bool {
        self.is_integer()
            || self.is_float()
            || self.is_bool()
            || self.is_array()
            || self.is_ptr()
            || self.is_void()
            || self.is_address()
            || self.is_const()
            || self.is_fn_ref()
    }

    #[inline]
    fn is_identifier(&self) -> bool {
        matches!(self, TokenType::Identifier)
    }

    #[inline]
    fn is_stmt(&self) -> bool {
        [
            TokenType::LBrace,
            TokenType::Return,
            TokenType::Static,
            TokenType::Const,
            TokenType::Struct,
            TokenType::Type,
            TokenType::Enum,
            TokenType::Var,
            TokenType::If,
            TokenType::For,
            TokenType::While,
            TokenType::Loop,
            TokenType::Continue,
            TokenType::ContinueAll,
            TokenType::Break,
            TokenType::BreakAll,
            TokenType::Defer,
        ]
        .contains(self)
    }

    #[inline]
    fn is_declaration(&self) -> bool {
        [
            TokenType::Type,
            TokenType::Struct,
            TokenType::Const,
            TokenType::Static,
            TokenType::Enum,
            TokenType::Fn,
            TokenType::AsmFn,
            TokenType::Intrinsic,
            TokenType::GlobalAsm,
            TokenType::Import,
            TokenType::Embedded,
        ]
        .contains(self)
    }
}

impl TokenTypeAttributesExtensions for TokenType {
    #[inline]
    fn is_attribute(&self) -> bool {
        matches!(
            self,
            TokenType::Ignore
                | TokenType::MinSize
                | TokenType::NoInline
                | TokenType::AlwaysInline
                | TokenType::InlineHint
                | TokenType::Hot
                | TokenType::SafeStack
                | TokenType::WeakStack
                | TokenType::StrongStack
                | TokenType::PreciseFloats
                | TokenType::Heap
                | TokenType::AsmThrow
                | TokenType::AsmSideEffects
                | TokenType::AsmAlignStack
                | TokenType::AsmSyntax
                | TokenType::Packed
                | TokenType::NoUnwind
                | TokenType::OptFuzzing
                | TokenType::Constructor
                | TokenType::Destructor
                | TokenType::Public
                | TokenType::Linkage
                | TokenType::Extern
                | TokenType::Convention
                | TokenType::Pure
                | TokenType::Thunk
                | TokenType::Cuda
        )
    }
}

impl TokenTypeBuiltinExtensions for TokenType {
    fn is_builtin(&self) -> bool {
        matches!(
            self,
            TokenType::Halloc
                | TokenType::MemCpy
                | TokenType::MemMove
                | TokenType::MemSet
                | TokenType::AlignOf
                | TokenType::SizeOf
                | TokenType::BitSizeOf
                | TokenType::AbiSizeOf
                | TokenType::AbiAlignOf
        )
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Keywords
            TokenType::Break => write!(f, "break"),
            TokenType::BreakAll => write!(f, "breakall"),
            TokenType::Const => write!(f, "const"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::ContinueAll => write!(f, "continueall"),
            TokenType::Defer => write!(f, "defer"),
            TokenType::Elif => write!(f, "elif"),
            TokenType::Else => write!(f, "else"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::False => write!(f, "false"),
            TokenType::Intrinsic => write!(f, "intrinsic"),
            TokenType::Embedded => write!(f, "embedded"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::For => write!(f, "for"),
            TokenType::If => write!(f, "if"),
            TokenType::Loop => write!(f, "loop"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::New => write!(f, "new"),
            TokenType::Return => write!(f, "return"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::True => write!(f, "true"),
            TokenType::Type => write!(f, "type"),
            TokenType::While => write!(f, "while"),
            TokenType::Write => write!(f, "write"),
            TokenType::Var => write!(f, "var"),
            TokenType::Static => write!(f, "static"),
            TokenType::Asm => write!(f, "asm"),
            TokenType::GlobalAsm => write!(f, "global_asm"),

            // Direct Reference
            TokenType::DirectRef => write!(f, "ref"),

            // Types
            TokenType::Address => write!(f, "address"),
            TokenType::Bool => write!(f, "bool"),
            TokenType::Char => write!(f, "char"),
            TokenType::F32 => write!(f, "f32"),
            TokenType::F64 => write!(f, "f64"),
            TokenType::FX8680 => write!(f, "f80"),
            TokenType::F128 => write!(f, "f128"),
            TokenType::FPPC128 => write!(f, "fppc_128"),
            TokenType::Ptr => write!(f, "ptr"),
            TokenType::Array => write!(f, "array"),
            TokenType::S8 => write!(f, "s8"),
            TokenType::S16 => write!(f, "s16"),
            TokenType::S32 => write!(f, "s32"),
            TokenType::S64 => write!(f, "s64"),
            TokenType::Ssize => write!(f, "ssize"),
            TokenType::CString => write!(f, "const array[char]"),
            TokenType::CNString => write!(f, "const array[char]"),
            TokenType::U8 => write!(f, "u8"),
            TokenType::U16 => write!(f, "u16"),
            TokenType::U32 => write!(f, "u32"),
            TokenType::U64 => write!(f, "u64"),
            TokenType::U128 => write!(f, "u128"),
            TokenType::Usize => write!(f, "usize"),
            TokenType::FnRef => write!(f, "Fn"),
            TokenType::Void => write!(f, "void"),

            // Special
            TokenType::Unreachable => write!(f, "unreachable"),

            // Modificators
            TokenType::LazyThread => write!(f, "lazythread"),
            TokenType::Volatile => write!(f, "volatile"),

            TokenType::AtomNone => write!(f, "atomicNone"),
            TokenType::AtomFree => write!(f, "atomicFree"),
            TokenType::AtomRelax => write!(f, "atomicRelax"),
            TokenType::AtomGrab => write!(f, "atomicGrab"),
            TokenType::AtomDrop => write!(f, "atomicDrop"),
            TokenType::AtomSync => write!(f, "atomicSync"),
            TokenType::AtomStrict => write!(f, "atomicStrict"),

            TokenType::ThreadDynamic => write!(f, "threadDyn"),
            TokenType::ThreadExec => write!(f, "threadExec"),
            TokenType::ThreadInit => write!(f, "threadInit"),
            TokenType::ThreadLDynamic => write!(f, "threadLDyn"),

            // Attributes
            TokenType::Linkage => write!(f, "@linkage"),
            TokenType::Align => write!(f, "@align"),
            TokenType::OptFuzzing => write!(f, "@optFuzzing"),
            TokenType::NoUnwind => write!(f, "@noUnwind"),
            TokenType::Packed => write!(f, "@packed"),
            TokenType::Thunk => writeln!(f, "@thunk"),
            TokenType::Heap => write!(f, "@heap"),
            TokenType::AlwaysInline => write!(f, "@alwaysInline"),
            TokenType::AsmAlignStack => write!(f, "@asmAlignStack"),
            TokenType::AsmSyntax => write!(f, "@asmSyntax"),
            TokenType::AsmSideEffects => write!(f, "@asmSideEffects"),
            TokenType::AsmThrow => write!(f, "@asmThrowErrors"),
            TokenType::Convention => write!(f, "@convention"),
            TokenType::Extern => write!(f, "@extern"),
            TokenType::Hot => write!(f, "@hot"),
            TokenType::Ignore => write!(f, "@arbitraryArgs"),
            TokenType::InlineHint => write!(f, "@inline"),
            TokenType::MinSize => write!(f, "@minSize"),
            TokenType::NoInline => write!(f, "@noInline"),
            TokenType::PreciseFloats => write!(f, "@preciseFloatingPoint"),
            TokenType::Public => write!(f, "@public"),
            TokenType::SafeStack => write!(f, "@safeStack"),
            TokenType::StrongStack => write!(f, "@strongStack"),
            TokenType::WeakStack => write!(f, "@weakStack"),
            TokenType::Pure => write!(f, "@pure"),
            TokenType::Cuda => write!(f, "@cuda"),
            TokenType::Destructor => write!(f, "@destructor"),
            TokenType::Constructor => write!(f, "@constructor"),

            // Operators, Punctuation, and Special Constructs
            TokenType::Or => write!(f, "||"),
            TokenType::And => write!(f, "&&"),
            TokenType::Float => write!(f, "integer"),
            TokenType::Integer => write!(f, "float"),
            TokenType::Addr => write!(f, "addr"),
            TokenType::Alloc => write!(f, "alloc"),
            TokenType::Arith => write!(f, "%"),
            TokenType::Arrow => write!(f, "->"),
            TokenType::AsmFn => write!(f, "asmfn"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEq => write!(f, "!="),
            TokenType::As => write!(f, "as"),
            TokenType::Colon => write!(f, ":"),
            TokenType::ColonColon => write!(f, "::"),
            TokenType::Comma => write!(f, ","),
            TokenType::Deref => write!(f, "deref"),
            TokenType::Dot => write!(f, "."),
            TokenType::Eof => write!(f, "EOF"),
            TokenType::Eq => write!(f, "="),
            TokenType::EqEq => write!(f, "=="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEq => write!(f, ">="),
            TokenType::Identifier => write!(f, "identifier"),
            TokenType::Fixed => write!(f, "fixed"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::LBracket => write!(f, "["),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEq => write!(f, "<="),
            TokenType::Load => write!(f, "load"),
            TokenType::LParen => write!(f, "("),
            TokenType::LShift => write!(f, "<<"),
            TokenType::Minus => write!(f, "-"),
            TokenType::MinusEq => write!(f, "-="),
            TokenType::MinusMinus => write!(f, "--"),
            TokenType::NullPtr => write!(f, "nullptr"),
            TokenType::Pass => write!(f, "..."),
            TokenType::Plus => write!(f, "+"),
            TokenType::PlusEq => write!(f, "+="),
            TokenType::PlusPlus => write!(f, "++"),
            TokenType::Range => write!(f, ".."),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::RBracket => write!(f, "]"),
            TokenType::RParen => write!(f, ")"),
            TokenType::RShift => write!(f, ">>"),
            TokenType::SemiColon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Xor => write!(f, "^"),
            TokenType::Not => write!(f, "~"),
            TokenType::Bor => write!(f, "|"),
            TokenType::BAnd => write!(f, "&"),
            TokenType::Star => write!(f, "*"),

            // Builtins
            TokenType::Halloc => write!(f, "halloc"),
            TokenType::AlignOf => write!(f, "alignOf"),
            TokenType::MemSet => write!(f, "memset"),
            TokenType::MemMove => write!(f, "memmove"),
            TokenType::MemCpy => write!(f, "memcpy"),
            TokenType::SizeOf => write!(f, "sizeOf"),
            TokenType::AbiSizeOf => write!(f, "abiSizeOf"),
            TokenType::BitSizeOf => write!(f, "bitSizeOf"),
            TokenType::AbiAlignOf => write!(f, "abiAlignOf"),

            // Import
            TokenType::Import => write!(f, "import"),
            TokenType::ImportC => write!(f, "importC"),
        }
    }
}
