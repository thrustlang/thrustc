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

use thrustc_backends::CompilerFeaturesMode;
use thrustc_errors::CompilationIssue;
use thrustc_token_type::TokenType;

use crate::Lexer;

use ahash::AHashMap as HashMap;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref ATOMIC: HashMap<&'static str, TokenType> = {
        let mut atomic: HashMap<&'static str, TokenType> = HashMap::with_capacity(u8::MAX as usize);

        atomic.insert("volatile", TokenType::Volatile);
        atomic.insert("lazyThread", TokenType::LazyThread);

        atomic.insert("atomicNone", TokenType::AtomNone);
        atomic.insert("atomicFree", TokenType::AtomFree);
        atomic.insert("atomicRelax", TokenType::AtomRelax);
        atomic.insert("atomicGrab", TokenType::AtomGrab);
        atomic.insert("atomicDrop", TokenType::AtomDrop);
        atomic.insert("atomicSync", TokenType::AtomSync);
        atomic.insert("atomicStrict", TokenType::AtomStrict);

        atomic.insert("threadInit", TokenType::ThreadInit);
        atomic.insert("threadDyn", TokenType::ThreadDynamic);
        atomic.insert("threadExec", TokenType::ThreadExec);
        atomic.insert("threadLDyn", TokenType::ThreadLDynamic);

        atomic
    };
}

lazy_static! {
    pub static ref ATTRIBUTES: HashMap<&'static str, TokenType> = {
        let mut attributes: HashMap<&'static str, TokenType> =
            HashMap::with_capacity(u8::MAX as usize);

        let compiler_mode: CompilerFeaturesMode = thrustc_backends::get_compiler_features();

        attributes.insert("@align", TokenType::Align);
        attributes.insert("@optFuzzing", TokenType::OptFuzzing);
        attributes.insert("@noUnwind", TokenType::NoUnwind);
        attributes.insert("@noReturn", TokenType::NoReturn);
        attributes.insert("@packed", TokenType::Packed);
        attributes.insert("@heap", TokenType::Heap);
        attributes.insert("@public", TokenType::Public);
        attributes.insert("@linkage", TokenType::Linkage);
        attributes.insert("@extern", TokenType::Extern);
        attributes.insert("@arbitraryArgs", TokenType::Ignore);
        attributes.insert("@hot", TokenType::Hot);
        attributes.insert("@minSize", TokenType::MinSize);
        attributes.insert("@alwaysInline", TokenType::AlwaysInline);
        attributes.insert("@noInline", TokenType::NoInline);
        attributes.insert("@inline", TokenType::InlineHint);
        attributes.insert("@safeStack", TokenType::SafeStack);
        attributes.insert("@weakStack", TokenType::WeakStack);
        attributes.insert("@strongStack", TokenType::StrongStack);
        attributes.insert("@preciseFloatingPoint", TokenType::PreciseFloats);
        attributes.insert("@convention", TokenType::Convention);
        attributes.insert("@pure", TokenType::Pure);
        attributes.insert("@thunk", TokenType::Thunk);
        attributes.insert("@cuda", TokenType::Cuda);
        attributes.insert("@constructor", TokenType::Constructor);
        attributes.insert("@destructor", TokenType::Destructor);

        if compiler_mode.is_unstable_mode() {
            attributes.insert("@promote", TokenType::Promote);
            attributes.insert("@asmAlignStack", TokenType::AsmAlignStack);
            attributes.insert("@asmSyntax", TokenType::AsmSyntax);
            attributes.insert("@asmThrowErrors", TokenType::AsmThrow);
            attributes.insert("@asmSideEffects", TokenType::AsmSideEffects);
        }

        attributes
    };
}

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, TokenType> = {
        let mut builtins: HashMap<&'static str, TokenType> =
            HashMap::with_capacity(u8::MAX as usize);

        builtins.insert("halloc", TokenType::Halloc);
        builtins.insert("sizeOf", TokenType::SizeOf);
        builtins.insert("memset", TokenType::MemSet);
        builtins.insert("memmove", TokenType::MemMove);
        builtins.insert("memcpy", TokenType::MemCpy);
        builtins.insert("alignOf", TokenType::AlignOf);
        builtins.insert("abiSizeOf", TokenType::AbiSizeOf);
        builtins.insert("bitSizeOf", TokenType::BitSizeOf);
        builtins.insert("abiAlignOf", TokenType::AbiAlignOf);

        builtins
    };
}

lazy_static! {
    pub static ref TYPES: HashMap<&'static str, TokenType> = {
        let mut types: HashMap<&'static str, TokenType> = HashMap::with_capacity(u8::MAX as usize);

        types.insert("s8", TokenType::S8);
        types.insert("s16", TokenType::S16);
        types.insert("s32", TokenType::S32);
        types.insert("s64", TokenType::S64);
        types.insert("ssize", TokenType::Ssize);
        types.insert("u8", TokenType::U8);
        types.insert("u16", TokenType::U16);
        types.insert("u32", TokenType::U32);
        types.insert("u64", TokenType::U64);
        types.insert("u128", TokenType::U128);
        types.insert("usize", TokenType::Usize);
        types.insert("f32", TokenType::F32);
        types.insert("f64", TokenType::F64);
        types.insert("f128", TokenType::F128);
        types.insert("f80", TokenType::FX8680);
        types.insert("fppc_128", TokenType::FPPC128);
        types.insert("bool", TokenType::Bool);
        types.insert("char", TokenType::Char);
        types.insert("ptr", TokenType::Ptr);
        types.insert("array", TokenType::Array);
        types.insert("void", TokenType::Void);
        types.insert("Fn", TokenType::FnRef);

        types
    };
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut keywords: HashMap<&'static str, TokenType> =
            HashMap::with_capacity(u8::MAX as usize);

        let compiler_mode: CompilerFeaturesMode = thrustc_backends::get_compiler_features();

        keywords.insert("var", TokenType::Var);
        keywords.insert("fn", TokenType::Fn);
        keywords.insert("if", TokenType::If);
        keywords.insert("elif", TokenType::Elif);
        keywords.insert("else", TokenType::Else);
        keywords.insert("for", TokenType::For);
        keywords.insert("while", TokenType::While);
        keywords.insert("loop", TokenType::Loop);
        keywords.insert("true", TokenType::True);
        keywords.insert("false", TokenType::False);
        keywords.insert("or", TokenType::Or);
        keywords.insert("and", TokenType::And);
        keywords.insert("const", TokenType::Const);
        keywords.insert("struct", TokenType::Struct);
        keywords.insert("return", TokenType::Return);
        keywords.insert("break", TokenType::Break);
        keywords.insert("continue", TokenType::Continue);
        keywords.insert("breakall", TokenType::BreakAll);
        keywords.insert("continueall", TokenType::ContinueAll);
        keywords.insert("defer", TokenType::Defer);
        keywords.insert("pass", TokenType::Pass);
        keywords.insert("nullptr", TokenType::NullPtr);
        keywords.insert("as", TokenType::As);

        keywords.insert("deref", TokenType::Deref);
        keywords.insert("type", TokenType::Type);
        keywords.insert("enum", TokenType::Enum);
        keywords.insert("alloc", TokenType::Alloc);
        keywords.insert("address", TokenType::Address);
        keywords.insert("addr", TokenType::Addr);
        keywords.insert("load", TokenType::Load);
        keywords.insert("write", TokenType::Write);
        keywords.insert("fixed", TokenType::Fixed);
        keywords.insert("ref", TokenType::Reference);
        keywords.insert("mut", TokenType::Mut);
        keywords.insert("static", TokenType::Static);
        keywords.insert("unreachable", TokenType::Unreachable);
        keywords.insert("intrinsic", TokenType::Intrinsic);
        keywords.insert("new", TokenType::New);
        keywords.insert("import", TokenType::Import);
        keywords.insert("only", TokenType::Only);

        if compiler_mode.is_unstable_mode() {
            keywords.insert("asmfn", TokenType::AsmFn);
            keywords.insert("asm", TokenType::Asm);
            keywords.insert("global_asm", TokenType::GlobalAsm);
            keywords.insert("embedded", TokenType::Embedded);
            keywords.insert("importC", TokenType::ImportC);
        }

        keywords
    };
}

pub fn lex(lexer: &mut Lexer) -> Result<(), CompilationIssue> {
    while lexer.is_identifier_boundary(lexer.peek()) {
        lexer.advance_only();
    }

    let lexem: String = lexer.lexeme();

    if let Some(keyword) = KEYWORDS.get(lexem.as_str()) {
        lexer.make(*keyword);
    } else if let Some(atomic_stuff) = ATOMIC.get(lexem.as_str()) {
        lexer.make(*atomic_stuff);
    } else if let Some(attribute) = ATTRIBUTES.get(lexem.as_str()) {
        lexer.make(*attribute);
    } else if let Some(builtin) = BUILTINS.get(lexem.as_str()) {
        lexer.make(*builtin);
    } else if let Some(r#type) = TYPES.get(lexem.as_str()) {
        lexer.make(*r#type);
    } else {
        lexer.make(TokenType::Identifier);
    }

    Ok(())
}
