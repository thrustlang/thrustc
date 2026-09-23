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

use thrustc_ast::traits::AstGetType;
use thrustc_ast::{Ast, NodeId, ast_builtins::AstBuiltin};
use thrustc_atomic_ordering::{ThrustAtomicOrdering, ThrustAtomicRMWOperation};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{ParserContext, expressions, typegeneration};

pub fn build_compiler_builtin<'parser>(
    ctx: &mut ParserContext<'parser>,
    tk_type: TokenType,
) -> Result<Ast<'parser>, CompilationIssue> {
    match tk_type {
        TokenType::Halloc => self::build_halloc(ctx),
        TokenType::MemSet => self::build_memset(ctx),
        TokenType::MemMove => self::build_memmove(ctx),
        TokenType::MemCpy => self::build_memcpy(ctx),
        TokenType::AbiSizeOf => self::build_abi_size_of(ctx),
        TokenType::BitSizeOf => self::build_bit_size_of(ctx),
        TokenType::AbiAlignOf => self::build_abi_align_of(ctx),
        TokenType::ArbitraryArg => self::build_arbitrary_arg(ctx),
        TokenType::ArbitraryArgs => self::build_arbitrary_args(ctx),

        TokenType::AtomicStore => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Store)
        }
        TokenType::AtomicAdd => self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Add),
        TokenType::AtomicSubtract => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Subtract)
        }
        TokenType::AtomicAnd => self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::And),
        TokenType::AtomicNand => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Nand)
        }
        TokenType::AtomicOr => self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Or),
        TokenType::AtomicXor => self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::Xor),
        TokenType::AtomicSignedMaximum => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::SignedMaximum)
        }
        TokenType::AtomicSignedMinimum => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::SignedMinimum)
        }
        TokenType::AtomicUnsignedMaximum => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::UnsignedMaximum)
        }
        TokenType::AtomicUnsignedMinimum => {
            self::build_atomic_rmw(ctx, tk_type, ThrustAtomicRMWOperation::UnsignedMinimum)
        }
        TokenType::AtomicCompareAndSwap => self::build_atomic_compare_and_swap(ctx),

        TokenType::ArbitraryArgsStart => self::build_arbitrary_args_start(ctx),
        TokenType::ArbitraryArgsCopy => self::build_arbitrary_args_copy(ctx),
        TokenType::ArbitraryArgsEnd => self::build_arbitrary_args_end(ctx),
        TokenType::ArbitraryArgFrom => self::build_arbitrary_arg_from(ctx),
        TokenType::ArbitraryArgsCount => self::build_arbitrary_args_count(ctx),

        _ => {
            let token: &Token = ctx.advance()?;
            let lexeme: &str = token.get_lexeme();
            let span: Span = token.get_span();

            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0003,
                format!("Unknown compiler intrinsic '{}'.", lexeme),
                "Compiler intrinsic doesn't exist on the compiler.".into(),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }
    }
}

pub fn build_atomic_ordering<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<ThrustAtomicOrdering, CompilationIssue> {
    let tk: &Token = ctx.peek();

    let ordering: ThrustAtomicOrdering = match tk.get_type() {
        TokenType::AtomNone => ThrustAtomicOrdering::AtomicNone,
        TokenType::AtomFree => ThrustAtomicOrdering::AtomicFree,
        TokenType::AtomRelax => ThrustAtomicOrdering::AtomicRelax,
        TokenType::AtomGrab => ThrustAtomicOrdering::AtomicGrab,
        TokenType::AtomDrop => ThrustAtomicOrdering::AtomicDrop,
        TokenType::AtomSync => ThrustAtomicOrdering::AtomicSync,
        TokenType::AtomStrict => ThrustAtomicOrdering::AtomicStrict,

        _ => {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "Expected an atomic ordering argument.".into(),
                "Use atomicNone, atomicFree, atomicRelax, atomicGrab, atomicDrop, atomicSync or atomicStrict.".into(),
                None,
                tk.get_span(),
            ))
        }
    };

    ctx.advance()?;

    Ok(ordering)
}

pub fn build_arbitrary_args_start<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ArbitraryArgsStart,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgsStart' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgsStart { span },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_args_copy<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ArbitraryArgsCopy,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgsCopy' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let source: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgsCopy {
            source: source.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_args_end<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ArbitraryArgsEnd,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgsEnd' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let list: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgsEnd {
            list: list.into(),
            span,
        },
        kind: Type::Void { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_arg_from<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ArbitraryArgFrom,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgFrom' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let list: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    let kind: Type = ty.clone();

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgFrom {
            list: list.into(),
            ty,
            span,
        },
        kind,
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_args_count<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::ArbitraryArgsCount,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgsCount' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgsCount { span },
        kind: Type::USize { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_halloc<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let halloc_tk: &Token = ctx.consume(
        TokenType::Halloc,
        CompilationIssueCode::E0001,
        "Expected 'halloc' keyword.".into(),
    )?;

    let span: Span = halloc_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let of: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::Halloc {
            of: of.clone(),
            span,
        },
        kind: Type::Ptr {
            subtype: Some(of.into()),
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memcpy<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemCpy,
        CompilationIssueCode::E0001,
        String::from("Expected 'memcpy' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let span: Span = memcpy_tk.get_span();

    let source: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemCpy {
            src: source.into(),
            dst: destination.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memmove<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemMove,
        CompilationIssueCode::E0001,
        String::from("Expected 'memmove' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        String::from("Expected '('."),
    )?;

    let span: Span = memcpy_tk.get_span();

    let source: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        String::from("Expected ')'."),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemMove {
            src: source.into(),
            dst: destination.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_memset<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let memcpy_tk: &Token = ctx.consume(
        TokenType::MemSet,
        CompilationIssueCode::E0001,
        String::from("Expected 'memset' keyword."),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        String::from("Expected '('."),
    )?;

    let span: Span = memcpy_tk.get_span();

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let new_size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        String::from("Expected ','."),
    )?;

    let size: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        String::from("Expected ')'."),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::MemSet {
            dst: destination.into(),
            new_size: new_size.into(),
            size: size.into(),
            span,
        },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_bit_size_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::BitSizeOf,
        CompilationIssueCode::E0001,
        String::from("Expected 'bit_size_of' keyword."),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::BitSizeOf { ty, span },
        kind: Type::U64 { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_abi_size_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::AbiSizeOf,
        CompilationIssueCode::E0001,
        "Expected 'abi_size_of' keyword.".into(),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AbiSizeOf { ty, span },
        kind: Type::U64 { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_abi_align_of<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let sizeof_tk: &Token = ctx.consume(
        TokenType::AbiAlignOf,
        CompilationIssueCode::E0001,
        "Expected 'abi_align_of' keyword.".into(),
    )?;

    let span: Span = sizeof_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AbiAlignOf { ty, span },
        kind: Type::U32 { span },
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_arg<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let arbitrary_tk: &Token = ctx.consume(
        TokenType::ArbitraryArg,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArg' keyword.".into(),
    )?;

    let span: Span = arbitrary_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let ty: Type = typegeneration::build_type(ctx, true)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArg {
            ty: ty.clone(),
            span,
        },
        kind: ty,
        span,
        id: NodeId::new(),
    })
}

pub fn build_arbitrary_args<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let arbitrary_tk: &Token = ctx.consume(
        TokenType::ArbitraryArgs,
        CompilationIssueCode::E0001,
        "Expected 'arbitraryArgs' keyword.".into(),
    )?;

    let span: Span = arbitrary_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::ArbitraryArgs { span },
        kind: Type::Ptr {
            subtype: None,
            address_space: None,
            span,
        },
        span,
        id: NodeId::new(),
    })
}

pub fn build_atomic_rmw<'parser>(
    ctx: &mut ParserContext<'parser>,
    tk_type: TokenType,
    operation: ThrustAtomicRMWOperation,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        tk_type,
        CompilationIssueCode::E0001,
        format!("Expected '{}' keyword.", tk_type),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let value: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    let destination_type: &Type = destination.get_value_type()?;

    let kind: Type = destination_type.clone();

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AtomicRMW {
            operation,
            destination: destination.into(),
            value: value.into(),
            span,
        },
        kind,
        span,
        id: NodeId::new(),
    })
}

pub fn build_atomic_compare_and_swap<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::AtomicCompareAndSwap,
        CompilationIssueCode::E0001,
        "Expected 'atomicCompareAndSwap' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let destination: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let expected: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let new_value: Ast = expressions::parse_expr(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let success: ThrustAtomicOrdering = self::build_atomic_ordering(ctx)?;

    ctx.consume(
        TokenType::Comma,
        CompilationIssueCode::E0001,
        "Expected ','.".into(),
    )?;

    let failure: ThrustAtomicOrdering = self::build_atomic_ordering(ctx)?;

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    Ok(Ast::Builtin {
        builtin: AstBuiltin::AtomicCompareAndSwap {
            destination: destination.into(),
            expected: expected.into(),
            new_value: new_value.into(),
            success,
            failure,
            span,
        },
        kind: Type::Bool { span },
        span,
        id: NodeId::new(),
    })
}
