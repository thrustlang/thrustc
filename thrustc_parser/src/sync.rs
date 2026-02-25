use either::Either;
use thrustc_ast::Ast;
use thrustc_token::Token;
use thrustc_token_type::TokenType;

use crate::{ParserContext, control::ParserSyncPosition, statements::block};

pub const SYNC_STATEMENTS: [TokenType; 11] = [
    TokenType::Return,
    TokenType::Local,
    TokenType::For,
    TokenType::New,
    TokenType::If,
    TokenType::While,
    TokenType::Continue,
    TokenType::Break,
    TokenType::Loop,
    TokenType::Const,
    TokenType::Static,
];

pub const SYNC_DECLARATIONS: [TokenType; 6] = [
    TokenType::Type,
    TokenType::Struct,
    TokenType::Fn,
    TokenType::Enum,
    TokenType::Const,
    TokenType::Static,
];

impl<'parser> ParserContext<'parser> {
    pub fn sync(&mut self) -> Either<Ast<'parser>, ()> {
        if let Some(position) = self.get_control_ctx().get_sync_position() {
            match position {
                ParserSyncPosition::Declaration => {
                    self::sync_with_declaration(self);
                    Either::Right(())
                }
                ParserSyncPosition::Statement => self::sync_with_statement(self),
                ParserSyncPosition::Expression => self::sync_with_expression(self),
                ParserSyncPosition::NoRelevant => Either::Right(()),
            }
        } else {
            Either::Right(())
        }
    }
}

fn sync_with_declaration(ctx: &mut ParserContext) {
    loop {
        if ctx.is_eof() {
            break;
        }

        let peeked: &Token = ctx.peek();

        if SYNC_DECLARATIONS.contains(&peeked.kind) && ctx.is_main_scope() {
            break;
        } else {
            if ctx.get_scope() != 0 {
                ctx.get_mut_symbols().end_scope();
                ctx.end_scope();
            }
        }

        let _ = ctx.only_advance();
    }

    ctx.get_mut_symbols().finish_parameters();
    ctx.get_mut_symbols().finish_scopes();

    ctx.reset_scope();
}

fn sync_with_statement<'parser>(ctx: &mut ParserContext<'parser>) -> Either<Ast<'parser>, ()> {
    loop {
        if ctx.is_eof() {
            break;
        }

        if ctx.get_scope() >= 1 {
            if ctx.check(TokenType::RBrace) {
                let _ = ctx.only_advance();

                if ctx.get_scope() != 0 {
                    ctx.get_mut_symbols().end_scope();
                    ctx.end_scope();
                }

                if ctx.is_main_scope() {
                    ctx.get_mut_symbols().finish_parameters();
                    ctx.get_mut_symbols().finish_scopes();
                }

                break;
            } else {
                let peeked: &Token = ctx.peek();

                if SYNC_STATEMENTS.contains(&peeked.kind) {
                    if ctx.get_scope() != 0 {
                        ctx.get_mut_symbols().end_scope();
                        ctx.end_scope();

                        let fixed_block: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                            block::build_block_without_start(ctx);

                        // We need to figure out how to erradicate the issue related with superior ast nodes.

                        return match fixed_block {
                            Ok(ast) => Either::Left(ast),
                            Err(_) => Either::Right(()),
                        };
                    }

                    break;
                }

                let has_ahead_rbrace: bool = ctx.check_ahead(TokenType::RBrace, &SYNC_DECLARATIONS);

                if !has_ahead_rbrace {
                    while !SYNC_DECLARATIONS.contains(&ctx.peek().kind) {
                        let _ = ctx.only_advance();

                        if ctx.get_scope() != 0 {
                            ctx.get_mut_symbols().end_scope();
                            ctx.end_scope();
                        }
                    }

                    break;
                }
            }
        } else {
            let peeked: &Token = ctx.peek();

            if SYNC_DECLARATIONS.contains(&peeked.kind) && ctx.is_main_scope() {
                ctx.get_mut_symbols().finish_parameters();
                ctx.get_mut_symbols().finish_scopes();
                ctx.reset_scope();

                break;
            }
        }

        let _ = ctx.only_advance();
    }

    Either::Right(())
}

fn sync_with_expression<'parser>(ctx: &mut ParserContext<'parser>) -> Either<Ast<'parser>, ()> {
    loop {
        if ctx.is_eof() {
            break;
        }

        if ctx.get_scope() >= 1 {
            if ctx.check(TokenType::RBrace) {
                let _ = ctx.only_advance();

                if ctx.get_scope() != 0 {
                    ctx.get_mut_symbols().end_scope();
                    ctx.end_scope();
                }

                if ctx.is_main_scope() {
                    ctx.get_mut_symbols().finish_parameters();
                    ctx.get_mut_symbols().finish_scopes();
                }

                break;
            } else {
                let peeked: &Token = ctx.peek();

                if SYNC_STATEMENTS.contains(&peeked.kind) {
                    if ctx.get_scope() != 0 {
                        ctx.get_mut_symbols().end_scope();
                        ctx.end_scope();

                        if ctx.is_main_scope() {
                            ctx.get_mut_symbols().finish_parameters();
                            ctx.get_mut_symbols().finish_scopes();
                        }

                        let fixed_block: Result<Ast<'_>, thrustc_errors::CompilationIssue> =
                            block::build_block_without_start(ctx);

                        return match fixed_block {
                            Ok(ast) => Either::Left(ast),
                            Err(_) => Either::Right(()),
                        };
                    }

                    break;
                }

                let has_ahead_rbrace: bool = ctx.check_ahead(TokenType::RBrace, &SYNC_DECLARATIONS);

                if !has_ahead_rbrace {
                    while !SYNC_DECLARATIONS.contains(&ctx.peek().kind) {
                        let _ = ctx.only_advance();

                        if ctx.get_scope() != 0 {
                            ctx.get_mut_symbols().end_scope();
                            ctx.end_scope();
                        }
                    }

                    break;
                }
            }
        } else {
            let peeked: &Token = ctx.peek();

            if SYNC_DECLARATIONS.contains(&peeked.kind) && ctx.is_main_scope() {
                ctx.get_mut_symbols().finish_parameters();
                ctx.get_mut_symbols().finish_scopes();

                ctx.reset_scope();

                break;
            }
        }

        let _ = ctx.only_advance();
    }

    Either::Right(())
}
