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

use thrustc_ast_modificators::{Modificator, Modificators};
use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};
use thrustc_token::traits::TokenExtensions;
use thrustc_token_type::TokenType;

use crate::parser::ModuleParser;

pub fn build_statement_modificator(
    ctx: &mut ModuleParser<'_>,
    limits: &[TokenType],
) -> Result<Modificators, ()> {
    let mut modificators: Modificators = Vec::with_capacity(u8::MAX as usize);

    const VALID_MODIFICATORS: &[TokenType] = &[
        TokenType::ThreadInit,
        TokenType::ThreadDynamic,
        TokenType::ThreadExec,
        TokenType::ThreadLDynamic,
        TokenType::AtomNone,
        TokenType::AtomFree,
        TokenType::AtomRelax,
        TokenType::AtomGrab,
        TokenType::AtomDrop,
        TokenType::Volatile,
        TokenType::LazyThread,
    ];

    while !limits.contains(&ctx.peek().get_type())
        && VALID_MODIFICATORS.contains(&ctx.peek().get_type())
    {
        let tk_type: TokenType = ctx.peek().get_type();

        match tk_type {
            TokenType::ThreadInit => {
                ctx.consume(TokenType::ThreadInit)?;
                modificators.push(Modificator::ThreadMode(
                    ThrustThreadMode::InitialExecTLSModel,
                ));
            }
            TokenType::ThreadDynamic => {
                ctx.consume(TokenType::ThreadDynamic)?;
                modificators.push(Modificator::ThreadMode(
                    ThrustThreadMode::GeneralDynamicTLSModel,
                ));
            }
            TokenType::ThreadExec => {
                ctx.consume(TokenType::ThreadExec)?;
                modificators.push(Modificator::ThreadMode(ThrustThreadMode::LocalExecTLSModel));
            }
            TokenType::ThreadLDynamic => {
                ctx.consume(TokenType::ThreadLDynamic)?;
                modificators.push(Modificator::ThreadMode(
                    ThrustThreadMode::LocalDynamicTLSModel,
                ));
            }
            TokenType::AtomNone => {
                ctx.consume(TokenType::AtomNone)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicNone,
                ));
            }
            TokenType::AtomFree => {
                ctx.consume(TokenType::AtomFree)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicFree,
                ));
            }
            TokenType::AtomRelax => {
                ctx.consume(TokenType::AtomRelax)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicRelax,
                ));
            }
            TokenType::AtomGrab => {
                ctx.consume(TokenType::AtomGrab)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicGrab,
                ));
            }
            TokenType::AtomDrop => {
                ctx.consume(TokenType::AtomDrop)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicDrop,
                ));
            }
            TokenType::AtomSync => {
                ctx.consume(TokenType::AtomSync)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicSync,
                ));
            }
            TokenType::AtomStrict => {
                ctx.consume(TokenType::AtomStrict)?;
                modificators.push(Modificator::AtomicOrdering(
                    ThrustAtomicOrdering::AtomicStrict,
                ));
            }
            TokenType::LazyThread => {
                ctx.consume(TokenType::LazyThread)?;
                modificators.push(Modificator::LazyThread);
            }
            TokenType::Volatile => {
                ctx.consume(TokenType::Volatile)?;
                modificators.push(Modificator::Volatile);
            }

            _ => break,
        }
    }

    Ok(modificators)
}
