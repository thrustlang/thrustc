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

use thrustc_ast::{Ast, NodeId};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::ParserContext;

pub fn build_global_assembler<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let tk: &Token = ctx.consume(
        TokenType::GlobalAsm,
        CompilationIssueCode::E0001,
        "Expected 'global_asm' keyword.".into(),
    )?;

    let span: Span = tk.get_span();

    let assembler_tk: &Token = ctx.consume(
        TokenType::Assembler,
        CompilationIssueCode::E0001,
        "Expected a '#asm{' block.".into(),
    )?;

    let asm: String = assembler_tk.get_lexeme().to_string();

    let _ = ctx.match_token(TokenType::SemiColon)?;

    Ok(Ast::GlobalAssembler {
        asm,
        span,
        kind: Type::Void { span },
        id: NodeId::new(),
    })
}
