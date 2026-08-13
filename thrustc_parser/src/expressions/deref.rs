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

use thrustc_ast::{Ast, NodeId, ast_metadata::DereferenceMetadata, traits::AstGetType};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_mir::atomicord::ThrustAtomicOrdering;
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::DereferenceExtensions};

use crate::{ParserContext, expressions, modificators};

pub fn build_dereference<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let initial_deref_tk: &Token = ctx.advance()?;
    let span: Span = initial_deref_tk.get_span();

    let modificators: Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let is_volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();

    let mut deref_count: u64 = 1;

    let mut current_expr: Ast = {
        while ctx.check(TokenType::Deref) {
            ctx.consume(
                TokenType::Deref,
                CompilationIssueCode::E0001,
                "Expected 'deref' keyword.".into(),
            )?;

            deref_count = deref_count.saturating_add(1);
        }

        let expr: Ast = expressions::parse_expr(ctx)?;

        expr
    };

    let mut current_type: Type = current_expr.get_value_type()?.clone();

    (0..deref_count).for_each(|_| {
        current_expr = Ast::Deref {
            value: current_expr.clone().into(),
            kind: current_type.dereference(),
            modificators: modificators.clone(),
            metadata: DereferenceMetadata::new(is_volatile, atomic_ord),
            span,
            id: NodeId::new(),
        };

        current_type = current_type.dereference();
    });

    Ok(current_expr)
}
