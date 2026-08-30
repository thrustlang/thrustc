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

use thrustc_ast::{Ast, NodeId, ast_metadata::LoadMetadata, traits::AstGetType};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_code_location::Span;
use thrustc_errors::CompilationIssue;
use thrustc_mir::atomicord::ThrustAtomicOrdering;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::TypeExtensions};

use crate::{ParserContext, expressions, modificators};

pub fn build_load<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let initial_load_tk: &Token = ctx.advance()?;
    let span: Span = initial_load_tk.get_span();

    let modificators: Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let is_volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();

    let source: Ast = expressions::parse_expr(ctx)?;

    let source_type: &Type = source.get_value_type()?;

    let kind: Type = source_type.get_type_pointer_load();

    Ok(Ast::Load {
        source: source.into(),
        kind,
        modificators,
        metadata: LoadMetadata::new(is_volatile, atomic_ord),
        span,
        id: NodeId::new(),
    })
}
