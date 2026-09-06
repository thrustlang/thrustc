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

use thrustc_ast::{Ast, NodeId, ast_metadata::ConstantMetadata, traits::AstGetType};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_mir::atomicord::ThrustAtomicOrdering;
use thrustc_parser_context::{Position, traits::TypeContextExtensions};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::InfererTypeExtensions};

use crate::{ParserContext, attributes, expressions, modificators, typegeneration};

pub fn build_global_const<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Const,
        CompilationIssueCode::E0001,
        "Expected 'const' keyword.".into(),
    )?;

    let modificators: Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let thread_local: bool = modificators.has_lazythread_modificator();
    let is_volatile: bool = modificators.has_volatile_modificator();

    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();

    let const_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let name: &str = const_tk.get_lexeme();
    let ascii_name: &str = const_tk.get_ascii_lexeme();

    let span: Span = const_tk.get_span();

    ctx.consume(
        TokenType::Colon,
        CompilationIssueCode::E0001,
        "Expected ':'.".into(),
    )?;

    let mut constant_type: Type = typegeneration::build_type(ctx, false)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Eq])?;

    ctx.consume(
        TokenType::Eq,
        CompilationIssueCode::E0001,
        "Expected '='.".into(),
    )?;

    ctx.get_mut_control_context()
        .set_position(Position::Constant);
    ctx.get_mut_type_context()
        .add_infered_type(constant_type.clone());

    let value: Ast = expressions::parse_expression(ctx)?;
    let value_type: &Type = value.get_value_type()?;

    ctx.get_mut_type_context().pop_infered_type();
    ctx.get_mut_control_context().reset_position();

    if let Some(infered_inner_type) = constant_type.pass_inner_type_from_type(value_type) {
        constant_type = infered_inner_type;
    }

    let metadata: ConstantMetadata =
        ConstantMetadata::new(true, thread_local, is_volatile, atomic_ord);

    if parse_forward {
        ctx.get_mut_symbols()
            .new_global_constant(name, (constant_type, attributes, Some(value.clone())))?;

        Ok(Ast::new_nullptr(span))
    } else {
        ctx.get_mut_symbols().new_global_constant(
            name,
            (
                constant_type.clone(),
                attributes.clone(),
                Some(value.clone()),
            ),
        )?;

        let constant: Ast<'_> = Ast::Const {
            name,
            ascii_name,
            kind: constant_type,
            value: value.into(),
            attributes,
            modificators,
            metadata,
            span,
            id: NodeId::new(),
        };

        Ok(constant)
    }
}
