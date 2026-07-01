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

use thrustc_ast::{Ast, NodeId, ast_metadata::LocalMetadata, traits::AstGetType};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_attributes::ThrustAttributes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_mir::atomicord::ThrustAtomicOrdering;
use thrustc_parser_context::{Position, traits::TypeContextExtensions};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::InfererTypeExtensions};

use crate::{ParserContext, attributes, expressions, modificators, typegeneration};

pub fn build_variable_stmt<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Var,
        CompilationIssueCode::E0001,
        "Expected 'var' keyword.".into(),
    )?;

    let modificators: Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;
    let is_volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();

    let local_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let name: &str = local_tk.get_lexeme();
    let ascii_name: &str = local_tk.get_ascii_lexeme();
    let span: Span = local_tk.get_span();

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Colon])?;

    ctx.consume(
        TokenType::Colon,
        CompilationIssueCode::E0001,
        "Expected ':'.".into(),
    )?;

    let mut assume_local_value_type: bool = false;

    let mut local_type: Type = if ctx.check(TokenType::Eq) {
        assume_local_value_type = true;
        Type::Void(span)
    } else {
        typegeneration::build_type(ctx, false)?
    };

    if ctx.match_token(TokenType::SemiColon)? {
        let metadata: LocalMetadata = LocalMetadata::new(true, true, is_volatile, atomic_ord);

        if !ctx.is_main_scope() {
            ctx.get_mut_symbols()
                .new_local(name, (local_type.clone(), metadata, span), span)?;

            let local: Ast = Ast::Var {
                name,
                ascii_name,
                kind: local_type,
                value: None,
                attributes,
                modificators,
                metadata,
                span,
                id: NodeId::new(),
            };

            Ok(local)
        } else {
            Ok(Ast::invalid_ast(span))
        }
    } else {
        let attributes: ThrustAttributes =
            attributes::build_compiler_attributes(ctx, &[TokenType::SemiColon, TokenType::Eq])?;

        let metadata: LocalMetadata = LocalMetadata::new(false, true, is_volatile, atomic_ord);

        ctx.consume(
            TokenType::Eq,
            CompilationIssueCode::E0001,
            String::from("Expected '='."),
        )?;

        if !assume_local_value_type {
            ctx.get_mut_type_context()
                .add_infered_type(local_type.clone());
        }

        ctx.get_mut_control_context()
            .set_position(Position::Variable);

        let value: Ast = expressions::parse_expression(ctx)?;
        let value_type: &Type = value.get_value_type()?;

        if assume_local_value_type {
            local_type = value_type.clone();
        }

        if !assume_local_value_type {
            ctx.get_mut_type_context().pop_infered_type();
        }

        ctx.get_mut_control_context().reset_position();

        if let Some(infered_inner_type) = local_type.inferer_inner_type_from_type(value_type) {
            local_type = infered_inner_type;
        }

        if !ctx.is_main_scope() {
            ctx.get_mut_symbols()
                .new_local(name, (local_type.clone(), metadata, span), span)?;

            let local: Ast = Ast::Var {
                name,
                ascii_name,
                kind: local_type,
                value: Some(value.into()),
                attributes,
                modificators,
                metadata,
                span,
                id: NodeId::new(),
            };

            Ok(local)
        } else {
            Ok(Ast::invalid_ast(span))
        }
    }
}
