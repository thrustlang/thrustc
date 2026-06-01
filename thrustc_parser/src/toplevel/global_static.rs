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

use thrustc_ast::{Ast, NodeId, ast_metadata::StaticMetadata, traits::AstGetType};
use thrustc_ast_modificators::{Modificators, traits::ModificatorsExtensions};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_mir::{atomicord::ThrustAtomicOrdering, threadmode::ThrustThreadMode};
use thrustc_parser_context::{Position, traits::TypeContextExtensions};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::InfererTypeExtensions};

use crate::{ParserContext, attributes, expressions, modificators, typegeneration};

pub fn build_global_static<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Static,
        CompilationIssueCode::E0001,
        "Expected 'static' keyword.".into(),
    )?;

    let is_mutable: bool = ctx.match_token(TokenType::Mut)?;

    let modificators: Modificators =
        modificators::build_statement_modificator(ctx, &[TokenType::Identifier])?;

    let thread_local: bool = modificators.has_lazythread_modificator();
    let is_volatile: bool = modificators.has_volatile_modificator();
    let atomic_ord: Option<ThrustAtomicOrdering> = modificators.get_atomic_ordering_modificator();
    let thread_mode: Option<ThrustThreadMode> = modificators.get_thread_mode_modificator();

    let static_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let name: &str = static_tk.get_lexeme();
    let ascii_name: &str = static_tk.get_ascii_lexeme();

    let span: Span = static_tk.get_span();

    ctx.consume(
        TokenType::Colon,
        CompilationIssueCode::E0001,
        "Expected ':'.".into(),
    )?;

    let mut static_type: Type = typegeneration::build_type(ctx, false)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Eq])?;
    let external: bool = attributes.has_extern_attribute();

    if ctx.match_token(TokenType::SemiColon)? {
        let metadata: StaticMetadata = StaticMetadata::new(
            true,
            is_mutable,
            true,
            thread_local,
            is_volatile,
            external,
            atomic_ord,
            thread_mode,
        );

        if parse_forward {
            ctx.get_mut_symbols()
                .new_global_static(name, (static_type, metadata, attributes))?;

            Ok(Ast::new_nullptr(span))
        } else {
            let static_: Ast = Ast::Static {
                name,
                ascii_name,
                kind: static_type,
                value: None,
                attributes,
                modificators,
                metadata,
                span,
                id: NodeId::new(),
            };

            Ok(static_)
        }
    } else {
        ctx.consume(
            TokenType::Eq,
            CompilationIssueCode::E0001,
            "Expected '='.".into(),
        )?;

        ctx.get_mut_control_context().set_position(Position::Static);
        ctx.get_mut_type_context()
            .add_infered_type(static_type.clone());

        let value: Ast = expressions::parse_expression(ctx)?;
        let value_type: &Type = value.get_value_type()?;

        ctx.get_mut_type_context().pop_infered_type();
        ctx.get_mut_control_context().reset_position();

        static_type.inferer_inner_type_from_type(value_type);

        let metadata: StaticMetadata = StaticMetadata::new(
            true,
            is_mutable,
            false,
            thread_local,
            is_volatile,
            external,
            atomic_ord,
            thread_mode,
        );

        if parse_forward {
            ctx.get_mut_symbols()
                .new_global_static(name, (static_type, metadata, attributes))?;

            Ok(Ast::new_nullptr(span))
        } else {
            let static_: Ast = Ast::Static {
                name,
                ascii_name,
                kind: static_type,
                value: Some(value.into()),
                attributes,
                modificators,
                metadata,
                span,
                id: NodeId::new(),
            };

            Ok(static_)
        }
    }
}
