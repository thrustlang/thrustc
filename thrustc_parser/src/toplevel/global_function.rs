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

use thrustc_ast::{Ast, NodeId, ast_metadata::FunctionParameterMetadata};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_entities::parser_entities::FunctionParametersTypes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{TokenType, traits::TokenTypeAttributesExtensions};
use thrustc_typesystem::{Type, traits::TypePointerExtensions};

use crate::{ParserContext, attributes, statements::code_block, typegeneration};

pub fn build_function<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Fn,
        CompilationIssueCode::E0001,
        "Expected 'fn' keyword.".into(),
    )?;

    let function_name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let name: &str = function_name_tk.get_lexeme();

    let ascii_name: &str = function_name_tk.get_ascii_lexeme();

    let span: Span = function_name_tk.get_span();

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let mut parameters: Vec<Ast> = Vec::with_capacity(16);
    let mut parameters_types: Vec<Type> = Vec::with_capacity(16);
    let mut parameter_position: u32 = 0;

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let parameter_name_tk: &Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected 'identifier'.".into(),
        )?;

        let name: &str = parameter_name_tk.get_lexeme();
        let ascii_name: &str = parameter_name_tk.get_ascii_lexeme();
        let span: Span = parameter_name_tk.get_span();

        ctx.consume(
            TokenType::Colon,
            CompilationIssueCode::E0001,
            "Expected ':'.".into(),
        )?;

        let kind: Type = typegeneration::build_type(ctx, false)?;
        let metadata: FunctionParameterMetadata =
            FunctionParameterMetadata::new(kind.is_ptr_like_type());

        parameters_types.push(kind.clone());

        parameters.push(Ast::FunctionParameter {
            name,
            ascii_name,
            kind,
            position: parameter_position,
            metadata,
            span,
            id: NodeId::new(),
        });

        parameter_position = parameter_position.saturating_add(1);

        if ctx.check(TokenType::RParen) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }
    }

    ctx.consume(
        TokenType::RParen,
        CompilationIssueCode::E0001,
        "Expected ')'.".into(),
    )?;

    let return_type: Type = if ctx.check(TokenType::LBrace) || ctx.peek().get_type().is_attribute()
    {
        let peeked: &Token = ctx.peek();
        let peeked_type: TokenType = peeked.get_type();

        let span: Span = if peeked_type.is_attribute() {
            peeked.get_span()
        } else {
            ctx.previous().get_span()
        };

        Type::Void { span }
    } else {
        typegeneration::build_type(ctx, false)?
    };

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::SemiColon, TokenType::LBrace])?;
    let function_has_ignore: bool = attributes.has_ignore_attribute();

    if parse_forward {
        ctx.get_mut_symbols().new_function(
            name,
            (
                return_type,
                FunctionParametersTypes(parameters_types),
                function_has_ignore,
            ),
        )?;

        Ok(Ast::new_nullptr(span))
    } else {
        if ctx.check(TokenType::SemiColon) {
            ctx.consume(
                TokenType::SemiColon,
                CompilationIssueCode::E0001,
                "Expected ';'.".into(),
            )?;

            let prototype: Ast = Ast::Function {
                name,
                ascii_name,
                parameters,
                parameter_types: parameters_types,
                body: None,
                return_type,
                attributes,
                span,
                id: NodeId::new(),
            };

            return Ok(prototype);
        }

        ctx.get_mut_symbols().new_parameters(&parameters)?;

        let function_body: Ast = code_block::parse_code_block_stmt(ctx)?;

        ctx.get_mut_symbols().finish_parameters();

        let mut prototype: Ast = Ast::Function {
            name,
            ascii_name,
            parameters,
            parameter_types: parameters_types,
            body: None,
            return_type,
            attributes,
            span,
            id: NodeId::new(),
        };

        if let Ast::Function { body, .. } = &mut prototype {
            *body = Some(function_body.into());
        }

        Ok(prototype)
    }
}
