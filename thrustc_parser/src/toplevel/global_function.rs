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
use thrustc_entities::parser_entities::{FunctionParameterNames, FunctionParametersTypes};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_parser_table::GenericFunctionEntry;
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

    let has_generics: bool = ctx.check(TokenType::LBracket);

    if has_generics {
        ctx.get_mut_symbols().begin_generic_scope();
    }

    let type_params: Vec<String> = crate::generics::parse_type_parameters(ctx)?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let mut parameters: Vec<Ast> = Vec::with_capacity(16);
    let mut parameters_types: Vec<Type> = Vec::with_capacity(16);
    let mut parameter_names: Vec<&'parser str> = Vec::with_capacity(16);
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
        parameter_names.push(name);

        parameters.push(Ast::FunctionParameter {
            name: name.to_string(),
            ascii_name: ascii_name.to_string(),
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

    let is_generic: bool = !type_params.is_empty();

    if is_generic {
        ctx.get_mut_symbols().new_generic_function(
            name,
            GenericFunctionEntry {
                name: name.to_string(),
                type_params,
                parameter_types: parameters_types.clone(),
                parameter_names: parameter_names.iter().map(|name| name.to_string()).collect(),
                return_type: return_type.clone(),
                attributes: attributes.clone(),
                has_local_template: true,
                has_varargs: function_has_ignore,
                span,
            },
        );
    }

    if parse_forward {
        if !is_generic {
            ctx.get_mut_symbols().new_function(
                name,
                (
                    return_type,
                    FunctionParametersTypes(parameters_types),
                    FunctionParameterNames(parameter_names),
                    function_has_ignore,
                ),
            )?;
        }

        if has_generics {
            ctx.get_mut_symbols().end_generic_scope();
        }

        Ok(Ast::new_nullptr(span))
    } else {
        if ctx.check(TokenType::SemiColon) {
            ctx.consume(
                TokenType::SemiColon,
                CompilationIssueCode::E0001,
                "Expected ';'.".into(),
            )?;

            if has_generics {
                ctx.get_mut_symbols().end_generic_scope();
            }

            let prototype: Ast = Ast::Function {
                name: name.to_string(),
                ascii_name: ascii_name.to_string(),
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

        ctx.set_current_function_name(name);

        let function_body: Ast = code_block::parse_code_block_stmt(ctx)?;

        ctx.clear_current_function_name();

        ctx.get_mut_symbols().finish_parameters();

        if has_generics {
            ctx.get_mut_symbols().end_generic_scope();
        }

        let mut prototype: Ast = Ast::Function {
            name: name.to_string(),
            ascii_name: ascii_name.to_string(),
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
