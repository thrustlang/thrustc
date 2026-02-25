use thrustc_ast::{Ast, metadata::FunctionParameterMetadata};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_entities::parser::FunctionParametersTypes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{TokenType, traits::TokenTypeAttributesExtensions};
use thrustc_typesystem::Type;

use crate::{ParserContext, attributes, statements::block, typegen};

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

    let mut parameters: Vec<Ast> = Vec::with_capacity(12);
    let mut parameters_types: Vec<Type> = Vec::with_capacity(12);
    let mut parameter_position: u32 = 0;

    loop {
        if ctx.check(TokenType::RParen) {
            break;
        }

        let is_mutable: bool = ctx.match_token(TokenType::Mut)?;

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

        let parameter_type: Type = typegen::build_type(ctx, false)?;

        parameters_types.push(parameter_type.clone());

        parameters.push(Ast::FunctionParameter {
            name,
            ascii_name,
            kind: parameter_type,
            position: parameter_position,
            metadata: FunctionParameterMetadata::new(is_mutable),
            span,
        });

        parameter_position += 1;

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

        Type::Void(span)
    } else {
        typegen::build_type(ctx, false)?
    };

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::SemiColon, TokenType::LBrace])?;
    let function_has_ignore: bool = attributes.has_ignore_attribute();

    if parse_forward {
        let proto: Ast = Ast::Function {
            name,
            ascii_name,
            parameters: parameters.clone(),
            parameter_types: parameters_types.clone(),
            body: None,
            return_type: return_type.clone(),
            attributes,
            span,
        };

        ctx.get_mut_symbols().new_function(
            name,
            (
                return_type,
                FunctionParametersTypes(parameters_types),
                function_has_ignore,
            ),
            span,
        )?;

        if ctx.match_token(TokenType::SemiColon)? {
            Ok(proto)
        } else {
            Ok(Ast::new_nullptr(span))
        }
    } else {
        if ctx.match_token(TokenType::SemiColon)? {
            let proto: Ast = Ast::Function {
                name,
                ascii_name,
                parameters,
                parameter_types: parameters_types,
                body: None,
                return_type,
                attributes,
                span,
            };

            return Ok(proto);
        }

        ctx.get_mut_symbols().new_parameters(&parameters)?;

        let function_body: Ast = block::build_block(ctx)?;

        ctx.get_mut_symbols().finish_parameters();

        let mut proto: Ast = Ast::Function {
            name,
            ascii_name,
            parameters,
            parameter_types: parameters_types,
            body: None,
            return_type,
            attributes,
            span,
        };

        if let Ast::Function { body, .. } = &mut proto {
            *body = Some(function_body.into());
        }

        Ok(proto)
    }
}
