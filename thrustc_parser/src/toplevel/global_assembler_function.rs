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

use thrustc_ast::{
    Ast, NodeId,
    traits::{AstCodeLocation, AstStandardExtensions},
};
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_entities::parser_entities::AssemblerFunctionParametersTypes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::{TokenType, traits::TokenTypeAttributesExtensions};
use thrustc_typesystem::Type;

use crate::{ParserContext, attributes, expressions, typegeneration};

pub fn build_assembler_function<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::AsmFn,
        CompilationIssueCode::E0001,
        "Expected 'asmfn' keyword.".into(),
    )?;

    let asm_function_name_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    ctx.consume(
        TokenType::LParen,
        CompilationIssueCode::E0001,
        "Expected '('.".into(),
    )?;

    let asm_function_name: &str = asm_function_name_tk.get_lexeme();
    let asm_function_ascii_name: &str = asm_function_name_tk.get_ascii_lexeme();

    let span: Span = asm_function_name_tk.get_span();

    let mut parameters: Vec<Ast> = Vec::with_capacity(u8::MAX as usize);
    let mut parameters_types: Vec<Type> = Vec::with_capacity(u8::MAX as usize);

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

        let parameter_name: &str = parameter_name_tk.get_lexeme();
        let parameter_span: Span = parameter_name_tk.get_span();

        let parameter_type: Type = typegeneration::build_type(ctx, false)?;

        parameters_types.push(parameter_type.clone());

        parameters.push(Ast::AssemblerFunctionParameter {
            name: parameter_name,
            kind: parameter_type,
            position: parameter_position,
            span: parameter_span,
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

        Type::Void(span)
    } else {
        typegeneration::build_type(ctx, false)?
    };

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::LBrace])?;
    let is_public: bool = attributes.has_public_attribute();

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let mut assembler: String = String::with_capacity(u8::MAX as usize);
    let mut assembler_pos: usize = 0;

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        let raw_str: Ast = expressions::parse_expr(ctx)?;
        let raw_str_span: Span = raw_str.get_span();

        if !raw_str.is_cstring() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "It is not a null terminated.".into(),
                "You should write a literal string with null termination.".into(),
                None,
                raw_str_span,
            ));
        }

        let assembly: String = if let Ast::CString { bytes, .. } = raw_str {
            String::from_utf8_lossy(&bytes).to_string()
        } else {
            String::new()
        };

        if assembler_pos != 0 {
            assembler.push('\n');
        }

        assembler.push_str(&assembly);

        if ctx.check(TokenType::RBrace) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        assembler_pos = assembler_pos.saturating_add(1);
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    ctx.consume(
        TokenType::LBrace,
        CompilationIssueCode::E0001,
        "Expected '{'.".into(),
    )?;

    let mut constraints: String = String::with_capacity(u8::MAX as usize);
    let mut constraint_pos: usize = 0;

    loop {
        if ctx.check(TokenType::RBrace) {
            break;
        }

        let raw_str: Ast = expressions::parse_expr(ctx)?;
        let raw_str_span: Span = raw_str.get_span();

        if !raw_str.is_cstring() {
            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                "It is not a null terminated.".into(),
                "You should write a literal string with null termination.".into(),
                None,
                raw_str_span,
            ));
        }

        let constraint: String = if let Ast::CString { bytes, .. } = raw_str {
            String::from_utf8_lossy(&bytes).to_string()
        } else {
            String::new()
        };

        if constraint_pos != 0 {
            constraints.push('\n');
        }

        constraints.push_str(&constraint);

        if ctx.check(TokenType::RBrace) {
            break;
        } else {
            ctx.consume(
                TokenType::Comma,
                CompilationIssueCode::E0001,
                "Expected ','.".into(),
            )?;
        }

        constraint_pos = constraint_pos.saturating_add(1);
    }

    ctx.consume(
        TokenType::RBrace,
        CompilationIssueCode::E0001,
        "Expected '}'.".into(),
    )?;

    if parse_forward {
        let parameters_types_repr: AssemblerFunctionParametersTypes =
            AssemblerFunctionParametersTypes(parameters_types);

        ctx.get_mut_symbols().new_asm_function(
            asm_function_name,
            (return_type, parameters_types_repr, is_public),
        )?;

        Ok(Ast::new_nullptr(span))
    } else {
        Ok(Ast::AssemblerFunction {
            name: asm_function_name,
            ascii_name: asm_function_ascii_name,
            parameters,
            parameters_types,
            assembler,
            constraints,
            return_type,
            attributes,
            span,
            id: NodeId::new(),
        })
    }
}
