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
use thrustc_attributes::{ThrustAttributes, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::AssemblerFunctionParametersTypes;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::TypeCodeLocation};

use crate::{ParserContext, attributes, typegeneration};

pub fn build_assembler_function<'parser>(
    ctx: &mut ParserContext<'parser>,
    parse_forward: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    ctx.consume(
        TokenType::Asm,
        CompilationIssueCode::E0001,
        "Expected 'asm' keyword.".into(),
    )?;

    ctx.consume(
        TokenType::Fn,
        CompilationIssueCode::E0001,
        "Expected 'fn' keyword.".into(),
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

        let parameter_type: Type = typegeneration::build_type(ctx, false)?;
        let parameter_span: Span = parameter_type.get_span();

        parameters_types.push(parameter_type.clone());

        parameters.push(Ast::AssemblerFunctionParameter {
            name: "",
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

    let return_type: Type = typegeneration::build_type(ctx, false)?;

    let attributes: ThrustAttributes =
        attributes::build_compiler_attributes(ctx, &[TokenType::Assembler])?;
    let is_public: bool = attributes.has_public_attribute();

    let assembler_tk: &Token = ctx.consume(
        TokenType::Assembler,
        CompilationIssueCode::E0001,
        "Expected a '#asm{' block.".into(),
    )?;

    let raw_assembler: String = assembler_tk.get_lexeme().to_string();
    let mut assembler_lines: Vec<&str> = raw_assembler.lines().collect();

    while let Some(last) = assembler_lines.last() {
        let instruction: &str = last.trim();

        if instruction.is_empty() {
            assembler_lines.pop();

            continue;
        }

        if instruction.eq_ignore_ascii_case("ret") || instruction.eq_ignore_ascii_case("retq") {
            assembler_lines.pop();
        }

        break;
    }

    let assembler: String = assembler_lines.join("\n");

    let _ = ctx.match_token(TokenType::SemiColon)?;

    if parse_forward {
        let parameters_types_repr: AssemblerFunctionParametersTypes =
            AssemblerFunctionParametersTypes(parameters_types);

        ctx.get_mut_symbols().new_asm_function(
            asm_function_name.to_string(),
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
            return_type,
            attributes,
            span,
            id: NodeId::new(),
        })
    }
}
