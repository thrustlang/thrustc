use thrustc_ast::{
    Ast,
    data::{EnumData, EnumDataField},
};
use thrustc_entities::parser::FoundSymbolId;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::Type;

use crate::{
    ParserContext,
    traits::{EnumExtensions, EnumFieldsExtensions, FoundSymbolEitherExtensions},
};

pub fn build_enum_value<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let field_tk: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected enum name.".into(),
    )?;

    let object: FoundSymbolId = ctx.get_symbols().get_symbols_id(name, span)?;

    let enum_id: (&str, usize) = object.expected_enum(span)?;
    let id: &str = enum_id.0;
    let scope_idx: usize = enum_id.1;

    let data: EnumData = ctx
        .get_symbols()
        .get_enum_by_id(id, scope_idx, span)?
        .get_fields();

    let field_name: &str = field_tk.get_lexeme();

    if !data.contain_field(field_name) {
        ctx.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Not found '{}' field in '{}' enum.", name, field_name),
            None,
            span,
        ));

        return Ok(Ast::invalid_ast(span));
    }

    let field: EnumDataField = data.get_field(field_name);

    let field_type: Type = field.1;
    let field_value: Ast = field.2;

    let canonical_name: String = format!("{}.{}", name, field_name);

    Ok(Ast::EnumValue {
        name: canonical_name,
        value: field_value.into(),
        kind: field_type,
        span,
    })
}
