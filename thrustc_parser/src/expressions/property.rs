use thrustc_ast::{
    Ast,
    data::{PropertyData, StructDataField, StructureData},
    metadata::PropertyMetadata,
    traits::{AstGetType, AstMemoryExtensions, AstStructureDataExtensions},
};
use thrustc_entities::parser::{FoundSymbolId, Struct};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{Type, traits::TypeCodeLocation};

use crate::{
    ParserContext,
    traits::{FoundSymbolEitherExtensions, StructSymbolExtensions},
};

pub fn build_property<'parser>(
    ctx: &mut ParserContext<'parser>,
    source: Ast<'parser>,
) -> Result<Ast<'parser>, CompilationIssue> {
    let base_type: &Type = source.get_value_type()?;
    let metadata: PropertyMetadata = PropertyMetadata::new(source.is_allocated());

    let mut property_names: Vec<&str> = Vec::with_capacity(10);

    let first: &Token = ctx.consume(
        TokenType::Identifier,
        CompilationIssueCode::E0001,
        "Expected identifier.".into(),
    )?;

    let mut span: Span = first.get_span();

    property_names.push(first.get_lexeme());

    while ctx.match_token(TokenType::Dot)? {
        let property: &Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected identifier.".into(),
        )?;

        span = property.get_span();
        property_names.push(property.get_lexeme());
    }

    let properties_result: Result<(Type, PropertyData), CompilationIssue> =
        self::decompose(ctx, 0, &source, property_names, base_type, span);

    match properties_result {
        Ok(properties) => {
            let kind: Type = properties.0;
            let data: PropertyData = properties.1;

            Ok(Ast::Property {
                source: source.into(),
                data,
                kind,
                metadata,
                span,
            })
        }
        Err(error) => {
            ctx.add_error(error);

            Ok(Ast::invalid_ast(span))
        }
    }
}

fn decompose<'parser>(
    ctx: &mut ParserContext<'parser>,
    mut position: usize,
    source: &Ast,
    property_names: Vec<&str>,
    base_type: &Type,
    span: Span,
) -> Result<(Type, PropertyData), CompilationIssue> {
    let mut indices: PropertyData = PropertyData::with_capacity(u8::MAX as usize);
    let mut is_parent_ptr: bool = false;

    if position >= property_names.len() {
        return Ok((base_type.clone(), indices));
    }

    let current_type: &Type = match base_type {
        Type::Ptr(Some(inner_type), ..) => {
            is_parent_ptr = true;
            inner_type
        }

        _ => base_type,
    };

    let property_name: &str = property_names[position];

    if let Type::Struct(name, ..) = current_type {
        let object: FoundSymbolId = ctx.get_symbols().get_symbols_id(name, span)?;

        let struct_id: (&str, usize) = object.expected_struct(span)?;
        let id: &str = struct_id.0;
        let scope_idx: usize = struct_id.1;

        let structure: Struct = ctx.get_symbols().get_struct_by_id(id, scope_idx, span)?;
        let data: StructureData = structure.get_data();

        let field: Option<StructDataField> = data
            .get_fields()
            .iter()
            .enumerate()
            .find(|(_, (other_property_name, ..))| *other_property_name == property_name);

        let Some((index, (_, field_type, ..))) = field else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0001,
                format!("Expected a property, got '{}'.", property_name),
                None,
                span,
            ));
        };

        let adjusted_inner_type: Type = if is_parent_ptr || source.is_allocated() {
            Type::Ptr(Some(field_type.clone().into()), field_type.get_span())
        } else {
            field_type.clone()
        };

        indices.push((
            current_type.clone(),
            (
                adjusted_inner_type.clone(),
                u32::try_from(index).map_err(|_| {
                    CompilationIssue::Error(
                        CompilationIssueCode::E0037,
                        "Too deeper for property indexing.".into(),
                        None,
                        span,
                    )
                })?,
            ),
        ));

        position = position.saturating_add(1);

        let (field_inner_type, mut nested_indices) =
            self::decompose(ctx, position, source, property_names, field_type, span)?;

        {
            for (base_subtype, ..) in nested_indices.iter_mut() {
                *base_subtype = if is_parent_ptr || source.is_allocated() {
                    Type::Ptr(Some(base_subtype.clone().into()), base_subtype.get_span())
                } else {
                    base_subtype.clone()
                };
            }
        }

        indices.append(&mut nested_indices);

        let adjusted_inner_type: Type = if is_parent_ptr || source.is_allocated() {
            Type::Ptr(
                Some(field_inner_type.clone().into()),
                field_inner_type.get_span(),
            )
        } else {
            field_inner_type
        };

        return Ok((adjusted_inner_type, indices));
    }

    if position < property_names.len() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0001,
            format!("Property '{}' isn't a structure.", property_name),
            None,
            span,
        ));
    }

    Ok((base_type.clone(), indices))
}
