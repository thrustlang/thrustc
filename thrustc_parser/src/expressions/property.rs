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
    ast_logic_data::{PropertyData, StructureData},
    ast_metadata::PropertyMetadata,
    traits::{AstGetType, AstMemoryExtensions, AstStructureDataExtensions},
};
use thrustc_code_location::Span;
use thrustc_entities::parser_entities::Struct;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeExtensions, TypePointerExtensions},
};

use thrustc_parser_table::traits::{
    FoundSymbolEitherExtensions, FoundSymbolExtensions, StructSymbolExtensions,
};

use crate::{ParserContext, abort};

pub fn build_property<'parser>(
    ctx: &mut ParserContext<'parser>,
    source: Ast<'parser>,
    deref: bool,
) -> Result<Ast<'parser>, CompilationIssue> {
    let base_type: &Type = source.get_value_type()?;

    let mut property_names: Vec<&str> = Vec::with_capacity(u8::MAX as usize);

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
        self::decompose_struct_property(ctx, 0, &source, property_names, base_type, span, deref);

    match properties_result {
        Ok(properties) => {
            let kind: Type = properties.0;
            let data: PropertyData = properties.1;

            let metadata: PropertyMetadata = PropertyMetadata::new(kind.is_ptr_like_type(), deref);

            Ok(Ast::Property {
                source: source.into(),
                data,
                kind,
                metadata,
                span,
                id: NodeId::new(),
            })
        }
        Err(error) => {
            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}

fn decompose_struct_property<'parser>(
    ctx: &mut ParserContext<'parser>,
    mut position: usize,
    source: &Ast,
    property_names: Vec<&str>,
    base_type: &Type,
    span: Span,
    deref: bool,
) -> Result<(Type, PropertyData), CompilationIssue> {
    let mut indices: PropertyData = PropertyData::with_capacity(u8::MAX as usize);
    let mut is_parent_ptr: bool = false;

    if position >= property_names.len() {
        return Ok((base_type.clone(), indices));
    }

    let current_type: &Type = match base_type {
        Type::Ptr {
            subtype: Some(inner_type),
            ..
        } => {
            is_parent_ptr = true;
            inner_type
        }

        _ => base_type,
    };

    let current_property_name: &str = property_names.get(position).unwrap_or_else(|| {
        abort::abort_compilation(
            ctx.get_mut_diagnostician(),
            CompilationPosition::Parser,
            "Cannot be parsed correctly!",
            span,
            std::path::PathBuf::from(file!()),
            line!(),
        )
    });

    if let Type::Struct { name, fields, .. } = current_type {
        let resolved: Option<(usize, Type)> = if let Ok(object) =
            ctx.get_symbols().get_symbols_id(name, span)
        {
            if !object.is_structure() {
                None
            } else {
                let struct_id: (&str, usize) = object.expected_struct(span)?;
                let id: &str = struct_id.0;
                let scope_idx: usize = struct_id.1;

                let structure: Struct = ctx.get_symbols().get_struct_by_id(id, scope_idx, span)?;
                let data: StructureData = structure.get_data();

                data.get_struct_fields()
                    .iter()
                    .enumerate()
                    .find(|(_, (other_property_name, ..))| {
                        *other_property_name == current_property_name
                    })
                    .map(|(index, (_, field_type, ..))| (index, field_type.clone()))
            }
        } else {
            ctx.get_symbols()
                .get_generic_struct(name)
                .and_then(|generic| {
                    generic
                        .field_names
                        .iter()
                        .position(|other| *other == current_property_name)
                        .and_then(|index| {
                            fields
                                .get(index)
                                .cloned()
                                .map(|field_type| (index, field_type))
                        })
                })
        };

        let Some((index, field_type)) = resolved else {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                "Unknown property".into(),
                "You should make sure that it exist in the structure type reference.".into(),
                None,
                span,
            ));
        };

        let adjusted_inner_type: Type =
            if (is_parent_ptr || source.is_memory_assigned_value()?) && !deref {
                Type::Ptr {
                    subtype: Some(field_type.clone().into()),
                    address_space: field_type.get_address_space(),
                    span: field_type.get_span(),
                }
            } else {
                field_type.clone()
            };

        indices.push((
            current_type.clone(),
            (
                adjusted_inner_type.clone(),
                u32::try_from(index).unwrap_or(u32::MAX),
            ),
        ));

        position = position.saturating_add(1);

        let (field_inner_type, mut nested_indices) = self::decompose_struct_property(
            ctx,
            position,
            source,
            property_names,
            &field_type,
            span,
            deref,
        )?;

        {
            for (base_subtype, ..) in nested_indices.iter_mut() {
                *base_subtype = if (is_parent_ptr || source.is_memory_assigned_value()?) && !deref {
                    Type::Ptr {
                        subtype: Some(base_subtype.clone().into()),
                        address_space: base_subtype.get_address_space(),
                        span: base_subtype.get_span(),
                    }
                } else {
                    base_subtype.clone()
                };
            }
        }

        indices.append(&mut nested_indices);

        let adjusted_inner_type: Type =
            if (is_parent_ptr || source.is_memory_assigned_value()?) && !deref {
                Type::Ptr {
                    subtype: Some(field_inner_type.clone().into()),
                    address_space: field_inner_type.get_address_space(),
                    span: field_inner_type.get_span(),
                }
            } else {
                field_inner_type
            };

        return Ok((adjusted_inner_type, indices));
    }

    if position < property_names.len() {
        return Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            "Unknown property".into(),
            "You should make sure that it exist in the structure type reference.".into(),
            None,
            span,
        ));
    }

    Ok((base_type.clone(), indices))
}
