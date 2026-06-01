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
    Ast,
    ast_builtins::AstBuiltin,
    traits::{AstCodeLocation, AstGetType},
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypeIsExtensions, VoidTypeExtensions},
};

use crate::TypeChecker;

pub fn validate<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    builtin: &'type_checker AstBuiltin,
) -> Result<(), CompilationIssue> {
    match builtin {
        AstBuiltin::MemSet {
            dst,
            new_size,
            size,
            ..
        } => self::validate_memset(typechecker, dst, new_size, size),

        AstBuiltin::MemMove { dst, src, size, .. } => {
            self::validate_memmove(typechecker, dst, src, size)
        }

        AstBuiltin::MemCpy { dst, src, size, .. } => {
            self::validate_memcpy(typechecker, dst, src, size)
        }

        AstBuiltin::Halloc { .. }
        | AstBuiltin::AlignOf { .. }
        | AstBuiltin::SizeOf { .. }
        | AstBuiltin::AbiSizeOf { .. }
        | AstBuiltin::BitSizeOf { .. }
        | AstBuiltin::AbiAlignOf { .. } => Ok(()),
    }
}

pub fn validate_memmove<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    destination: &'type_checker Ast,
    source: &'type_checker Ast,
    size: &'type_checker Ast,
) -> Result<(), CompilationIssue> {
    let source_type: &Type = source.get_value_type()?;
    let source_span: Span = source.get_span();

    let destination_type: &Type = destination.get_value_type()?;
    let destination_span: Span = destination.get_span();

    let size_type: &Type = size.get_value_type()?;
    let size_span: Span = size.get_span();

    if source_type.contains_void_type() || source_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            source_type.get_span(),
        ));
    }

    if destination_type.contains_void_type() || destination_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            destination_type.get_span(),
        ));
    }

    if size_type.contains_void_type() || size_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            size_type.get_span(),
        ));
    }

    if !source_type.is_ptr_type() && !source_type.is_address_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected pointer type, got '{}' type.", source_type),
            "You should make the type match.".into(),
            None,
            source_span,
        ));
    }

    if !destination_type.is_ptr_type() && !destination_type.is_address_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected pointer type, got '{}' type.", destination_type),
            "You should make the type match.".into(),
            None,
            destination_span,
        ));
    }

    if !size_type.is_unsigned_integer_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected unsigned integer type, got '{}' type.", size_type),
            "You should make the type match.".into(),
            None,
            size_span,
        ));
    }

    typechecker.analyze_expr(source)?;
    typechecker.analyze_expr(destination)?;
    typechecker.analyze_expr(size)?;

    Ok(())
}

pub fn validate_memcpy<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    destination: &'type_checker Ast,
    source: &'type_checker Ast,
    size: &'type_checker Ast,
) -> Result<(), CompilationIssue> {
    let source_type: &Type = source.get_value_type()?;
    let source_span: Span = source.get_span();

    let destination_type: &Type = destination.get_value_type()?;
    let destination_span: Span = destination.get_span();

    let size_type: &Type = size.get_value_type()?;
    let size_span: Span = size.get_span();

    if source_type.contains_void_type() || source_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            source_type.get_span(),
        ));
    }

    if destination_type.contains_void_type() || destination_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            destination_type.get_span(),
        ));
    }

    if size_type.contains_void_type() || size_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            size_type.get_span(),
        ));
    }

    if !source_type.is_ptr_type() && !source_type.is_address_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected pointer type, got '{}' type.", source_type),
            "You should make the type match.".into(),
            None,
            source_span,
        ));
    }

    if !destination_type.is_ptr_type() && !destination_type.is_address_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected pointer type, got '{}' type.", destination_type),
            "You should make the type match.".into(),
            None,
            destination_span,
        ));
    }

    if size_type.is_unsigned_integer_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected unsigned integer type, got '{}' type.", size_type),
            "You should make the type match.".into(),
            None,
            size_span,
        ));
    }

    typechecker.analyze_expr(source)?;
    typechecker.analyze_expr(destination)?;
    typechecker.analyze_expr(size)?;

    Ok(())
}

pub fn validate_memset<'type_checker>(
    typechecker: &mut TypeChecker<'type_checker>,
    destination: &'type_checker Ast,
    new_size: &'type_checker Ast,
    size: &'type_checker Ast,
) -> Result<(), CompilationIssue> {
    let destination_type: &Type = destination.get_value_type()?;
    let destination_span: Span = destination.get_span();

    let new_size_type: &Type = new_size.get_value_type()?;
    let new_size_span: Span = new_size.get_span();

    let size_type: &Type = size.get_value_type()?;
    let size_span: Span = size.get_span();

    if destination_type.contains_void_type() || destination_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            destination_type.get_span(),
        ));
    }

    if new_size_type.contains_void_type() || new_size_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            new_size_type.get_span(),
        ));
    }

    if size_type.contains_void_type() || size_type.is_void_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            "Cannot use 'void' as a value.".into(),
            "You should remove whatever type or value where void type belongs.".into(),
            None,
            size_type.get_span(),
        ));
    }

    if !destination_type.is_ptr_type() && !destination_type.is_address_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected pointer type, got '{}' type.", size_type),
            "You should make the type match.".into(),
            None,
            destination_span,
        ));
    }

    if !new_size_type.is_unsigned_integer_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!(
                "Expected unsigned integer type, got '{}' type.",
                new_size_type
            ),
            "You should make the type match.".into(),
            None,
            new_size_span,
        ));
    }

    if !size_type.is_unsigned_integer_type() {
        typechecker.add_error_report(CompilationIssue::Error(
            CompilationIssueCode::E0019,
            format!("Expected unsigned integer type, got '{}' type.", size_type),
            "You should make the type match.".into(),
            None,
            size_span,
        ));
    }

    typechecker.analyze_expr(destination)?;
    typechecker.analyze_expr(new_size)?;
    typechecker.analyze_expr(size)?;

    Ok(())
}
