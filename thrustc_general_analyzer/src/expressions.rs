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
    traits::{
        AstBaseReferenceExtensions, AstCodeLocation, AstGetType, AstMemoryExtensions,
        AstStandardExtensions,
    },
};
use thrustc_atomic_ordering::ThrustAtomicOrdering;
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_token_type::traits::TokenTypeExtensions;
use thrustc_typesystem::{
    Type,
    traits::{ConstantTypeExtensions, TypeExtensions, TypeIsExtensions, TypePointerExtensions},
};

use crate::GeneralAnalyzer;

pub fn validate_node<'analyzer>(
    analyzer: &mut GeneralAnalyzer<'analyzer>,
    node: &'analyzer Ast,
) -> Result<(), CompilationIssue> {
    match node {
        Ast::BinaryOp {
            left,
            right,
            operator,
            ..
        } => {
            if let Ast::Reference { metadata, .. } = &**left {
                if operator.is_compound_assignment_operator() && metadata.is_constant_ref() {
                    analyzer.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0038,
                        "Cannot assign to a constant; constants are immutable.".into(),
                        "You should remove the compound assignment or use a mutable variable instead."
                            .into(),
                        None,
                        left.get_span(),
                    ));
                }
            }

            analyzer.analyze_expr(left)?;
            analyzer.analyze_expr(right)?;

            Ok(())
        }

        Ast::UnaryOp { node, .. } => {
            analyzer.analyze_expr(node)?;

            Ok(())
        }

        Ast::Group { node, .. } => {
            analyzer.analyze_expr(node)?;

            Ok(())
        }

        Ast::FixedArray { items, .. } => {
            for node in items.iter() {
                analyzer.analyze_expr(node)?;
            }

            Ok(())
        }

        Ast::Array { items, .. } => {
            for node in items.iter() {
                analyzer.analyze_expr(node)?;
            }

            Ok(())
        }

        Ast::Index { source, index, .. } => {
            let source_type: &Type = source.get_any_type();

            if source.is_reference() && !source.is_memory_assigned_value()? {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0007,
                    "An reference with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            if (!source.is_memory_assigned_value()? || !source.is_reference())
                && source_type.is_value()
            {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            analyzer.analyze_expr(index)?;

            Ok(())
        }

        Ast::Load { source, .. } => {
            let source_type: &Type = source.get_any_type();

            if source.is_reference() && !source.is_memory_assigned_value()? {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0007,
                    "An reference with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            if (!source.is_memory_assigned_value()? || !source.is_reference())
                && source_type.is_value()
            {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    source.get_span(),
                ));
            }

            analyzer.analyze_expr(source)?;

            Ok(())
        }

        Ast::Property { source, .. } => {
            analyzer.analyze_expr(source)?;
            Ok(())
        }

        Ast::Constructor { data, .. } => {
            {
                for (_, node, ..) in data.iter() {
                    analyzer.analyze_expr(node)?;
                }
            }

            Ok(())
        }

        Ast::Call { args, .. } => {
            {
                for arg in args.iter() {
                    analyzer.analyze_expr(arg)?;
                }
            }

            Ok(())
        }

        Ast::IndirectCall { function, args, .. } => {
            analyzer.analyze_expr(function)?;

            {
                for argument in args.iter() {
                    analyzer.analyze_expr(argument)?;
                }
            }

            Ok(())
        }

        Ast::GetLocation { expr, span, .. } => {
            let expr_type: &Type = expr.get_value_type()?;

            if expr.is_reference() && !expr.is_memory_assigned_value()? {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0007,
                    "An reference with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    *span,
                ));
            } else if !expr.is_reference() && !expr_type.is_ptr_like_type() {
                analyzer.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0008,
                    "An value with memory address was expected.".into(),
                    "You should try to allocate it and pass it as a direct reference.".into(),
                    None,
                    *span,
                ));
            }

            analyzer.analyze_expr(expr)?;

            Ok(())
        }
        Ast::Deref { value, .. } => {
            analyzer.analyze_expr(value)?;
            Ok(())
        }
        Ast::As { from, .. } => {
            analyzer.analyze_expr(from)?;

            Ok(())
        }
        Ast::Builtin { builtin, .. } => match builtin {
            AstBuiltin::MemSet {
                dst,
                new_size,
                size,
                ..
            } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(new_size)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::MemMove { dst, src, size, .. } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(src)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::MemCpy { dst, src, size, .. } => {
                analyzer.analyze_expr(dst)?;
                analyzer.analyze_expr(src)?;
                analyzer.analyze_expr(size)?;

                Ok(())
            }

            AstBuiltin::Halloc { .. }
            | AstBuiltin::AbiSizeOf { .. }
            | AstBuiltin::BitSizeOf { .. }
            | AstBuiltin::AbiAlignOf { .. }
            | AstBuiltin::ArbitraryArg { .. }
            | AstBuiltin::ArbitraryArgs { .. }
            | AstBuiltin::DeferredCompileTime { .. } => Ok(()),

            AstBuiltin::AtomicRMW {
                destination,
                value,
                span,
                ..
            } => {
                analyzer.analyze_expr(destination)?;
                analyzer.analyze_expr(value)?;

                self::validate_atomic_operation(analyzer, destination, *span)?;
                self::validate_atomic_integer_operands(analyzer, destination, value, *span)?;

                Ok(())
            }

            AstBuiltin::AtomicCompareAndSwap {
                destination,
                expected,
                new_value,
                success,
                failure,
                span,
            } => {
                analyzer.analyze_expr(destination)?;
                analyzer.analyze_expr(expected)?;
                analyzer.analyze_expr(new_value)?;

                self::validate_atomic_operation(analyzer, destination, *span)?;
                self::validate_cmpxchg_orderings(analyzer, *success, *failure, *span)?;

                Ok(())
            }

            AstBuiltin::ArbitraryArgsStart { .. } => Ok(()),

            AstBuiltin::ArbitraryArgsCopy { source, .. } => {
                analyzer.analyze_expr(source)?;

                Ok(())
            }

            AstBuiltin::ArbitraryArgsEnd { list, .. } => {
                analyzer.analyze_expr(list)?;

                Ok(())
            }

            AstBuiltin::ArbitraryArgFrom { list, ty, span } => {
                analyzer.analyze_expr(list)?;

                self::validate_variadic_argument_type(analyzer, ty, *span)?;

                Ok(())
            }

            AstBuiltin::ArbitraryArgsCount { .. } => Ok(()),
        },

        Ast::AsmValue { .. }
        | Ast::EnumValue { .. }
        | Ast::Reference { .. }
        | Ast::Integer { .. }
        | Ast::Boolean { .. }
        | Ast::CString { .. }
        | Ast::CNString { .. }
        | Ast::Float { .. }
        | Ast::NullPtr { .. }
        | Ast::Char { .. } => Ok(()),

        _ => {
            let span: Span = node.get_span();

            analyzer.add_bug(CompilationIssue::FrontendBug(
                "Expression not caught".into(),
                "Expression could not be caught for processing.".into(),
                span,
                CompilationPosition::Analyzer,
                std::path::PathBuf::from(file!()),
                line!(),
            ));

            Ok(())
        }
    }
}

pub fn validate_atomic_operation(
    analyzer: &mut GeneralAnalyzer<'_>,
    destination: &Ast<'_>,
    span: Span,
) -> Result<(), CompilationIssue> {
    let reference: &Ast<'_> = match destination.get_base_reference() {
        Some(reference) => reference,

        None => {
            analyzer.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0008,
                "An value with memory address was expected.".into(),
                "The atomic operation destination must be a memory location backed by an atomic variable.".into(),
                None,
                span,
            ));

            return Ok(());
        }
    };

    let Ast::Reference { metadata, .. } = reference else {
        return Ok(());
    };

    let Some(atomic_ord) = metadata.get_atomic_ord() else {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The atomic operation target has no atomic ordering.".into(),
            "Declare the target with an atomic ordering modificator such as atomicRelax, atomicGrab, atomicDrop, atomicSync or atomicStrict.".into(),
            None,
            span,
        ));

        return Ok(());
    };

    if matches!(
        atomic_ord,
        ThrustAtomicOrdering::AtomicNone | ThrustAtomicOrdering::AtomicFree
    ) {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The atomic operation requires an ordering of at least atomicRelax.".into(),
            "Use an atomic ordering modificator stronger than atomicNone or atomicFree.".into(),
            None,
            span,
        ));
    }

    Ok(())
}

// Enforces the ordering rules of the LLVM cmpxchg instruction.
pub fn validate_cmpxchg_orderings(
    analyzer: &mut GeneralAnalyzer<'_>,
    success: ThrustAtomicOrdering,
    failure: ThrustAtomicOrdering,
    span: Span,
) -> Result<(), CompilationIssue> {
    // LLVM LangRef (cmpxchg): both the success and failure orderings must be at least monotonic.
    if matches!(
        success,
        ThrustAtomicOrdering::AtomicNone | ThrustAtomicOrdering::AtomicFree
    ) {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The success ordering of an atomic compare-and-swap must be at least atomicRelax."
                .into(),
            "Use an atomic ordering modificator stronger than atomicNone or atomicFree.".into(),
            None,
            span,
        ));
    }

    if matches!(
        failure,
        ThrustAtomicOrdering::AtomicNone | ThrustAtomicOrdering::AtomicFree
    ) {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The failure ordering of an atomic compare-and-swap must be at least atomicRelax."
                .into(),
            "Use an atomic ordering modificator stronger than atomicNone or atomicFree.".into(),
            None,
            span,
        ));
    }

    // LLVM LangRef (cmpxchg): the failure ordering must not be release or acquire-release.
    if matches!(
        failure,
        ThrustAtomicOrdering::AtomicDrop | ThrustAtomicOrdering::AtomicSync
    ) {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The failure ordering of an atomic compare-and-swap cannot be release or acquire-release.".into(),
            "Use a failure ordering such as atomicRelax or atomicGrab.".into(),
            None,
            span,
        ));
    }

    // LLVM LangRef (cmpxchg): the failure ordering must not be stricter than the success ordering.
    // Ranks each ordering so they can be compared: relax < grab < drop < sync < strict.
    let success_strength: u8 = match success {
        ThrustAtomicOrdering::AtomicRelax => 0,
        ThrustAtomicOrdering::AtomicGrab => 1,
        ThrustAtomicOrdering::AtomicDrop => 2,
        ThrustAtomicOrdering::AtomicSync => 3,
        ThrustAtomicOrdering::AtomicStrict => 4,

        _ => 0,
    };

    let failure_strength: u8 = match failure {
        ThrustAtomicOrdering::AtomicRelax => 0,
        ThrustAtomicOrdering::AtomicGrab => 1,
        ThrustAtomicOrdering::AtomicDrop => 2,
        ThrustAtomicOrdering::AtomicSync => 3,
        ThrustAtomicOrdering::AtomicStrict => 4,

        _ => 0,
    };

    // Rejects a failure ordering stricter than the success ordering.
    if failure_strength > success_strength {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            "The failure ordering of an atomic compare-and-swap cannot be stronger than the success ordering.".into(),
            "Use a failure ordering equal to or weaker than the success ordering.".into(),
            None,
            span,
        ));
    }

    Ok(())
}

pub fn validate_atomic_integer_operands(
    analyzer: &mut GeneralAnalyzer<'_>,
    destination: &Ast<'_>,
    value: &Ast<'_>,
    span: Span,
) -> Result<(), CompilationIssue> {
    let destination_type: &Type = destination.get_value_type()?;
    let value_type: &Type = value.get_value_type()?;

    if !destination_type.is_integer_type() {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            format!(
                "The atomic operation requires an integer destination, got '{}' type.",
                destination_type
            ),
            "Use an integer variable as the atomic operation destination.".into(),
            None,
            span,
        ));
    }

    if !value_type.is_integer_type() {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0056,
            format!(
                "The atomic operation requires an integer value, got '{}' type.",
                value_type
            ),
            "Use an integer value as the atomic operation operand.".into(),
            None,
            span,
        ));
    }

    Ok(())
}

pub fn validate_variadic_argument_type(
    analyzer: &mut GeneralAnalyzer<'_>,
    ty: &Type,
    span: Span,
) -> Result<(), CompilationIssue> {
    let inner_ty: Type = ty.remove_all_constant_type();

    if inner_ty.is_struct_type() || inner_ty.is_fixed_array_type() || inner_ty.is_array_type() {
        analyzer.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0057,
            "The 'arbitraryArgFrom' builtin cannot read a struct or array type from a variable arguments list.".into(),
            "Use a pointer type such as ptr or ptr[T] to receive a struct or array.".into(),
            None,
            span,
        ));
    }

    Ok(())
}
