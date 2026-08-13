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

use thrustc_ast::Ast;
use thrustc_attributes::{
    ThrustAttribute, ThrustAttributeComparator, ThrustAttributes,
    traits::{ThrustAttributeComparatorExtensions, ThrustAttributesExtensions},
};
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;
use thrustc_typesystem::traits::TypeIsExtensions;

use crate::applicant::AttributeCheckerAttributeApplicant;

use ahash::AHashSet as HashSet;

mod applicant;

#[derive(Debug)]
pub struct AttributeChecker<'attr_checker> {
    ast: &'attr_checker [Ast<'attr_checker>],

    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,

    options: &'attr_checker CompilerOptions,
    diagnostician: Diagnostician,
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    #[inline]
    pub fn new(
        ast: &'attr_checker [Ast<'attr_checker>],
        file: &'attr_checker CompilationUnit,
        options: &'attr_checker CompilerOptions,
    ) -> Self {
        Self {
            ast,
            errors: Vec::with_capacity(u8::MAX as usize),
            warnings: Vec::with_capacity(u8::MAX as usize),

            options,
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    pub fn start(&mut self) -> bool {
        for node in self.ast.iter() {
            self.analyze_ast(node);
        }

        self.check()
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    fn check(&mut self) -> bool {
        let warnings_to_disable: &[CompilationIssueCode] = self.options.get_warnings_to_disable();

        thrustc_errors::filter_warnings(warnings_to_disable, &mut self.errors);

        if !self.warnings.is_empty() {
            for warning in self.warnings.iter() {
                self.diagnostician
                    .dispatch_diagnostic(warning, thrustc_logging::LoggingType::Warning);
            }
        }

        if !self.errors.is_empty() {
            for error in self.errors.iter() {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            }

            return true;
        }

        false
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    fn analyze_ast(&mut self, node: &'attr_checker Ast) {
        match node {
            Ast::Function {
                attributes,
                body,
                return_type,
                span,
                ..
            } => {
                if body.is_some() && attributes.has_extern_attribute() {
                    if let Some(span) = attributes.match_attr(ThrustAttributeComparator::Extern) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "FFI functions cannot have a body.".into(),
                            "You should remove the body '{...}.".into(),
                            None,
                            span,
                        ));
                    }
                }

                if body.is_none() && !attributes.has_extern_attribute() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0011,
                        "A FFI function without body always need the external attribute.".into(),
                        "Add the '@extern(\"externalName\")' attribute.".into(),
                        None,
                        *span,
                    ));
                }

                if let Some(body) = body {
                    self.analyze_ast(body);
                }

                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::Function { return_type },
                    *span,
                );
            }
            Ast::CompilerIntrinsic {
                attributes, span, ..
            } => {
                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::Intrinsic,
                    *span,
                );
            }
            Ast::AssemblerFunction {
                attributes, span, ..
            } => {
                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::AssemblerFunction,
                    *span,
                );
            }
            Ast::Struct {
                attributes, span, ..
            } => {
                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::Struct,
                    *span,
                );
            }
            Ast::Enum {
                attributes, span, ..
            } => {
                self.analyze_attrs(attributes, AttributeCheckerAttributeApplicant::Enum, *span);
            }
            Ast::Const {
                attributes,
                metadata,
                span,
                ..
            } => {
                if !metadata.is_global() && attributes.has_public_attribute() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0013,
                        "Local constant cannot have public visibility.".into(),
                        "Remove the '@public' attribute.".into(),
                        None,
                        *span,
                    ));
                }

                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::Constant,
                    *span,
                );
            }
            Ast::Static {
                attributes,
                metadata,
                span,
                ..
            } => {
                if !metadata.is_global() && attributes.has_public_attribute() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0013,
                        "Local static cannot have public visibility.".into(),
                        "Remove the '@public' attribute.".into(),
                        None,
                        *span,
                    ));
                }

                self.analyze_attrs(
                    attributes,
                    AttributeCheckerAttributeApplicant::Static,
                    *span,
                );
            }
            Ast::Var {
                attributes, span, ..
            } => {
                self.analyze_attrs(attributes, AttributeCheckerAttributeApplicant::Local, *span);
            }
            Ast::Block { nodes, post, .. } => {
                for node in nodes.iter() {
                    self.analyze_ast(node);
                }

                for postnode in post.iter() {
                    self.analyze_ast(postnode);
                }
            }
            Ast::Defer { node, .. } => {
                self.analyze_ast(node);
            }

            Ast::For { local: node, .. } => {
                self.analyze_ast(node);
            }
            Ast::While {
                variable: Some(node),
                ..
            } => {
                self.analyze_ast(node);
            }

            _ => (),
        }
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    fn analyze_attrs(
        &mut self,
        attributes: &'attr_checker ThrustAttributes,
        applicant: AttributeCheckerAttributeApplicant,
        span: Span,
    ) {
        match applicant {
            AttributeCheckerAttributeApplicant::Function { return_type, .. } => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Constructor) {
                    if !return_type.is_void_type() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that execute before the entry point should not return anything.".into(),
                            "You should rewrite it to return a void type.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if !attributes.has_public_attribute() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run before the entry point must be public.".into(),
                            "Add '@public' attribute.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Extern) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run before the entry point cannot be external.".into(),
                            "You should remove it.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Linkage) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run before the entrypoint cannot have custom linkage."
                                .into(),
                            "You should remove it.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }
                }

                if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Destructor) {
                    if !return_type.is_void_type() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that execute after the entry point should not return anything.".into(),
                            "You should rewrite to return a void type.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if !attributes.has_public_attribute() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run after the entry point must be public.".into(),
                            "Add '@public' attribute.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Extern) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run after the entry point cannot be external.".into(),
                            "You should remove it.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }

                    if let Some(attr) = attributes.get_attr(ThrustAttributeComparator::Linkage) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0013,
                            "Functions that run after the entrypoint cannot have custom linkage."
                                .into(),
                            "You should remove it.".into(),
                            None,
                            attr.get_span(),
                        ));
                    }
                }

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::Intrinsic => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                if !attributes.has_public_attribute() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0012,
                        "Missing attribute".into(),
                        "Compiler intrinsic should always have public visibility.".into(),
                        None,
                        span,
                    ));
                }

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::Static => {
                if let Some(align_attribute) = attributes.get_attr(ThrustAttributeComparator::Align)
                {
                    let ThrustAttribute::Align(align_to, _) = align_attribute else {
                        return;
                    };

                    let is_power_of_two: bool = align_to % 2 == 0;
                    let is_up_128: bool = align_to > 128;

                    if !is_power_of_two {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment should be power of two.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }

                    if is_up_128 {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment supasses 128 of aligment limit. Reduce it.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }
                }

                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::AssemblerFunction => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                if !attributes.has_asmsyntax_attribute() {
                    if let Some(span) = attributes.match_attr(ThrustAttributeComparator::Extern) {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0011,
                            "Missing attribute".into(),
                            "A pure assembler function always have syntax mode. Add the '@asmsyntax' attribute.".into(),
                            None,
                            span,
                        ));
                    }
                }

                if let Some(ThrustAttribute::AsmSyntax(syntax, span)) =
                    attributes.get_attr(ThrustAttributeComparator::AsmSyntax)
                {
                    const INLINE_ASSEMBLER_SYNTAXES: [&str; 2] = ["Intel", "AT&T"];

                    if !INLINE_ASSEMBLER_SYNTAXES.contains(&syntax.as_str()) {
                        let displayed: String = INLINE_ASSEMBLER_SYNTAXES.join("or ");

                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Expected a valid assembler syntax".into(),
                            format!("You should utilize either '{}'.", displayed),
                            None,
                            span,
                        ));
                    }
                }

                if let Some(ThrustAttribute::Convention(conv, span)) =
                    attributes.get_attr(ThrustAttributeComparator::Convention)
                {
                    if !thrustc_attributes::callconventions::CALL_CONVENTIONS_AVAILABLE
                        .contains(&conv.as_str())
                    {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0002,
                            "Unknown calling convention, setting C by default.".into(),
                            span,
                        ));
                    }
                }

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::Constant => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                if let Some(align_attribute) = attributes.get_attr(ThrustAttributeComparator::Align)
                {
                    let ThrustAttribute::Align(align_to, _) = align_attribute else {
                        return;
                    };

                    let is_power_of_two: bool = align_to % 2 == 0;
                    let is_up_128: bool = align_to > 128;

                    if !is_power_of_two {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment should be power of two.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }

                    if is_up_128 {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment supasses 128 of aligment limit. Reduce it.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }
                }

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::Local => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                if let Some(align_attribute) = attributes.get_attr(ThrustAttributeComparator::Align)
                {
                    let ThrustAttribute::Align(align_to, _) = align_attribute else {
                        return;
                    };

                    let is_power_of_two: bool = align_to % 2 == 0;
                    let is_up_128: bool = align_to > 128;

                    if !is_power_of_two {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment should be power of two.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }

                    if is_up_128 {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0012,
                            "Invalid memory aligment".into(),
                            "The specified memory alignment supasses 128 of aligment limit. Reduce it.".into(),
                            None,
                            align_attribute.get_span(),
                        ));
                    }
                }

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }

            AttributeCheckerAttributeApplicant::Struct
            | AttributeCheckerAttributeApplicant::Enum => {
                self.check_irrelevant_attributes(attributes, applicant);
                self.check_illogical_attributes(attributes, applicant);

                self.get_repeated_attrs(attributes).iter().for_each(|attr| {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0033,
                        "Attribute conflict".into(),
                        "Repetitive attributes are disallowed. Remove each one.".into(),
                        None,
                        attr.get_span(),
                    ));
                });
            }
        }
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    fn check_irrelevant_attributes(
        &mut self,
        attributes: &ThrustAttributes,
        applicant: AttributeCheckerAttributeApplicant,
    ) {
        const VALID_FUNCTION_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::AlwaysInline,
            ThrustAttributeComparator::InlineHint,
            ThrustAttributeComparator::NoInline,
            ThrustAttributeComparator::Convention,
            ThrustAttributeComparator::Extern,
            ThrustAttributeComparator::Ignore,
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Hot,
            ThrustAttributeComparator::NoUnwind,
            ThrustAttributeComparator::OptFuzzing,
            ThrustAttributeComparator::MinSize,
            ThrustAttributeComparator::WeakStack,
            ThrustAttributeComparator::StrongStack,
            ThrustAttributeComparator::PreciseFloats,
            ThrustAttributeComparator::Linkage,
            ThrustAttributeComparator::Thunk,
            ThrustAttributeComparator::Constructor,
            ThrustAttributeComparator::Destructor,
            ThrustAttributeComparator::Cuda,
            ThrustAttributeComparator::Promote,
        ];

        const VALID_INTRINSIC_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::AlwaysInline,
            ThrustAttributeComparator::InlineHint,
            ThrustAttributeComparator::NoInline,
            ThrustAttributeComparator::Convention,
            ThrustAttributeComparator::Extern,
            ThrustAttributeComparator::Ignore,
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Hot,
            ThrustAttributeComparator::NoUnwind,
            ThrustAttributeComparator::OptFuzzing,
            ThrustAttributeComparator::MinSize,
            ThrustAttributeComparator::WeakStack,
            ThrustAttributeComparator::StrongStack,
            ThrustAttributeComparator::PreciseFloats,
            ThrustAttributeComparator::Linkage,
        ];

        const VALID_ASSEMBLER_FUNCTION_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::AlwaysInline,
            ThrustAttributeComparator::InlineHint,
            ThrustAttributeComparator::NoInline,
            ThrustAttributeComparator::Convention,
            ThrustAttributeComparator::Ignore,
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Hot,
            ThrustAttributeComparator::NoUnwind,
            ThrustAttributeComparator::OptFuzzing,
            ThrustAttributeComparator::MinSize,
            ThrustAttributeComparator::WeakStack,
            ThrustAttributeComparator::StrongStack,
            ThrustAttributeComparator::PreciseFloats,
            ThrustAttributeComparator::Linkage,
            ThrustAttributeComparator::Thunk,
            ThrustAttributeComparator::AsmAlignStack,
            ThrustAttributeComparator::AsmSyntax,
            ThrustAttributeComparator::AsmSideEffects,
            ThrustAttributeComparator::AsmThrow,
            ThrustAttributeComparator::Constructor,
            ThrustAttributeComparator::Destructor,
        ];

        const VALID_STATIC_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Extern,
            ThrustAttributeComparator::Linkage,
            ThrustAttributeComparator::Align,
        ];

        const VALID_CONSTANT_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Extern,
            ThrustAttributeComparator::Linkage,
            ThrustAttributeComparator::Align,
        ];

        const VALID_ENUM_ATTRIBUTES: &[ThrustAttributeComparator] =
            &[ThrustAttributeComparator::Public];

        const VALID_STRUCTS_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::Public,
            ThrustAttributeComparator::Packed,
        ];

        const VALID_LOCAL_ATTRIBUTES: &[ThrustAttributeComparator] = &[
            ThrustAttributeComparator::Heap,
            ThrustAttributeComparator::Align,
        ];

        match applicant {
            AttributeCheckerAttributeApplicant::Function { .. } => {
                attributes.iter().for_each(|attr| {
                    if !VALID_FUNCTION_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for functions.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Intrinsic => {
                attributes.iter().for_each(|attr| {
                    if !VALID_INTRINSIC_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for a intrinsic.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Constant => {
                attributes.iter().for_each(|attr| {
                    if !VALID_CONSTANT_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for constants.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::AssemblerFunction => {
                attributes.iter().for_each(|attr| {
                    if !VALID_ASSEMBLER_FUNCTION_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for assembler functions.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Enum => {
                attributes.iter().for_each(|attr| {
                    if !VALID_ENUM_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for enumerations.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Static => {
                attributes.iter().for_each(|attr| {
                    if !VALID_STATIC_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for static symbols.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Struct => {
                attributes.iter().for_each(|attr| {
                    if !VALID_STRUCTS_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for structures.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
            AttributeCheckerAttributeApplicant::Local => {
                attributes.iter().for_each(|attr| {
                    if !VALID_LOCAL_ATTRIBUTES.contains(&attr.as_attr_cmp()) {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0001,
                            "This attribute is not applicable for local variable.".into(),
                            attr.get_span(),
                        ));
                    }
                });
            }
        }
    }

    fn check_illogical_attributes(
        &mut self,
        attributes: &ThrustAttributes,
        applicant: AttributeCheckerAttributeApplicant,
    ) {
        if attributes.has_extern_attribute() && !attributes.has_public_attribute() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::Extern) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0013,
                    "Attribute conflict".into(),
                    "A external symbol always have public visibility. Add the '@public' attribute."
                        .into(),
                    None,
                    span,
                ));
            }
        }

        if attributes.has_convention_attribute() {
            if let Some(ThrustAttribute::Convention(conv, span)) =
                attributes.get_attr(ThrustAttributeComparator::Convention)
            {
                if !thrustc_attributes::callconventions::CALL_CONVENTIONS_AVAILABLE
                    .contains(&conv.as_str())
                {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0002,
                        "Unknown call convention, assuming C standard call convention by default."
                            .into(),
                        span,
                    ));
                }
            }
        }

        if attributes.has_linkage_attribute() {
            if let Some(ThrustAttribute::Linkage(linkage, linkage_raw, span)) =
                attributes.get_attr(ThrustAttributeComparator::Linkage)
            {
                if !thrustc_attributes::linkage::LINKAGES_AVAILABLE.contains(&linkage_raw.as_str())
                {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0003,
                        "Unknown linking, assuming non-proprietary C standard.".into(),
                        span,
                    ));
                }

                if applicant.is_function() && linkage.is_common() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0013,
                        "A function can't have a common linkage.".into(),
                        "You should change the linkage value.".into(),
                        None,
                        span,
                    ));
                }

                if !attributes.has_public_attribute()
                    && (linkage.is_linker_private() || linkage.is_linker_private_weak())
                {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This attribute is meaningless; The linkage is already private or private weak by default.".into(),
                        span,
                    ));
                }

                if attributes.has_public_attribute() && linkage.is_standard() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This attribute is meaningless; the linkage is the same as @public.".into(),
                        span,
                    ));
                }

                if attributes.has_public_attribute() && linkage.is_linker_private() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@public' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }

                if attributes.has_public_attribute() && linkage.is_linker_private_weak() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@public' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }

                if attributes.has_public_attribute() && linkage.is_internal() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@public' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }

                if attributes.has_extern_attribute() && linkage.is_linker_private() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@extern' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }

                if attributes.has_extern_attribute() && linkage.is_linker_private_weak() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@extern' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }

                if attributes.has_extern_attribute() && linkage.is_internal() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0004,
                        "This will cause a linking failure; the '@extern' attribute requires non-proprietary linking.".into(),
                        span,
                    ));
                }
            }
        }

        if attributes.has_constructor_attribute() && attributes.has_destructor_attribute() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::Destructor) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0012,
                    "A symbol cannot be both a constructor and a destructor at the same time."
                        .into(),
                    "You should remove an attribute.".into(),
                    None,
                    span,
                ));
            }
        }

        if !attributes.has_extern_attribute() && attributes.has_ignore_attribute() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::Ignore) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0013,
                    "Attribute conflict".into(),
                    "The @arbitraryArgs attribute requires a FFI symbol. You should add the external FFI attribute '@extern(\"externalName\")'.".into(),
                    None,
                    span,
                ));
            }
        }

        if attributes.has_inlinealways_attr() && attributes.has_inline_attr() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::InlineHint) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0033,
                    "Attribute conflict".into(),
                    "You should use either '@alwaysInline' or '@inline' attribute.".into(),
                    None,
                    span,
                ));
            }
        }

        if attributes.has_inline_attr() && attributes.has_noinline_attr() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::NoInline) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0033,
                    "Attribute conflict".into(),
                    "You should use either '@noInline' or '@inline' attribute.".into(),
                    None,
                    span,
                ));
            }
        }

        if attributes.has_inlinealways_attr() && attributes.has_noinline_attr() {
            if let Some(span) = attributes.match_attr(ThrustAttributeComparator::NoInline) {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0033,
                    "Attribute conflict".into(),
                    "You should use either '@alwaysInline' or '@inline' attribute.".into(),
                    None,
                    span,
                ));
            }
        }
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    fn get_repeated_attrs(&self, attributes: &'attr_checker ThrustAttributes) -> ThrustAttributes {
        let mut storage: HashSet<ThrustAttributeComparator> =
            HashSet::with_capacity(u8::MAX as usize);
        let mut repeated_attrs: ThrustAttributes = Vec::with_capacity(u8::MAX as usize);

        {
            for attribute in attributes.iter() {
                if !storage.insert(attribute.as_attr_cmp()) {
                    repeated_attrs.push(attribute.clone());
                }
            }
        }

        repeated_attrs
    }
}

impl<'attr_checker> AttributeChecker<'attr_checker> {
    #[inline]
    fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline]
    fn add_warning(&mut self, warning: CompilationIssue) {
        self.warnings.push(warning);
    }
}
