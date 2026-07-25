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

#![allow(clippy::collapsible_match)]

use thrustc_ast::{
    Ast,
    ast_builtins::AstBuiltin,
    traits::{
        AstCodeLocation, AstExpressionExtensions, AstStandardExtensions, AstStatementExtensions,
    },
};
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::CompilationIssue;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_typesystem::traits::{ConstantTypeExtensions, TypeIsExtensions};

#[derive(Debug)]
pub struct AstVerifier<'ast_verifier> {
    ast: &'ast_verifier [Ast<'ast_verifier>],
    errors: Vec<CompilationIssue>,
    diagnostician: Diagnostician,
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    #[inline]
    pub fn new(
        ast: &'ast_verifier [Ast<'ast_verifier>],
        file: &CompilationUnit,
        options: &CompilerOptions,
    ) -> Self {
        Self {
            ast,
            errors: Vec::with_capacity(u8::MAX as usize),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    pub fn analyze(&mut self) -> bool {
        self.analyze_top_ast();

        let it_failed: bool = self.failed();

        it_failed
    }
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    pub fn analyze_top_ast(&mut self) {
        for node in self.ast.iter() {
            match node {
                Ast::Function {
                    parameters, body, ..
                } => {
                    if let Some(body) = body {
                        self.expected_statement(body);
                        self.analyze_stmt(body);
                    }

                    for parameter in parameters.iter() {
                        if !parameter.is_function_parameter() {
                            self.add_error(CompilationIssue::Error(
                                thrustc_errors::CompilationIssueCode::E0001,
                                "Expected a parameter.".into(),
                                "You should remove it.".into(),
                                None,
                                node.get_span(),
                            ));
                        }
                    }
                }

                Ast::Intrinsic { parameters, .. } => {
                    for parameter in parameters.iter() {
                        if !parameter.is_function_parameter() {
                            self.add_error(CompilationIssue::Error(
                                thrustc_errors::CompilationIssueCode::E0001,
                                "Expected a parameter.".into(),
                                "You should remove it.".into(),
                                None,
                                node.get_span(),
                            ));
                        }
                    }
                }

                Ast::AssemblerFunction { parameters, .. } => {
                    for parameter in parameters.iter() {
                        if !parameter.is_function_parameter() {
                            self.add_error(CompilationIssue::Error(
                                thrustc_errors::CompilationIssueCode::E0001,
                                "Expected a parameter.".into(),
                                "You should remove it.".into(),
                                None,
                                node.get_span(),
                            ));
                        }
                    }
                }

                Ast::Const { value, .. } => {
                    self.analyze_expression(value);
                }

                Ast::Static { value, .. } => {
                    if let Some(value) = value {
                        self.analyze_expression(value);
                    }
                }

                Ast::Enum { data, .. } => {
                    for (_, _, node, ..) in data.iter() {
                        self.analyze_expression(node);
                    }
                }

                Ast::GlobalAssembler { .. }
                | Ast::CustomType { .. }
                | Ast::Import { .. }
                | Ast::Embedded { .. }
                | Ast::Struct { .. } => {}

                _ => {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a top entity, not a statement, and never an expression.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
        }
    }

    pub fn analyze_stmt(&mut self, node: &Ast<'_>) {
        match node {
            Ast::Block { nodes, post, .. } => {
                {
                    for node in nodes.iter() {
                        self.expected_statement_or_loose_expression(node);
                        self.analyze_stmt(node);
                    }
                }

                {
                    for node in post.iter() {
                        self.expected_statement_or_loose_expression(node);
                        self.analyze_stmt(node);
                    }
                }
            }

            Ast::Const { value, .. } => {
                self.analyze_expression(value);
            }

            Ast::Static { value, .. } => {
                if let Some(value) = value {
                    self.analyze_expression(value);
                }
            }

            Ast::If {
                condition,
                then_branch,
                else_if_branch,
                else_branch,
                ..
            } => {
                self.expected_expression(condition);
                self.analyze_expression(condition);

                self.expected_statement(then_branch);
                self.analyze_stmt(then_branch);

                for node in else_if_branch.iter() {
                    self.expected_statement_or_loose_expression(node);
                    self.analyze_stmt(node);
                }

                if let Some(node) = else_branch {
                    self.expected_statement_or_loose_expression(node);
                    self.analyze_stmt(node);
                }
            }

            Ast::Elif {
                condition, block, ..
            } => {
                self.expected_expression(condition);
                self.analyze_expression(condition);

                self.expected_statement_or_loose_expression(block);
                self.analyze_stmt(block);
            }

            Ast::Else { block, .. } => {
                self.expected_statement_or_loose_expression(block);
                self.analyze_stmt(block);
            }

            Ast::While {
                variable,
                condition,
                block,
                ..
            } => {
                if let Some(node) = variable {
                    self.expected_statement(node);
                    self.analyze_stmt(node);
                }

                self.expected_expression(condition);
                self.analyze_expression(condition);

                self.expected_statement_or_loose_expression(block);
                self.analyze_stmt(block);
            }
            Ast::Loop { block, .. } => {
                self.expected_statement_or_loose_expression(block);
                self.analyze_stmt(block);
            }
            Ast::For {
                local,
                condition,
                actions,
                block,
                ..
            } => {
                self.expected_statement(local);
                self.analyze_stmt(local);

                self.expected_expression(condition);
                self.analyze_expression(condition);

                self.expected_expression(actions);
                self.analyze_expression(actions);

                self.expected_statement_or_loose_expression(block);
                self.analyze_stmt(block);
            }

            Ast::Continue { .. }
            | Ast::ContinueAll { .. }
            | Ast::Break { .. }
            | Ast::BreakAll { .. }
            | Ast::Struct { .. } => {}

            Ast::Mutation { source, value, .. } => {
                self.expected_expression(source);
                self.analyze_expression(source);

                self.expected_expression(value);
                self.analyze_expression(value);
            }

            Ast::Return { expression, .. } => {
                if let Some(node) = expression {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }
            }

            Ast::Defer { node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            node => self.analyze_expression(node),
        }
    }

    pub fn analyze_expression(&mut self, node: &Ast<'_>) {
        match node {
            Ast::BinaryOp { left, right, .. } => {
                self.expected_expression(left);
                self.analyze_expression(left);

                self.expected_expression(right);
                self.analyze_expression(right);
            }

            Ast::UnaryOp { node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            Ast::Group { node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            Ast::FixedArray {
                kind: ty, items, ..
            } => {
                for node in items.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }

                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_fixed_array_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal fixed array value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }

            Ast::Array {
                kind: ty, items, ..
            } => {
                for node in items.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }

                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_array_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal array value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }

            Ast::Index { source, index, .. } => {
                self.expected_expression(source);
                self.analyze_expression(source);

                self.expected_expression(index);
                self.analyze_expression(index);
            }

            Ast::Property { source, .. } => {
                self.expected_expression(source);
                self.analyze_expression(source);
            }

            Ast::Constructor { kind: ty, data, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                for (_, node, _, _) in data.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }

                if !ty.is_struct_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal string with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }

            Ast::Call { args, .. } => {
                for node in args.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }
            }

            Ast::IndirectCall { args, .. } => {
                for node in args.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }
            }

            Ast::Defer { node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            Ast::As { from: node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            Ast::AsmValue { args, .. } => {
                for node in args.iter() {
                    self.expected_expression(node);
                    self.analyze_expression(node);
                }
            }

            Ast::EnumValue { value: node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            Ast::GetLocation { expr: node, .. } => {
                self.expected_expression(node);
                self.analyze_expression(node);
            }

            /* literals */
            Ast::CString { kind: ty, .. } | Ast::CNString { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_array_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal string with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::Char { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_char_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal character with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::Boolean { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_bool_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal boolean value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::Integer { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_integer_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal integer value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::Float { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_float_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal float value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::NullPtr { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_ptr_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a literal null pointer value with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }
            Ast::Struct { kind: ty, .. } => {
                let ty: thrustc_typesystem::Type = ty.remove_all_constant_type();

                if !ty.is_struct_type() {
                    self.add_error(CompilationIssue::Error(
                        thrustc_errors::CompilationIssueCode::E0001,
                        "Expected a struct type with a valid type.".into(),
                        "You should remove it.".into(),
                        None,
                        node.get_span(),
                    ));
                }
            }

            Ast::Builtin { builtin, .. } => match builtin {
                AstBuiltin::MemMove { src, dst, size, .. } => {
                    self.expected_expression(src);
                    self.analyze_expression(src);

                    self.expected_expression(dst);
                    self.analyze_expression(dst);

                    self.expected_expression(size);
                    self.analyze_expression(size);
                }
                AstBuiltin::MemSet {
                    dst,
                    new_size,
                    size,
                    ..
                } => {
                    self.expected_expression(dst);
                    self.analyze_expression(dst);

                    self.expected_expression(new_size);
                    self.analyze_expression(new_size);

                    self.expected_expression(size);
                    self.analyze_expression(size);
                }
                AstBuiltin::MemCpy { dst, src, size, .. } => {
                    self.expected_expression(src);
                    self.analyze_expression(src);

                    self.expected_expression(dst);
                    self.analyze_expression(dst);

                    self.expected_expression(size);
                    self.analyze_expression(size);
                }

                _ => (),
            },

            _ => (),
        }
    }
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    pub fn expected_statement(&mut self, node: &Ast<'_>) {
        if !node.is_statement_keyword() {
            self.add_error(CompilationIssue::Error(
                thrustc_errors::CompilationIssueCode::E0001,
                "Expected a statement, not an expression, and never a top entity.".into(),
                "You should remove it.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    pub fn expected_statement_or_loose_expression(&mut self, node: &Ast<'_>) {
        if !node.is_statement_keyword() && !node.is_expression() {
            self.add_error(CompilationIssue::Error(
                thrustc_errors::CompilationIssueCode::E0001,
                "Expected a statement or a expression, and never a top entity.".into(),
                "You should remove it.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    pub fn expected_expression(&mut self, node: &Ast<'_>) {
        if !node.is_expression() {
            self.add_error(CompilationIssue::Error(
                thrustc_errors::CompilationIssueCode::E0001,
                "Expected a expression, not a statement, and never a top entity.".into(),
                "You should remove it.".into(),
                None,
                node.get_span(),
            ));
        }
    }
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    fn failed(&mut self) -> bool {
        if !self.errors.is_empty() {
            self.errors.iter().for_each(|error| {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            });

            true
        } else {
            false
        }
    }
}

impl<'ast_verifier> AstVerifier<'ast_verifier> {
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }
}
