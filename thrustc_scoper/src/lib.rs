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
    traits::{AstCodeLocation, AstDeclarationExtensions, AstStandardExtensions},
};
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};

use crate::context::ScoperContext;

mod checks;
mod context;

#[derive(Debug)]
pub struct Scoper<'scoper> {
    ast: &'scoper [Ast<'scoper>],
    context: ScoperContext,
    errors: Vec<CompilationIssue>,
    diagnostician: Diagnostician,
}

impl<'scoper> Scoper<'scoper> {
    #[inline]
    pub fn new(
        ast: &'scoper [Ast<'scoper>],
        file: &CompilationUnit,
        options: &CompilerOptions,
    ) -> Self {
        Self {
            ast,
            context: ScoperContext::new(),
            errors: Vec::with_capacity(u8::MAX as usize),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'scoper> Scoper<'scoper> {
    pub fn start(&mut self) -> bool {
        for node in self.ast.iter() {
            self.analyze_global_node(node);
        }

        self.check()
    }
}

impl<'scoper> Scoper<'scoper> {
    fn check(&mut self) -> bool {
        if !self.errors.is_empty() {
            for error in self.errors.iter() {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            }

            true
        } else {
            false
        }
    }
}

impl<'scoper> Scoper<'scoper> {
    fn analyze_global_node(&mut self, node: &Ast) {
        if !node.is_declaration_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Statements and expressions are not allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if let Ast::Function { body, .. } = node {
            let Some(body) = body else {
                return;
            };

            self.get_mut_context().enter_function();
            self.analyze_local_node(body);
            self.get_mut_context().leave_function();
        }
    }

    fn analyze_local_node(&mut self, node: &Ast) {
        if node.is_function_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Function is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_asm_function() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Assembler function is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_type_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Type is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_global_asm_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Global assembler is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_enum_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Enum is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_import_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Module importation is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if node.is_intrinsic_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Compiler intrinsic is only allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        if let Ast::Block { nodes, post, .. } = node {
            checks::check_for_multiple_terminators(self, node);
            checks::check_for_unreachable_code_instructions(self, node);

            for node in nodes.iter() {
                self.analyze_local_node(node);
            }

            for postnode in post.iter() {
                self.analyze_local_node(postnode);
            }
        }

        match node {
            Ast::If {
                then_branch,
                else_if_branch,
                else_branch,
                ..
            } => {
                self.analyze_local_node(then_branch);

                {
                    for node in else_if_branch.iter() {
                        self.analyze_local_node(node);
                    }
                }

                if let Some(node) = else_branch {
                    self.analyze_local_node(node);
                }
            }
            Ast::Elif { block, .. } => {
                self.analyze_local_node(block);
            }
            Ast::Else { block, .. } => {
                self.analyze_local_node(block);
            }

            Ast::While { block, .. } => {
                self.get_mut_context().enter_loop();
                self.analyze_local_node(block);
                self.get_mut_context().leave_loop();
            }
            Ast::Loop { block, .. } => {
                self.get_mut_context().enter_loop();
                self.analyze_local_node(block);
                self.get_mut_context().leave_loop();
            }
            Ast::For { block, .. } => {
                self.get_mut_context().enter_loop();
                self.analyze_local_node(block);
                self.get_mut_context().leave_loop();
            }

            Ast::Continue { .. }
            | Ast::ContinueAll { .. }
            | Ast::Break { .. }
            | Ast::BreakAll { .. }
                if !self.get_context().is_inside_loop() =>
            {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0018,
                    "Loop control statement outside of a loop.".into(),
                    "It should be inside a loop. Reposition inside it.".into(),
                    None,
                    node.get_span(),
                ));
            }
            Ast::Return { span, .. } if !self.get_context().is_inside_function() => {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0018,
                    "Terminator is outside a function.".into(),
                    "It should be inside a function. Reposition inside it.".into(),
                    None,
                    *span,
                ));
            }

            Ast::Defer { node, .. } => {
                self.analyze_local_node(node);
            }

            _ => (),
        }
    }
}

impl<'scoper> Scoper<'scoper> {
    #[inline]
    pub fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }
}

impl<'scoper> Scoper<'scoper> {
    #[inline]
    fn get_context(&self) -> &ScoperContext {
        &self.context
    }
}

impl<'scoper> Scoper<'scoper> {
    #[inline]
    fn get_mut_context(&mut self) -> &mut ScoperContext {
        &mut self.context
    }
}
