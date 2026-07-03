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
    traits::{
        AstCodeLocation, AstConstantExtensions, AstGetType, AstMemoryExtensions,
        AstStandardExtensions,
    },
};
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};

use thrustc_span::Span;
use thrustc_typesystem::{Type, traits::TypeExtensions};

use crate::context::AnalyzerContext;

mod context;
mod expressions;

#[derive(Debug)]
pub struct GeneralAnalyzer<'analyzer> {
    ast: &'analyzer [Ast<'analyzer>],

    bugs: Vec<CompilationIssue>,
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,

    diagnostician: Diagnostician,

    context: AnalyzerContext,
}

impl<'analyzer> GeneralAnalyzer<'analyzer> {
    #[inline]
    pub fn new(
        ast: &'analyzer [Ast<'analyzer>],
        file: &'analyzer CompilationUnit,
        options: &CompilerOptions,
    ) -> Self {
        Self {
            ast,

            bugs: Vec::with_capacity(u8::MAX as usize),
            errors: Vec::with_capacity(u8::MAX as usize),
            warnings: Vec::with_capacity(u8::MAX as usize),

            diagnostician: Diagnostician::new(file, options),
            context: AnalyzerContext::new(),
        }
    }
}

impl<'analyzer> GeneralAnalyzer<'analyzer> {
    pub fn start(&mut self) -> bool {
        for node in self.ast.iter() {
            if let Err(error) = self.analyze_decl(node) {
                self.add_error(error);
            }
        }

        self.check()
    }
}

impl<'analyzer> GeneralAnalyzer<'analyzer> {
    fn check(&mut self) -> bool {
        self.warnings.iter().for_each(|warn| {
            self.diagnostician
                .dispatch_diagnostic(warn, thrustc_logging::LoggingType::Warning);
        });

        if !self.errors.is_empty() || !self.bugs.is_empty() {
            self.bugs.iter().for_each(|warn| {
                self.diagnostician
                    .dispatch_diagnostic(warn, thrustc_logging::LoggingType::Bug);
            });

            self.errors.iter().for_each(|error| {
                self.diagnostician
                    .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
            });

            return true;
        }

        false
    }
}

impl<'analyzer> GeneralAnalyzer<'analyzer> {
    fn analyze_decl(&mut self, node: &'analyzer Ast) -> Result<(), CompilationIssue> {
        match node {
            Ast::AssemblerFunction {
                parameters, span, ..
            } => {
                if parameters.len() > 12 {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0036,
                        "Too many arguments for a single function signature.".into(),
                        "You should pass them through pointers.".into(),
                        None,
                        *span,
                    ));
                }

                Ok(())
            }

            Ast::Function {
                parameters,
                body,
                span,
                ..
            } => {
                let values_at_registers: usize = parameters
                    .iter()
                    .filter_map(|parameter| parameter.get_any_type().ok())
                    .filter(|ty| ty.is_value())
                    .count();

                if values_at_registers > 12 {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0036,
                        "Too many arguments for a single function signature.".into(),
                        "You should pass them through pointers.".into(),
                        None,
                        *span,
                    ));
                }

                if let Some(body) = body {
                    self.analyze_stmt(body)?;
                }

                Ok(())
            }

            Ast::GlobalAssembler { span, .. } => {
                if self.get_context().has_global_assembler() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0005,
                        "Multiple global assembler inyection are not allowed.".into(),
                        "Remove all; keep one.".into(),
                        None,
                        *span,
                    ));
                }

                self.get_mut_context().set_has_global_assembler();

                Ok(())
            }

            Ast::Enum { data, .. } => {
                {
                    for (_, _, expr) in data.iter() {
                        let span: Span = expr.get_span();

                        if !expr.is_constant_value() {
                            self.add_error(CompilationIssue::Error(
                                CompilationIssueCode::E0006,
                                "Expected constant expression.".into(),
                                "You should pass a constant expression.".into(),
                                None,
                                span,
                            ));
                        }

                        self.analyze_expr(expr)?;
                    }
                }

                Ok(())
            }
            Ast::Static { value, .. } => {
                if let Some(value) = value {
                    let span: Span = value.get_span();

                    if !value.is_constant_value() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0006,
                            "Expected constant expression.".into(),
                            "You should pass a constant expression.".into(),
                            None,
                            span,
                        ));
                    }

                    self.analyze_expr(value)?;
                }

                Ok(())
            }
            Ast::Const { value, .. } => {
                let span: Span = value.get_span();

                if !value.is_constant_value() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0006,
                        "Expected constant expression.".into(),
                        "You should pass a constant expression.".into(),
                        None,
                        span,
                    ));
                }

                self.analyze_expr(value)?;

                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn analyze_stmt(&mut self, node: &'analyzer Ast) -> Result<(), CompilationIssue> {
        match node {
            Ast::Enum { data, .. } => {
                {
                    for (_, _, expr) in data.iter() {
                        let span: Span = expr.get_span();

                        if !expr.is_constant_value() {
                            self.add_error(CompilationIssue::Error(
                                CompilationIssueCode::E0006,
                                "Expected constant expression.".into(),
                                "You should pass a constant expression.".into(),
                                None,
                                span,
                            ));
                        }

                        self.analyze_expr(expr)?;
                    }
                }

                Ok(())
            }
            Ast::Static { value, .. } => {
                if let Some(value) = value {
                    if !value.is_constant_value() {
                        self.add_error(CompilationIssue::Error(
                            CompilationIssueCode::E0006,
                            "Expected constant expression.".into(),
                            "You should pass a constant expression.".into(),
                            None,
                            value.get_span(),
                        ));
                    }

                    self.analyze_expr(value)?;
                }

                Ok(())
            }
            Ast::Const { value, .. } => {
                let span: Span = value.get_span();

                if !value.is_constant_value() {
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0006,
                        "Expected constant expression.".into(),
                        "You should pass a constant expression.".into(),
                        None,
                        span,
                    ));
                }

                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Var {
                value, metadata, ..
            } => {
                if let Some(value) = value {
                    if !metadata.is_unitialized() {
                        self.analyze_expr(value)?;
                    }
                }

                Ok(())
            }
            Ast::If {
                condition,
                then_branch,
                else_if_branch,
                else_branch,
                ..
            } => {
                self.analyze_expr(condition)?;

                {
                    for node in else_if_branch.iter() {
                        self.analyze_stmt(node)?;
                    }
                }

                if let Some(node) = else_branch {
                    self.analyze_stmt(node)?;
                }

                self.analyze_stmt(then_branch)?;

                Ok(())
            }

            Ast::Elif {
                condition, block, ..
            } => {
                self.analyze_expr(condition)?;
                self.analyze_stmt(block)?;

                Ok(())
            }
            Ast::Else { block, .. } => {
                self.analyze_stmt(block)?;

                Ok(())
            }

            Ast::For {
                local,
                condition,
                actions,
                block,
                ..
            } => {
                self.analyze_stmt(local)?;
                self.analyze_expr(condition)?;

                self.analyze_expr(actions)?;
                self.analyze_stmt(block)?;

                Ok(())
            }

            Ast::While {
                variable,
                condition,
                block,
                ..
            } => {
                if let Some(node) = variable {
                    self.analyze_stmt(node)?;
                }

                self.analyze_expr(condition)?;
                self.analyze_stmt(block)?;

                Ok(())
            }

            Ast::Loop { block, .. } => {
                self.analyze_stmt(block)?;

                Ok(())
            }
            Ast::Continue { .. }
            | Ast::ContinueAll { .. }
            | Ast::Break { .. }
            | Ast::BreakAll { .. } => Ok(()),
            Ast::Mutation { source, value, .. } => {
                let source_type: &Type = source.get_value_type()?;

                if source.is_reference() && !source.is_memory_assigned_value()? {
                    self.add_error(CompilationIssue::Error(
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
                    self.add_error(CompilationIssue::Error(
                        CompilationIssueCode::E0008,
                        "An value with memory address was expected.".into(),
                        "You should try to allocate it and pass it as a direct reference.".into(),
                        None,
                        source.get_span(),
                    ));
                }

                {
                    if source.is_reference() {
                        if let Ast::Reference { metadata, .. } = &**source {
                            if metadata.is_static_ref() && !metadata.is_mutable() {
                                self.add_error(CompilationIssue::Error(
                                    CompilationIssueCode::E0038,
                                    "Missing mutability.'".into(),
                                    "You should mark it as mutable using 'mut' keyword".into(),
                                    None,
                                    source.get_span(),
                                ));
                            }
                        }
                    }
                }

                self.analyze_expr(source)?;
                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Block { nodes, post, .. } => {
                {
                    for node in nodes.iter() {
                        self.analyze_stmt(node)?;
                    }

                    for postnode in post.iter() {
                        self.analyze_stmt(postnode)?;
                    }
                }

                Ok(())
            }
            Ast::Defer { node, .. } => {
                self.analyze_stmt(node)?;

                Ok(())
            }

            Ast::Return { expression, .. } => {
                if let Some(expr) = expression {
                    self.analyze_expr(expr)?;
                }

                Ok(())
            }

            node => self.analyze_expr(node),
        }
    }

    fn analyze_expr(&mut self, node: &'analyzer Ast) -> Result<(), CompilationIssue> {
        expressions::validate_node(self, node)
    }
}

impl GeneralAnalyzer<'_> {
    #[inline]
    fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline]
    fn add_bug(&mut self, error: CompilationIssue) {
        self.bugs.push(error);
    }
}

impl GeneralAnalyzer<'_> {
    #[inline]
    fn get_context(&self) -> &AnalyzerContext {
        &self.context
    }
}

impl GeneralAnalyzer<'_> {
    #[inline]
    fn get_mut_context(&mut self) -> &mut AnalyzerContext {
        &mut self.context
    }
}
