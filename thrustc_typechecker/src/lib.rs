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
    traits::{AstCodeLocation, AstGetType, AstLiteralExtensions, AstStandardExtensions},
};

use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;
use thrustc_typesystem::{
    Type,
    traits::{
        DereferenceExtensions, TypeCodeLocation, TypeIsExtensions, TypePointerExtensions,
        VoidTypeExtensions,
    },
};

use crate::{
    context::{TypeCheckerControlContext, TypeCheckerTypeContext},
    metadata::TypeCheckerNodeMetadata,
    table::TypeCheckerSymbolsTable,
};

mod check;
mod context;
mod expressions;
mod globals;
mod metadata;
mod operations;
mod table;

#[derive(Debug)]
pub struct TypeChecker<'type_checker> {
    ast: &'type_checker [Ast<'type_checker>],
    position: usize,

    bugs: Vec<CompilationIssue>,
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,

    control_context: TypeCheckerControlContext,
    type_context: TypeCheckerTypeContext<'type_checker>,

    table: TypeCheckerSymbolsTable<'type_checker>,

    diagnostician: Diagnostician,
}

impl<'type_checker> TypeChecker<'type_checker> {
    pub fn new(
        ast: &'type_checker [Ast<'type_checker>],
        file: &'type_checker CompilationUnit,
        options: &CompilerOptions,
    ) -> Self {
        Self {
            ast,
            position: 0,

            bugs: Vec::with_capacity(u8::MAX as usize),
            errors: Vec::with_capacity(u8::MAX as usize),
            warnings: Vec::with_capacity(u8::MAX as usize),

            control_context: TypeCheckerControlContext::new(),
            type_context: TypeCheckerTypeContext::new(),
            table: TypeCheckerSymbolsTable::new(),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    pub fn start(&mut self) -> bool {
        self.parse_top();

        while !self.is_eof() {
            let node: &Ast = self.peek_node();

            match self.analyze_decl(node) {
                Ok(()) => (),
                Err(error) => {
                    self.add_error_report(error);

                    {
                        let context: &mut TypeCheckerControlContext =
                            self.get_mut_control_context();

                        context.reset_checking_depth();
                        context.reset_type_cast_depth();

                        self.get_mut_type_context().unset_current_function_type();
                    }
                }
            }

            self.advance();
        }

        {
            for warning in self.warnings.iter() {
                self.diagnostician
                    .dispatch_diagnostic(warning, thrustc_logging::LoggingType::Warning);
            }

            if !self.errors.is_empty() || !self.bugs.is_empty() {
                {
                    for bug in self.bugs.iter() {
                        self.diagnostician
                            .dispatch_diagnostic(bug, thrustc_logging::LoggingType::Bug);
                    }

                    for error in self.errors.iter() {
                        self.diagnostician
                            .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
                    }
                }

                true
            } else {
                false
            }
        }
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    pub fn analyze_decl(&mut self, node: &'type_checker Ast) -> Result<(), CompilationIssue> {
        match node {
            Ast::Intrinsic { .. } | Ast::AssemblerFunction { .. } | Ast::Function { .. } => {
                globals::functions::validate(self, node)
            }
            Ast::CustomType { .. } | Ast::GlobalAssembler { .. } | Ast::Struct { .. } => Ok(()),
            Ast::Enum { data, .. } => {
                {
                    for (_, target_type, expr) in data.iter() {
                        let from_type: &Type = expr.get_value_type()?;

                        let metadata: TypeCheckerNodeMetadata =
                            TypeCheckerNodeMetadata::new(expr.is_totaly_literal_value());

                        {
                            let control_context: &mut TypeCheckerControlContext =
                                self.get_mut_control_context();

                            check::check_type_together(
                                target_type,
                                from_type,
                                Some(expr),
                                None,
                                metadata,
                                expr.get_span(),
                                control_context,
                            )?;

                            control_context.reset_checking_depth();
                        }

                        self.analyze_expr(expr)?;
                    }
                }

                Ok(())
            }
            Ast::Static {
                kind: static_type,
                value,
                ..
            } => {
                let Some(value) = value else {
                    return Ok(());
                };

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let value_type: &Type = value.get_value_type()?;

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        static_type,
                        value_type,
                        Some(value),
                        None,
                        metadata,
                        node.get_span(),
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Const {
                kind: const_type,
                value,
                ..
            } => {
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let value_type: &Type = value.get_value_type()?;

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        const_type,
                        value_type,
                        Some(value),
                        None,
                        metadata,
                        node.get_span(),
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                self.analyze_expr(value)?;

                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn analyze_stmt(&mut self, node: &'type_checker Ast) -> Result<(), CompilationIssue> {
        match node {
            Ast::CustomType { .. }
            | Ast::Struct { .. }
            | Ast::Continue { .. }
            | Ast::ContinueAll { .. }
            | Ast::Break { .. }
            | Ast::BreakAll { .. } => Ok(()),

            Ast::Enum { data, .. } => {
                {
                    for (_, target_type, expr) in data.iter() {
                        let from_type: &Type = expr.get_value_type()?;

                        let metadata: TypeCheckerNodeMetadata =
                            TypeCheckerNodeMetadata::new(expr.is_totaly_literal_value());

                        {
                            let control_context: &mut TypeCheckerControlContext =
                                self.get_mut_control_context();

                            control_context.reset_checking_depth();

                            if let Err(error) = check::check_type_together(
                                target_type,
                                from_type,
                                Some(expr),
                                None,
                                metadata,
                                expr.get_span(),
                                control_context,
                            ) {
                                self.add_error_report(error);
                            }
                        }

                        self.analyze_expr(expr)?;
                    }
                }

                Ok(())
            }
            Ast::Static {
                kind: static_type,
                value,
                ..
            } => {
                let Some(value) = value else {
                    return Ok(());
                };

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let value_type: &Type = value.get_value_type()?;

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        static_type,
                        value_type,
                        Some(value),
                        None,
                        metadata,
                        node.get_span(),
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Const {
                kind: const_type,
                value,
                ..
            } => {
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let value_type: &Type = value.get_value_type()?;

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        const_type,
                        value_type,
                        Some(value),
                        None,
                        metadata,
                        node.get_span(),
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Var {
                name,
                kind: local_type,
                value,
                span,
                ..
            } => {
                self.get_mut_table().new_local(name, (local_type, *span));

                if local_type.contains_void_type() || local_type.is_void_type() {
                    self.add_error_report(CompilationIssue::Error(
                        CompilationIssueCode::E0019,
                        "Cannot use 'void' as a value.".into(),
                        "You should remove whatever type or value where void type belongs.".into(),
                        None,
                        *span,
                    ));
                }

                let Some(value) = value else {
                    return Ok(());
                };

                let type_metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let value_type: &Type = value.get_value_type()?;

                let is_value_literal_ptr: bool = value.is_literal_ptr_value();

                let is_ptr_type: bool = value_type.is_ptr_like_type() && value.is_reference()
                    || value_type.is_ptr_like_type()
                        && (!value.is_literal_value() || is_value_literal_ptr);

                if is_ptr_type {
                    {
                        let ptr_type: Type = Type::Ptr {
                            subtype: Some(value_type.clone().into()),
                            address_space: value_type.get_address_space(),
                            span: value_type.get_span(),
                        };

                        let control_context: &mut TypeCheckerControlContext =
                            self.get_mut_control_context();

                        control_context.reset_checking_depth();

                        if let Err(error) = check::check_type_together(
                            local_type,
                            &ptr_type,
                            Some(value),
                            None,
                            type_metadata,
                            node.get_span(),
                            control_context,
                        ) {
                            self.add_error_report(error);
                        }
                    }
                } else {
                    {
                        let control_context: &mut TypeCheckerControlContext =
                            self.get_mut_control_context();

                        control_context.reset_checking_depth();

                        if let Err(error) = check::check_type_together(
                            local_type,
                            value_type,
                            Some(value),
                            None,
                            type_metadata,
                            node.get_span(),
                            control_context,
                        ) {
                            self.add_error_report(error);
                        }
                    }
                }

                self.analyze_expr(value)?;

                Ok(())
            }
            Ast::Block { nodes, post, .. } => {
                self.begin_scope();

                {
                    for node in nodes.iter() {
                        self.analyze_stmt(node)?;
                    }

                    for postnode in post.iter() {
                        self.analyze_stmt(postnode)?;
                    }
                }

                self.end_scope();

                Ok(())
            }
            Ast::Defer { node, .. } => {
                self.analyze_stmt(node)?;

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

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(condition.is_totaly_literal_value());

                let span: Span = condition.get_span();

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        &Type::Bool { span },
                        condition.get_value_type()?,
                        Some(condition),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                {
                    for node in else_if_branch.iter() {
                        self.analyze_stmt(node)?;
                    }
                }

                if let Some(otherwise) = else_branch {
                    self.analyze_stmt(otherwise)?;
                }

                self.analyze_stmt(then_branch)?;

                Ok(())
            }
            Ast::Elif {
                condition, block, ..
            } => {
                self.analyze_expr(condition)?;

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(condition.is_totaly_literal_value());

                let span: Span = condition.get_span();

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    let target_type: Type = Type::Bool {
                        span: condition.get_span(),
                    };

                    if let Err(error) = check::check_type_together(
                        &target_type,
                        condition.get_value_type()?,
                        Some(condition),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

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

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(condition.is_totaly_literal_value());

                let span: Span = condition.get_span();

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    let target_type: Type = Type::Bool { span };

                    if let Err(error) = check::check_type_together(
                        &target_type,
                        condition.get_value_type()?,
                        Some(condition),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

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
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(condition.is_totaly_literal_value());

                let span: Span = condition.get_span();

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    let target_type: Type = Type::Bool { span };

                    if let Err(error) = check::check_type_together(
                        &target_type,
                        condition.get_value_type()?,
                        Some(condition),
                        None,
                        metadata,
                        span,
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                if let Some(variable) = variable {
                    self.analyze_stmt(variable)?;
                }

                self.analyze_expr(condition)?;
                self.analyze_stmt(block)?;

                Ok(())
            }
            Ast::Loop { block, .. } => {
                self.analyze_stmt(block)?;

                Ok(())
            }

            Ast::Return { expression, .. } => {
                let Some(expr) = expression else {
                    return Ok(());
                };

                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(expr.is_totaly_literal_value());

                let Some((return_type, function_loc)) =
                    self.get_type_context().get_current_function_type()
                else {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0018,
                        "Terminator is outside a function.".into(),
                        "It should be inside a function. Reposition inside it.".into(),
                        None,
                        expr.get_span(),
                    ));
                };

                {
                    let control_context: &mut TypeCheckerControlContext =
                        self.get_mut_control_context();

                    control_context.reset_checking_depth();

                    if let Err(error) = check::check_type_together(
                        return_type,
                        expr.get_value_type()?,
                        Some(expr),
                        None,
                        metadata,
                        function_loc,
                        control_context,
                    ) {
                        self.add_error_report(error);
                    }
                }

                self.analyze_expr(expr)?;

                Ok(())
            }
            Ast::Mut { source, value, .. } => {
                let metadata: TypeCheckerNodeMetadata =
                    TypeCheckerNodeMetadata::new(value.is_totaly_literal_value());

                let source_type: &Type = source.get_value_type()?;
                let value_type: &Type = value.get_value_type()?;

                {
                    let lhs_pure_type: Type = source_type.dereference_until_value();
                    let rhs_pure_type: Type = value_type.dereference_until_value();

                    {
                        let control_context: &mut TypeCheckerControlContext =
                            self.get_mut_control_context();

                        control_context.reset_checking_depth();

                        if let Err(error) = check::check_type_together(
                            &lhs_pure_type,
                            &rhs_pure_type,
                            Some(value),
                            None,
                            metadata,
                            source.get_span(),
                            control_context,
                        ) {
                            self.add_error_report(error);
                        }
                    }
                }

                self.analyze_expr(source)?;
                self.analyze_expr(value)?;

                Ok(())
            }
            _ => self.analyze_expr(node),
        }
    }

    fn analyze_expr(&mut self, node: &'type_checker Ast) -> Result<(), CompilationIssue> {
        expressions::validate(self, node)
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    fn post_expression_evaluation(&mut self) {
        self.control_context.reset_checking_depth();
    }
}

impl TypeChecker<'_> {
    fn parse_top(&mut self) {
        {
            for node in self.ast.iter().rev() {
                match node {
                    Ast::AssemblerFunction {
                        name,
                        parameters_types: types,
                        attributes,
                        return_type,
                        ..
                    } => {
                        self.get_mut_table()
                            .new_asm_function(name, (return_type, types, attributes));
                    }
                    Ast::Function {
                        name,
                        parameter_types: types,
                        attributes,
                        return_type,
                        ..
                    } => {
                        self.get_mut_table()
                            .new_function(name, (return_type, types, attributes));
                    }
                    Ast::Intrinsic {
                        name,
                        parameters_types: types,
                        attributes,
                        return_type,
                        ..
                    } => {
                        self.get_mut_table()
                            .new_intrinsic(name, (return_type, types, attributes));
                    }

                    _ => (),
                }
            }
        }
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    #[inline]
    fn advance(&mut self) {
        if !self.is_eof() {
            self.position = self.position.saturating_add(1);
        }
    }

    #[inline]
    fn peek_node(&self) -> &'type_checker Ast<'type_checker> {
        &self.ast[self.position]
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.position >= self.ast.len()
    }
}

impl TypeChecker<'_> {
    #[inline]
    fn add_error_report(&mut self, error: CompilationIssue) {
        self.errors.push(error);
    }

    #[inline]
    fn add_bug(&mut self, error: CompilationIssue) {
        self.bugs.push(error);
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    #[inline]
    fn get_table(&self) -> &TypeCheckerSymbolsTable<'type_checker> {
        &self.table
    }

    #[inline]
    fn get_type_context(&self) -> &TypeCheckerTypeContext<'type_checker> {
        &self.type_context
    }

    #[inline]
    fn get_control_context(&self) -> &TypeCheckerControlContext {
        &self.control_context
    }
}

impl<'type_checker> TypeChecker<'type_checker> {
    #[inline]
    fn get_mut_table(&mut self) -> &mut TypeCheckerSymbolsTable<'type_checker> {
        &mut self.table
    }

    #[inline]
    fn get_mut_type_context(&mut self) -> &mut TypeCheckerTypeContext<'type_checker> {
        &mut self.type_context
    }

    #[inline]
    fn get_mut_control_context(&mut self) -> &mut TypeCheckerControlContext {
        &mut self.control_context
    }
}

impl TypeChecker<'_> {
    #[inline]
    fn begin_scope(&mut self) {
        self.table.begin_scope();
    }

    #[inline]
    fn end_scope(&mut self) {
        self.table.end_scope();
    }
}
