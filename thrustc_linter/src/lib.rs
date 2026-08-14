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
    traits::{AstAttributeExtensions, AstCodeLocation},
};
use thrustc_attributes::{ThrustAttributeComparator, traits::ThrustAttributesExtensions};
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};

use ahash::AHashMap as HashMap;

use crate::table::LinterSymbolsTable;

mod expressions;
mod table;

#[derive(Debug)]
pub struct Linter<'linter> {
    ast: &'linter [Ast<'linter>],

    warnings: Vec<CompilationIssue>,
    bugs: Vec<CompilationIssue>,

    options: &'linter CompilerOptions,
    diagnostician: Diagnostician,

    symbols: LinterSymbolsTable<'linter>,
}

impl<'linter> Linter<'linter> {
    pub fn new(
        ast: &'linter [Ast],
        file: &'linter CompilationUnit,

        options: &'linter CompilerOptions,
    ) -> Self {
        Self {
            ast,
            warnings: Vec::with_capacity(u8::MAX as usize),
            bugs: Vec::with_capacity(u8::MAX as usize),

            options,
            diagnostician: Diagnostician::new(file, options),

            symbols: LinterSymbolsTable::new(),
        }
    }
}

impl<'linter> Linter<'linter> {
    pub fn start(&mut self) {
        self.declare_forward();

        for node in self.ast.iter() {
            self.analyze_decl(node);
        }

        self.generate_warnings();

        for bug in self.bugs.iter() {
            self.diagnostician
                .dispatch_diagnostic(bug, thrustc_logging::LoggingType::Bug);
        }

        let warnings_to_disable: &[CompilationIssueCode] = self.options.get_warnings_to_disable();

        thrustc_errors::filter_warnings(warnings_to_disable, &mut self.warnings);

        for warning in self.warnings.iter() {
            self.diagnostician
                .dispatch_diagnostic(warning, thrustc_logging::LoggingType::Warning);
        }
    }
}

impl<'linter> Linter<'linter> {
    fn analyze_decl(&mut self, node: &'linter Ast) {
        self.analyze_attributes(node);

        match node {
            Ast::Enum { data, .. } => {
                for (_, _, expr) in data.iter() {
                    self.analyze_expr(expr);
                }
            }
            Ast::Static {
                name,
                value,
                metadata,
                span,
                ..
            } => {
                self.symbols
                    .new_local_static(name, (*span, false, !metadata.is_mutable()));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }
            }
            Ast::Const {
                name, value, span, ..
            } => {
                self.symbols.new_global_constant(name, (*span, false));
                self.analyze_expr(value);
            }
            Ast::Function {
                parameters,
                body: Some(body),
                ..
            } => {
                self.symbols.declare_parameters(parameters);
                self.analyze_stmt(body);
                self.symbols.finish_parameters();

                self.generate_scoped_function_warnings();
            }

            Ast::AssemblerFunction { span, .. } => {
                self.add_warning(CompilationIssue::Warning(
                    CompilationIssueCode::W0019,
                    "An unstable feature could produce unexpected compiler panics on weird behaviors."
                        .into(),
                    *span,
                ));
            }

            _ => (),
        }
    }

    fn analyze_stmt(&mut self, node: &'linter Ast) {
        self.analyze_attributes(node);

        match node {
            Ast::Var {
                name,
                value,
                span,
                metadata,
                ..
            } => {
                self.symbols
                    .new_local(name, (*span, false, !metadata.is_mutable()));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }
            }
            Ast::Enum { data, .. } => {
                for (_, _, expr) in data.iter() {
                    self.analyze_expr(expr);
                }
            }
            Ast::Static {
                name,
                value,
                metadata,
                span,
                ..
            } => {
                self.symbols
                    .new_local_static(name, (*span, false, !metadata.is_mutable()));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }
            }
            Ast::Const {
                name, value, span, ..
            } => {
                self.symbols.new_local_constant(name, (*span, false));
                self.analyze_expr(value);
            }
            Ast::CustomType { .. } | Ast::Struct { .. } => (),
            Ast::Block { nodes, post, .. } => {
                self.begin_scope();

                {
                    for node in nodes.iter() {
                        self.analyze_stmt(node);
                    }

                    for postnode in post.iter() {
                        self.analyze_stmt(postnode);
                    }
                }

                self.generate_scoped_warnings();

                self.end_scope();
            }
            Ast::Defer { node, .. } => {
                self.analyze_stmt(node);
            }

            Ast::For {
                local,
                actions,
                condition,
                block,
                ..
            } => {
                self.analyze_stmt(local);
                self.analyze_expr(actions);
                self.analyze_expr(condition);
                self.analyze_stmt(block);
            }
            Ast::While {
                variable,
                condition,
                block,
                ..
            } => {
                if let Some(node) = variable {
                    self.analyze_stmt(node);
                }

                self.analyze_expr(condition);
                self.analyze_stmt(block);
            }
            Ast::Loop { block, .. } => {
                self.analyze_stmt(block);
            }

            Ast::Continue { .. }
            | Ast::ContinueAll { .. }
            | Ast::Break { .. }
            | Ast::BreakAll { .. } => (),

            Ast::If {
                condition,
                then_branch,
                else_if_branch,
                else_branch,
                ..
            } => {
                self.analyze_expr(condition);
                self.analyze_stmt(then_branch);

                {
                    for node in else_if_branch.iter() {
                        self.analyze_stmt(node);
                    }
                }

                if let Some(node) = else_branch {
                    self.analyze_stmt(node);
                }
            }
            Ast::Elif {
                condition, block, ..
            } => {
                self.analyze_expr(condition);
                self.analyze_stmt(block);
            }
            Ast::Else { block, .. } => {
                self.analyze_stmt(block);
            }

            Ast::Mutation { source, value, .. } => {
                if let Ast::Reference { name, .. } = &**source {
                    self::mark_as_used(self, name);
                    self::mark_as_mutated(self, name);
                }

                self.analyze_expr(source);
                self.analyze_expr(value);
            }

            Ast::Return { expression, .. } => {
                if let Some(expr) = expression {
                    self.analyze_expr(expr);
                }
            }

            expr => self.analyze_expr(expr),
        }
    }

    fn analyze_expr(&mut self, expr: &'linter Ast) {
        expressions::analyze(self, expr);
    }
}

impl Linter<'_> {
    fn declare_forward(&mut self) {
        for ast in self.ast.iter() {
            match ast {
                Ast::Static {
                    name,
                    metadata,
                    span,
                    ..
                } => {
                    self.symbols
                        .new_global_static(name, (*span, false, !metadata.is_mutable()));
                }
                Ast::Const { name, span, .. } => {
                    self.symbols.new_global_constant(name, (*span, false));
                }
                Ast::Struct {
                    name,
                    data,
                    span,
                    attributes,
                    ..
                } => {
                    let mut converted_fields: HashMap<&str, (Span, bool)> =
                        HashMap::with_capacity(100);

                    for (field_name, _, _, span) in data.1.iter() {
                        converted_fields.insert(field_name, (*span, false));
                    }

                    self.symbols.new_struct(
                        name,
                        (converted_fields, *span, attributes.has_public_attribute()),
                    );
                }

                Ast::Enum {
                    name, data, span, ..
                } => {
                    let mut converted_fields: HashMap<&str, (Span, bool)> =
                        HashMap::with_capacity(100);

                    for (field_name, _, expr) in data.iter() {
                        let expr_span: Span = expr.get_span();

                        converted_fields.insert(field_name, (expr_span, false));
                    }

                    self.symbols
                        .new_enum(name, (converted_fields, *span, false));
                }

                Ast::Function {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_function(name, (*span, attributes.has_public_attribute()));
                }

                Ast::CompilerIntrinsic {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_intrinsic(name, (*span, attributes.has_public_attribute()));
                }

                Ast::AssemblerFunction {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_asm_function(name, (*span, attributes.has_public_attribute()));
                }

                _ => (),
            }
        }
    }
}

impl Linter<'_> {
    fn analyze_attributes(&mut self, node: &Ast) {
        let Some(attributes) = node.get_attributes() else {
            return;
        };

        if let Some(promote_attr) = attributes.get_attr(ThrustAttributeComparator::Promote) {
            let span: Span = promote_attr.get_span();

            self.add_warning(CompilationIssue::Warning(
                CompilationIssueCode::W0019,
                "An unstable feature could produce unexpected compiler panics on weird behaviors."
                    .into(),
                span,
            ));
        }

        if let Some(asm_syntax_attr) = attributes.get_attr(ThrustAttributeComparator::AsmSyntax) {
            let span: Span = asm_syntax_attr.get_span();

            self.add_warning(CompilationIssue::Warning(
                CompilationIssueCode::W0019,
                "An unstable feature could produce unexpected compiler panics on weird behaviors."
                    .into(),
                span,
            ));
        }

        if let Some(asm_align_attr) = attributes.get_attr(ThrustAttributeComparator::AsmAlignStack)
        {
            let span: Span = asm_align_attr.get_span();

            self.add_warning(CompilationIssue::Warning(
                CompilationIssueCode::W0019,
                "An unstable feature could produce unexpected compiler panics on weird behaviors."
                    .into(),
                span,
            ));
        }

        if let Some(asm_throw_attr) = attributes.get_attr(ThrustAttributeComparator::AsmThrow) {
            let span: Span = asm_throw_attr.get_span();

            self.add_warning(CompilationIssue::Warning(
                CompilationIssueCode::W0019,
                "An unstable feature could produce unexpected compiler panics on weird behaviors."
                    .into(),
                span,
            ));
        }

        if let Some(asm_side_effects_attr) =
            attributes.get_attr(ThrustAttributeComparator::AsmSideEffects)
        {
            let span: Span = asm_side_effects_attr.get_span();

            self.add_warning(CompilationIssue::Warning(
                CompilationIssueCode::W0019,
                "An unstable feature could produce unexpected compiler panics on weird behaviors."
                    .into(),
                span,
            ));
        }
    }
}

impl Linter<'_> {
    fn generate_scoped_warnings(&mut self) {
        let mut warnings: Vec<CompilationIssue> = Vec::with_capacity(u8::MAX.into());

        if let Some(last_scope) = self.symbols.get_all_locals().last() {
            for (name, info) in last_scope.iter() {
                let span: Span = info.0;
                let used: bool = info.1;

                if !used {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0005,
                        format!("'{}' not used.", name),
                        span,
                    ));
                }
            }
        }

        if let Some(last_scope) = self.symbols.get_all_local_constants().last() {
            for (name, info) in last_scope.iter() {
                let span: Span = info.0;
                let used: bool = info.1;

                if !used {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0010,
                        format!("'{}' not used.", name),
                        span,
                    ));
                }
            }
        }

        if let Some(last_scope) = self.symbols.get_all_locals_statics().last() {
            for (name, info) in last_scope.iter() {
                let span: Span = info.0;
                let used: bool = info.1;

                if !used {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0009,
                        format!("'{}' not used.", name),
                        span,
                    ));
                }
            }
        }

        self.add_bulk_warnings(warnings);
    }

    fn generate_scoped_function_warnings(&mut self) {
        let mut warnings: Vec<CompilationIssue> = Vec::with_capacity(u8::MAX.into());

        for (name, info) in self.symbols.get_all_function_parameters().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0008,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        self.add_bulk_warnings(warnings);
    }

    fn generate_warnings(&mut self) {
        let mut warnings: Vec<CompilationIssue> = Vec::with_capacity(u8::MAX.into());

        for (name, info) in self.symbols.get_all_global_statics().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0009,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        for (name, info) in self.symbols.get_all_global_constants().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0010,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        for (name, info) in self.symbols.get_all_functions().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0017,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        for (name, info) in self.symbols.get_all_asm_functions().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0011,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        for (name, info) in self.symbols.get_all_enums().iter() {
            let span: Span = info.1;
            let used: bool = info.2;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0012,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            let fields: &HashMap<&str, (Span, bool)> = &info.0;

            for (field_name, field_info) in fields.iter() {
                let span: Span = field_info.0;
                let used: bool = field_info.1;

                if !used {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0013,
                        format!("'{}' not used.", field_name),
                        span,
                    ));
                }
            }
        }

        for (name, info) in self.symbols.get_all_intrinsics().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0014,
                    format!("'{}' not used.", name),
                    span,
                ));
            }
        }

        for (name, info) in self.symbols.get_all_structs().iter() {
            let span: Span = info.1;
            let used: bool = info.2;

            if !used {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0015,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            let fields: &HashMap<&str, (Span, bool)> = &info.0;

            for (field_name, field_info) in fields.iter() {
                let span: Span = field_info.0;
                let used: bool = field_info.1;

                if !used {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0016,
                        format!("'{}' not used.", field_name),
                        span,
                    ));
                }
            }
        }

        self.add_bulk_warnings(warnings);
    }
}

impl Linter<'_> {
    #[inline]
    fn add_bulk_warnings(&mut self, warnings: Vec<CompilationIssue>) {
        self.warnings.extend(warnings);
    }

    #[inline]
    fn add_warning(&mut self, warning: CompilationIssue) {
        self.warnings.push(warning);
    }
}

impl Linter<'_> {
    #[inline]
    fn add_bug(&mut self, bug: CompilationIssue) {
        self.bugs.push(bug);
    }
}

impl<'linter> Linter<'linter> {
    #[inline]
    fn begin_scope(&mut self) {
        self.symbols.begin_scope();
    }

    #[inline]
    fn end_scope(&mut self) {
        self.symbols.end_scope();
    }
}

impl<'linter> Linter<'linter> {
    #[inline]
    pub fn get_mut_symbols(&mut self) -> &mut LinterSymbolsTable<'linter> {
        &mut self.symbols
    }
}

#[inline]
pub fn mark_as_mutated<'linter>(linter: &mut Linter<'linter>, name: &'linter str) {
    if let Some(static_var) = linter.symbols.get_static_info(name) {
        static_var.2 = true;
    } else if let Some(local) = linter.symbols.get_local_info(name) {
        local.2 = true;
    } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
        parameter.2 = true;
    }
}

#[inline]
pub fn mark_as_used<'linter>(linter: &mut Linter<'linter>, name: &'linter str) {
    if let Some(local) = linter.symbols.get_local_info(name) {
        local.1 = true;
    } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
        parameter.1 = true;
    } else if let Some(constant) = linter.symbols.get_constant_info(name) {
        constant.1 = true;
    } else if let Some(staticvar) = linter.symbols.get_static_info(name) {
        staticvar.1 = true;
    }
}
