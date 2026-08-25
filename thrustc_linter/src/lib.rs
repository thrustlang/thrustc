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
use thrustc_token_type::{TokenType, traits::TokenTypeExtensions};

use ahash::AHashMap as HashMap;

use crate::table::LinterSymbolsTable;

mod expressions;
mod table;

#[derive(Debug)]
pub struct Linter<'linter> {
    ast: &'linter [Ast<'linter>],

    warnings: Vec<CompilationIssue>,
    bugs: Vec<CompilationIssue>,
    errors: Vec<CompilationIssue>,

    options: &'linter CompilerOptions,
    diagnostician: Diagnostician,

    symbols: LinterSymbolsTable<'linter>,

    noreturn_functions: ahash::AHashSet<&'linter str>,

    node_depth: u32,
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
            errors: Vec::with_capacity(u8::MAX as usize),

            options,
            diagnostician: Diagnostician::new(file, options),

            symbols: LinterSymbolsTable::new(),

            noreturn_functions: ahash::AHashSet::new(),

            node_depth: 0,
        }
    }
}

impl<'linter> Linter<'linter> {
    pub fn start(&mut self) {
        self.declare_forward();

        self.reset_node_depth();

        for node in self.ast.iter() {
            self.analyze_decl(node);
        }

        self.generate_warnings();

        for error in self.errors.iter() {
            self.diagnostician
                .dispatch_diagnostic(error, thrustc_logging::LoggingType::Error);
        }

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
                    .new_local_static(name, (*span, false, metadata.is_mutable(), false, None, false));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }

                if value.is_some() {
                    self::mark_as_written(self, name, *span, false);
                }
            }
            Ast::Const {
                name,
                value,
                span,
                attributes,
                ..
            } => {
                self.symbols.new_global_constant(
                    name,
                    (*span, false, attributes.has_public_attribute()),
                );
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

                self.generate_params_function_warnings();
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
        self.enter_node();

        if self.too_deep() {
            self.leave_node();

            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0037,
                "Too many depth for a node.".into(),
                "You should remove the code nesting".into(),
                None,
                node.get_span(),
            ));

            return;
        }

        self.analyze_stmt_inner(node);

        self.leave_node();
    }

    fn analyze_stmt_inner(&mut self, node: &'linter Ast) {
        self.analyze_attributes(node);

        match node {
            Ast::Var {
                name,
                value,
                span,
                metadata,
                ..
            } => {
                if self.symbols.shadows_local(name) {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0021,
                        format!("'{}' shadows an outer declaration.", name),
                        *span,
                    ));
                }

                self.symbols
                    .new_local(name, (*span, false, metadata.is_mutable(), false, None));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }

                if value.is_some() {
                    self::mark_as_written(self, name, *span, false);
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
                    .new_local_static(name, (*span, false, metadata.is_mutable(), false, None, false));

                if let Some(value) = value {
                    self.analyze_expr(value);
                }

                if value.is_some() {
                    self::mark_as_written(self, name, *span, false);
                }
            }
            Ast::Const {
                name, value, span, ..
            } => {
                self.symbols.new_local_constant(name, (*span, false, false));
                self.analyze_expr(value);
            }
            Ast::CustomType { .. } | Ast::Struct { .. } => (),
            Ast::Block {
                nodes, post, span, ..
            } => {
                if nodes.is_empty() && post.is_empty() {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0023,
                        "Empty block has no effect.".into(),
                        *span,
                    ));
                }

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
                span,
                ..
            } => {
                self.analyze_stmt(local);
                self.analyze_expr(actions);
                self.analyze_expr(condition);
                self.analyze_stmt(block);

                if let Some(warning) = self::constant_condition_warning(condition, *span) {
                    self.add_warning(warning);
                }

                let constant_true_condition: bool = matches!(
                    &**condition,
                    Ast::Boolean { value, .. } if *value != 0
                );

                if constant_true_condition
                    && !self::node_has_escape(&self.noreturn_functions, block, 0)
                    && !self::node_has_escape(&self.noreturn_functions, actions, 0)
                {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0025,
                        "For loop with constant true condition may be infinite.".into(),
                        *span,
                    ));
                }
            }
            Ast::While {
                variable,
                condition,
                block,
                span,
                ..
            } => {
                if let Some(node) = variable {
                    self.analyze_stmt(node);
                }

                self.analyze_expr(condition);
                self.analyze_stmt(block);

                if let Some(warning) = self::constant_condition_warning(condition, *span) {
                    self.add_warning(warning);
                }

                let constant_true_condition: bool = matches!(
                    &**condition,
                    Ast::Boolean { value, .. } if *value != 0
                );

                if constant_true_condition
                    && !self::node_has_escape(&self.noreturn_functions, block, 0)
                {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0025,
                        "While loop with constant true condition may be infinite.".into(),
                        *span,
                    ));
                }
            }
            Ast::Loop { block, span, .. } => {
                self.analyze_stmt(block);

                if !self::node_has_escape(&self.noreturn_functions, block, 0) {
                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0025,
                        "Loop has no exit condition and may be infinite.".into(),
                        *span,
                    ));
                }
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
                span,
                ..
            } => {
                if let Some(warning) = self::constant_condition_warning(condition, *span) {
                    self.add_warning(warning);
                }

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
                condition,
                block,
                span,
                ..
            } => {
                if let Some(warning) = self::constant_condition_warning(condition, *span) {
                    self.add_warning(warning);
                }

                self.analyze_expr(condition);
                self.analyze_stmt(block);
            }
            Ast::Else { block, .. } => {
                self.analyze_stmt(block);
            }

            Ast::Mutation {
                source,
                value,
                span,
                ..
            } => {
                if let Ast::Reference { name, .. } = &**source {
                    self::mark_as_used(self, name);
                } else {
                    self.analyze_expr(source);
                }

                if let (Ast::Reference { name: lname, .. }, Ast::Reference { name: rname, .. }) =
                    (&**source, &**value)
                {
                    if lname == rname {
                        self.add_warning(CompilationIssue::Warning(
                            CompilationIssueCode::W0022,
                            format!("'{}' is assigned to itself, which has no effect.", lname),
                            *span,
                        ));
                    }
                }

                self.analyze_expr(value);

                if let Ast::Reference { name, .. } = &**source {
                    let compound: bool = self::expr_references(value, name);
                    self::mark_as_mutated(self, name, *span, compound);
                } else if let Some(name) = self::lvalue_base_reference(source) {
                    self::mark_as_mutated_through(self, name);
                }
            }

            Ast::Return { expression, .. } => {
                if let Some(expr) = expression {
                    self.analyze_expr(expr);
                }
            }

            expr => {
                if self::expr_has_no_effect(expr) {
                    let span: Span = expr.get_span();

                    self.add_warning(CompilationIssue::Warning(
                        CompilationIssueCode::W0028,
                        "This statement has no effect.".into(),
                        span,
                    ));
                }

                self.analyze_expr(expr);
            }
        }
    }

    fn analyze_expr(&mut self, expr: &'linter Ast) {
        self.enter_node();

        if self.too_deep() {
            self.leave_node();

            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0037,
                "Too many depth for a node.".into(),
                "You should remove the code nesting".into(),
                None,
                expr.get_span(),
            ));

            return;
        }

        expressions::analyze(self, expr);

        self.leave_node();
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
                    attributes,
                    value,
                    ..
                } => {
                    let info: thrustc_entities::linter_entities::LinterStaticInfo = (
                        *span,
                        false,
                        metadata.is_mutable(),
                        false,
                        None,
                        attributes.has_public_attribute(),
                    );

                    if value.is_none() && attributes.has_extern_attribute() {
                        self.symbols.new_extern_static(name, info);
                    } else {
                        self.symbols.new_global_static(name, info);
                    }
                }
                Ast::Const {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_global_constant(name, (*span, false, attributes.has_public_attribute()));
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
                    let mut field_names: Vec<&str> = Vec::with_capacity(100);

                    for (field_name, _, _, span) in data.1.iter() {
                        converted_fields.insert(field_name, (*span, false));
                        field_names.push(field_name);
                    }

                    self.symbols.new_struct(
                        name,
                        (
                            converted_fields,
                            field_names,
                            *span,
                            false,
                            attributes.has_public_attribute(),
                        ),
                    );
                }

                Ast::Enum {
                    name,
                    data,
                    span,
                    attributes,
                    ..
                } => {
                    let mut converted_fields: HashMap<&str, (Span, bool)> =
                        HashMap::with_capacity(100);

                    for (field_name, _, expr) in data.iter() {
                        let expr_span: Span = expr.get_span();

                        converted_fields.insert(field_name, (expr_span, false));
                    }

                    self.symbols.new_enum(
                        name,
                        (converted_fields, *span, false, attributes.has_public_attribute()),
                    );
                }

                Ast::Function {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_function(name, (*span, false, attributes.has_public_attribute()));

                    if attributes.has_noreturn_attribute() {
                        self.noreturn_functions.insert(name);
                    }
                }

                Ast::CompilerIntrinsic {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_intrinsic(name, (*span, false, attributes.has_public_attribute()));

                    if attributes.has_noreturn_attribute() {
                        self.noreturn_functions.insert(name);
                    }
                }

                Ast::AssemblerFunction {
                    name,
                    span,
                    attributes,
                    ..
                } => {
                    self.symbols
                        .new_asm_function(name, (*span, false, attributes.has_public_attribute()));
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

                let is_mutable: bool = info.2;
                let was_mutated: bool = info.3;

                if is_mutable && !was_mutated {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0020,
                        format!("'{}' is declared mutable but never mutated.", name),
                        span,
                    ));
                }

                if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                    warnings.push(warning);
                }

                if used {
                    if let Some(dead_span) = info.4 {
                        warnings.push(CompilationIssue::Warning(
                            CompilationIssueCode::W0027,
                            format!("'{}' is assigned a value that is never read.", name),
                            dead_span,
                        ));
                    }
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

                if let Some(warning) = bad_name_warning(name, span, NameKind::Constant) {
                    warnings.push(warning);
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

                let is_mutable: bool = info.2;
                let was_mutated: bool = info.3;

                if is_mutable && !was_mutated {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0020,
                        format!("'{}' is declared mutable but never mutated.", name),
                        span,
                    ));
                }

                if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                    warnings.push(warning);
                }

                if used {
                    if let Some(dead_span) = info.4 {
                        warnings.push(CompilationIssue::Warning(
                            CompilationIssueCode::W0027,
                            format!("'{}' is assigned a value that is never read.", name),
                            dead_span,
                        ));
                    }
                }
            }
        }

        self.add_bulk_warnings(warnings);
    }

    fn generate_params_function_warnings(&mut self) {
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

            let is_mutable: bool = info.2;
            let was_mutated: bool = info.3;

            if is_mutable && !was_mutated {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0020,
                    format!("'{}' is declared mutable but never mutated.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                warnings.push(warning);
            }

            if used {
                if let Some(dead_span) = info.4 {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0027,
                        format!("'{}' is assigned a value that is never read.", name),
                        dead_span,
                    ));
                }
            }
        }

        self.add_bulk_warnings(warnings);
    }

    fn generate_warnings(&mut self) {
        let mut warnings: Vec<CompilationIssue> = Vec::with_capacity(u8::MAX.into());

        for (name, info) in self.symbols.get_all_global_statics().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used && !info.5 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0009,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            let is_mutable: bool = info.2;
            let was_mutated: bool = info.3;

            if is_mutable && !was_mutated {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0020,
                    format!("'{}' is declared mutable but never mutated.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                warnings.push(warning);
            }

            if used {
                if let Some(dead_span) = info.4 {
                    warnings.push(CompilationIssue::Warning(
                        CompilationIssueCode::W0027,
                        format!("'{}' is assigned a value that is never read.", name),
                        dead_span,
                    ));
                }
            }
        }

        for (name, info) in self.symbols.get_all_global_constants().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used && !info.2 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0010,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Constant) {
                warnings.push(warning);
            }
        }

        for (name, info) in self.symbols.get_all_functions().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used && !info.2 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0017,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                warnings.push(warning);
            }
        }

        for (name, info) in self.symbols.get_all_asm_functions().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used && !info.2 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0011,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                warnings.push(warning);
            }
        }

        for (name, info) in self.symbols.get_all_enums().iter() {
            let span: Span = info.1;
            let used: bool = info.2;

            if !used && !info.3 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0012,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Type) {
                warnings.push(warning);
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

                if let Some(warning) = bad_name_warning(field_name, span, NameKind::Value) {
                    warnings.push(warning);
                }
            }
        }

        for (name, info) in self.symbols.get_all_intrinsics().iter() {
            let span: Span = info.0;
            let used: bool = info.1;

            if !used && !info.2 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0014,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Value) {
                warnings.push(warning);
            }
        }

        for (name, info) in self.symbols.get_all_structs().iter() {
            let span: Span = info.2;
            let used: bool = info.3;

            if !used && !info.4 {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0015,
                    format!("'{}' not used.", name),
                    span,
                ));
            }

            if let Some(warning) = bad_name_warning(name, span, NameKind::Type) {
                warnings.push(warning);
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

                if let Some(warning) = bad_name_warning(field_name, span, NameKind::Value) {
                    warnings.push(warning);
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
    fn enter_node(&mut self) {
        self.node_depth = self.node_depth.saturating_add(1);
    }

    #[inline]
    fn leave_node(&mut self) {
        self.node_depth = self.node_depth.saturating_sub(1);
    }

    #[inline]
    fn reset_node_depth(&mut self) {
        self.node_depth = 0;
    }

    #[inline]
    fn too_deep(&self) -> bool {
        self.node_depth > thrustc_constants::COMPILER_TOO_MANY_EXPRESSION_DEPTH
    }
}

impl Linter<'_> {
    #[inline]
    fn add_bug(&mut self, bug: CompilationIssue) {
        self.bugs.push(bug);
    }

    fn add_error(&mut self, error: CompilationIssue) {
        self.errors.push(error);
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

#[derive(Debug, Clone, Copy)]
enum NameKind {
    Type,
    Value,
    Constant,
}

#[inline]
fn is_lower_camel_case(name: &str) -> bool {
    let mut chars: std::str::Chars<'_> = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    first.is_ascii_lowercase() && chars.all(|c| c.is_ascii_alphanumeric())
}

#[inline]
fn is_upper_camel_case(name: &str) -> bool {
    let mut chars: std::str::Chars<'_> = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    first.is_ascii_uppercase() && chars.all(|c| c.is_ascii_alphanumeric())
}

#[inline]
fn is_upper_snake_case(name: &str) -> bool {
    let mut chars: std::str::Chars<'_> = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    first.is_ascii_uppercase()
        && chars.all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
}

fn bad_name_warning(name: &str, span: Span, kind: NameKind) -> Option<CompilationIssue> {
    let valid: bool = match kind {
        NameKind::Type => is_upper_camel_case(name),
        NameKind::Value => is_lower_camel_case(name),
        NameKind::Constant => is_lower_camel_case(name) || is_upper_snake_case(name),
    };

    if valid {
        return None;
    }

    let hint: &str = match kind {
        NameKind::Type => "type names should use PascalCase",
        NameKind::Value => "identifiers should use lowerCamelCase",
        NameKind::Constant => "constants should use lowerCamelCase or UPPER_SNAKE",
    };

    Some(CompilationIssue::Warning(
        CompilationIssueCode::W0024,
        format!("'{}' violates naming convention: {}.", name, hint),
        span,
    ))
}

fn expr_has_no_effect(node: &Ast) -> bool {
    match node {
        Ast::Integer { .. }
        | Ast::Float { .. }
        | Ast::Boolean { .. }
        | Ast::Char { .. }
        | Ast::CString { .. }
        | Ast::CNString { .. }
        | Ast::NullPtr { .. } => true,

        Ast::Group { node, .. } => self::expr_has_no_effect(node),
        Ast::As { from, .. } => self::expr_has_no_effect(from),
        Ast::Deref { value, .. } => self::expr_has_no_effect(value),

        Ast::BinaryOp { left, right, .. } => {
            self::expr_has_no_effect(left) && self::expr_has_no_effect(right)
        }
        Ast::UnaryOp { operator, node, .. } => {
            !operator.is_minus_minus_operator()
                && !operator.is_plus_plus_operator()
                && self::expr_has_no_effect(node)
        }

        _ => false,
    }
}

fn expr_references(node: &Ast, name: &str) -> bool {
    match node {
        Ast::Reference { name: n, .. } => *n == name,
        Ast::Group { node, .. } => self::expr_references(node, name),
        Ast::As { from, .. } => self::expr_references(from, name),
        Ast::Deref { value, .. } => self::expr_references(value, name),
        Ast::BinaryOp { left, right, .. } => {
            self::expr_references(left, name) || self::expr_references(right, name)
        }
        Ast::UnaryOp { node, .. } => self::expr_references(node, name),
        Ast::Property { source, .. } => self::expr_references(source, name),
        Ast::Index { source, index, .. } => {
            self::expr_references(source, name) || self::expr_references(index, name)
        }
        Ast::Call { args, .. } => args.iter().any(|arg| self::expr_references(arg, name)),
        _ => false,
    }
}

fn lvalue_base_reference<'a>(source: &'a Ast<'a>) -> Option<&'a str> {
    match source {
        Ast::Reference { name, .. } => Some(name),
        Ast::Group { node, .. } => self::lvalue_base_reference(node),
        Ast::Property { source, .. } => self::lvalue_base_reference(source),
        Ast::Deref { value, .. } => self::lvalue_base_reference(value),
        Ast::Index { source, .. } => self::lvalue_base_reference(source),
        Ast::Load { source, .. } => self::lvalue_base_reference(source),
        Ast::GetLocation { expr, .. } => self::lvalue_base_reference(expr),
        _ => None,
    }
}

fn constant_condition_warning(condition: &Ast, span: Span) -> Option<CompilationIssue> {
    if let Some(always_true) = self::constant_truth_value(condition) {
        return Some(CompilationIssue::Warning(
            CompilationIssueCode::W0029,
            format!(
                "Condition is always {}.",
                if always_true { "true" } else { "false" }
            ),
            span,
        ));
    }

    None
}

fn constant_truth_value(node: &Ast) -> Option<bool> {
    match node {
        Ast::Boolean { value, .. } => Some(*value != 0),
        Ast::Integer { value, .. } => Some(*value != 0),

        Ast::Group { node, .. } => self::constant_truth_value(node),

        Ast::UnaryOp { operator, node, .. } if *operator == TokenType::Bang => {
            self::constant_truth_value(node).map(|truth| !truth)
        }

        Ast::BinaryOp {
            operator,
            left,
            right,
            ..
        } if *operator == TokenType::And => {
            let left: Option<bool> = self::constant_truth_value(left);
            let right: Option<bool> = self::constant_truth_value(right);

            if left == Some(false) || right == Some(false) {
                Some(false)
            } else if left == Some(true) && right == Some(true) {
                Some(true)
            } else {
                None
            }
        }

        Ast::BinaryOp {
            operator,
            left,
            right,
            ..
        } if *operator == TokenType::Or => {
            let left: Option<bool> = self::constant_truth_value(left);
            let right: Option<bool> = self::constant_truth_value(right);

            if left == Some(true) || right == Some(true) {
                Some(true)
            } else if left == Some(false) && right == Some(false) {
                Some(false)
            } else {
                None
            }
        }

        _ => None,
    }
}

fn body_has_escape(
    noreturn_functions: &ahash::AHashSet<&str>,
    nodes: &[Ast],
    depth: usize,
) -> bool {
    nodes
        .iter()
        .any(|node| self::node_has_escape(noreturn_functions, node, depth))
}

fn node_has_escape(noreturn_functions: &ahash::AHashSet<&str>, node: &Ast, depth: usize) -> bool {
    match node {
        Ast::Break { .. } | Ast::BreakAll { .. } => depth == 0,
        Ast::Return { .. } | Ast::Unreachable { .. } => true,
        Ast::Call { name, .. } => noreturn_functions.contains(name),

        Ast::Loop { block, .. } => self::node_has_escape(noreturn_functions, block, depth + 1),
        Ast::While {
            variable, block, ..
        } => {
            variable
                .as_ref()
                .is_some_and(|node| self::node_has_escape(noreturn_functions, node, depth + 1))
                || self::node_has_escape(noreturn_functions, block, depth + 1)
        }
        Ast::For {
            local,
            actions,
            condition,
            block,
            ..
        } => {
            self::node_has_escape(noreturn_functions, local, depth + 1)
                || self::node_has_escape(noreturn_functions, actions, depth + 1)
                || self::node_has_escape(noreturn_functions, condition, depth + 1)
                || self::node_has_escape(noreturn_functions, block, depth + 1)
        }

        Ast::Block { nodes, post, .. } => {
            self::body_has_escape(noreturn_functions, nodes, depth)
                || self::body_has_escape(noreturn_functions, post, depth)
        }
        Ast::If {
            then_branch,
            else_if_branch,
            else_branch,
            ..
        } => {
            self::node_has_escape(noreturn_functions, then_branch, depth)
                || else_if_branch
                    .iter()
                    .any(|node| self::node_has_escape(noreturn_functions, node, depth))
                || else_branch
                    .as_ref()
                    .is_some_and(|node| self::node_has_escape(noreturn_functions, node, depth))
        }
        Ast::Elif { block, .. } => self::node_has_escape(noreturn_functions, block, depth),
        Ast::Else { block, .. } => self::node_has_escape(noreturn_functions, block, depth),
        Ast::Defer { node, .. } => self::node_has_escape(noreturn_functions, node, depth),

        _ => false,
    }
}

pub fn mark_as_written<'linter>(
    linter: &mut Linter<'linter>,
    name: &'linter str,
    write_span: Span,
    compound: bool,
) {
    let dead_store_span: Option<Span> = {
        let pending: Option<&mut Option<Span>> =
            if let Some(static_var) = linter.symbols.get_static_info(name) {
                Some(&mut static_var.4)
            } else if let Some(local) = linter.symbols.get_local_info(name) {
                Some(&mut local.4)
            } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
                Some(&mut parameter.4)
            } else {
                None
            };

        let Some(pending) = pending else {
            return;
        };

        if compound {
            *pending = Some(write_span);
            return;
        }

        let old: Option<Span> = pending.take();
        *pending = Some(write_span);
        old
    };

    if let Some(dead_span) = dead_store_span {
        linter.add_warning(CompilationIssue::Warning(
            CompilationIssueCode::W0027,
            "Value assigned but never read before being overwritten.".into(),
            dead_span,
        ));
    }
}

#[inline]
pub fn mark_as_mutated<'linter>(
    linter: &mut Linter<'linter>,
    name: &'linter str,
    write_span: Span,
    compound: bool,
) {
    if let Some(static_var) = linter.symbols.get_static_info(name) {
        static_var.3 = true;
    } else if let Some(local) = linter.symbols.get_local_info(name) {
        local.3 = true;
    } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
        parameter.3 = true;
    }

    self::mark_as_written(linter, name, write_span, compound);
}

#[inline]
pub fn mark_as_mutated_through<'linter>(linter: &mut Linter<'linter>, name: &'linter str) {
    if let Some(static_var) = linter.symbols.get_static_info(name) {
        static_var.3 = true;
    } else if let Some(local) = linter.symbols.get_local_info(name) {
        local.3 = true;
    } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
        parameter.3 = true;
    }
}

#[inline]
pub fn mark_as_read<'linter>(linter: &mut Linter<'linter>, name: &'linter str) {
    if let Some(static_var) = linter.symbols.get_static_info(name) {
        static_var.4 = None;
    } else if let Some(local) = linter.symbols.get_local_info(name) {
        local.4 = None;
    } else if let Some(parameter) = linter.symbols.get_parameter_info(name) {
        parameter.4 = None;
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
