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
    traits::{AstCodeLocation, AstDeclarationExtensions, AstStandardExtensions},
};
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_options::{CompilationUnit, CompilerOptions};

use crate::{context::ScoperContext, table::ScoperSymbolTable};

mod checks;
mod context;
mod table;

#[derive(Debug)]
pub struct Scoper<'scoper> {
    ast: &'scoper [Ast<'scoper>],
    context: ScoperContext,
    errors: Vec<CompilationIssue>,
    diagnostician: Diagnostician,

    table: ScoperSymbolTable<'scoper>,
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

            table: ScoperSymbolTable::new(),
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
    fn analyze_global_node(&mut self, node: &Ast<'scoper>) {
        if !node.is_declaration_keyword() {
            self.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0016,
                "Statements and expressions are not allowed at module scope.".into(),
                "Remove it from this scope.".into(),
                None,
                node.get_span(),
            ));
        }

        match node {
            Ast::Function {
                name,
                parameters,
                body,
                ..
            } => {
                self.get_mut_table().add_function(name);

                let Some(body) = body else {
                    return;
                };

                {
                    for parameter in parameters.iter() {
                        let Ast::FunctionParameter { name, .. } = parameter else {
                            continue;
                        };

                        self.get_mut_table().add_parameter(name);
                    }
                }

                self.get_mut_context().enter_function();
                self.analyze_local_node(body);
                self.get_mut_context().leave_function();
                self.get_mut_table().drop_parameters();
            }

            Ast::AssemblerFunction { name, .. } => {
                self.get_mut_table().add_assembler_function(name);
            }

            Ast::Intrinsic { name, .. } => {
                self.get_mut_table().add_compiler_intrinsic(name);
            }

            Ast::Static { name, value, .. } => {
                self.get_mut_table().add_static(name);

                let Some(value) = value else {
                    return;
                };

                self.analyze_local_node(value);
            }

            Ast::Const { name, value, .. } => {
                self.get_mut_table().add_constant(name);
                self.analyze_local_node(value);
            }

            _ => (),
        }
    }

    fn analyze_local_node(&mut self, node: &Ast<'scoper>) {
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
            self.get_mut_table().add_scope();

            checks::check_for_multiple_terminators(self, node);
            checks::check_for_unreachable_code_instructions(self, node);

            for node in nodes.iter() {
                self.analyze_local_node(node);
            }

            for postnode in post.iter() {
                self.analyze_local_node(postnode);
            }

            self.get_mut_table().pop_scope();
        }

        match node {
            Ast::Static { name, value, .. } => {
                self.get_mut_table().add_local(name);

                let Some(value) = value else {
                    return;
                };

                self.analyze_local_node(value);
            }

            Ast::Const { name, value, .. } => {
                self.get_mut_table().add_local(name);
                self.analyze_local_node(value);
            }

            Ast::Var { name, value, .. } => {
                self.get_mut_table().add_local(name);

                let Some(value) = value else {
                    return;
                };

                self.analyze_local_node(value);
            }

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

            Ast::BinaryOp { left, right, .. } => {
                self.analyze_local_node(left);
                self.analyze_local_node(right);
            }
            Ast::UnaryOp { node, .. } => {
                self.analyze_local_node(node);
            }
            Ast::Group { node, .. } => {
                self.analyze_local_node(node);
            }

            Ast::FixedArray { items, .. } => {
                for item in items.iter() {
                    self.analyze_local_node(item);
                }
            }
            Ast::Array { items, .. } => {
                for item in items.iter() {
                    self.analyze_local_node(item);
                }
            }

            Ast::Index { source, index, .. } => {
                self.analyze_local_node(source);
                self.analyze_local_node(index);
            }
            Ast::Property { source, .. } => {
                self.analyze_local_node(source);
            }
            Ast::Deref { value, .. } => {
                self.analyze_local_node(value);
            }
            Ast::GetLocation { expr, .. } => {
                self.analyze_local_node(expr);
            }

            Ast::Constructor { data, .. } => {
                for (_, expr, _, _) in data.iter() {
                    self.analyze_local_node(expr);
                }
            }

            Ast::Call { args, .. } => {
                for arg in args.iter() {
                    self.analyze_local_node(arg);
                }
            }
            Ast::IndirectCall { function, args, .. } => {
                self.analyze_local_node(function);

                for arg in args.iter() {
                    self.analyze_local_node(arg);
                }
            }

            Ast::As { from, .. } => {
                self.analyze_local_node(from);
            }

            Ast::AsmValue { args, .. } => {
                for arg in args.iter() {
                    self.analyze_local_node(arg);
                }
            }

            Ast::EnumValue { value, .. } => {
                self.analyze_local_node(value);
            }

            Ast::Reference { name, span, .. } if !self.get_table().symbol_exists(name) => {
                self.add_error(CompilationIssue::Error(
                    CompilationIssueCode::E0040,
                    format!("'{}' not found", name),
                    "You should either create it or reference it correctly.".into(),
                    None,
                    *span,
                ));
            }

            Ast::Builtin { builtin, .. } => match builtin {
                AstBuiltin::MemSet {
                    dst,
                    new_size,
                    size,
                    ..
                } => {
                    self.analyze_local_node(dst);
                    self.analyze_local_node(new_size);
                    self.analyze_local_node(size);
                }

                AstBuiltin::MemMove { dst, src, size, .. } => {
                    self.analyze_local_node(dst);
                    self.analyze_local_node(src);
                    self.analyze_local_node(size);
                }

                AstBuiltin::MemCpy { dst, src, size, .. } => {
                    self.analyze_local_node(dst);
                    self.analyze_local_node(src);
                    self.analyze_local_node(size);
                }

                // No envuelven ninguna subexpresión: operan sobre
                // tipos, no sobre valores.
                AstBuiltin::Halloc { .. }
                | AstBuiltin::AlignOf { .. }
                | AstBuiltin::SizeOf { .. }
                | AstBuiltin::AbiSizeOf { .. }
                | AstBuiltin::BitSizeOf { .. }
                | AstBuiltin::AbiAlignOf { .. } => (),
            },

            // ---------------------------------------------------------
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

    #[inline]
    fn get_table(&self) -> &ScoperSymbolTable<'scoper> {
        &self.table
    }
}

impl<'scoper> Scoper<'scoper> {
    #[inline]
    fn get_mut_context(&mut self) -> &mut ScoperContext {
        &mut self.context
    }

    #[inline]
    fn get_mut_table(&mut self) -> &mut ScoperSymbolTable<'scoper> {
        &mut self.table
    }
}
