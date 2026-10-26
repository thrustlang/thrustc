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

use crate::macro_ast::MacroExpr;

use std::collections::HashMap;
use std::marker::PhantomData;
use std::path::PathBuf;
use thrustc_errors::CompilationIssue;

#[derive(Debug)]
pub struct MacroContext<'clang> {
    table: crate::macro_table::MacroTable,
    errors: Vec<CompilationIssue>,
    warnings: Vec<CompilationIssue>,
    macros_errors: Vec<CompilationIssue>,
    expansion_stack: Vec<String>,
    temporary_counter: u64,
    pending_statements: Vec<String>,
    type_environment: HashMap<String, crate::macro_type::InferredType>,
    inline_bindings: HashMap<String, MacroExpr>,
    value_types: HashMap<String, crate::macro_type::MacroValueType>,
    record_fields: HashMap<String, HashMap<String, crate::macro_type::MacroValueType>>,
    return_type: Option<String>,
    loop_contexts: Vec<MacroLoopContext>,
    marker: PhantomData<&'clang ()>,
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn new(main_file: PathBuf) -> Self {
        Self {
            table: crate::macro_table::MacroTable::new(main_file),
            errors: Vec::new(),
            warnings: Vec::new(),
            macros_errors: Vec::new(),
            expansion_stack: Vec::new(),
            temporary_counter: 0,
            pending_statements: Vec::new(),
            type_environment: HashMap::new(),
            inline_bindings: HashMap::new(),
            value_types: HashMap::new(),
            record_fields: HashMap::new(),
            return_type: None,
            loop_contexts: Vec::new(),
            marker: PhantomData,
        }
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_expansion(&mut self, name: &str) -> bool {
        if self.expansion_stack.iter().any(|entry| entry == name) {
            return true;
        }

        self.expansion_stack.push(name.to_string());

        false
    }

    #[inline]
    pub fn pop_expansion(&mut self) {
        self.expansion_stack.pop();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_macro_table(&self) -> &crate::macro_table::MacroTable {
        &self.table
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_mut_macro_table(&mut self) -> &mut crate::macro_table::MacroTable {
        &mut self.table
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn next_temporary_name(&mut self) -> String {
        let index: u64 = self.temporary_counter;

        self.temporary_counter = self.temporary_counter.saturating_add(1);

        format!("thrust_temporary_{index}")
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn pending_statements_len(&self) -> usize {
        self.pending_statements.len()
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_pending_statement(&mut self, statement: String) {
        self.pending_statements.push(statement);
    }

    #[inline]
    pub fn take_pending_statements_since(&mut self, base: usize) -> Vec<String> {
        if base >= self.pending_statements.len() {
            return Vec::new();
        }

        self.pending_statements.split_off(base)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_type_environment(&self) -> &HashMap<String, crate::macro_type::InferredType> {
        &self.type_environment
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_type_binding(&mut self, name: String, kind: crate::macro_type::InferredType) {
        self.type_environment.insert(name, kind);
    }

    #[inline]
    pub fn set_type_environment(
        &mut self,
        environment: HashMap<String, crate::macro_type::InferredType>,
    ) {
        self.type_environment = environment;
    }

    #[inline]
    pub fn clear_type_environment(&mut self) {
        self.type_environment.clear();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_return_type(&self) -> Option<&str> {
        self.return_type.as_deref()
    }

    #[inline]
    pub fn get_inline_binding(&self, name: &str) -> Option<&MacroExpr> {
        self.inline_bindings.get(name)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_inline_bindings(&mut self, bindings: HashMap<String, MacroExpr>) {
        self.inline_bindings = bindings;
    }

    #[inline]
    pub fn clear_inline_bindings(&mut self) {
        self.inline_bindings.clear();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_return_type(&mut self, return_type: Option<String>) {
        self.return_type = return_type;
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn push_loop_context(&mut self, context: MacroLoopContext) {
        self.loop_contexts.push(context);
    }

    #[inline]
    pub fn pop_loop_context(&mut self) {
        self.loop_contexts.pop();
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_last_loop_context(&self) -> Option<&MacroLoopContext> {
        self.loop_contexts.last()
    }

    #[inline]
    pub fn get_expansion_stack(&self) -> &[String] {
        &self.expansion_stack
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn add_error(&mut self, issue: CompilationIssue) {
        self.errors.push(issue);
    }

    #[inline]
    pub fn add_warning(&mut self, issue: CompilationIssue) {
        self.warnings.push(issue);
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    #[inline]
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    #[inline]
    pub fn macros_error_count(&self) -> usize {
        self.macros_errors.len()
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    #[inline]
    pub fn has_macros_errors(&self) -> bool {
        !self.macros_errors.is_empty()
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn take_errors(&mut self) -> Vec<CompilationIssue> {
        let taken: Vec<CompilationIssue> = std::mem::take(&mut self.errors);

        let mut unique: Vec<CompilationIssue> = Vec::with_capacity(taken.len());

        for issue in taken {
            let seen: bool = unique
                .iter()
                .any(|known| format!("{known:?}") == format!("{issue:?}"));

            if !seen {
                unique.push(issue);
            }
        }

        unique
    }

    #[inline]
    pub fn take_warnings(&mut self) -> Vec<CompilationIssue> {
        let taken: Vec<CompilationIssue> = std::mem::take(&mut self.warnings);

        let mut unique: Vec<CompilationIssue> = Vec::with_capacity(taken.len());

        for issue in taken {
            let seen: bool = unique
                .iter()
                .any(|known| format!("{known:?}") == format!("{issue:?}"));

            if !seen {
                unique.push(issue);
            }
        }

        unique
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn add_macros_error(&mut self, issue: CompilationIssue) {
        let seen: bool = self
            .macros_errors
            .iter()
            .any(|known| format!("{known:?}") == format!("{issue:?}"));

        if !seen {
            self.macros_errors.push(issue);
        }
    }

    #[inline]
    pub fn take_macros_errors(&mut self) -> Vec<CompilationIssue> {
        std::mem::take(&mut self.macros_errors)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn take_new_errors_since(&mut self, base_error_count: usize) -> Vec<CompilationIssue> {
        if base_error_count >= self.errors.len() {
            return Vec::new();
        }

        self.errors.split_off(base_error_count)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn get_record_field(
        &self,
        record: &str,
        field: &str,
    ) -> Option<&crate::macro_type::MacroValueType> {
        self.record_fields
            .get(record)
            .and_then(|fields| fields.get(field))
    }

    #[inline]
    pub fn get_value_type(&self, name: &str) -> Option<&crate::macro_type::MacroValueType> {
        self.value_types.get(name)
    }
}

impl<'clang> MacroContext<'clang> {
    #[inline]
    pub fn set_value_type(&mut self, name: String, kind: crate::macro_type::MacroValueType) {
        self.value_types.insert(name, kind);
    }

    #[inline]
    pub fn clear_value_types(&mut self) {
        self.value_types.clear();
    }

    #[inline]
    pub fn register_record_fields(
        &mut self,
        record: String,
        fields: HashMap<String, crate::macro_type::MacroValueType>,
    ) {
        self.record_fields.insert(record, fields);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroLoopContext {
    Loop { continue_action: Option<String> },
    Switch { continue_flag: String },
    DoWhileZero,
}

impl MacroLoopContext {
    #[inline]
    pub fn is_switch(&self) -> bool {
        matches!(self, Self::Switch { .. })
    }

    #[inline]
    pub fn continue_action(&self) -> Option<&str> {
        match self {
            Self::Loop { continue_action } => continue_action.as_deref(),
            Self::Switch { .. } | Self::DoWhileZero => None,
        }
    }

    #[inline]
    pub fn continue_flag(&self) -> Option<&str> {
        match self {
            Self::Switch { continue_flag } => Some(continue_flag),
            Self::Loop { .. } | Self::DoWhileZero => None,
        }
    }
}
