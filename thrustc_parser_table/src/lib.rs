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

mod abort;
mod generic;
mod impls;
pub mod traits;

pub use self::generic::{GenericCustomTypeEntry, GenericFunctionEntry, GenericStructEntry};

use std::path::PathBuf;

use thrustc_ast::Ast;
use thrustc_code_location::Span;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};

use thrustc_entities::parser_entities::{
    AssemblerFunction, AssemblerFunctions, ConstantSymbol, CustomTypeSymbol, EnumSymbol,
    FoundSymbolId, Function, Functions, GlobalConstants, GlobalCustomTypes, GlobalEnums,
    GlobalStatics, GlobalStructs, Intrinsic, Intrinsics, LLISymbol, LLIs, LocalConstants,
    LocalCustomTypes, LocalEnums, LocalStatics, LocalStructs, LocalSymbol, Locals, ParameterSymbol,
    Parameters, StaticSymbol, Struct,
};

use thrustc_generics::GenericScope;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable<'parser> {
    functions: Functions<'parser>,
    asm_functions: AssemblerFunctions<'parser>,
    intrinsics: Intrinsics<'parser>,

    global_custom_types: GlobalCustomTypes<'parser>,
    global_statics: GlobalStatics<'parser>,
    global_structs: GlobalStructs<'parser>,
    global_constants: GlobalConstants<'parser>,
    global_enums: GlobalEnums<'parser>,

    generic_functions: ahash::AHashMap<&'parser str, GenericFunctionEntry>,
    generic_structs: ahash::AHashMap<&'parser str, GenericStructEntry<'parser>>,
    generic_custom_types: ahash::AHashMap<&'parser str, GenericCustomTypeEntry>,

    local_structs: LocalStructs<'parser>,
    local_statics: LocalStatics<'parser>,
    local_constants: LocalConstants<'parser>,
    local_custom_types: LocalCustomTypes<'parser>,
    local_enums: LocalEnums<'parser>,

    locals: Locals<'parser>,
    llis: LLIs<'parser>,
    parameters: Parameters<'parser>,

    imported_symbols: ahash::AHashMap<&'parser str, PathBuf>,

    type_parameter_scope: GenericScope,

    diagnostician: Diagnostician,
}

impl<'parser> SymbolTable<'parser> {
    pub fn with_functions(
        functions: Functions<'parser>,
        asm_functions: AssemblerFunctions<'parser>,
        options: &CompilerOptions,
        file: &CompilationUnit,
    ) -> Self {
        Self {
            functions,
            asm_functions,

            intrinsics: ahash::AHashMap::with_capacity(u8::MAX as usize),

            global_structs: ahash::AHashMap::with_capacity(u8::MAX as usize),
            global_statics: ahash::AHashMap::with_capacity(u8::MAX as usize),
            global_constants: ahash::AHashMap::with_capacity(u8::MAX as usize),
            global_custom_types: ahash::AHashMap::with_capacity(u8::MAX as usize),
            global_enums: ahash::AHashMap::with_capacity(u8::MAX as usize),

            generic_functions: ahash::AHashMap::with_capacity(u8::MAX as usize),
            generic_structs: ahash::AHashMap::with_capacity(u8::MAX as usize),
            generic_custom_types: ahash::AHashMap::with_capacity(u8::MAX as usize),

            local_structs: Vec::with_capacity(u8::MAX as usize),
            local_statics: Vec::with_capacity(u8::MAX as usize),
            local_constants: Vec::with_capacity(u8::MAX as usize),
            local_custom_types: Vec::with_capacity(u8::MAX as usize),
            local_enums: Vec::with_capacity(u8::MAX as usize),
            locals: Vec::with_capacity(u8::MAX as usize),
            llis: Vec::with_capacity(u8::MAX as usize),

            parameters: ahash::AHashMap::with_capacity(10),
            imported_symbols: ahash::AHashMap::with_capacity(u8::MAX as usize),
            type_parameter_scope: GenericScope::new(),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl SymbolTable<'_> {
    #[inline]
    pub fn has_function(&self, id: &str) -> bool {
        self.functions.contains_key(id)
    }

    #[inline]
    pub fn has_global_constant(&self, id: &str) -> bool {
        self.global_constants.contains_key(id)
    }

    #[inline]
    pub fn has_global_static(&self, id: &str) -> bool {
        self.global_statics.contains_key(id)
    }

    #[inline]
    pub fn has_global_custom_type(&self, id: &str) -> bool {
        self.global_custom_types.contains_key(id)
    }

    #[inline]
    pub fn has_global_struct(&self, id: &str) -> bool {
        self.global_structs.contains_key(id)
    }
}

impl<'parser> SymbolTable<'parser> {
    #[inline]
    pub fn record_import_origin(&mut self, id: &'parser str, path: PathBuf) {
        self.imported_symbols.insert(id, path);
    }

    #[inline]
    pub fn get_import_origin(&self, id: &str) -> Option<&PathBuf> {
        self.imported_symbols.get(id)
    }
}

impl SymbolTable<'_> {
    #[inline]
    pub fn has_any_generic(&self) -> bool {
        !self.generic_functions.is_empty()
            || !self.generic_structs.is_empty()
            || !self.generic_custom_types.is_empty()
    }

    #[inline]
    pub fn begin_generic_scope(&mut self) {
        self.type_parameter_scope.enter_scope();
    }

    #[inline]
    pub fn end_generic_scope(&mut self) {
        self.type_parameter_scope.exit_scope();
    }

    #[inline]
    pub fn push_type_parameter(&mut self, name: String, span: Span) {
        self.type_parameter_scope.push_parameter(name, span);
    }

    #[inline]
    pub fn resolve_type_parameter(&self, name: &str) -> Option<Span> {
        self.type_parameter_scope.resolve(name)
    }

    #[inline]
    pub fn has_in_scope_type_parameters(&self) -> bool {
        !self.type_parameter_scope.is_empty()
    }
}

impl<'parser> SymbolTable<'parser> {
    #[inline]
    pub fn new_generic_function(&mut self, id: &'parser str, entry: GenericFunctionEntry) {
        self.generic_functions.insert(id, entry);
    }

    #[inline]
    pub fn new_generic_struct(&mut self, id: &'parser str, entry: GenericStructEntry<'parser>) {
        self.generic_structs.insert(id, entry);
    }

    #[inline]
    pub fn new_generic_custom_type(&mut self, id: &'parser str, entry: GenericCustomTypeEntry) {
        self.generic_custom_types.insert(id, entry);
    }
}

impl<'parser> SymbolTable<'parser> {
    #[inline]
    pub fn has_generic_function(&self, id: &str) -> bool {
        self.generic_functions.contains_key(id)
    }

    #[inline]
    pub fn has_generic_struct(&self, id: &str) -> bool {
        self.generic_structs.contains_key(id)
    }

    #[inline]
    pub fn has_generic_custom_type(&self, id: &str) -> bool {
        self.generic_custom_types.contains_key(id)
    }
}

impl<'parser> SymbolTable<'parser> {
    #[inline]
    pub fn get_generic_function(&self, id: &str) -> Option<&GenericFunctionEntry> {
        self.generic_functions.get(id)
    }

    #[inline]
    pub fn get_generic_struct(&self, id: &str) -> Option<&GenericStructEntry<'parser>> {
        self.generic_structs.get(id)
    }

    #[inline]
    pub fn get_generic_custom_type(&self, id: &str) -> Option<&GenericCustomTypeEntry> {
        self.generic_custom_types.get(id)
    }
}

impl SymbolTable<'_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.local_structs
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
        self.local_custom_types
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
        self.local_statics
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
        self.local_constants
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
        self.local_enums
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));

        self.locals
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
        self.llis
            .push(ahash::AHashMap::with_capacity(u8::MAX as usize));
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.local_statics.pop();
        self.local_constants.pop();
        self.local_structs.pop();
        self.local_custom_types.pop();
        self.local_enums.pop();

        self.locals.pop();
        self.llis.pop();
    }

    #[inline]
    pub fn finish_scopes(&mut self) {
        self.local_statics.clear();
        self.local_constants.clear();
        self.local_structs.clear();
        self.local_custom_types.clear();
        self.local_enums.clear();

        self.locals.clear();
        self.llis.clear();
    }

    #[inline]
    pub fn finish_parameters(&mut self) {
        self.parameters.clear();
    }
}

impl<'parser> SymbolTable<'parser> {
    pub fn new_parameters(&mut self, parameters: &[Ast<'parser>]) -> Result<(), CompilationIssue> {
        for node in parameters.iter() {
            if let Ast::FunctionParameter {
                name: id,
                kind,
                span,
                metadata,
                ..
            } = node
            {
                if self.parameters.contains_key(id.as_str()) {
                    return Err(CompilationIssue::Error(
                        CompilationIssueCode::E0004,
                        format!("'{}' parameter was declared before.", id),
                        "You should rename it or remove the copy.".into(),
                        None,
                        *span,
                    ));
                }

                let parameter: ParameterSymbol = (kind.clone(), *metadata, *span);

                self.parameters.insert(id.clone(), parameter);
            }
        }

        Ok(())
    }
}

impl<'parser> SymbolTable<'parser> {
    pub fn new_local(
        &mut self,
        id: &'parser str,
        local: LocalSymbol<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.locals.last_mut() {
            last_scope.insert(id, local);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_global_static(
        &mut self,
        id: &'parser str,
        static_: StaticSymbol<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.global_statics.insert(id, static_);

        Ok(())
    }

    pub fn new_static(
        &mut self,
        id: &'parser str,
        static_: StaticSymbol<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.local_statics.last_mut() {
            last_scope.insert(id, static_);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_global_constant(
        &mut self,
        id: &'parser str,
        constant: ConstantSymbol<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.global_constants.insert(id, constant);

        Ok(())
    }

    pub fn new_constant(
        &mut self,
        id: &'parser str,
        constant: ConstantSymbol<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.local_constants.last_mut() {
            last_scope.insert(id, constant);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_global_custom_type(
        &mut self,
        id: &'parser str,
        ctype: CustomTypeSymbol<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.global_custom_types.insert(id, ctype);

        Ok(())
    }

    pub fn new_custom_type(
        &mut self,
        id: &'parser str,
        ctype: CustomTypeSymbol<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.local_custom_types.last_mut() {
            last_scope.insert(id, ctype);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_global_struct(
        &mut self,
        id: &'parser str,
        fields: Struct<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.global_structs.insert(id, fields);

        Ok(())
    }

    pub fn new_struct(
        &mut self,
        id: &'parser str,
        fields: Struct<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.local_structs.last_mut() {
            last_scope.insert(id, fields);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_global_enum(
        &mut self,
        id: &'parser str,
        union: EnumSymbol<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.global_enums.insert(id, union);

        Ok(())
    }

    pub fn new_enum(
        &mut self,
        id: &'parser str,
        union: EnumSymbol<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if let Some(last_scope) = self.local_enums.last_mut() {
            if last_scope.contains_key(id) {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0004,
                    format!("Enum '{}' was declared before.", id),
                    "You should rename it or remove the copy.".into(),
                    None,
                    span,
                ));
            }

            last_scope.insert(id, union);

            Ok(())
        } else {
            abort::abort_compilation(
                &mut self.diagnostician,
                CompilationPosition::Parser,
                "Unable to get the last scope!",
                span,
                std::path::PathBuf::from(file!()),
                line!(),
            )
        }
    }

    pub fn new_asm_function(
        &mut self,
        id: &'parser str,
        function: AssemblerFunction<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.asm_functions.insert(id, function);

        Ok(())
    }

    pub fn new_function(
        &mut self,
        id: &'parser str,
        function: Function<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.functions.insert(id, function);

        Ok(())
    }

    pub fn new_compiler_intrinsic(
        &mut self,
        id: &'parser str,
        intrinsic: Intrinsic<'parser>,
    ) -> Result<(), CompilationIssue> {
        self.intrinsics.insert(id, intrinsic);

        Ok(())
    }
}

impl<'parser> SymbolTable<'parser> {
    pub fn get_symbols_id(
        &self,
        id: &'parser str,
        span: Span,
    ) -> Result<FoundSymbolId<'parser>, CompilationIssue> {
        for (idx, scope) in self.locals.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some((id, idx)),
                    None,
                ));
            }
        }

        for (idx, scope) in self.local_structs.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    Some((id, idx)),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
            }
        }

        for (idx, scope) in self.local_enums.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    None,
                    None,
                    Some((id, idx)),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
            }
        }

        for (idx, scope) in self.local_custom_types.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some((id, idx)),
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
            }
        }

        for (idx, scope) in self.local_constants.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    None,
                    None,
                    None,
                    None,
                    Some((id, idx)),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
            }
        }

        for (idx, scope) in self.local_statics.iter().enumerate().rev() {
            if scope.contains_key(id) {
                return Ok((
                    None,
                    None,
                    None,
                    Some((id, idx)),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                ));
            }
        }

        if self.parameters.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                None,
                None,
                None,
                Some(id),
                None,
                None,
                None,
                None,
            ));
        }

        if self.global_structs.contains_key(id) {
            return Ok((
                Some((id, 0)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            ));
        }

        if self.global_enums.contains_key(id) {
            return Ok((
                None,
                None,
                Some((id, 0)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            ));
        }

        if self.global_custom_types.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                None,
                None,
                Some((id, 0)),
                None,
                None,
                None,
                None,
                None,
            ));
        }

        if self.intrinsics.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(id),
            ));
        }

        if self.functions.contains_key(id) {
            return Ok((
                None,
                Some(id),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            ));
        }

        if self.asm_functions.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(id),
                None,
                None,
                None,
            ));
        }

        if self.global_constants.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                None,
                Some((id, 0)),
                None,
                None,
                None,
                None,
                None,
                None,
            ));
        }

        if self.global_statics.contains_key(id) {
            return Ok((
                None,
                None,
                None,
                Some((id, 0)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("'{}' not found", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }
}

impl<'parser> SymbolTable<'parser> {
    #[inline]
    pub fn get_lli_by_id(
        &self,
        id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<&LLISymbol<'parser>, CompilationIssue> {
        if let Some(scope) = self.llis.get(scope_idx) {
            if let Some(lli) = scope.get(id) {
                return Ok(lli);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Scope not caught"),
                String::from("The scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            String::from("LLI not found."),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_asm_function_by_id(
        &self,
        span: Span,
        id: &'parser str,
    ) -> Result<AssemblerFunction<'parser>, CompilationIssue> {
        if let Some(asm_function) = self.asm_functions.get(id).cloned() {
            Ok(asm_function)
        } else {
            Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("Assembler function '{}' not found in this scope.", id),
                "You should either create it or reference it correctly.".into(),
                None,
                span,
            ))
        }
    }

    #[inline]
    pub fn get_function_by_id(
        &self,
        span: Span,
        id: &'parser str,
    ) -> Result<Function<'parser>, CompilationIssue> {
        if let Some(function) = self.functions.get(id).cloned() {
            Ok(function)
        } else {
            Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("Function '{}' not found in this scope.", id),
                "You should either create it or reference it correctly.".into(),
                None,
                span,
            ))
        }
    }

    #[inline]
    pub fn get_intrinsic_by_id(
        &self,
        span: Span,
        id: &'parser str,
    ) -> Result<Intrinsic<'parser>, CompilationIssue> {
        if let Some(intrinsic) = self.intrinsics.get(id).cloned() {
            Ok(intrinsic)
        } else {
            Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("Compiler intrinsic '{}' not found in this scope.", id),
                "You should either create it or reference it correctly.".into(),
                None,
                span,
            ))
        }
    }

    #[inline]
    pub fn get_enum_by_id(
        &self,
        id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<EnumSymbol<'parser>, CompilationIssue> {
        if scope_idx == 0 {
            if let Some(lenum) = self.global_enums.get(id).cloned() {
                return Ok(lenum);
            }
        }

        if let Some(scope) = self.local_enums.get(scope_idx) {
            if let Some(lenum) = scope.get(id).cloned() {
                return Ok(lenum);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Last scope not caught"),
                String::from("The last scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Enum '{}' not found.", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_custom_type_by_id(
        &self,
        id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<CustomTypeSymbol<'parser>, CompilationIssue> {
        if scope_idx == 0 {
            if let Some(ctype) = self.global_custom_types.get(id).cloned() {
                return Ok(ctype);
            }
        }

        if let Some(scope) = self.local_custom_types.get(scope_idx) {
            if let Some(ctype) = scope.get(id).cloned() {
                return Ok(ctype);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Last scope not caught"),
                String::from("The last scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Type '{}' not found in this scope.", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_local_by_id(
        &self,
        local_id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<&LocalSymbol<'parser>, CompilationIssue> {
        if let Some(scope) = self.locals.get(scope_idx) {
            if let Some(local) = scope.get(local_id) {
                return Ok(local);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Scope not caught"),
                String::from("The scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Variable '{}' not found in this scope.", local_id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_static_by_id(
        &self,
        id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<StaticSymbol<'parser>, CompilationIssue> {
        if scope_idx == 0 {
            if let Some(static_var) = self.global_statics.get(id).cloned() {
                return Ok(static_var);
            }
        }

        if let Some(scope) = self.local_statics.get(scope_idx) {
            if let Some(static_var) = scope.get(id).cloned() {
                return Ok(static_var);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Last scope not caught"),
                String::from("The last scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Static '{}' not found in this scope.", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_const_by_id(
        &self,
        id: &'parser str,
        scope_idx: usize,
        span: Span,
    ) -> Result<ConstantSymbol<'parser>, CompilationIssue> {
        if scope_idx == 0 {
            if let Some(constant) = self.global_constants.get(id).cloned() {
                return Ok(constant);
            }
        }

        if let Some(scope) = self.local_constants.get(scope_idx) {
            if let Some(local_const) = scope.get(id).cloned() {
                return Ok(local_const);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Last scope not caught"),
                String::from("The last scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Constant '{}' not found in this scope.", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_parameter_by_id(
        &self,
        parameter_id: &'parser str,
        span: Span,
    ) -> Result<ParameterSymbol<'parser>, CompilationIssue> {
        if let Some(parameter) = self.parameters.get(parameter_id).cloned() {
            Ok(parameter)
        } else {
            Err(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("Parameter '{}' not found in this scope.", parameter_id),
                "You should either create it or reference it correctly.".into(),
                None,
                span,
            ))
        }
    }

    #[inline]
    pub fn get_struct_by_id(
        &self,
        id: &str,
        scope_idx: usize,
        span: Span,
    ) -> Result<Struct<'parser>, CompilationIssue> {
        if scope_idx == 0 {
            if let Some(structure) = self.global_structs.get(id).cloned() {
                return Ok(structure);
            }
        }

        if let Some(scope) = self.local_structs.get(scope_idx) {
            if let Some(local_struct) = scope.get(id).cloned() {
                return Ok(local_struct);
            }
        } else {
            return Err(CompilationIssue::FrontendBug(
                String::from("Last scope not caught"),
                String::from("The last scope could not be obtained."),
                span,
                CompilationPosition::Parser,
                std::path::PathBuf::from(file!()),
                line!(),
            ));
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("'{}' structure not found in this scope.", id),
            "You should either create it or reference it correctly.".into(),
            None,
            span,
        ))
    }
}
