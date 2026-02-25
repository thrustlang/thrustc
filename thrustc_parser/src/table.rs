use thrustc_ast::Ast;
use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationIssueCode, CompilationPosition};
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_span::Span;

use thrustc_entities::parser::{
    AssemblerFunction, AssemblerFunctions, ConstantSymbol, CustomTypeSymbol, EnumSymbol,
    FoundSymbolId, Function, Functions, GlobalConstants, GlobalCustomTypes, GlobalEnums,
    GlobalStatics, GlobalStructs, Intrinsic, Intrinsics, LLISymbol, LLIs, LocalConstants,
    LocalCustomTypes, LocalEnums, LocalStatics, LocalStructs, LocalSymbol, Locals, ParameterSymbol,
    Parameters, StaticSymbol, Struct,
};

pub const PREALLOCATED_GLOBAL_TABLE_CAPACITY: usize = 1000;
pub const PREALLOCATED_LOCAL_TABLE_CAPACITY: usize = 255;

#[derive(Clone, Debug, Default)]
pub struct SymbolsTable<'parser> {
    functions: Functions<'parser>,
    asm_functions: AssemblerFunctions<'parser>,
    intrinsics: Intrinsics<'parser>,

    global_custom_types: GlobalCustomTypes<'parser>,
    global_statics: GlobalStatics<'parser>,
    global_structs: GlobalStructs<'parser>,
    global_constants: GlobalConstants<'parser>,
    global_enums: GlobalEnums<'parser>,

    local_structs: LocalStructs<'parser>,
    local_statics: LocalStatics<'parser>,
    local_constants: LocalConstants<'parser>,
    local_custom_types: LocalCustomTypes<'parser>,
    local_enums: LocalEnums<'parser>,

    locals: Locals<'parser>,
    llis: LLIs<'parser>,
    parameters: Parameters<'parser>,

    diagnostician: Diagnostician,
}

impl<'parser> SymbolsTable<'parser> {
    pub fn with_functions(
        functions: Functions<'parser>,
        asm_functions: AssemblerFunctions<'parser>,
        options: &CompilerOptions,
        file: &CompilationUnit,
    ) -> Self {
        Self {
            functions,
            asm_functions,

            intrinsics: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),

            global_structs: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            global_statics: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            global_constants: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            global_custom_types: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            global_enums: ahash::AHashMap::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),

            local_structs: Vec::with_capacity(PREALLOCATED_LOCAL_TABLE_CAPACITY),
            local_statics: Vec::with_capacity(PREALLOCATED_LOCAL_TABLE_CAPACITY),
            local_constants: Vec::with_capacity(PREALLOCATED_LOCAL_TABLE_CAPACITY),
            local_custom_types: Vec::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            local_enums: Vec::with_capacity(PREALLOCATED_GLOBAL_TABLE_CAPACITY),
            locals: Vec::with_capacity(PREALLOCATED_LOCAL_TABLE_CAPACITY),
            llis: Vec::with_capacity(PREALLOCATED_LOCAL_TABLE_CAPACITY),

            parameters: ahash::AHashMap::with_capacity(10),
            diagnostician: Diagnostician::new(file, options),
        }
    }
}

impl SymbolsTable<'_> {
    #[inline]
    pub fn begin_scope(&mut self) {
        self.local_structs.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
        self.local_custom_types.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
        self.local_statics.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
        self.local_constants.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
        self.local_enums.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));

        self.locals.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
        self.llis.push(ahash::AHashMap::with_capacity(
            PREALLOCATED_LOCAL_TABLE_CAPACITY,
        ));
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

impl<'parser> SymbolsTable<'parser> {
    pub fn new_parameters(&mut self, parameters: &[Ast<'parser>]) -> Result<(), CompilationIssue> {
        {
            for node in parameters.iter() {
                if let Ast::FunctionParameter {
                    name: id,
                    kind,
                    span,
                    metadata,
                    ..
                } = node
                {
                    if self.parameters.contains_key(id) {
                        return Err(CompilationIssue::Error(
                            CompilationIssueCode::E0004,
                            format!("'{}' parameter was already declared before.", id),
                            None,
                            *span,
                        ));
                    }

                    self.parameters.insert(id, (kind.clone(), *metadata, *span));
                }
            }
        }

        Ok(())
    }
}

impl<'parser> SymbolsTable<'parser> {
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
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.global_statics.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("Static '{}' was already declared before.", id),
                None,
                span,
            ));
        }

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
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.global_constants.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("Constant '{}' was already declared before.", id),
                None,
                span,
            ));
        }

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
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.global_custom_types.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("Type '{}' was already declared before.", id),
                None,
                span,
            ));
        }

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
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.global_structs.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("Structure '{}' was already declared before.", id),
                None,
                span,
            ));
        }

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
            if last_scope.contains_key(id) {
                return Err(CompilationIssue::Error(
                    CompilationIssueCode::E0004,
                    format!("Structure '{}' was already declared before.", id),
                    None,
                    span,
                ));
            }

            last_scope.insert(id, fields);

            Ok(())
        } else {
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.global_enums.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("Enum '{}' was already declared before.", id),
                None,
                span,
            ));
        }

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
                    format!("Enum '{}' was already declared before.", id),
                    None,
                    span,
                ));
            }

            last_scope.insert(id, union);

            Ok(())
        } else {
            thrustc_frontend_abort::abort_compilation(
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
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.asm_functions.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("'{}' assembler function was already declared before.", id),
                None,
                span,
            ));
        }

        self.asm_functions.insert(id, function);

        Ok(())
    }

    pub fn new_function(
        &mut self,
        id: &'parser str,
        function: Function<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.functions.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("'{}' function was already declared before.", id),
                None,
                span,
            ));
        }

        self.functions.insert(id, function);

        Ok(())
    }

    pub fn new_intrinsic(
        &mut self,
        id: &'parser str,
        intrinsic: Intrinsic<'parser>,
        span: Span,
    ) -> Result<(), CompilationIssue> {
        if self.intrinsics.contains_key(id) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0004,
                format!("'{}' intrinsic was already declared before.", id),
                None,
                span,
            ));
        }

        self.intrinsics.insert(id, intrinsic);

        Ok(())
    }
}

impl<'parser> SymbolsTable<'parser> {
    pub fn get_symbols_id(
        &self,
        id: &'parser str,
        span: Span,
    ) -> Result<FoundSymbolId<'parser>, CompilationIssue> {
        for (idx, scope) in self.llis.iter().enumerate().rev() {
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
                    Some((id, idx)),
                    None,
                    None,
                ));
            }
        }

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
            format!("'{}' isn't declared or defined.", id),
            None,
            span,
        ))
    }
}

impl<'parser> SymbolsTable<'parser> {
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
            return Err(CompilationIssue::FrontEndBug(
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
            return Ok(asm_function);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Assembler Function '{}' not found in this scope.", id),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_function_by_id(
        &self,
        span: Span,
        id: &'parser str,
    ) -> Result<Function<'parser>, CompilationIssue> {
        if let Some(function) = self.functions.get(id).cloned() {
            return Ok(function);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Function '{}' not found in this scope.", id),
            None,
            span,
        ))
    }

    #[inline]
    pub fn get_intrinsic_by_id(
        &self,
        span: Span,
        id: &'parser str,
    ) -> Result<Intrinsic<'parser>, CompilationIssue> {
        if let Some(intrinsic) = self.intrinsics.get(id).cloned() {
            return Ok(intrinsic);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Compiler Intrinsic '{}' not found in this scope.", id),
            None,
            span,
        ))
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
            return Err(CompilationIssue::FrontEndBug(
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
            "Enum reference not found.".into(),
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
            return Err(CompilationIssue::FrontEndBug(
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
            "Custom type reference not found.".into(),
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
            return Err(CompilationIssue::FrontEndBug(
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
            "Local not found.".into(),
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
            return Err(CompilationIssue::FrontEndBug(
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
            "Static reference not found.".into(),
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
            return Err(CompilationIssue::FrontEndBug(
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
            "Constant reference not found.".into(),
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
            return Ok(parameter);
        }

        Err(CompilationIssue::Error(
            CompilationIssueCode::E0028,
            format!("Parameter '{}' not found in this scope.", parameter_id),
            None,
            span,
        ))
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
            return Err(CompilationIssue::FrontEndBug(
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
            format!("'{}' structure not defined.", id),
            None,
            span,
        ))
    }
}
