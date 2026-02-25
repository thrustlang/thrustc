use thrustc_ast::{
    Ast,
    metadata::{
        FunctionParameterMetadata, LocalMetadata, ReferenceMetadata, ReferenceType, StaticMetadata,
    },
};
use thrustc_entities::parser::{
    ConstantSymbol, FoundSymbolId, Function, LLISymbol, LocalSymbol, ParameterSymbol, StaticSymbol,
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_span::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
    traits::TypeIsExtensions,
};

use crate::{
    ParserContext,
    traits::{
        ConstantSymbolExtensions, FoundSymbolEitherExtensions, FoundSymbolExtensions,
        FunctionExtensions, FunctionParameterSymbolExtensions, LLISymbolExtensions,
        LocalSymbolExtensions, StaticSymbolExtensions,
    },
};

pub fn build_reference<'parser>(
    ctx: &mut ParserContext<'parser>,
    name: &'parser str,
    span: Span,
) -> Result<Ast<'parser>, CompilationIssue> {
    let reference: Result<FoundSymbolId, CompilationIssue> =
        ctx.get_symbols().get_symbols_id(name, span);

    match reference {
        Ok(object) => {
            if object.is_function() {
                let id: &str = object.expected_function(span)?;

                let reference: Result<Function, CompilationIssue> =
                    ctx.get_symbols().get_function_by_id(span, id);

                match reference {
                    Ok(object) => {
                        let function_type: Type = object.get_type();
                        let function_parameter_types: Vec<Type> = object.1.0;
                        let has_ignore_attr: bool = object.2;

                        return Ok(Ast::Reference {
                            name,
                            kind: Type::Fn(
                                function_parameter_types,
                                function_type.into(),
                                FunctionReferenceTypeModificator::new(
                                    LLVMFunctionReferenceTypeModificator::new(has_ignore_attr),
                                    GCCFunctionReferenceTypeModificator::new(),
                                ),
                                span,
                            ),
                            span,
                            metadata: ReferenceMetadata::new(true, false, ReferenceType::default()),
                        });
                    }
                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            if object.is_static() {
                let static_var: (&str, usize) = object.expected_static(span)?;

                let static_id: &str = static_var.0;
                let scope_idx: usize = static_var.1;

                let reference: Result<StaticSymbol, CompilationIssue> = ctx
                    .get_symbols()
                    .get_static_by_id(static_id, scope_idx, span);

                match reference {
                    Ok(object) => {
                        let static_type: Type = object.get_type();
                        let metadata: StaticMetadata = object.get_metadata();

                        let is_mutable: bool = metadata.is_mutable();

                        return Ok(Ast::Reference {
                            name,
                            kind: static_type,
                            span,
                            metadata: ReferenceMetadata::new(
                                true,
                                is_mutable,
                                ReferenceType::Static,
                            ),
                        });
                    }

                    Err(error) => {
                        ctx.add_error(error);

                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            if object.is_constant() {
                let constant: (&str, usize) = object.expected_constant(span)?;

                let const_id: &str = constant.0;
                let scope_idx: usize = constant.1;

                let reference: Result<ConstantSymbol, CompilationIssue> =
                    ctx.get_symbols().get_const_by_id(const_id, scope_idx, span);

                match reference {
                    Ok(object) => {
                        let constant_type: Type = object.get_type();

                        return Ok(Ast::Reference {
                            name,
                            kind: constant_type,
                            span,
                            metadata: ReferenceMetadata::new(true, false, ReferenceType::Constant),
                        });
                    }

                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            if object.is_parameter() {
                let parameter_id: &str = object.expected_parameter(span)?;

                let reference: Result<ParameterSymbol, CompilationIssue> =
                    ctx.get_symbols().get_parameter_by_id(parameter_id, span);

                match reference {
                    Ok(object) => {
                        let metadata: FunctionParameterMetadata = object.get_metadata();
                        let parameter_type: Type = object.get_type();

                        let is_mutable: bool = metadata.is_mutable();
                        let is_allocated: bool = parameter_type.is_ptr_like_type();

                        return Ok(Ast::Reference {
                            name,
                            kind: parameter_type,
                            span,
                            metadata: ReferenceMetadata::new(
                                is_allocated,
                                is_mutable,
                                ReferenceType::default(),
                            ),
                        });
                    }

                    Err(error) => {
                        ctx.add_error(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            if object.is_lli() {
                let lli: (&str, usize) = object.expected_lli(span)?;

                let lli_id: &str = lli.0;
                let scope_idx: usize = lli.1;

                let parameter: &LLISymbol =
                    ctx.get_symbols().get_lli_by_id(lli_id, scope_idx, span)?;
                let lli_type: Type = parameter.get_type();

                let is_allocated: bool = lli_type.is_ptr_type() || lli_type.is_address_type();

                return Ok(Ast::Reference {
                    name,
                    kind: lli_type,
                    span,
                    metadata: ReferenceMetadata::new(is_allocated, false, ReferenceType::default()),
                });
            }

            if object.is_local() {
                let local_position: (&str, usize) = object.expected_local(span)?;
                let local_id: &str = local_position.0;
                let scope_idx: usize = local_position.1;

                let reference: Result<&LocalSymbol, CompilationIssue> =
                    ctx.get_symbols().get_local_by_id(local_id, scope_idx, span);

                match reference {
                    Ok(object) => {
                        let metadata: LocalMetadata = object.get_metadata();
                        let local_type: Type = object.get_type();

                        let is_mutable: bool = metadata.is_mutable();

                        let reference: Ast = Ast::Reference {
                            name,
                            kind: local_type.clone(),
                            span,
                            metadata: ReferenceMetadata::new(
                                true,
                                is_mutable,
                                ReferenceType::default(),
                            ),
                        };

                        if ctx.match_token(TokenType::PlusPlus)?
                            || ctx.match_token(TokenType::MinusMinus)?
                        {
                            let operator_tk: &Token = ctx.previous();
                            let operator: TokenType = operator_tk.get_type();
                            let span: Span = operator_tk.get_span();

                            let unaryop: Ast = Ast::UnaryOp {
                                operator,
                                expression: reference.into(),
                                kind: local_type,
                                is_pre: false,
                                span,
                            };

                            return Ok(unaryop);
                        }

                        return Ok(reference);
                    }

                    Err(error) => {
                        ctx.add_error(error);

                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            ctx.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}' isn't declared or defined.", name),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }

        Err(error) => {
            ctx.add_error(error);

            Ok(Ast::invalid_ast(span))
        }
    }
}
