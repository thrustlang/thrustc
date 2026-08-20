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
    Ast, NodeId,
    ast_metadata::{
        FunctionParameterMetadata, LocalMetadata, ReferenceMetadata, ReferenceType, StaticMetadata,
    },
};
use thrustc_entities::parser_entities::{
    ConstantSymbol, FoundSymbolId, Function, LLISymbol, LocalSymbol, ParameterSymbol, StaticSymbol,
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_code_location::Span;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{TypeIsExtensions, TypePointerExtensions},
    type_modificators::{
        FunctionReferenceTypeModificator, GCCFunctionReferenceTypeModificator,
        LLVMFunctionReferenceTypeModificator,
    },
};

use thrustc_parser_table::traits::{
    ConstantSymbolExtensions, FoundSymbolEitherExtensions, FoundSymbolExtensions,
    FunctionExtensions, FunctionParameterSymbolExtensions, LLISymbolExtensions,
    LocalSymbolExtensions, StaticSymbolExtensions,
};

use crate::ParserContext;

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
                        let return_type: Type = object.get_type();
                        let parameter_types: Vec<Type> = object.1.0;

                        let has_ignore: bool = object.3;

                        let modificator: FunctionReferenceTypeModificator =
                            FunctionReferenceTypeModificator::new(
                                LLVMFunctionReferenceTypeModificator::new(has_ignore),
                                GCCFunctionReferenceTypeModificator::default(),
                            );

                        let function_ty: Type = Type::Fn {
                            return_type: return_type.into(),
                            parameter_types,
                            modificator,
                            span,
                        };

                        return Ok(Ast::Reference {
                            name,
                            kind: function_ty,
                            span,
                            metadata: ReferenceMetadata::new(
                                true,
                                false,
                                ReferenceType::default(),
                                false,
                            ),
                            id: NodeId::new(),
                        });
                    }
                    Err(error) => {
                        ctx.add_error_report(error);
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
                        let is_unitialized: bool = metadata.is_unitialized();

                        return Ok(Ast::Reference {
                            name,
                            kind: static_type,
                            span,
                            metadata: ReferenceMetadata::new(
                                true,
                                is_mutable,
                                ReferenceType::Static,
                                is_unitialized,
                            ),
                            id: NodeId::new(),
                        });
                    }

                    Err(error) => {
                        ctx.add_error_report(error);
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
                            metadata: ReferenceMetadata::new(
                                true,
                                false,
                                ReferenceType::Constant,
                                false,
                            ),
                            id: NodeId::new(),
                        });
                    }

                    Err(error) => {
                        ctx.add_error_report(error);
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
                                ReferenceType::Parameter,
                                false,
                            ),
                            id: NodeId::new(),
                        });
                    }

                    Err(error) => {
                        ctx.add_error_report(error);
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

                let is_allocated: bool = lli_type.is_ptr_type();

                return Ok(Ast::Reference {
                    name,
                    kind: lli_type,
                    span,
                    metadata: ReferenceMetadata::new(
                        is_allocated,
                        false,
                        ReferenceType::default(),
                        false,
                    ),
                    id: NodeId::new(),
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
                        let is_unitialized: bool = metadata.is_unitialized();

                        let reference: Ast = Ast::Reference {
                            name,
                            kind: local_type.clone(),
                            span,
                            metadata: ReferenceMetadata::new(
                                true,
                                is_mutable,
                                ReferenceType::Local,
                                is_unitialized,
                            ),
                            id: NodeId::new(),
                        };

                        if ctx.match_token(TokenType::PlusPlus)?
                            || ctx.match_token(TokenType::MinusMinus)?
                        {
                            let operator_tk: &Token = ctx.previous();
                            let operator: TokenType = operator_tk.get_type();
                            let span: Span = operator_tk.get_span();

                            let unaryop: Ast = Ast::UnaryOp {
                                operator,
                                node: reference.into(),
                                kind: local_type,
                                before: false,
                                span,
                                id: NodeId::new(),
                            };

                            return Ok(unaryop);
                        }

                        return Ok(reference);
                    }

                    Err(error) => {
                        ctx.add_error_report(error);
                        return Ok(Ast::invalid_ast(span));
                    }
                }
            }

            ctx.add_error_report(CompilationIssue::Error(
                CompilationIssueCode::E0028,
                format!("'{}' not found.", name),
                "You should make sure that it exist at this scope.".into(),
                None,
                span,
            ));

            Ok(Ast::invalid_ast(span))
        }

        Err(error) => {
            ctx.add_error_report(error);
            Ok(Ast::invalid_ast(span))
        }
    }
}
