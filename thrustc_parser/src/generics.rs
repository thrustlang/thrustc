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

use std::collections::{HashMap, HashSet};

use thrustc_ast::{
    Ast, ModuleExpressionValues, NodeId,
    ast_builtins::AstBuiltin,
    ast_logic_data::{ConstructorData, EnumData},
    ast_metadata::FunctionParameterMetadata,
    traits::AstGetType,
};
use thrustc_attributes::{ThrustAttribute, ThrustAttributes};
use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_parser_table::GenericFunctionEntry;
use thrustc_token::{Token, traits::TokenExtensions};
use thrustc_token_type::TokenType;
use thrustc_typesystem::{
    Type,
    traits::{TypeCodeLocation, TypePointerExtensions},
};

use crate::ParserContext;

pub fn parse_type_parameters<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> Result<Vec<String>, CompilationIssue> {
    if !ctx.check(TokenType::LBracket) {
        return Ok(Vec::with_capacity(0));
    }

    ctx.consume(
        TokenType::LBracket,
        CompilationIssueCode::E0001,
        "Expected '['.".into(),
    )?;

    let mut parameters: Vec<String> = Vec::with_capacity(4);

    loop {
        if ctx.check(TokenType::RBracket) {
            break;
        }

        let parameter_tk: &Token = ctx.consume(
            TokenType::Identifier,
            CompilationIssueCode::E0001,
            "Expected identifier.".into(),
        )?;

        let name: String = parameter_tk.get_lexeme().to_string();
        let span: Span = parameter_tk.get_span();

        if parameters.contains(&name) {
            return Err(CompilationIssue::Error(
                CompilationIssueCode::E0055,
                format!("Type parameter '{}' is already declared.", name),
                "Use a distinct name for each generic parameter.".into(),
                None,
                span,
            ));
        }

        ctx.get_mut_symbols()
            .push_type_parameter(name.clone(), span);

        parameters.push(name);

        if ctx.check(TokenType::RBracket) {
            break;
        }

        ctx.consume(
            TokenType::Comma,
            CompilationIssueCode::E0001,
            "Expected ','.".into(),
        )?;
    }

    ctx.consume(
        TokenType::RBracket,
        CompilationIssueCode::E0001,
        "Expected ']'.".into(),
    )?;

    Ok(parameters)
}

pub fn resolve_generics<'parser>(ctx: &mut ParserContext<'parser>) {
    if !ctx.get_symbols().has_any_generic() {
        return;
    }

    let templates: HashMap<String, Ast<'parser>> = self::collect_local_templates(ctx);

    let mut memo: HashSet<String> = HashSet::new();

    let existing: Vec<Ast<'parser>> = std::mem::take(ctx.get_mut_ast());

    let mut output: Vec<Ast<'parser>> = Vec::with_capacity(existing.len() + 8);

    for node in existing {
        let is_generic_template: bool = match &node {
            Ast::Function { name, .. } => ctx.get_symbols().has_generic_function(name),
            Ast::Struct { name, .. } => ctx.get_symbols().has_generic_struct(name),
            Ast::CustomType { name, .. } => ctx.get_symbols().has_generic_custom_type(name),
            _ => false,
        };

        if is_generic_template {
            continue;
        }

        let resolved: Ast<'parser> =
            self::resolve_ast(ctx, node, &templates, &mut memo, &mut output);

        output.push(resolved);
    }

    let file_path: std::path::PathBuf = ctx.get_file().get_path().to_path_buf();

    let pending: Vec<thrustc_generics::PendingInstantiation> =
        thrustc_generics::drain_pending(&file_path);

    for pending in pending {
        let Some(entry) = ctx
            .get_symbols()
            .get_generic_function(&pending.function)
            .cloned()
        else {
            continue;
        };

        if !entry.has_local_template {
            continue;
        }

        let module_str: String = pending.module.to_string_lossy().to_string();

        let key: String =
            thrustc_generics::instantiation_key(Some(&module_str), &entry.name, &pending.env);

        if !memo.insert(key.clone()) {
            continue;
        }

        let Some(template) = templates.get(&entry.name) else {
            continue;
        };

        let mut concrete: Ast<'parser> =
            thrustc_generics::substitute_ast(template.clone(), &pending.env);

        if let Ast::Function {
            name, ascii_name, ..
        } = &mut concrete
        {
            *name = key.clone();
            *ascii_name = key.clone();
        }

        if let Ast::Function { original_name, .. } = &mut concrete {
            *original_name = Some(entry.name.clone());
        }

        let resolved: Ast<'parser> =
            self::resolve_ast(ctx, concrete, &templates, &mut memo, &mut output);

        output.push(resolved);
    }

    self::emit_unused_type_parameter_warnings(ctx, &templates);

    *ctx.get_mut_ast() = output;
}

fn emit_unused_type_parameter_warnings<'parser>(
    ctx: &mut ParserContext<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
) {
    let mut warnings: Vec<CompilationIssue> = Vec::new();
    let mut hints: std::collections::HashSet<String> = std::collections::HashSet::new();

    for (_id, entry) in ctx.get_symbols().iter_generic_functions() {
        hints.clear();

        for ty in entry.parameter_types.iter() {
            thrustc_generics::collect_unresolved_type_hints(ty, &mut hints);
        }

        thrustc_generics::collect_unresolved_type_hints(&entry.return_type, &mut hints);

        if let Some(template) = templates.get(&entry.name) {
            thrustc_generics::collect_unresolved_hints(template, &mut hints);
        }

        for parameter in entry.type_params.iter() {
            if !hints.contains(parameter) {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0032,
                    format!("Type parameter '{}' is never used.", parameter),
                    entry.span,
                ));
            }
        }
    }

    for (_id, entry) in ctx.get_symbols().iter_generic_structs() {
        hints.clear();

        for ty in entry.field_types.iter() {
            thrustc_generics::collect_unresolved_type_hints(ty, &mut hints);
        }

        for parameter in entry.type_params.iter() {
            if !hints.contains(parameter) {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0032,
                    format!("Type parameter '{}' is never used.", parameter),
                    entry.span,
                ));
            }
        }
    }

    for (_id, entry) in ctx.get_symbols().iter_generic_custom_types() {
        hints.clear();

        thrustc_generics::collect_unresolved_type_hints(&entry.kind, &mut hints);

        for parameter in entry.type_params.iter() {
            if !hints.contains(parameter) {
                warnings.push(CompilationIssue::Warning(
                    CompilationIssueCode::W0032,
                    format!("Type parameter '{}' is never used.", parameter),
                    entry.kind.get_span(),
                ));
            }
        }
    }

    for warning in warnings {
        ctx.add_warning_report(warning);
    }
}

fn resolve_ast<'parser>(
    ctx: &mut ParserContext<'parser>,
    node: Ast<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> Ast<'parser> {
    match node {
        Ast::Call {
            name,
            args,
            generic_args,
            kind,
            span,
            id,
        } if !generic_args.is_empty() || ctx.get_symbols().has_generic_function(&name) => {
            let Some(entry) = ctx.get_symbols().get_generic_function(&name).cloned() else {
                return Ast::Call {
                    name,
                    args: self::resolve_ast_list(ctx, args, templates, memo, output),
                    generic_args,
                    kind,
                    span,
                    id,
                };
            };

            let argument_types: Vec<Type> = args
                .iter()
                .map(|argument| match argument.get_value_type() {
                    Ok(ty) => ty.clone(),
                    Err(_) => Type::Void { span },
                })
                .collect();

            let result: Result<thrustc_generics::SolveResult, CompilationIssue> =
                thrustc_generics::solve(
                    &entry.type_params,
                    &generic_args,
                    &entry.parameter_types,
                    &argument_types,
                    &entry.return_type,
                    entry.has_varargs,
                    span,
                );

            match result {
                Ok(result) => {
                    let origin: Option<String> = ctx
                        .get_symbols()
                        .get_import_origin(&name)
                        .map(|path| path.to_string_lossy().to_string());

                    let key: String = thrustc_generics::instantiation_key(
                        origin.as_deref(),
                        &entry.name,
                        &result.env,
                    );

                    self::ensure_instantiation(
                        ctx,
                        &entry,
                        &result.env,
                        &key,
                        templates,
                        memo,
                        output,
                    );

                    Ast::Call {
                        name: key,
                        args: self::resolve_ast_list(ctx, args, templates, memo, output),
                        generic_args: Vec::with_capacity(0),
                        kind: result.return_type,
                        span,
                        id,
                    }
                }
                Err(_) => Ast::Call {
                    name,
                    args: self::resolve_ast_list(ctx, args, templates, memo, output),
                    generic_args: Vec::with_capacity(0),
                    kind,
                    span,
                    id,
                },
            }
        }

        other => self::resolve_children(ctx, other, templates, memo, output),
    }
}

fn ensure_instantiation<'parser>(
    ctx: &mut ParserContext<'parser>,
    entry: &GenericFunctionEntry,
    env: &thrustc_generics::TypeEnv,
    key: &str,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) {
    if !memo.insert(key.to_string()) {
        return;
    }

    if entry.has_local_template {
        let Some(template) = templates.get(&entry.name) else {
            return;
        };

        let mut concrete: Ast<'parser> = thrustc_generics::substitute_ast(template.clone(), env);

        if let Ast::Function {
            name, ascii_name, ..
        } = &mut concrete
        {
            *name = key.to_string();
            *ascii_name = key.to_string();
        }

        if let Ast::Function { original_name, .. } = &mut concrete {
            *original_name = Some(entry.name.clone());
        }

        let resolved: Ast<'parser> = self::resolve_ast(ctx, concrete, templates, memo, output);

        output.push(resolved);

        return;
    }

    let return_type: Type = thrustc_generics::substitute(&entry.return_type, env);

    let parameter_types: Vec<Type> = entry
        .parameter_types
        .iter()
        .map(|parameter| thrustc_generics::substitute(parameter, env))
        .collect();

    let parameters: Vec<Ast<'parser>> = entry
        .parameter_names
        .iter()
        .zip(parameter_types.iter())
        .enumerate()
        .map(
            |(position, (parameter_name, parameter_type))| Ast::FunctionParameter {
                name: parameter_name.clone(),
                ascii_name: parameter_name.clone(),
                kind: parameter_type.clone(),
                position: position as u32,
                metadata: FunctionParameterMetadata::new(parameter_type.is_ptr_like_type()),
                span: entry.span,
                id: NodeId::new(),
            },
        )
        .collect();

    let mut attributes: ThrustAttributes = entry
        .attributes
        .iter()
        .filter(|attribute| {
            !matches!(
                attribute,
                ThrustAttribute::Public(_) | ThrustAttribute::Extern(..)
            )
        })
        .cloned()
        .collect();

    attributes.push(ThrustAttribute::Public(entry.span));
    attributes.push(ThrustAttribute::Extern(key.to_string(), entry.span));

    output.push(Ast::Function {
        name: key.to_string(),
        ascii_name: key.to_string(),
        original_name: Some(entry.name.clone()),
        parameters,
        parameter_types,
        body: None,
        return_type,
        attributes,
        span: entry.span,
        id: NodeId::new(),
    });

    if let Some(origin) = ctx
        .get_symbols()
        .get_import_origin(&entry.name)
        .map(|path| path.to_path_buf())
    {
        thrustc_generics::record_pending(origin, entry.name.clone(), env.clone());
    }
}

fn resolve_children<'parser>(
    ctx: &mut ParserContext<'parser>,
    node: Ast<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> Ast<'parser> {
    match node {
        Ast::Call {
            name,
            args,
            generic_args,
            kind,
            span,
            id,
        } => Ast::Call {
            name,
            args: self::resolve_ast_list(ctx, args, templates, memo, output),
            generic_args,
            kind,
            span,
            id,
        },
        Ast::CString {
            bytes,
            kind,
            span,
            id,
        } => Ast::CString {
            bytes,
            kind,
            span,
            id,
        },
        Ast::CNString {
            bytes,
            kind,
            span,
            id,
        } => Ast::CNString {
            bytes,
            kind,
            span,
            id,
        },
        Ast::Char {
            kind,
            byte,
            span,
            id,
        } => Ast::Char {
            kind,
            byte,
            span,
            id,
        },
        Ast::Boolean {
            kind,
            value,
            span,
            id,
        } => Ast::Boolean {
            kind,
            value,
            span,
            id,
        },
        Ast::Integer {
            kind,
            value,
            span,
            id,
        } => Ast::Integer {
            kind,
            value,
            span,
            id,
        },
        Ast::Float {
            kind,
            value,
            span,
            id,
        } => Ast::Float {
            kind,
            value,
            span,
            id,
        },
        Ast::NullPtr { span, kind } => Ast::NullPtr { span, kind },
        Ast::GlobalAssembler {
            asm,
            span,
            kind,
            id,
        } => Ast::GlobalAssembler {
            asm,
            span,
            kind,
            id,
        },
        Ast::FixedArray {
            items,
            kind,
            span,
            id,
        } => Ast::FixedArray {
            items: self::resolve_ast_list(ctx, items, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Array {
            items,
            kind,
            span,
            id,
        } => Ast::Array {
            items: self::resolve_ast_list(ctx, items, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Index {
            source,
            index,
            metadata,
            kind,
            span,
        } => Ast::Index {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            index: std::boxed::Box::new(self::resolve_ast(ctx, *index, templates, memo, output)),
            metadata,
            kind,
            span,
        },
        Ast::Embedded {
            name,
            path,
            literal,
            kind,
            span,
            id,
        } => Ast::Embedded {
            name,
            path,
            literal,
            kind,
            span,
            id,
        },
        Ast::Struct {
            name,
            data,
            kind,
            span,
            attributes,
            id,
        } => Ast::Struct {
            name,
            data,
            kind,
            span,
            attributes,
            id,
        },
        Ast::Constructor {
            name,
            data,
            kind,
            span,
            id,
        } => Ast::Constructor {
            name,
            data: self::resolve_constructor_data(ctx, data, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Property {
            source,
            data,
            metadata,
            kind,
            span,
            id,
        } => Ast::Property {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            data,
            metadata,
            kind,
            span,
            id,
        },
        Ast::If {
            condition,
            then_branch,
            else_if_branch,
            else_branch,
            kind,
            span,
            id,
        } => Ast::If {
            condition: std::boxed::Box::new(self::resolve_ast(
                ctx, *condition, templates, memo, output,
            )),
            then_branch: std::boxed::Box::new(self::resolve_ast(
                ctx,
                *then_branch,
                templates,
                memo,
                output,
            )),
            else_if_branch: self::resolve_ast_list(ctx, else_if_branch, templates, memo, output),
            else_branch: else_branch.map(|branch| {
                std::boxed::Box::new(self::resolve_ast(ctx, *branch, templates, memo, output))
            }),
            kind,
            span,
            id,
        },
        Ast::Elif {
            condition,
            block,
            kind,
            span,
            id,
        } => Ast::Elif {
            condition: std::boxed::Box::new(self::resolve_ast(
                ctx, *condition, templates, memo, output,
            )),
            block: std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Else {
            block,
            kind,
            span,
            id,
        } => Ast::Else {
            block: std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::For {
            local,
            condition,
            actions,
            block,
            kind,
            span,
            id,
        } => Ast::For {
            local: std::boxed::Box::new(self::resolve_ast(ctx, *local, templates, memo, output)),
            condition: std::boxed::Box::new(self::resolve_ast(
                ctx, *condition, templates, memo, output,
            )),
            actions: std::boxed::Box::new(self::resolve_ast(
                ctx, *actions, templates, memo, output,
            )),
            block: std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::While {
            variable,
            condition,
            block,
            kind,
            span,
            id,
        } => Ast::While {
            variable: variable.map(|local| {
                std::boxed::Box::new(self::resolve_ast(ctx, *local, templates, memo, output))
            }),
            condition: std::boxed::Box::new(self::resolve_ast(
                ctx, *condition, templates, memo, output,
            )),
            block: std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Loop {
            block,
            kind,
            span,
            id,
        } => Ast::Loop {
            block: std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Continue { kind, span, id } => Ast::Continue { kind, span, id },
        Ast::Break { kind, span, id } => Ast::Break { kind, span, id },
        Ast::ContinueAll { kind, span, id } => Ast::ContinueAll { kind, span, id },
        Ast::BreakAll { kind, span, id } => Ast::BreakAll { kind, span, id },
        Ast::Block {
            nodes,
            post,
            kind,
            span,
            id,
        } => Ast::Block {
            nodes: self::resolve_ast_list(ctx, nodes, templates, memo, output),
            post: self::resolve_ast_list(ctx, post, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Defer {
            node,
            kind,
            span,
            id,
        } => Ast::Defer {
            node: std::boxed::Box::new(self::resolve_ast(ctx, *node, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::CustomType {
            name,
            kind,
            span,
            id,
        } => Ast::CustomType {
            name,
            kind,
            span,
            id,
        },
        Ast::Enum {
            name,
            data,
            attributes,
            kind,
            span,
            id,
        } => Ast::Enum {
            name,
            data: self::resolve_enum_data(ctx, data, templates, memo, output),
            attributes,
            kind,
            span,
            id,
        },
        Ast::EnumValue {
            name,
            value,
            kind,
            span,
            id,
        } => Ast::EnumValue {
            name,
            value: std::boxed::Box::new(self::resolve_ast(ctx, *value, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::CompilerIntrinsic {
            name,
            external_name,
            parameters,
            parameters_types,
            return_type,
            attributes,
            span,
            id,
        } => Ast::CompilerIntrinsic {
            name,
            external_name,
            parameters: self::resolve_ast_list(ctx, parameters, templates, memo, output),
            parameters_types,
            return_type,
            attributes,
            span,
            id,
        },
        Ast::CompilerIntrinsicParameter { kind, span, id } => {
            Ast::CompilerIntrinsicParameter { kind, span, id }
        }
        Ast::AssemblerFunction {
            name,
            ascii_name,
            parameters,
            parameters_types,
            assembler,
            constraints,
            return_type,
            attributes,
            span,
            id,
        } => Ast::AssemblerFunction {
            name,
            ascii_name,
            parameters: self::resolve_ast_list(ctx, parameters, templates, memo, output),
            parameters_types,
            assembler,
            constraints,
            return_type,
            attributes,
            span,
            id,
        },
        Ast::AssemblerFunctionParameter {
            name,
            kind,
            position,
            span,
            id,
        } => Ast::AssemblerFunctionParameter {
            name,
            kind,
            position,
            span,
            id,
        },
        Ast::Function {
            name,
            ascii_name,
            original_name,
            parameters,
            parameter_types,
            body,
            return_type,
            attributes,
            span,
            id,
        } => Ast::Function {
            name,
            ascii_name,
            original_name,
            parameters: self::resolve_ast_list(ctx, parameters, templates, memo, output),
            parameter_types,
            body: body.map(|block| {
                std::boxed::Box::new(self::resolve_ast(ctx, *block, templates, memo, output))
            }),
            return_type,
            attributes,
            span,
            id,
        },
        Ast::FunctionParameter {
            name,
            ascii_name,
            kind,
            position,
            metadata,
            span,
            id,
        } => Ast::FunctionParameter {
            name,
            ascii_name,
            kind,
            position,
            metadata,
            span,
            id,
        },
        Ast::Return {
            expression,
            kind,
            span,
            id,
        } => Ast::Return {
            expression: expression.map(|value| {
                std::boxed::Box::new(self::resolve_ast(ctx, *value, templates, memo, output))
            }),
            kind,
            span,
            id,
        },
        Ast::Static {
            name,
            ascii_name,
            kind,
            value,
            attributes,
            modificators,
            metadata,
            span,
            id,
        } => Ast::Static {
            name,
            ascii_name,
            kind,
            value: value.map(|initializer| {
                std::boxed::Box::new(self::resolve_ast(
                    ctx,
                    *initializer,
                    templates,
                    memo,
                    output,
                ))
            }),
            attributes,
            modificators,
            metadata,
            span,
            id,
        },
        Ast::Const {
            name,
            ascii_name,
            kind,
            value,
            attributes,
            modificators,
            metadata,
            span,
            id,
        } => Ast::Const {
            name,
            ascii_name,
            kind,
            value: std::boxed::Box::new(self::resolve_ast(ctx, *value, templates, memo, output)),
            attributes,
            modificators,
            metadata,
            span,
            id,
        },
        Ast::Var {
            name,
            ascii_name,
            kind,
            value,
            attributes,
            modificators,
            metadata,
            span,
            id,
        } => Ast::Var {
            name,
            ascii_name,
            kind,
            value: value.map(|initializer| {
                std::boxed::Box::new(self::resolve_ast(
                    ctx,
                    *initializer,
                    templates,
                    memo,
                    output,
                ))
            }),
            attributes,
            modificators,
            metadata,
            span,
            id,
        },
        Ast::Reference {
            name,
            kind,
            metadata,
            span,
            id,
        } => Ast::Reference {
            name,
            kind,
            metadata,
            span,
            id,
        },
        Ast::Mutation {
            source,
            value,
            kind,
            span,
            id,
        } => Ast::Mutation {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            value: std::boxed::Box::new(self::resolve_ast(ctx, *value, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Address {
            source,
            indexes,
            kind,
            span,
            id,
        } => Ast::Address {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            indexes: self::resolve_ast_list(ctx, indexes, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Write {
            source,
            write_value,
            write_type,
            span,
            id,
        } => Ast::Write {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            write_value: std::boxed::Box::new(self::resolve_ast(
                ctx,
                *write_value,
                templates,
                memo,
                output,
            )),
            write_type,
            span,
            id,
        },
        Ast::Load {
            source,
            kind,
            span,
            id,
        } => Ast::Load {
            source: std::boxed::Box::new(self::resolve_ast(ctx, *source, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Deref {
            value,
            kind,
            modificators,
            metadata,
            span,
            id,
        } => Ast::Deref {
            value: std::boxed::Box::new(self::resolve_ast(ctx, *value, templates, memo, output)),
            kind,
            modificators,
            metadata,
            span,
            id,
        },
        Ast::As {
            from,
            cast,
            metadata,
            span,
            id,
        } => Ast::As {
            from: std::boxed::Box::new(self::resolve_ast(ctx, *from, templates, memo, output)),
            cast,
            metadata,
            span,
            id,
        },
        Ast::GetLocation {
            expr,
            kind,
            span,
            id,
        } => Ast::GetLocation {
            expr: std::boxed::Box::new(self::resolve_ast(ctx, *expr, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::ModuleExpression {
            data,
            values,
            span,
            id,
        } => Ast::ModuleExpression {
            data,
            values: self::resolve_module_expression_values(ctx, values, templates, memo, output),
            span,
            id,
        },
        Ast::IndirectCall {
            function,
            function_type,
            args,
            kind,
            span,
            id,
        } => Ast::IndirectCall {
            function: std::boxed::Box::new(self::resolve_ast(
                ctx, *function, templates, memo, output,
            )),
            function_type,
            args: self::resolve_ast_list(ctx, args, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::AsmValue {
            assembler,
            constraints,
            args,
            kind,
            attributes,
            span,
            id,
        } => Ast::AsmValue {
            assembler,
            constraints,
            args: self::resolve_ast_list(ctx, args, templates, memo, output),
            kind,
            attributes,
            span,
            id,
        },
        Ast::BinaryOp {
            left,
            operator,
            right,
            kind,
            span,
            id,
        } => Ast::BinaryOp {
            left: std::boxed::Box::new(self::resolve_ast(ctx, *left, templates, memo, output)),
            operator,
            right: std::boxed::Box::new(self::resolve_ast(ctx, *right, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::UnaryOp {
            operator,
            kind,
            node,
            before,
            span,
            id,
        } => Ast::UnaryOp {
            operator,
            kind,
            node: std::boxed::Box::new(self::resolve_ast(ctx, *node, templates, memo, output)),
            before,
            span,
            id,
        },
        Ast::Group {
            node,
            kind,
            span,
            id,
        } => Ast::Group {
            node: std::boxed::Box::new(self::resolve_ast(ctx, *node, templates, memo, output)),
            kind,
            span,
            id,
        },
        Ast::Builtin {
            builtin,
            kind,
            span,
            id,
        } => Ast::Builtin {
            builtin: self::resolve_builtin(ctx, builtin, templates, memo, output),
            kind,
            span,
            id,
        },
        Ast::Import { span, kind, id } => Ast::Import { span, kind, id },
        Ast::ImportC { span, kind, id } => Ast::ImportC { span, kind, id },
        Ast::Unreachable { span, kind, id } => Ast::Unreachable { span, kind, id },
        Ast::Invalid { kind, span, id } => Ast::Invalid { kind, span, id },
    }
}

fn resolve_constructor_data<'parser>(
    ctx: &mut ParserContext<'parser>,
    data: ConstructorData<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> ConstructorData<'parser> {
    data.into_iter()
        .map(|(field_name, expression, target_type, position)| {
            (
                field_name,
                self::resolve_ast(ctx, expression, templates, memo, output),
                target_type,
                position,
            )
        })
        .collect()
}

fn resolve_enum_data<'parser>(
    ctx: &mut ParserContext<'parser>,
    data: EnumData<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> EnumData<'parser> {
    data.into_iter()
        .map(|(name, ty, expression)| {
            (
                name,
                ty,
                self::resolve_ast(ctx, expression, templates, memo, output),
            )
        })
        .collect()
}

fn resolve_module_expression_values<'parser>(
    ctx: &mut ParserContext<'parser>,
    values: ModuleExpressionValues<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> ModuleExpressionValues<'parser> {
    match values {
        ModuleExpressionValues::Call { arguments, span } => ModuleExpressionValues::Call {
            arguments: self::resolve_ast_list(ctx, arguments, templates, memo, output),
            span,
        },
        ModuleExpressionValues::Reference { name, span } => {
            ModuleExpressionValues::Reference { name, span }
        }
    }
}

fn resolve_builtin<'parser>(
    ctx: &mut ParserContext<'parser>,
    builtin: AstBuiltin<'parser>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> AstBuiltin<'parser> {
    match builtin {
        AstBuiltin::Halloc { of, span } => AstBuiltin::Halloc { of, span },
        AstBuiltin::MemCpy {
            src,
            dst,
            size,
            span,
        } => AstBuiltin::MemCpy {
            src: std::boxed::Box::new(self::resolve_ast(ctx, *src, templates, memo, output)),
            dst: std::boxed::Box::new(self::resolve_ast(ctx, *dst, templates, memo, output)),
            size: std::boxed::Box::new(self::resolve_ast(ctx, *size, templates, memo, output)),
            span,
        },
        AstBuiltin::MemMove {
            src,
            dst,
            size,
            span,
        } => AstBuiltin::MemMove {
            src: std::boxed::Box::new(self::resolve_ast(ctx, *src, templates, memo, output)),
            dst: std::boxed::Box::new(self::resolve_ast(ctx, *dst, templates, memo, output)),
            size: std::boxed::Box::new(self::resolve_ast(ctx, *size, templates, memo, output)),
            span,
        },
        AstBuiltin::MemSet {
            dst,
            new_size,
            size,
            span,
        } => AstBuiltin::MemSet {
            dst: std::boxed::Box::new(self::resolve_ast(ctx, *dst, templates, memo, output)),
            new_size: std::boxed::Box::new(self::resolve_ast(
                ctx, *new_size, templates, memo, output,
            )),
            size: std::boxed::Box::new(self::resolve_ast(ctx, *size, templates, memo, output)),
            span,
        },
        AstBuiltin::BitSizeOf { ty, span } => AstBuiltin::BitSizeOf { ty, span },
        AstBuiltin::AbiSizeOf { ty, span } => AstBuiltin::AbiSizeOf { ty, span },
        AstBuiltin::AbiAlignOf { ty, span } => AstBuiltin::AbiAlignOf { ty, span },
        AstBuiltin::ArbitraryArg { ty, span } => AstBuiltin::ArbitraryArg { ty, span },
        AstBuiltin::ArbitraryArgs { span } => AstBuiltin::ArbitraryArgs { span },
    }
}

fn collect_local_templates<'parser>(
    ctx: &mut ParserContext<'parser>,
) -> HashMap<String, Ast<'parser>> {
    let mut templates: HashMap<String, Ast<'parser>> = HashMap::new();

    let ast: Vec<Ast<'parser>> = ctx.get_mut_ast().clone();

    for node in ast.iter() {
        if let Ast::Function {
            name,
            body: Some(_),
            ..
        } = node
        {
            if ctx.get_symbols().has_generic_function(name) {
                templates.insert(name.clone(), node.clone());
            }
        }
    }

    templates
}

fn resolve_ast_list<'parser>(
    ctx: &mut ParserContext<'parser>,
    nodes: Vec<Ast<'parser>>,
    templates: &HashMap<String, Ast<'parser>>,
    memo: &mut HashSet<String>,
    output: &mut Vec<Ast<'parser>>,
) -> Vec<Ast<'parser>> {
    nodes
        .into_iter()
        .map(|node| self::resolve_ast(ctx, node, templates, memo, output))
        .collect()
}
