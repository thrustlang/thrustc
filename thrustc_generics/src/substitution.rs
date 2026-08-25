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
    Ast, ModuleExpressionValues,
    ast_builtins::AstBuiltin,
    ast_logic_data::{ConstructorData, EnumData, PropertyData, StructureData},
};
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

use crate::solve::TypeEnv;

pub fn substitute(ty: &Type, env: &TypeEnv) -> Type {
    match ty {
        Type::Unresolved { hint, .. } => {
            if let Some(concrete) = env.get(hint) {
                concrete.clone()
            } else {
                ty.clone()
            }
        }
        Type::Const(inner, span) => {
            Type::Const(std::boxed::Box::new(self::substitute(inner, env)), *span)
        }
        Type::Ptr {
            subtype,
            address_space,
            span,
        } => Type::Ptr {
            subtype: subtype
                .as_ref()
                .map(|inner| std::boxed::Box::new(self::substitute(inner, env))),
            address_space: *address_space,
            span: *span,
        },
        Type::Struct {
            name,
            fields,
            metadata,
            span,
        } => Type::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| self::substitute(field, env))
                .collect(),
            metadata: *metadata,
            span: *span,
        },
        Type::FixedArray {
            base_type,
            size,
            metadata,
            span,
        } => Type::FixedArray {
            base_type: std::boxed::Box::new(self::substitute(base_type, env)),
            size: *size,
            metadata: metadata.clone(),
            span: *span,
        },
        Type::Array {
            base_type,
            infered_type,
            metadata,
            span,
        } => Type::Array {
            base_type: std::boxed::Box::new(self::substitute(base_type, env)),
            infered_type: infered_type
                .as_ref()
                .map(|(inner, count)| (std::boxed::Box::new(self::substitute(inner, env)), *count)),
            metadata: metadata.clone(),
            span: *span,
        },
        Type::Fn {
            return_type,
            parameter_types,
            modificator,
            span,
        } => Type::Fn {
            return_type: std::boxed::Box::new(self::substitute(return_type, env)),
            parameter_types: parameter_types
                .iter()
                .map(|parameter| self::substitute(parameter, env))
                .collect(),
            modificator: *modificator,
            span: *span,
        },
        other => other.clone(),
    }
}

pub fn substitute_ast<'ast>(node: Ast<'ast>, env: &TypeEnv) -> Ast<'ast> {
    match node {
        Ast::CString {
            bytes,
            kind,
            span,
            id,
        } => Ast::CString {
            bytes,
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Char {
            kind,
            byte,
            span,
            id,
        } => Ast::Char {
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
            value,
            span,
            id,
        },
        Ast::NullPtr { span, kind } => Ast::NullPtr {
            span,
            kind: self::substitute(&kind, env),
        },
        Ast::GlobalAssembler {
            asm,
            span,
            kind,
            id,
        } => Ast::GlobalAssembler {
            asm,
            span,
            kind: self::substitute(&kind, env),
            id,
        },
        Ast::FixedArray {
            items,
            kind,
            span,
            id,
        } => Ast::FixedArray {
            items: self::substitute_ast_list(items, env),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Array {
            items,
            kind,
            span,
            id,
        } => Ast::Array {
            items: self::substitute_ast_list(items, env),
            kind: self::substitute(&kind, env),
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
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            index: std::boxed::Box::new(self::substitute_ast(*index, env)),
            metadata,
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
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
            data: self::substitute_structure_data(data, env),
            kind: self::substitute(&kind, env),
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
            data: self::substitute_constructor_data(data, env),
            kind: self::substitute(&kind, env),
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
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            data: self::substitute_property_data(data, env),
            metadata,
            kind: self::substitute(&kind, env),
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
            condition: std::boxed::Box::new(self::substitute_ast(*condition, env)),
            then_branch: std::boxed::Box::new(self::substitute_ast(*then_branch, env)),
            else_if_branch: self::substitute_ast_list(else_if_branch, env),
            else_branch: else_branch
                .map(|branch| std::boxed::Box::new(self::substitute_ast(*branch, env))),
            kind: self::substitute(&kind, env),
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
            condition: std::boxed::Box::new(self::substitute_ast(*condition, env)),
            block: std::boxed::Box::new(self::substitute_ast(*block, env)),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Else {
            block,
            kind,
            span,
            id,
        } => Ast::Else {
            block: std::boxed::Box::new(self::substitute_ast(*block, env)),
            kind: self::substitute(&kind, env),
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
            local: std::boxed::Box::new(self::substitute_ast(*local, env)),
            condition: std::boxed::Box::new(self::substitute_ast(*condition, env)),
            actions: std::boxed::Box::new(self::substitute_ast(*actions, env)),
            block: std::boxed::Box::new(self::substitute_ast(*block, env)),
            kind: self::substitute(&kind, env),
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
            variable: variable.map(|local| std::boxed::Box::new(self::substitute_ast(*local, env))),
            condition: std::boxed::Box::new(self::substitute_ast(*condition, env)),
            block: std::boxed::Box::new(self::substitute_ast(*block, env)),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Loop {
            block,
            kind,
            span,
            id,
        } => Ast::Loop {
            block: std::boxed::Box::new(self::substitute_ast(*block, env)),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Continue { kind, span, id } => Ast::Continue {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Break { kind, span, id } => Ast::Break {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::ContinueAll { kind, span, id } => Ast::ContinueAll {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::BreakAll { kind, span, id } => Ast::BreakAll {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Block {
            nodes,
            post,
            kind,
            span,
            id,
        } => Ast::Block {
            nodes: self::substitute_ast_list(nodes, env),
            post: self::substitute_ast_list(post, env),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Defer {
            node,
            kind,
            span,
            id,
        } => Ast::Defer {
            node: std::boxed::Box::new(self::substitute_ast(*node, env)),
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
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
            data: self::substitute_enum_data(data, env),
            attributes,
            kind: self::substitute(&kind, env),
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
            value: std::boxed::Box::new(self::substitute_ast(*value, env)),
            kind: self::substitute(&kind, env),
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
            parameters: self::substitute_ast_list(parameters, env),
            parameters_types: self::substitute_type_list(parameters_types, env),
            return_type: self::substitute(&return_type, env),
            attributes,
            span,
            id,
        },
        Ast::CompilerIntrinsicParameter { kind, span, id } => Ast::CompilerIntrinsicParameter {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
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
            parameters: self::substitute_ast_list(parameters, env),
            parameters_types: self::substitute_type_list(parameters_types, env),
            assembler,
            constraints,
            return_type: self::substitute(&return_type, env),
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
            kind: self::substitute(&kind, env),
            position,
            span,
            id,
        },
        Ast::Function {
            name,
            ascii_name,
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
            parameters: self::substitute_ast_list(parameters, env),
            parameter_types: self::substitute_type_list(parameter_types, env),
            body: body.map(|block| std::boxed::Box::new(self::substitute_ast(*block, env))),
            return_type: self::substitute(&return_type, env),
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
            kind: self::substitute(&kind, env),
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
            expression: expression
                .map(|value| std::boxed::Box::new(self::substitute_ast(*value, env))),
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
            value: value
                .map(|initializer| std::boxed::Box::new(self::substitute_ast(*initializer, env))),
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
            kind: self::substitute(&kind, env),
            value: std::boxed::Box::new(self::substitute_ast(*value, env)),
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
            kind: self::substitute(&kind, env),
            value: value
                .map(|initializer| std::boxed::Box::new(self::substitute_ast(*initializer, env))),
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
            kind: self::substitute(&kind, env),
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
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            value: std::boxed::Box::new(self::substitute_ast(*value, env)),
            kind: self::substitute(&kind, env),
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
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            indexes: self::substitute_ast_list(indexes, env),
            kind: self::substitute(&kind, env),
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
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            write_value: std::boxed::Box::new(self::substitute_ast(*write_value, env)),
            write_type: self::substitute(&write_type, env),
            span,
            id,
        },
        Ast::Load {
            source,
            kind,
            span,
            id,
        } => Ast::Load {
            source: std::boxed::Box::new(self::substitute_ast(*source, env)),
            kind: self::substitute(&kind, env),
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
            value: std::boxed::Box::new(self::substitute_ast(*value, env)),
            kind: self::substitute(&kind, env),
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
            from: std::boxed::Box::new(self::substitute_ast(*from, env)),
            cast: self::substitute(&cast, env),
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
            expr: std::boxed::Box::new(self::substitute_ast(*expr, env)),
            kind: self::substitute(&kind, env),
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
            values: self::substitute_module_expression_values(values, env),
            span,
            id,
        },
        Ast::Call {
            name,
            args,
            generic_args,
            kind,
            span,
            id,
        } => Ast::Call {
            name,
            args: self::substitute_ast_list(args, env),
            generic_args: self::substitute_type_list(generic_args, env),
            kind: self::substitute(&kind, env),
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
            function: std::boxed::Box::new(self::substitute_ast(*function, env)),
            function_type: self::substitute(&function_type, env),
            args: self::substitute_ast_list(args, env),
            kind: self::substitute(&kind, env),
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
            args: self::substitute_ast_list(args, env),
            kind: self::substitute(&kind, env),
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
            left: std::boxed::Box::new(self::substitute_ast(*left, env)),
            operator,
            right: std::boxed::Box::new(self::substitute_ast(*right, env)),
            kind: self::substitute(&kind, env),
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
            kind: self::substitute(&kind, env),
            node: std::boxed::Box::new(self::substitute_ast(*node, env)),
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
            node: std::boxed::Box::new(self::substitute_ast(*node, env)),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Builtin {
            builtin,
            kind,
            span,
            id,
        } => Ast::Builtin {
            builtin: self::substitute_builtin(builtin, env),
            kind: self::substitute(&kind, env),
            span,
            id,
        },
        Ast::Import { span, kind, id } => Ast::Import {
            span,
            kind: self::substitute(&kind, env),
            id,
        },
        Ast::ImportC { span, kind, id } => Ast::ImportC {
            span,
            kind: self::substitute(&kind, env),
            id,
        },
        Ast::Unreachable { span, kind, id } => Ast::Unreachable {
            span,
            kind: self::substitute(&kind, env),
            id,
        },
        Ast::Invalid { kind, span, id } => Ast::Invalid {
            kind: self::substitute(&kind, env),
            span,
            id,
        },
    }
}

fn substitute_ast_list<'ast>(nodes: Vec<Ast<'ast>>, env: &TypeEnv) -> Vec<Ast<'ast>> {
    nodes
        .into_iter()
        .map(|node| self::substitute_ast(node, env))
        .collect()
}

fn substitute_type_list(types: Vec<Type>, env: &TypeEnv) -> Vec<Type> {
    types
        .into_iter()
        .map(|ty| self::substitute(&ty, env))
        .collect()
}

fn substitute_structure_data<'ast>(
    data: StructureData<'ast>,
    env: &TypeEnv,
) -> StructureData<'ast> {
    let (name, fields, metadata, span) = data;

    let fields: Vec<(&'ast str, Type, u32, Span)> = fields
        .into_iter()
        .map(|(field_name, field_type, position, field_span)| {
            (
                field_name,
                self::substitute(&field_type, env),
                position,
                field_span,
            )
        })
        .collect();

    (name, fields, metadata, span)
}

fn substitute_constructor_data<'ast>(
    data: ConstructorData<'ast>,
    env: &TypeEnv,
) -> ConstructorData<'ast> {
    data.into_iter()
        .map(|(field_name, expression, target_type, position)| {
            (
                field_name,
                self::substitute_ast(expression, env),
                self::substitute(&target_type, env),
                position,
            )
        })
        .collect()
}

fn substitute_property_data(data: PropertyData, env: &TypeEnv) -> PropertyData {
    data.into_iter()
        .map(|(ty, (inner_ty, index))| {
            (
                self::substitute(&ty, env),
                (self::substitute(&inner_ty, env), index),
            )
        })
        .collect()
}

fn substitute_enum_data<'ast>(data: EnumData<'ast>, env: &TypeEnv) -> EnumData<'ast> {
    data.into_iter()
        .map(|(name, ty, expression)| {
            (
                name,
                self::substitute(&ty, env),
                self::substitute_ast(expression, env),
            )
        })
        .collect()
}

fn substitute_module_expression_values<'ast>(
    values: ModuleExpressionValues<'ast>,
    env: &TypeEnv,
) -> ModuleExpressionValues<'ast> {
    match values {
        ModuleExpressionValues::Call { arguments, span } => ModuleExpressionValues::Call {
            arguments: self::substitute_ast_list(arguments, env),
            span,
        },
        ModuleExpressionValues::Reference { name, span } => {
            ModuleExpressionValues::Reference { name, span }
        }
    }
}

fn substitute_builtin<'ast>(builtin: AstBuiltin<'ast>, env: &TypeEnv) -> AstBuiltin<'ast> {
    match builtin {
        AstBuiltin::Halloc { of, span } => AstBuiltin::Halloc {
            of: self::substitute(&of, env),
            span,
        },
        AstBuiltin::MemCpy {
            src,
            dst,
            size,
            span,
        } => AstBuiltin::MemCpy {
            src: std::boxed::Box::new(self::substitute_ast(*src, env)),
            dst: std::boxed::Box::new(self::substitute_ast(*dst, env)),
            size: std::boxed::Box::new(self::substitute_ast(*size, env)),
            span,
        },
        AstBuiltin::MemMove {
            src,
            dst,
            size,
            span,
        } => AstBuiltin::MemMove {
            src: std::boxed::Box::new(self::substitute_ast(*src, env)),
            dst: std::boxed::Box::new(self::substitute_ast(*dst, env)),
            size: std::boxed::Box::new(self::substitute_ast(*size, env)),
            span,
        },
        AstBuiltin::MemSet {
            dst,
            new_size,
            size,
            span,
        } => AstBuiltin::MemSet {
            dst: std::boxed::Box::new(self::substitute_ast(*dst, env)),
            new_size: std::boxed::Box::new(self::substitute_ast(*new_size, env)),
            size: std::boxed::Box::new(self::substitute_ast(*size, env)),
            span,
        },
        AstBuiltin::BitSizeOf { ty, span } => AstBuiltin::BitSizeOf {
            ty: self::substitute(&ty, env),
            span,
        },
        AstBuiltin::AbiSizeOf { ty, span } => AstBuiltin::AbiSizeOf {
            ty: self::substitute(&ty, env),
            span,
        },
        AstBuiltin::AbiAlignOf { ty, span } => AstBuiltin::AbiAlignOf {
            ty: self::substitute(&ty, env),
            span,
        },
        AstBuiltin::ArbitraryArg { ty, span } => AstBuiltin::ArbitraryArg {
            ty: self::substitute(&ty, env),
            span,
        },
        AstBuiltin::ArbitraryArgs { span } => AstBuiltin::ArbitraryArgs { span },
    }
}
