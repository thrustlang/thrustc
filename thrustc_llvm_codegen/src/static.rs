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

#![allow(clippy::too_many_arguments)]

use inkwell::AddressSpace;
use inkwell::ThreadLocalMode;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::GlobalValue;
use inkwell::values::PointerValue;

use thrustc_ast::ast_metadata::ConstantMetadata;
use thrustc_ast::ast_metadata::LLVMConstantMetadata;
use thrustc_ast::ast_metadata::LLVMStaticMetadata;
use thrustc_ast::ast_metadata::StaticMetadata;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributeComparator;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;
use thrustc_typesystem::Type;

use crate::attributebuilder::AttributeBuilder;
use crate::attributebuilder::LLVMAttributeApplicant;
use crate::context::LLVMCodeGenContext;
use crate::memory;
use crate::utils;

fn generate_name(
    context: &LLVMCodeGenContext,
    base_name: &str,
    prefix: &str,
    attributes: Option<&LLVMAttributes>,
) -> String {
    if let Some(attrs) = attributes {
        if let Some(LLVMAttribute::Extern(extern_name, ..)) =
            attrs.get_attr(LLVMAttributeComparator::Extern)
        {
            return extern_name.to_string();
        }
        if attrs.has_public_attribute() {
            return base_name.to_string();
        }
    }

    format!(
        "{}.{}{}",
        prefix,
        utils::generate_string(context, utils::SHORT_RANGE_OBFUSCATION),
        base_name
    )
}

fn set_global_common<'ctx>(
    global: &GlobalValue<'ctx>,
    constant: bool,
    unnamed_addr: bool,
    thread_local: bool,
    thread_mode: Option<ThreadLocalMode>,
    initializer: Option<&BasicValueEnum<'ctx>>,
    alignment: Option<u32>,
    linkage: Option<Linkage>,
) {
    if let Some(align) = alignment {
        global.set_alignment(align);
    }
    if let Some(link) = linkage {
        global.set_linkage(link);
    }
    if constant {
        global.set_constant(true);
    }
    if unnamed_addr {
        global.set_unnamed_addr(true);
    }
    if thread_local {
        global.set_thread_local(true);
    }
    if let Some(init) = initializer {
        global.set_initializer(init);
    }

    global.set_thread_local_mode(thread_mode);
}

pub fn allocate_local_constant<'ctx>(
    context: &LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    ty: &Type,
    llvm_type: BasicTypeEnum<'ctx>,
    value: BasicValueEnum<'ctx>,
    attributes: LLVMAttributes<'ctx>,
    metadata: ConstantMetadata,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();
    let target_data: &TargetData = context.get_target_data();
    let llvm_metadata: LLVMConstantMetadata = metadata.get_llvm_metadata();

    let name: String = self::generate_name(context, name, "local.const", None);

    let address_space: Option<AddressSpace> = memory::get_address_space(ty);

    let global: GlobalValue = llvm_module.add_global(llvm_type, address_space, &name);

    AttributeBuilder::add_global_attributes(&attributes, LLVMAttributeApplicant::Global(global));

    self::set_global_common(
        &global,
        true,
        true,
        llvm_metadata.thread_local,
        None,
        Some(&value),
        Some(target_data.get_preferred_alignment_of_global(&global)),
        Some(Linkage::LinkerPrivate),
    );

    global.as_pointer_value()
}

pub fn allocate_global_constant<'ctx>(
    context: &LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    ty: &Type,
    llvm_type: BasicTypeEnum<'ctx>,
    value: BasicValueEnum<'ctx>,
    attributes: LLVMAttributes<'ctx>,
    metadata: ConstantMetadata,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();

    let target_data: &TargetData = context.get_target_data();
    let llvm_metadata: LLVMConstantMetadata = metadata.get_llvm_metadata();

    let name: String = self::generate_name(context, name, "global.constant", Some(&attributes));

    let address_space: Option<AddressSpace> = memory::get_address_space(ty);

    let global: GlobalValue = llvm_module.add_global(llvm_type, address_space, &name);

    let linkage: Option<Linkage> =
        if !attributes.has_public_attribute() && !attributes.has_linkage_attribute() {
            Some(Linkage::LinkerPrivate)
        } else {
            None
        };

    AttributeBuilder::add_global_attributes(&attributes, LLVMAttributeApplicant::Global(global));

    self::set_global_common(
        &global,
        true,
        true,
        llvm_metadata.thread_local,
        None,
        Some(&value),
        Some(target_data.get_preferred_alignment_of_global(&global)),
        linkage,
    );

    global.as_pointer_value()
}

pub fn allocate_local_static<'ctx>(
    context: &LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    ty: &Type,
    llvm_type: BasicTypeEnum<'ctx>,
    value: Option<BasicValueEnum<'ctx>>,
    attributes: LLVMAttributes<'ctx>,
    metadata: StaticMetadata,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();
    let target_data: &TargetData = context.get_target_data();

    let llvm_metadata: LLVMStaticMetadata = metadata.get_llvm_metadata();

    let name: String = self::generate_name(context, name, "local.static", None);

    let address_space: Option<AddressSpace> = memory::get_address_space(ty);

    let global: GlobalValue = llvm_module.add_global(llvm_type, address_space, &name);

    AttributeBuilder::add_global_attributes(&attributes, LLVMAttributeApplicant::Global(global));

    if value.is_none() {
        global.set_initializer(&llvm_type.const_zero());
    }

    self::set_global_common(
        &global,
        llvm_metadata.constant,
        llvm_metadata.unnamed_addr,
        llvm_metadata.thread_local,
        llvm_metadata
            .thread_mode
            .map(|threadmode| threadmode.as_llvm_threadmode()),
        value.as_ref(),
        Some(target_data.get_preferred_alignment_of_global(&global)),
        Some(Linkage::LinkerPrivate),
    );

    global.as_pointer_value()
}

pub fn allocate_global_static<'ctx>(
    context: &LLVMCodeGenContext<'_, 'ctx>,
    name: &str,
    ty: &Type,
    llvm_type: BasicTypeEnum<'ctx>,
    value: Option<BasicValueEnum<'ctx>>,
    attributes: LLVMAttributes<'ctx>,
    metadata: StaticMetadata,
) -> PointerValue<'ctx> {
    let llvm_module: &Module = context.get_llvm_module();

    let target_data: &TargetData = context.get_target_data();
    let llvm_metadata: LLVMStaticMetadata = metadata.get_llvm_metadata();

    let name: String = self::generate_name(context, name, "global.static", Some(&attributes));

    let address_space: Option<AddressSpace> = memory::get_address_space(ty);

    let global: GlobalValue = llvm_module.add_global(llvm_type, address_space, &name);

    let linkage: Option<Linkage> = if !attributes.has_public_attribute()
        && !attributes.has_extern_attribute()
        && !attributes.has_linkage_attribute()
    {
        Some(Linkage::LinkerPrivate)
    } else {
        None
    };

    if !attributes.has_extern_attribute() && value.is_none() {
        global.set_initializer(&llvm_type.const_zero());
    }

    AttributeBuilder::add_global_attributes(&attributes, LLVMAttributeApplicant::Global(global));

    self::set_global_common(
        &global,
        llvm_metadata.constant,
        llvm_metadata.unnamed_addr,
        llvm_metadata.thread_local,
        llvm_metadata
            .thread_mode
            .map(|thread_mode| thread_mode.as_llvm_threadmode()),
        value.as_ref(),
        Some(target_data.get_preferred_alignment_of_global(&global)),
        linkage,
    );

    global.as_pointer_value()
}
