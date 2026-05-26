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

use inkwell::attributes::Attribute;
use inkwell::attributes::AttributeLoc;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::values::FunctionValue;
use inkwell::values::GlobalValue;
use thrustc_llvm_attributes::LLVMAttribute;
use thrustc_llvm_attributes::LLVMAttributes;
use thrustc_llvm_attributes::traits::LLVMAttributesExtensions;

use crate::context::LLVMCodeGenContext;

#[derive(Debug)]
pub enum LLVMAttributeApplicant<'ctx> {
    Function(FunctionValue<'ctx>),
    Global(GlobalValue<'ctx>),
}

#[derive(Debug)]
pub struct AttributeBuilder {}

impl<'ctx> AttributeBuilder {
    pub fn add_function_attributes(
        context: &mut LLVMCodeGenContext<'_, 'ctx>,
        attributes: &LLVMAttributes<'ctx>,
        applicant: LLVMAttributeApplicant<'ctx>,
    ) {
        if let LLVMAttributeApplicant::Function(function) = applicant {
            let llvm_context: &Context = context.get_llvm_context();

            let is_public: bool = attributes.has_public_attribute();
            let is_extern: bool = attributes.has_extern_attribute();

            if !is_public && !is_extern {
                function.set_linkage(Linkage::LinkerPrivate);
            }

            attributes.iter().for_each(|attribute| match attribute {
                LLVMAttribute::Linkage(linkage) => {
                    function.set_linkage(*linkage);
                }

                LLVMAttribute::AlwaysInline => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("alwaysinline"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::InlineHint => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("inlinehint"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::NoInline => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("noinline"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::Hot => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("hot"), 0),
                    );
                }

                LLVMAttribute::MinSize => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("optsize"), 0),
                    );
                }

                LLVMAttribute::SafeStack => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("safestack"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::WeakStack => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("ssp"), 0),
                    );
                }

                LLVMAttribute::StrongStack => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("sspstrong"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::PreciseFloats => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("strictfp"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::NoUnwind => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("nounwind"),
                            0,
                        ),
                    );
                }

                LLVMAttribute::OptFuzzing => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("optforfuzzing"),
                            0,
                        ),
                    );
                }
                LLVMAttribute::Pure => {
                    function.add_attribute(
                        AttributeLoc::Function,
                        llvm_context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("naked"), 0),
                    );
                }

                LLVMAttribute::Thunk => {
                    let thunk_attribute: Attribute =
                        llvm_context.create_string_attribute("thunk", "");

                    function.add_attribute(AttributeLoc::Function, thunk_attribute);
                }

                LLVMAttribute::Constructor => {
                    context.add_ctor(function.as_global_value().as_pointer_value());
                }

                LLVMAttribute::Destructor => {
                    context.add_dtor(function.as_global_value().as_pointer_value());
                }

                _ => (),
            })
        }
    }

    pub fn add_global_attributes(
        attributes: &LLVMAttributes<'ctx>,
        applicant: LLVMAttributeApplicant<'ctx>,
    ) {
        if let LLVMAttributeApplicant::Global(global) = applicant {
            {
                for attribute in attributes.iter() {
                    if let LLVMAttribute::Linkage(linkage) = attribute {
                        global.set_linkage(*linkage);
                    }

                    if let LLVMAttribute::Align(value, ..) = attribute {
                        global.set_alignment((*value).try_into().unwrap_or(u32::MAX));
                    }
                }
            }
        }
    }
}
