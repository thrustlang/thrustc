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
use inkwell::targets::CodeModel;
use inkwell::targets::RelocMode;
use thrustc_backends::llvm::target::LLVMTarget;
use thrustc_logging::{self, LoggingType};

use inkwell::debug_info;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::MetadataValue;
use thrustc_backends::llvm::LLVMBackend;
use thrustc_options::CompilerOptions;

use thrustc_llvm_target_triple::LLVMTargetTriple;

use crate::context::LLVMCodeGenContext;

#[derive(Debug)]
pub struct LLVMMetadata<'a, 'ctx> {
    context: &'a LLVMCodeGenContext<'a, 'ctx>,
}

impl<'a, 'ctx> LLVMMetadata<'a, 'ctx> {
    #[inline]
    pub fn setup_platform_independent_metadata(context: &'a LLVMCodeGenContext<'a, 'ctx>) {
        let inner: LLVMMetadata<'a, 'ctx> = Self { context };

        inner.setup_metadata();
    }

    pub fn setup_platform_specific_metadata(context: &'a LLVMCodeGenContext<'a, 'ctx>) {
        let inner: LLVMMetadata<'a, 'ctx> = Self { context };

        inner.setup_target_specific_metadata_or_attributes();
    }
}

impl<'a, 'ctx> LLVMMetadata<'a, 'ctx> {
    fn setup_metadata(&self) {
        self.setup_llvm_module_flags();
        self.setup_compiler_info();
        self.setup_build_id();
    }

    fn setup_target_specific_metadata_or_attributes(&self) {
        let options: &CompilerOptions = self.get_codegen_context().get_compiler_options();
        let llvm_backend: &LLVMBackend = options.get_llvm_backend();

        {
            let features: &str = llvm_backend.get_target_cpu().get_cpu_features();
            let cpu: &str = llvm_backend.get_target_cpu().get_cpu_name();

            let features_attr: Attribute = self
                .get_codegen_context()
                .get_llvm_context()
                .create_string_attribute("target-features", features);

            let target_cpu_attr: Attribute = self
                .get_codegen_context()
                .get_llvm_context()
                .create_string_attribute("target-cpu", cpu);

            let tune_cpu_attr: Attribute = self
                .get_codegen_context()
                .get_llvm_context()
                .create_string_attribute("tune-cpu", cpu);

            {
                for function in self.get_codegen_context().get_llvm_module().get_functions() {
                    function.add_attribute(AttributeLoc::Function, target_cpu_attr);
                    function.add_attribute(AttributeLoc::Function, tune_cpu_attr);
                    function.add_attribute(AttributeLoc::Function, features_attr);

                    if !llvm_backend.omit_trapping_math() {
                        let no_trapping_math_attr: Attribute = self
                            .get_codegen_context()
                            .get_llvm_context()
                            .create_string_attribute("no-trapping-math", "true");

                        function.add_attribute(AttributeLoc::Function, no_trapping_math_attr);
                    }

                    if !llvm_backend.omit_frame_pointer() {
                        let frame_pointer_attr: Attribute = self
                            .get_codegen_context()
                            .get_llvm_context()
                            .create_string_attribute("frame-pointer", "all");

                        function.add_attribute(AttributeLoc::Function, frame_pointer_attr);
                    }
                }
            }
        }
    }

    fn setup_llvm_module_flags(&self) {
        let options: &CompilerOptions = self.get_codegen_context().get_compiler_options();
        let llvm_backend: &LLVMBackend = options.get_llvm_backend();

        let lvl_max: BasicMetadataValueEnum = self
            .get_codegen_context()
            .get_llvm_context()
            .i32_type()
            .const_int(7, false)
            .into();

        let lvl_min: BasicMetadataValueEnum = self
            .get_codegen_context()
            .get_llvm_context()
            .i32_type()
            .const_int(8, false)
            .into();

        let lvl_error: BasicMetadataValueEnum = self
            .get_codegen_context()
            .get_llvm_context()
            .i32_type()
            .const_int(1, false)
            .into();

        {
            if llvm_backend.get_debug_config().is_debug_mode() {
                let dwarf_version: u64 = llvm_backend.get_debug_config().get_dwarf_version();
                let debug_info_version: u32 = debug_info::debug_metadata_version();

                let dwarf_v: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_max,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("Dwarf Version")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_int(dwarf_version, false)
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &dwarf_v)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Dwarf Version' metadata failed to set up.",
                        );
                    });

                let debug_info_v: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_error,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("Debug Info Version")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_int(debug_info_version as u64, false)
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &debug_info_v)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Debug Info Version' metadata failed to set up.",
                        );
                    });
            }
        }

        {
            let repr: u64 = match llvm_backend.get_reloc_mode() {
                RelocMode::Static | RelocMode::Default => 0,
                RelocMode::PIC => 1,
                RelocMode::DynamicNoPic => 1,
            };

            let pic_level: MetadataValue = self
                .get_codegen_context()
                .get_llvm_context()
                .metadata_node(&[
                    lvl_min,
                    self.get_codegen_context()
                        .get_llvm_context()
                        .metadata_string("PIC Level")
                        .into(),
                    self.get_codegen_context()
                        .get_llvm_context()
                        .i32_type()
                        .const_int(repr, false)
                        .into(),
                ]);

            self.get_codegen_context()
                .get_llvm_module()
                .add_global_metadata("llvm.module.flags", &pic_level)
                .unwrap_or_else(|_| {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        "'PIC Level' metadata failed to set up.",
                    );
                });
        }

        {
            let repr: u64 = match llvm_backend.get_reloc_mode() {
                RelocMode::Static | RelocMode::Default => 0,
                RelocMode::PIC => 1,
                RelocMode::DynamicNoPic => 1,
            };

            let pie_level: MetadataValue = self
                .get_codegen_context()
                .get_llvm_context()
                .metadata_node(&[
                    lvl_max,
                    self.get_codegen_context()
                        .get_llvm_context()
                        .metadata_string("PIE Level")
                        .into(),
                    self.get_codegen_context()
                        .get_llvm_context()
                        .i32_type()
                        .const_int(repr, false)
                        .into(),
                ]);

            self.get_codegen_context()
                .get_llvm_module()
                .add_global_metadata("llvm.module.flags", &pie_level)
                .unwrap_or_else(|_| {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        "'PIE Level' metadata failed to set up.",
                    );
                });
        }

        {
            let repr: u64 = match llvm_backend.get_code_model() {
                CodeModel::Default => 0,
                CodeModel::JITDefault => 0,
                CodeModel::Small => 0,
                CodeModel::Kernel => 2,
                CodeModel::Medium => 3,
                CodeModel::Large => 4,
            };

            let code_level: MetadataValue = self
                .get_codegen_context()
                .get_llvm_context()
                .metadata_node(&[
                    lvl_error,
                    self.get_codegen_context()
                        .get_llvm_context()
                        .metadata_string("Code Model")
                        .into(),
                    self.get_codegen_context()
                        .get_llvm_context()
                        .i32_type()
                        .const_int(repr, false)
                        .into(),
                ]);

            self.get_codegen_context()
                .get_llvm_module()
                .add_global_metadata("llvm.module.flags", &code_level)
                .unwrap_or_else(|_| {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        "'Code Model' metadata failed to set up.",
                    );
                });
        }

        {
            let lvl_warning: BasicMetadataValueEnum = self
                .get_codegen_context()
                .get_llvm_context()
                .i32_type()
                .const_int(2, false)
                .into();

            if let Some(sdk_macos_version) = llvm_backend.get_target().get_macos_version() {
                let major: u64 = sdk_macos_version.0;
                let minor: u64 = sdk_macos_version.1;
                let patch: u64 = sdk_macos_version.2;

                let sdk_v: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_warning,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("SDK Version")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_array(&[
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(major, false),
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(minor, false),
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(patch, false),
                            ])
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &sdk_v)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'MacOS SDK Version' metadata failed to set up.",
                        );
                    });
            }

            if let Some(sdk_ios_version) = llvm_backend.get_target().get_ios_version() {
                let major: u64 = sdk_ios_version.0;
                let minor: u64 = sdk_ios_version.1;
                let patch: u64 = sdk_ios_version.2;

                let sdk_v: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_warning,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("SDK Version")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_array(&[
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(major, false),
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(minor, false),
                                self.get_codegen_context()
                                    .get_llvm_context()
                                    .i32_type()
                                    .const_int(patch, false),
                            ])
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &sdk_v)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'IOS SDK Version' metadata failed to set up.",
                        );
                    });
            }
        }

        {
            let target: &LLVMTarget = llvm_backend.get_target();
            let is_nvptx: bool = target.get_normalized_target_triple().is_nvptx_arch();
            let default_nvvmir_version: (u64, u64) = (1, 11);

            //  https://docs.nvidia.com/cuda/nvvm-ir-spec/index.html

            let generate_nvvmir_version = |nvidia_cuda_version: (u64, u64)| {
                let major: u64 = nvidia_cuda_version.0;
                let minor: u64 = nvidia_cuda_version.1;

                let cuda_version: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_int(major, false)
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_int(minor, false)
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("nvvmir.version", &cuda_version)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Nvidia Cuda Version' metadata failed to set up.",
                        );
                    });
            };

            if is_nvptx {
                generate_nvvmir_version(default_nvvmir_version)
            } else {
                if let Some(specific_nvidia_cuda_version) =
                    llvm_backend.get_target().get_cuda_version()
                {
                    generate_nvvmir_version(specific_nvidia_cuda_version);
                }
            }
        }

        {
            let triple_formatted: String = self
                .get_codegen_context()
                .get_target_triple()
                .as_str()
                .to_string_lossy()
                .to_string();

            let target_triple: LLVMTargetTriple = LLVMTargetTriple::new(triple_formatted);

            let abi: &str = target_triple.get_abi();

            if abi != "unknown" {
                let metadata: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_error,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("target-abi")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string(abi)
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &metadata)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Target ABI' metadata failed to set up.",
                        );
                    });
            }
        }

        {
            let is_no_pic: bool = matches!(
                llvm_backend.get_reloc_mode(),
                RelocMode::Static | RelocMode::Default
            );

            if is_no_pic
                || llvm_backend.is_full_jit() && !llvm_backend.omit_direct_access_external_data()
            {
                let direct_access_external_data: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_max,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("direct-access-external-data")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .i32_type()
                            .const_int(1, false)
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &direct_access_external_data)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Direct Access External Data' metadata failed to set up.",
                        );
                    });
            }
        }

        {
            if let Some(target_triple_darwin_variant) =
                llvm_backend.get_target().get_target_triple_darwin_variant()
            {
                let code_level: MetadataValue = self
                    .get_codegen_context()
                    .get_llvm_context()
                    .metadata_node(&[
                        lvl_error,
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string("darwin.target_variant.triple")
                            .into(),
                        self.get_codegen_context()
                            .get_llvm_context()
                            .metadata_string(
                                target_triple_darwin_variant
                                    .as_str()
                                    .to_string_lossy()
                                    .as_ref(),
                            )
                            .into(),
                    ]);

                self.get_codegen_context()
                    .get_llvm_module()
                    .add_global_metadata("llvm.module.flags", &code_level)
                    .unwrap_or_else(|_| {
                        thrustc_logging::print_warning(
                            LoggingType::Warning,
                            "'Darwin Target Triple' metadata failed to set up.",
                        );
                    });
            }
        }

        {
            let is_pic: bool = matches!(llvm_backend.get_reloc_mode(), RelocMode::PIC);

            if !llvm_backend.omit_rtlibusegot() {
                let triple_formatted: String = self
                    .get_codegen_context()
                    .get_target_triple()
                    .as_str()
                    .to_string_lossy()
                    .to_string();

                let target_triple: LLVMTargetTriple = LLVMTargetTriple::new(triple_formatted);

                if target_triple.is_arm_family() && is_pic && target_triple.has_posix_thread_model()
                {
                    let rt_lib_use_got: MetadataValue = self
                        .get_codegen_context()
                        .get_llvm_context()
                        .metadata_node(&[
                            lvl_error,
                            self.get_codegen_context()
                                .get_llvm_context()
                                .metadata_string("RtLibUseGOT")
                                .into(),
                            self.get_codegen_context()
                                .get_llvm_context()
                                .i32_type()
                                .const_int(1, false)
                                .into(),
                        ]);

                    self.get_codegen_context()
                        .get_llvm_module()
                        .add_global_metadata("llvm.module.flags", &rt_lib_use_got)
                        .unwrap_or_else(|_| {
                            thrustc_logging::print_warning(
                                LoggingType::Warning,
                                "'RtLibUseGOT' metadata failed to set up.",
                            );
                        });
                }
            }
        }

        if !llvm_backend.get_optimization().is_high_opt() && !llvm_backend.omit_frame_pointer() {
            let frame_pointer: MetadataValue = self
                .get_codegen_context()
                .get_llvm_context()
                .metadata_node(&[
                    lvl_max,
                    self.get_codegen_context()
                        .get_llvm_context()
                        .metadata_string("frame-pointer")
                        .into(),
                    self.get_codegen_context()
                        .get_llvm_context()
                        .i32_type()
                        .const_int(2, false)
                        .into(),
                ]);

            self.get_codegen_context()
                .get_llvm_module()
                .add_global_metadata("llvm.module.flags", &frame_pointer)
                .unwrap_or_else(|_| {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        "'Frame Pointer' metadata failed to set up.",
                    );
                });
        }

        if !llvm_backend.omit_uwtable() {
            let uwtable: MetadataValue = self
                .get_codegen_context()
                .get_llvm_context()
                .metadata_node(&[
                    lvl_max,
                    self.get_codegen_context()
                        .get_llvm_context()
                        .metadata_string("uwtable")
                        .into(),
                    self.get_codegen_context()
                        .get_llvm_context()
                        .i32_type()
                        .const_int(2, false)
                        .into(),
                ]);

            self.get_codegen_context()
                .get_llvm_module()
                .add_global_metadata("llvm.module.flags", &uwtable)
                .unwrap_or_else(|_| {
                    thrustc_logging::print_warning(
                        LoggingType::Warning,
                        "'Unwind Table' metadata failed to set up.",
                    );
                });
        }
    }

    fn setup_compiler_info(&self) {
        let version: MetadataValue = self
            .get_codegen_context()
            .get_llvm_context()
            .metadata_string(thrustc_constants::COMPILER_ID);

        let node: MetadataValue = self
            .get_codegen_context()
            .get_llvm_context()
            .metadata_node(&[version.into()]);

        let _ = self
            .get_codegen_context()
            .get_llvm_module()
            .add_global_metadata("llvm.ident", &node);
    }

    fn setup_build_id(&self) {
        let options: &CompilerOptions = self.get_codegen_context().get_compiler_options();
        let id: String = options.build_id().to_string();

        let build_id: MetadataValue = self
            .get_codegen_context()
            .get_llvm_context()
            .metadata_string(&id);

        let llvm_major: u32 = inkwell::support::get_llvm_version().0;
        let llvm_minor: u32 = inkwell::support::get_llvm_version().1;
        let llvm_patch: u32 = inkwell::support::get_llvm_version().2;

        let llvm_v: MetadataValue = self
            .get_codegen_context()
            .get_llvm_context()
            .metadata_string(&format!(
                "LLVM {}.{}.{}",
                llvm_major, llvm_minor, llvm_patch
            ));

        let node: MetadataValue = self
            .get_codegen_context()
            .get_llvm_context()
            .metadata_node(&[build_id.into(), llvm_v.into()]);

        self.get_codegen_context()
            .get_llvm_module()
            .add_global_metadata("build", &node)
            .unwrap_or_else(|_| {
                thrustc_logging::print_warning(
                    LoggingType::Warning,
                    "'Build Compiler Info' metadata failed to set up.",
                );
            });
    }
}

impl<'a, 'ctx> LLVMMetadata<'a, 'ctx> {
    #[inline]
    fn get_codegen_context(&self) -> &'a LLVMCodeGenContext<'a, 'ctx> {
        self.context
    }
}
