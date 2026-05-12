use ahash::AHashMap as HashMap;
use lazy_static::lazy_static;
use thrustc_backends::{
    ThrustCodeModel, ThrustRelocMode,
    llvm::{cpu::LLVMTargetCPU, debug::DwarfVersion},
};

lazy_static! {
    static ref LLVM_AVAILABLE_DIRECTIVES_VALUES: HashMap<String, &'static [&'static str]> = {
        let mut map: HashMap<String, &'static [&'static str]> = HashMap::new();

        map.insert("ENABLE_DEBUG_INFO".into(), &["true", "false"]);
        map.insert("DWARF_VERSION".into(), &["v4", "v5"]);

        map
    };
}

pub const LLVM_AVAILABLE_DIRECTIVES: &[&str] = &[
    "TARGET",
    "TARGET_TRIPLE",
    "CPU",
    "CPU_FEATURES",
    "RELOC_MODEL",
    "CODE_MODEL",
    "IOS_VERSION",
    "MACOS_VERSION",
    "ENABLE_DEBUG_INFO",
    "DWARF_VERSION",
];

#[derive(Debug)]
pub struct CompilerDirectiveContext {
    llvm_directive: LLVMDirective,
}

impl CompilerDirectiveContext {
    pub fn new() -> Self {
        Self {
            llvm_directive: LLVMDirective::new(),
        }
    }
}

#[derive(Debug)]
pub struct LLVMDirective {
    target: String,
    target_triple: String,
    cpu: LLVMTargetCPU,
    reloc_model: ThrustRelocMode,
    code_model: ThrustCodeModel,
    dwarf_version: DwarfVersion,

    enable: bool,
}

impl LLVMDirective {
    pub fn new() -> Self {
        Self {
            target: "".into(),
            target_triple: "".into(),
            cpu: LLVMTargetCPU {
                target_cpu: "".into(),
                target_cpu_features: "".into(),
            },
            reloc_model: ThrustRelocMode::Default,
            code_model: ThrustCodeModel::Default,
            dwarf_version: DwarfVersion::V4,
            enable: false,
        }
    }
}
