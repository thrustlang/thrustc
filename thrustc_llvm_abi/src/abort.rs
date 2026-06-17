use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_llvm_nvidia_cuda_abi::CudaABIContext;
use thrustc_llvm_system_v_abi::SystemVABIContext;
use thrustc_logging::LoggingType;
use thrustc_span::Span;

pub fn abort_system_v_abi_codegen(
    context: &mut SystemVABIContext,
    message: &str,
    span: Span,
    file: std::path::PathBuf,
    line: u32,
) -> ! {
    let diagnostician: &mut Diagnostician = context.get_mut_diagnostician();

    diagnostician.dispatch_diagnostic(
        &CompilationIssue::BackendBug(
            "Failed to Compile".into(),
            message.into(),
            span,
            CompilationPosition::LLVMBackend,
            file,
            line,
        ),
        LoggingType::BackendBug,
    );

    std::process::exit(thrustc_constants::FAILURE_CODE);
}

pub fn abort_cuda_abi_codegen(
    context: &mut CudaABIContext,
    message: &str,
    span: Span,
    file: std::path::PathBuf,
    line: u32,
) -> ! {
    let diagnostician: &mut Diagnostician = context.get_mut_diagnostician();

    diagnostician.dispatch_diagnostic(
        &CompilationIssue::BackendBug(
            "Failed to Compile".into(),
            message.into(),
            span,
            CompilationPosition::LLVMBackend,
            file,
            line,
        ),
        LoggingType::BackendBug,
    );

    std::process::exit(thrustc_constants::FAILURE_CODE);
}
