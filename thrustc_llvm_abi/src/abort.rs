use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_llvm_x86_abi::X86SystemVABIContext;
use thrustc_logging::LoggingType;
use thrustc_span::Span;

pub fn abort_system_v_abi_codegen(
    context: &mut X86SystemVABIContext,
    message: &str,
    span: Span,
    file: std::path::PathBuf,
    line: u32,
) -> ! {
    let diagnostician: &mut Diagnostician = context.get_mut_diagnostician();

    diagnostician.dispatch_diagnostic(
        &CompilationIssue::BackenEndBug(
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
