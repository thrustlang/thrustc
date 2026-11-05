<img src= "https://github.com/thrustlang/.github/blob/main/assets/logos/new%20logo/thrustlang-logo-banner-text-italic.png" alt= "logo" style= "width: 80%; height: 80%;"></img>

# The Thrust compiler

<img src="https://github.com/thrustlang/.github/blob/main/assets/standard-text-separator.png" alt="standard-separator" style="width: 1hv;">

The compiler reports diagnostics like these:

Use `--enable-ansi-color` to display them in color. The option is off by default.

### Errors

#### Type mismatch

<img src= "https://github.com/thrustlang/thrustc/blob/master/assets/examples/diagnostics/TypeMismatchDiagnostic.png"></img>

#### Value without address

<img src= "https://github.com/thrustlang/thrustc/blob/master/assets/examples/diagnostics/ValueWithoutAddressDiagnostic.png"></img>

#### Invalid atomic operation

`E0056` reports an atomic operation whose target has no atomic ordering, or whose ordering arguments are not valid for LLVM. Use `thrustc --explain E0056` for details.

### Warnings

#### Unknown call convention

<img src= "https://github.com/thrustlang/thrustc/blob/master/assets/examples/diagnostics/UnknownCallConventionWarningDiagnostic.png"></img>

#### Attribute conflict

<img src= "https://github.com/thrustlang/thrustc/blob/master/assets/examples/diagnostics/AttributeConflictWarningDiagnostic.png"></img>

Use `thrustc --explain W0004` or `thrustc --explain E0007` to understand a diagnostic. The compiler explains why the issue exists in the code when an explanation is available.
