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

#![allow(clippy::result_unit_err)]

use colored::Colorize;

use thrustc_code_location::Span;
use thrustc_logging::{self, LoggingType};

#[derive(Debug, Clone)]
pub enum CompilationIssue {
    Error(CompilationIssueCode, String, String, Option<String>, Span),
    Warning(CompilationIssueCode, String, Span),

    FrontendBug(
        String,
        String,
        Span,
        CompilationPosition,
        std::path::PathBuf,
        u32,
    ),

    BackendBug(
        String,
        String,
        Span,
        CompilationPosition,
        std::path::PathBuf,
        u32,
    ),
}

impl CompilationIssue {
    #[inline]
    pub fn is_bug(&self) -> bool {
        matches!(
            self,
            CompilationIssue::FrontendBug(..) | CompilationIssue::BackendBug(..)
        )
    }
}

lazy_static::lazy_static! {
    pub static ref COMPILATION_ISSUE_CODE_EXPLANATIONS: ahash::AHashMap<CompilationIssueCode, &'static str> = {
        let mut explanations: ahash::AHashMap<CompilationIssueCode, &'static str> = ahash::AHashMap::with_capacity(u8::MAX as usize);

        explanations.insert(CompilationIssueCode::E0001, r#""#);
        explanations.insert(CompilationIssueCode::E0002, r#""#);
        explanations.insert(CompilationIssueCode::E0003, r#""#);
        explanations.insert(CompilationIssueCode::E0004, r#""#);
        explanations.insert(CompilationIssueCode::E0005, r#""#);
        explanations.insert(CompilationIssueCode::E0006, r#""#);
        explanations.insert(CompilationIssueCode::E0007, r#""#);
        explanations.insert(CompilationIssueCode::E0008, r#""#);
        explanations.insert(CompilationIssueCode::E0010, r#""#);
        explanations.insert(CompilationIssueCode::E0011, r#""#);
        explanations.insert(CompilationIssueCode::E0012, r#""#);
        explanations.insert(CompilationIssueCode::E0013, r#""#);
        explanations.insert(CompilationIssueCode::E0014, r#""#);
        explanations.insert(CompilationIssueCode::E0015, r#""#);
        explanations.insert(CompilationIssueCode::E0016, r#""#);
        explanations.insert(CompilationIssueCode::E0017, r#""#);
        explanations.insert(CompilationIssueCode::E0018, r#""#);
        explanations.insert(CompilationIssueCode::E0019, r#""#);
        explanations.insert(CompilationIssueCode::E0020, r#""#);
        explanations.insert(CompilationIssueCode::E0021, r#""#);
        explanations.insert(CompilationIssueCode::E0022, r#""#);
        explanations.insert(CompilationIssueCode::E0023, r#""#);
        explanations.insert(CompilationIssueCode::E0024, r#""#);
        explanations.insert(CompilationIssueCode::E0025, r#""#);
        explanations.insert(CompilationIssueCode::E0026, r#""#);
        explanations.insert(CompilationIssueCode::E0027, r#""#);
        explanations.insert(CompilationIssueCode::E0028, r#""#);
        explanations.insert(CompilationIssueCode::E0029, r#""#);
        explanations.insert(CompilationIssueCode::E0030, r#""#);
        explanations.insert(CompilationIssueCode::E0031, r#""#);
        explanations.insert(CompilationIssueCode::E0032, r#""#);
        explanations.insert(CompilationIssueCode::E0033, r#""#);

        explanations.insert(CompilationIssueCode::W0001, r#""#);
        explanations.insert(CompilationIssueCode::W0002, r#""#);
        explanations.insert(CompilationIssueCode::W0003, r#""#);
        explanations.insert(CompilationIssueCode::W0004, r#""#);
        explanations.insert(CompilationIssueCode::W0005, r#""#);
        explanations.insert(CompilationIssueCode::W0007, r#""#);
        explanations.insert(CompilationIssueCode::W0008, r#""#);
        explanations.insert(CompilationIssueCode::W0009, r#""#);
        explanations.insert(CompilationIssueCode::W0010, r#""#);
        explanations.insert(CompilationIssueCode::W0011, r#""#);
        explanations.insert(CompilationIssueCode::W0012, r#""#);
        explanations.insert(CompilationIssueCode::W0013, r#""#);
        explanations.insert(CompilationIssueCode::W0014, r#""#);
        explanations.insert(CompilationIssueCode::W0015, r#""#);
        explanations.insert(CompilationIssueCode::W0016, r#""#);
        explanations.insert(CompilationIssueCode::W0017, r#""#);
        explanations.insert(CompilationIssueCode::W0020, r#""#);
        explanations.insert(CompilationIssueCode::W0021, r#""#);
        explanations.insert(CompilationIssueCode::W0022, r#""#);
        explanations.insert(CompilationIssueCode::W0023, r#""#);
        explanations.insert(CompilationIssueCode::W0024, r#""#);
        explanations.insert(CompilationIssueCode::W0025, r#""#);
        explanations.insert(CompilationIssueCode::W0026, r#""#);
        explanations.insert(CompilationIssueCode::W0027, r#""#);
        explanations.insert(CompilationIssueCode::W0028, r#""#);
        explanations.insert(CompilationIssueCode::W0029, r#""#);
        explanations.insert(CompilationIssueCode::W0030, r#""#);
        explanations.insert(CompilationIssueCode::W0031, r#""#);
        explanations.insert(CompilationIssueCode::E0043, r#""#);
        explanations.insert(CompilationIssueCode::E0044, r#""#);
        explanations.insert(CompilationIssueCode::E0045, r#""#);
        explanations.insert(CompilationIssueCode::E0046, r#""#);
        explanations.insert(CompilationIssueCode::E0047, r#""#);

        explanations
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilationIssueCode {
    E0001, // Syntax Error.
    E0002, // EOF.
    E0003, // Unknown compiler built-in.
    E0004, // Duplicated.
    E0005, // Duplicated global assembler.
    E0006, // Non-constant value
    E0007, // Reference without an address
    E0008, // Value without an address
    E0010, // Possible undefined behavior.
    E0011, // Missing Attribute Error.
    E0012, // Attribute Syntax Error.
    E0013, // Attribute Situation Error.
    E0014, // Unreaceable instruction.
    E0015, // Terminator declared before.
    E0016, // Invalid Scope Position.
    E0017, // Loop Control Flow outside of a loop
    E0018, // NoSense Statement
    E0019, // Type Error
    E0020, // Mismatched Types
    E0021, // Mismatched attributes
    E0022, // Missing call arguments,
    E0023, // Mismatched call arguments
    E0024, // Unsopported call convention.
    E0025, // Unknown Compiler intrinsic.
    E0026, // Too many fields.
    E0027, // Missing fields.
    E0028, // Unknown reference.
    E0029, // Import Error.
    E0030, // Incompatible Operation
    E0031, // Unknown Operation
    E0032, // Incompatible Type Cast
    E0033, // Attribute Conflict
    E0034, // Invalid Intrinsic compiler syntax
    E0035, // Import Error
    E0036, // Too many parameters
    E0037, // Too many depth,
    E0038, // Not Mutable
    E0039, // Unsupported Native Type
    E0040, // Not Found
    E0041, // Unresolved Type
    E0042, // Type could not be determined
    E0043, // Ambiguous imported name
    E0044, // Unknown named argument.
    E0045, // Duplicated named argument.
    E0046, // Positional argument after a named argument.
    E0047, // Variable arguments builtin outside of a variadic function.

    W0001, // Irrelevant Attribute
    W0002, // Unknown Call Convention
    W0003, // Unknown Linkage
    W0004, // Attribute Conflict,
    W0005, // Local not used
    W0007, // LLI not used
    W0008, // Parameter not used,
    W0009, // Static not used,
    W0010, // Constant no used,
    W0011, // Assembler Function not used
    W0012, // Enum not used,
    W0013, // Enum field not used,
    W0014, // Intrinsic not Used
    W0015, // Strucuture not Used,
    W0016, // Structure Field not Used,
    W0017, // Function not used
    W0018, // Circular Import
    W0019, // Unstable Feature
    W0020, // Mutable but never mutated
    W0021, // Name shadows an outer declaration
    W0022, // Self assignment
    W0023, // Empty block
    W0024, // Non-standard naming
    W0025, // Possible infinite loop
    W0026, // Tautological comparison
    W0027, // Dead store
    W0028, // Statement with no effect
    W0029, // Condition always constant
    W0030, // Module signature without public may fail at linking
    W0031, // Compilation warning
}

#[inline]
pub fn filter_warnings(to: &[CompilationIssueCode], from: &mut Vec<CompilationIssue>) {
    from.retain(|issue| match issue {
        CompilationIssue::Warning(code, _, _) => !to.contains(code),
        _ => true,
    });
}

impl CompilationIssueCode {
    pub fn to_title(self) -> String {
        match self {
            CompilationIssueCode::E0001 => format!("SYNTAX ERROR - {}", "E0001".bright_red()),
            CompilationIssueCode::E0002 => {
                format!("UNEXPECTED END OF FILE - {}", "E0002".bright_red())
            }
            CompilationIssueCode::E0003 => {
                format!("UNKNOWN COMPILER BUILT-IN - {}", "E0003".bright_red())
            }
            CompilationIssueCode::E0004 => {
                format!("ALREADY DEFINED OR DECLARED - {}", "E0004".bright_red())
            }
            CompilationIssueCode::E0005 => {
                format!("DUPLICATED GLOBAL ASSEMBLER - {}", "E0005".bright_red())
            }
            CompilationIssueCode::E0006 => format!("NON-CONSTANT VALUE - {}", "E0006".bright_red()),
            CompilationIssueCode::E0007 => {
                format!("REFERENCE WITHOUT ADDRESS - {}", "E0007".bright_red())
            }
            CompilationIssueCode::E0008 => {
                format!("VALUE WITHOUT ADDRESS - {}", "E0008".bright_red())
            }
            CompilationIssueCode::E0010 => {
                format!("POSSIBLE UNDEFINED BEHAVIOR - {}", "E0010".bright_red())
            }
            CompilationIssueCode::E0011 => format!("MISSING ATTRIBUTE - {}", "E0011".bright_red()),
            CompilationIssueCode::E0012 => {
                format!("ATTRIBUTE SYNTAX ERROR - {}", "E0012".bright_red())
            }
            CompilationIssueCode::E0013 => {
                format!("ATTRIBUTE SITUATION ERROR - {}", "E0013".bright_red())
            }
            CompilationIssueCode::E0014 => {
                format!("UNREACHABLE INSTRUCTION - {}", "E0014".bright_red())
            }
            CompilationIssueCode::E0015 => {
                format!("TERMINATOR ALREADY DECLARED - {}", "E0015".bright_red())
            }
            CompilationIssueCode::E0016 => {
                format!("INVALID SCOPE POSITION - {}", "E0016".bright_red())
            }
            CompilationIssueCode::E0017 => {
                format!("LOOP CONTROL FLOW OUTSIDE LOOP - {}", "E0017".bright_red())
            }
            CompilationIssueCode::E0018 => format!("NOSENSE STATEMENT - {}", "E0018".bright_red()),
            CompilationIssueCode::E0019 => format!("TYPE ERROR - {}", "E0019".bright_red()),
            CompilationIssueCode::E0020 => format!("MISMATCHED TYPES - {}", "E0020".bright_red()),
            CompilationIssueCode::E0021 => {
                format!("MISMATCHED ATTRIBUTES - {}", "E0021".bright_red())
            }
            CompilationIssueCode::E0022 => {
                format!("MISSING CALL ARGUMENTS - {}", "E0022".bright_red())
            }
            CompilationIssueCode::E0023 => {
                format!("MISMATCHED CALL ARGUMENTS - {}", "E0023".bright_red())
            }
            CompilationIssueCode::E0024 => {
                format!("UNSUPPORTED CALL CONVENTION - {}", "E0024".bright_red())
            }
            CompilationIssueCode::E0025 => {
                format!("UNKNOWN COMPILER INTRINSIC - {}", "E0025".bright_red())
            }
            CompilationIssueCode::E0026 => {
                format!("TOO MANY FIELDS - {}", "E0026".bright_red())
            }
            CompilationIssueCode::E0027 => {
                format!("MISSING FIELDS - {}", "E0027".bright_red())
            }
            CompilationIssueCode::E0028 => {
                format!("UNKNOWN REFERENCE - {}", "E0028".bright_red())
            }
            CompilationIssueCode::E0029 => {
                format!("IMPORT ERROR - {}", "E0029".bright_red())
            }
            CompilationIssueCode::E0030 => {
                format!("INCOMPATIBLE OPERATION - {}", "E0030".bright_red())
            }
            CompilationIssueCode::E0031 => {
                format!("UNKNOWN OPERATION - {}", "E0031".bright_red())
            }
            CompilationIssueCode::E0032 => {
                format!("INCOMPATIBLE TYPE CAST - {}", "E0032".bright_red())
            }
            CompilationIssueCode::E0033 => {
                format!("ATTRIBUTE CONFLICT - {}", "E0033".bright_red())
            }
            CompilationIssueCode::E0034 => {
                format!("INTRINSIC SYNTAX - {}", "E0034".bright_red())
            }
            CompilationIssueCode::E0035 => {
                format!("IMPORT ERROR - {}", "E0035".bright_red())
            }
            CompilationIssueCode::E0036 => {
                format!("TOO MANY PARAMETERS - {}", "E0036".bright_red())
            }
            CompilationIssueCode::E0037 => {
                format!("TOO DEEP - {}", "E0037".bright_red())
            }
            CompilationIssueCode::E0038 => {
                format!("NOT MUTABLE - {}", "E0038".bright_red())
            }
            CompilationIssueCode::E0039 => {
                format!("UNSUPPORTED NATIVE TYPE - {}", "E0039".bright_red())
            }
            CompilationIssueCode::E0040 => {
                format!("NOT FOUND - {}", "E0040".bright_red())
            }
            CompilationIssueCode::E0041 => {
                format!("UNRESOLVED TYPE - {}", "E0041".bright_red())
            }
            CompilationIssueCode::E0042 => {
                format!("TYPE COULD NOT BE DETERMINED - {}", "E0042".bright_red())
            }
            CompilationIssueCode::E0043 => {
                format!("AMBIGUOUS IMPORTED NAME - {}", "E0043".bright_red())
            }
            CompilationIssueCode::E0044 => {
                format!("UNKNOWN NAMED ARGUMENT - {}", "E0044".bright_red())
            }
            CompilationIssueCode::E0045 => {
                format!("DUPLICATED NAMED ARGUMENT - {}", "E0045".bright_red())
            }
            CompilationIssueCode::E0046 => {
                format!("POSITIONAL AFTER NAMED ARGUMENT - {}", "E0046".bright_red())
            }
            CompilationIssueCode::E0047 => {
                format!(
                    "VARIABLE ARGUMENTS BUILTIN OUTSIDE OF A VARIADIC FUNCTION - {}",
                    "E0047".bright_red()
                )
            }
            CompilationIssueCode::W0001 => {
                format!("IRRELEVANT ATTRIBUTE - {}", "W0001".bright_yellow())
            }
            CompilationIssueCode::W0002 => {
                format!("UNKNOWN CALL CONVENTION - {}", "W0002".bright_yellow())
            }
            CompilationIssueCode::W0003 => format!("UNKNOWN LINKAGE - {}", "W0003".bright_yellow()),
            CompilationIssueCode::W0004 => {
                format!("ATTRIBUTE CONFLICT - {}", "W0004".bright_yellow())
            }
            CompilationIssueCode::W0005 => {
                format!("UNUSED LOCAL VARIABLE - {}", "W0005".bright_yellow())
            }
            CompilationIssueCode::W0007 => format!("UNUSED LLI - {}", "W0007".bright_yellow()),
            CompilationIssueCode::W0008 => {
                format!("UNUSED PARAMETER - {}", "W0008".bright_yellow())
            }
            CompilationIssueCode::W0009 => format!("UNUSED STATIC - {}", "W0009".bright_yellow()),
            CompilationIssueCode::W0010 => format!("UNUSED CONSTANT - {}", "W0010".bright_yellow()),
            CompilationIssueCode::W0011 => {
                format!("UNUSED ASSEMBLER FUNCTION - {}", "W0011".bright_yellow())
            }
            CompilationIssueCode::W0012 => format!("UNUSED ENUM - {}", "W0012".bright_yellow()),
            CompilationIssueCode::W0013 => {
                format!("UNUSED ENUM FIELD - {}", "W0013".bright_yellow())
            }
            CompilationIssueCode::W0014 => {
                format!("UNUSED INTRINSIC - {}", "W0014".bright_yellow())
            }
            CompilationIssueCode::W0015 => {
                format!("UNUSED STRUCTURE - {}", "W0015".bright_yellow())
            }
            CompilationIssueCode::W0016 => {
                format!("UNUSED STRUCTURE FIELD - {}", "W0016".bright_yellow())
            }
            CompilationIssueCode::W0017 => {
                format!("UNUSED FUNCTION - {}", "W0017".bright_yellow())
            }
            CompilationIssueCode::W0018 => {
                format!("CIRCULAR IMPORT - {}", "W0018".bright_yellow())
            }
            CompilationIssueCode::W0019 => {
                format!("UNSTABLE FEATURE - {}", "W0019".bright_yellow())
            }
            CompilationIssueCode::W0020 => {
                format!("MUTABLE BUT NEVER MUTATED - {}", "W0020".bright_yellow())
            }
            CompilationIssueCode::W0021 => {
                format!("NAME SHADOWS OUTER DECLARATION - {}", "W0021".bright_yellow())
            }
            CompilationIssueCode::W0022 => {
                format!("SELF ASSIGNMENT - {}", "W0022".bright_yellow())
            }
            CompilationIssueCode::W0023 => {
                format!("EMPTY BLOCK - {}", "W0023".bright_yellow())
            }
            CompilationIssueCode::W0024 => {
                format!("NON-STANDARD NAMING - {}", "W0024".bright_yellow())
            }
            CompilationIssueCode::W0025 => {
                format!("POSSIBLE INFINITE LOOP - {}", "W0025".bright_yellow())
            }
            CompilationIssueCode::W0026 => {
                format!("TAUTOLOGICAL COMPARISON - {}", "W0026".bright_yellow())
            }
            CompilationIssueCode::W0027 => {
                format!("DEAD STORE - {}", "W0027".bright_yellow())
            }
            CompilationIssueCode::W0028 => {
                format!("STATEMENT WITH NO EFFECT - {}", "W0028".bright_yellow())
            }
            CompilationIssueCode::W0029 => {
                format!("CONDITION ALWAYS CONSTANT - {}", "W0029".bright_yellow())
            }
            CompilationIssueCode::W0030 => {
                format!(
                    "MODULE SIGNATURE WITHOUT PUBLIC - {}",
                    "W0030".bright_yellow()
                )
            }
            CompilationIssueCode::W0031 => {
                format!("COMPILATION WARNING - {}", "W0031".bright_yellow())
            }
        }
    }

    pub fn get_explanation(&self) -> &str {
        COMPILATION_ISSUE_CODE_EXPLANATIONS
            .get(self)
            .unwrap_or_else(|| {
                thrustc_logging::print_warning(
                    LoggingType::Warning,
                    &format!(
                        "Unable to get the properly '{}' issue explanation.",
                        self.to_title()
                    ),
                );

                &""
            })
    }
}

impl CompilationIssueCode {
    pub fn parse(n: &str) -> Result<Self, ()> {
        match n {
            "E0001" => Ok(CompilationIssueCode::E0001),
            "E0002" => Ok(CompilationIssueCode::E0002),
            "E0003" => Ok(CompilationIssueCode::E0003),
            "E0004" => Ok(CompilationIssueCode::E0004),
            "E0005" => Ok(CompilationIssueCode::E0005),
            "E0006" => Ok(CompilationIssueCode::E0006),
            "E0007" => Ok(CompilationIssueCode::E0007),
            "E0008" => Ok(CompilationIssueCode::E0008),
            "E0010" => Ok(CompilationIssueCode::E0010),
            "E0011" => Ok(CompilationIssueCode::E0011),
            "E0012" => Ok(CompilationIssueCode::E0012),
            "E0013" => Ok(CompilationIssueCode::E0013),
            "E0014" => Ok(CompilationIssueCode::E0014),
            "E0015" => Ok(CompilationIssueCode::E0015),
            "E0016" => Ok(CompilationIssueCode::E0016),
            "E0017" => Ok(CompilationIssueCode::E0017),
            "E0018" => Ok(CompilationIssueCode::E0018),
            "E0019" => Ok(CompilationIssueCode::E0019),
            "E0020" => Ok(CompilationIssueCode::E0020),
            "E0021" => Ok(CompilationIssueCode::E0021),
            "E0022" => Ok(CompilationIssueCode::E0022),
            "E0023" => Ok(CompilationIssueCode::E0023),
            "E0024" => Ok(CompilationIssueCode::E0024),
            "E0025" => Ok(CompilationIssueCode::E0025),
            "E0026" => Ok(CompilationIssueCode::E0026),
            "E0027" => Ok(CompilationIssueCode::E0027),
            "E0028" => Ok(CompilationIssueCode::E0028),
            "E0029" => Ok(CompilationIssueCode::E0029),
            "E0030" => Ok(CompilationIssueCode::E0030),
            "E0031" => Ok(CompilationIssueCode::E0031),
            "E0032" => Ok(CompilationIssueCode::E0032),
            "E0033" => Ok(CompilationIssueCode::E0033),
            "E0042" => Ok(CompilationIssueCode::E0042),
            "E0043" => Ok(CompilationIssueCode::E0043),
            "E0044" => Ok(CompilationIssueCode::E0044),
            "E0045" => Ok(CompilationIssueCode::E0045),
            "E0046" => Ok(CompilationIssueCode::E0046),

            "W0001" => Ok(CompilationIssueCode::W0001),
            "W0002" => Ok(CompilationIssueCode::W0002),
            "W0003" => Ok(CompilationIssueCode::W0003),
            "W0004" => Ok(CompilationIssueCode::W0004),
            "W0005" => Ok(CompilationIssueCode::W0005),
            "W0007" => Ok(CompilationIssueCode::W0007),
            "W0008" => Ok(CompilationIssueCode::W0008),
            "W0009" => Ok(CompilationIssueCode::W0009),
            "W0010" => Ok(CompilationIssueCode::W0010),
            "W0011" => Ok(CompilationIssueCode::W0011),
            "W0012" => Ok(CompilationIssueCode::W0012),
            "W0013" => Ok(CompilationIssueCode::W0013),
            "W0014" => Ok(CompilationIssueCode::W0014),
            "W0015" => Ok(CompilationIssueCode::W0015),
            "W0016" => Ok(CompilationIssueCode::W0016),
            "W0017" => Ok(CompilationIssueCode::W0017),
            "W0020" => Ok(CompilationIssueCode::W0020),
            "W0021" => Ok(CompilationIssueCode::W0021),
            "W0022" => Ok(CompilationIssueCode::W0022),
            "W0023" => Ok(CompilationIssueCode::W0023),
            "W0024" => Ok(CompilationIssueCode::W0024),
            "W0025" => Ok(CompilationIssueCode::W0025),
            "W0026" => Ok(CompilationIssueCode::W0026),
            "W0027" => Ok(CompilationIssueCode::W0027),
            "W0028" => Ok(CompilationIssueCode::W0028),
            "W0029" => Ok(CompilationIssueCode::W0029),
            "W0030" => Ok(CompilationIssueCode::W0030),
            "W0031" => Ok(CompilationIssueCode::W0031),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompilationPosition {
    Lexer,
    Parser,
    TypeChecker,
    Analyzer,
    Linter,
    LLVMBackend,
}

impl std::fmt::Display for CompilationPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer => write!(f, "{}", "Lexer".bright_blue().bold()),
            Self::Parser => write!(f, "{}", "Parser".red().bold()),
            Self::TypeChecker => write!(f, "{}", "Type Checker".bright_yellow().bold()),
            Self::Analyzer => write!(f, "{}", "Analyzer".bright_blue().bold()),
            Self::Linter => write!(f, "{}", "Linter".bright_magenta().bold()),
            Self::LLVMBackend => write!(f, "{}", "LLVMBackend".bright_red().bold()),
        }
    }
}
